library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(digest)

dir_helm <- path.expand("~/helm_mmlu")
dir_runs <- file.path(dir_helm, "runs")
dir_v1   <- file.path(dir_runs, "v1.0.0")
dir_run  <- list.files(dir_v1, full.names = TRUE)
dir_run  <- dir_run[file.info(dir_run)$isdir]

spec <- function(dir_one) {
  fp <- file.path(dir_one, "run_spec.json")
  sp <- read_json(fp, simplifyVector = FALSE)
  rs <- if (!is.null(sp$run_spec)) sp$run_spec else sp
  sc <- rs$scenario_spec
  ad <- rs$adapter_spec
  sb <- "unknown_subject"
  if (!is.null(sc$args$subject)) sb <- sc$args$subject
  ds <- paste0("mmlu:subject=", sb)
  md <- "unknown_model"
  if (!is.null(ad$model)) {
    md <- ad$model
  } else if (!is.null(ad$model_deployment)) {
    md <- ad$model_deployment
  }
  tibble(run_dir = basename(dir_one), dataset_id = ds, model = md)
}

hash <- function(dir_one) {
  fp <- file.path(dir_one, "run_spec.json")
  sp <- read_json(fp, simplifyVector = FALSE)
  rs <- if (!is.null(sp$run_spec)) sp$run_spec else sp
  if (!is.null(rs$adapter_spec$model)) rs$adapter_spec$model <- NULL
  if (!is.null(rs$adapter_spec$model_deployment)) rs$adapter_spec$model_deployment <- NULL
  digest(rs, algo = "xxhash64")
}

run <- bind_rows(lapply(dir_run, spec))
run$cfg_hash <- vapply(dir_run, hash, character(1))

met_pool <- c(
  "exact_match",
  "inference_runtime",
  "logprob",
  "perplexity",
  "num_output_tokens",
  "num_bytes"
)

low <- c(
  "inference_runtime",
  "num_output_tokens",
  "num_bytes",
  "perplexity"
)

met_tbl <- function(stats, met_pool) {
  nm <- vapply(
    stats,
    function(x) {
      if (is.null(x$name) || is.null(x$name$name) || length(x$name$name) == 0L) NA_character_ else as.character(x$name$name[1])
    },
    character(1)
  )
  mu <- vapply(
    stats,
    function(x) {
      if (is.null(x$mean) || length(x$mean) == 0L) NA_real_ else as.numeric(x$mean[1])
    },
    numeric(1)
  )
  tb <- tibble(metric = nm, score = mu)
  tb <- tb[!is.na(tb$metric) & !is.na(tb$score) & tb$metric %in% met_pool, ]
  tb
}

fp_stats <- vapply(run$run_dir, function(rd) file.path(dir_v1, rd, "stats.json"), character(1))
st_raw <- lapply(fp_stats, function(fp) read_json(fp, simplifyVector = FALSE))

long_raw <- bind_rows(lapply(
  seq_along(st_raw),
  function(i) {
    mt <- met_tbl(st_raw[[i]], met_pool)
    mt$dataset_id <- run$dataset_id[i]
    mt$model      <- run$model[i]
    mt$cfg_hash   <- run$cfg_hash[i]
    mt
  }
))

cfg <- select(
  ungroup(
    slice_max(
      group_by(
        count(long_raw, dataset_id, model, cfg_hash, name = "n_runs"),
        dataset_id, model
      ),
      n_runs,
      n = 1,
      with_ties = FALSE
    )
  ),
  dataset_id, model, cfg_hash
)

long <- summarise(
  group_by(
    select(
      inner_join(long_raw, cfg, by = c("dataset_id", "model", "cfg_hash")),
      -cfg_hash
    ),
    dataset_id, model, metric
  ),
  score = mean(score, na.rm = TRUE),
  .groups = "drop"
)

long <- filter(mutate(long, score = as.numeric(score)), is.finite(score))

long <- mutate(
  long,
  score = if_else(metric %in% c("num_output_tokens", "num_bytes") & score <= 0, NA_real_, score)
)

long <- filter(long, !is.na(score))

long <- mutate(long, aligned = if_else(metric %in% low, -score, score))

wide <- function(long, ds, met) {
  pivot_wider(
    select(filter(long, dataset_id == ds, metric %in% met), model, metric, aligned),
    names_from = metric,
    values_from = aligned
  )
}

top <- function(wd, sel, n) wd

eps <- function(v, rel, abs) {
  x <- v[is.finite(v)]
  if (length(x) == 0L) return(abs)
  sc <- max(1, max(abs(x)))
  max(abs, rel * sc)
}

maj <- function(U, eps_vec) {
  md <- colnames(U)
  n  <- length(md)
  bt <- matrix(FALSE, nrow = n, ncol = n, dimnames = list(md, md))
  for (i in seq_len(n)) for (j in seq_len(n)) if (i != j) {
    a <- U[, i]
    b <- U[, j]
    ok <- !is.na(a) & !is.na(b) & (abs(a - b) > eps_vec)
    k  <- sum(ok)
    if (k < nrow(U)) next
    v  <- sum(a[ok] > b[ok])
    bt[i, j] <- (v > (k - v))
  }
  bt
}

### Search for Condorcet cycles

cyc_all <- function(bt) {
  md <- rownames(bt)
  if (length(md) < 3L) return(list())
  tr <- combn(md, 3, simplify = FALSE)
  out <- list()
  for (t in tr) {
    a <- t[1]; b <- t[2]; c <- t[3]
    if (bt[a, b] && bt[b, c] && bt[c, a]) out[[length(out) + 1L]] <- c(a, b, c)
    if (bt[a, c] && bt[c, b] && bt[b, a]) out[[length(out) + 1L]] <- c(a, c, b)
  }
  out
}

buf <- function(cyc, U, eps_vec) {
  ed <- list(c(cyc[1], cyc[2]), c(cyc[2], cyc[3]), c(cyc[3], cyc[1]))
  bf <- function(a, b) {
    d <- U[, a] - U[, b]
    ok <- is.finite(d) & (abs(d) > eps_vec)
    vt <- ok & (d > 0)
    if (!any(vt)) return(-Inf)
    min(d[vt] - eps_vec[vt])
  }
  min(vapply(ed, function(e) bf(e[1], e[2]), numeric(1)))
}

scan_one <- function(long, ds, met_pool, rel, abs) {
  av <- unique(filter(long, dataset_id == ds)$metric)
  mp <- intersect(met_pool, av)
  if (length(mp) < 3L) return(NULL)
  wd <- wide(long, ds, mp)
  if (nrow(wd) < 3L) return(NULL)
  tr <- combn(mp, 3, simplify = FALSE)
  best <- NULL
  best_b <- -Inf
  for (ms in tr) {
    sb0 <- select(wd, model, all_of(ms))
    sb  <- sb0[complete.cases(sb0), ]
    if (nrow(sb) < 3L) next
    U <- t(as.matrix(select(sb, all_of(ms))))
    colnames(U) <- sb$model
    e <- vapply(seq_len(nrow(U)), function(i) eps(U[i, ], rel, abs), numeric(1))
    bt <- maj(U, e)
    cs <- cyc_all(bt)
    if (length(cs) == 0L) next
    bs <- vapply(cs, function(cy) buf(cy, U, e), numeric(1))
    j  <- which.max(bs)
    if (bs[j] > best_b) {
      best_b <- bs[j]
      best <- list(dataset_id = ds, metrics = ms, models = cs[[j]], buffer = bs[j], wide = sb)
    }
  }
  best
}
ds_all <- sort(unique(long$dataset_id))

rel <- 1e-4
abs <- 0

hit <- setNames(lapply(ds_all, function(ds) scan_one(long, ds, met_pool, rel, abs)), ds_all)

has <- !vapply(hit, is.null, logical(1))

res <- arrange(
  tibble(
    dataset_id = ds_all,
    has_cycle  = has,
    metrics    = vapply(hit, function(x) if (is.null(x)) NA_character_ else paste(x$metrics, collapse = ", "), character(1)),
    models     = vapply(hit, function(x) if (is.null(x)) NA_character_ else paste(x$models, collapse = " -> "), character(1)),
    buffer     = vapply(hit, function(x) if (is.null(x)) NA_real_ else x$buffer, numeric(1))
  ),
  desc(has_cycle),
  desc(buffer)
)

print(sum(res$has_cycle, na.rm = TRUE))
print(res, n = Inf)

###Examine one dataset

ds_ex <- "mmlu:subject=formal_logic"
ex <- hit[[ds_ex]]

cert <- NULL
if (!is.null(ex)) {
  cert <- arrange(
    mutate(
      pivot_longer(
        mutate(
          select(filter(ex$wide, model %in% ex$models), model, all_of(ex$metrics)),
          model = factor(model, levels = ex$models)
        ),
        -model,
        names_to = "metric",
        values_to = "aligned"
      ),
      metric = factor(metric, levels = ex$metrics)
    ),
    metric,
    model
  )
  print(cert, n = Inf)
} 


### Instability to changes in the model set

M <- c("exact_match", "inference_runtime", "num_bytes")

wd <- function(long, ds, M) {
  pivot_wider(
    select(filter(long, dataset_id == ds, metric %in% M), model, metric, aligned),
    names_from = metric,
    values_from = aligned
  )
}

rk <- function(x, e) {
  out <- rep(NA_real_, length(x))
  ok <- is.finite(x)
  if (!any(ok)) return(out)
  
  idx <- which(ok)
  xok <- x[idx]
  
  o  <- order(xok, decreasing = TRUE)
  xs <- xok[o]
  n  <- length(xs)
  
  blk <- integer(n)
  b <- 1L
  blk[1L] <- b
  if (n >= 2L) {
    for (i in 2L:n) {
      if (abs(xs[i - 1L] - xs[i]) <= e) blk[i] <- b else { b <- b + 1L; blk[i] <- b }
    }
  }
  
  r <- numeric(n)
  for (bb in seq_len(max(blk))) {
    id <- which(blk == bb)
    r[id] <- mean(id)
  }
  
  rok <- numeric(n)
  rok[o] <- r
  out[idx] <- rok
  out
}

borda <- function(tbl, M, e) {
  stopifnot(length(e) == length(M))
  
  R <- matrix(NA_real_, nrow = nrow(tbl), ncol = length(M))
  rownames(R) <- tbl$model
  colnames(R) <- M
  
  for (j in seq_along(M)) {
    mt <- M[j]
    R[, j] <- rk(tbl[[mt]], e[j])
  }
  
  a <- rowMeans(R)
  names(a) <- tbl$model
  list(a = a, R = R)
}

soc <- function(a, A, B) {
  if (!(A %in% names(a)) || !(B %in% names(a))) return(list(dir = NA_integer_, mar = NA_real_))
  d <- a[B] - a[A]
  if (!is.finite(d) || d == 0) return(list(dir = 0L, mar = 0))
  list(dir = ifelse(d > 0, 1L, -1L), mar = abs(d))
}

sig <- function(all, A, B, M, e) {
  ra <- all[all$model == A, M, drop = FALSE]
  rb <- all[all$model == B, M, drop = FALSE]
  if (nrow(ra) != 1L || nrow(rb) != 1L) return(NA_character_)
  
  dif <- as.numeric(ra[1L, ] - rb[1L, ])
  s   <- integer(length(M))
  
  for (j in seq_along(M)) {
    if (!is.finite(dif[j]) || abs(dif[j]) <= e[j]) s[j] <- 0L
    else if (dif[j] > 0) s[j] <- 1L
    else s[j] <- -1L
  }
  
  paste(s, collapse = ",")
}

wit_add1 <- function(long, ds, M, n = 15L, anchor = "exact_match", rel = 1e-4, abs = 0) {
  all0 <- wd(long, ds, M)
  
  ok <- complete.cases(all0[, M, drop = FALSE])
  all <- all0[ok, , drop = FALSE]
  
  if (nrow(all) < (n + 1L)) return(NULL)
  if (!(anchor %in% names(all))) return(NULL)
  
  e <- vapply(M, function(mt) eps(all[[mt]], rel, abs), numeric(1))
  names(e) <- M
  
  o <- order(all[[anchor]], decreasing = TRUE)
  base <- all[o, , drop = FALSE]
  base <- base[seq_len(n), , drop = FALSE]
  
  pool <- all[!(all$model %in% base$model), , drop = FALSE]
  if (nrow(pool) == 0L) return(NULL)
  
  a0 <- borda(base, M, e)$a
  pr <- combn(base$model, 2, simplify = FALSE)
  
  best <- NULL
  best_b <- -Inf
  
  for (C in pool$model) {
    ext <- rbind(base, pool[pool$model == C, , drop = FALSE])
    a1  <- borda(ext, M, e)$a
    
    for (ab in pr) {
      A <- ab[[1L]]
      B <- ab[[2L]]
      
      s0 <- soc(a0, A, B)
      s1 <- soc(a1, A, B)
      
      if (is.na(s0$dir) || is.na(s1$dir)) next
      if (s0$dir == 0L || s1$dir == 0L) next
      if (s0$dir == s1$dir) next
      
      b <- min(s0$mar, s1$mar)
      if (b > best_b) {
        best_b <- b
        best <- list(
          dataset_id = ds,
          metrics = M,
          n = n,
          anchor = anchor,
          A = A, B = B, C = C,
          signature = sig(all, A, B, M, e),
          base_dir = s0$dir, base_margin = s0$mar,
          ext_dir  = s1$dir, ext_margin  = s1$mar,
          buffer = b,
          base = base[, c("model", M), drop = FALSE],
          ext  = ext[,  c("model", M), drop = FALSE]
        )
      }
    }
  }
  
  best
}

lbs <- function(wit) {
  stopifnot(!is.null(wit))
  
  M <- wit$metrics
  before <- wit$base
  after  <- wit$ext
  
  all <- rbind(before, after[!(after$model %in% before$model), , drop = FALSE])
  e <- vapply(M, function(mt) eps(all[[mt]], 1e-4, 0), numeric(1))
  names(e) <- M
  
  ob <- borda(before, M, e)
  oa <- borda(after,  M, e)
  
  b <- tibble(model = names(ob$a), avg_rank = as.numeric(ob$a))
  b <- b[order(b$avg_rank), , drop = FALSE]
  b$pos_before <- seq_len(nrow(b))
  
  a <- tibble(model = names(oa$a), avg_rank = as.numeric(oa$a))
  a <- a[order(a$avg_rank), , drop = FALSE]
  a$pos_after <- seq_len(nrow(a))
  
  mv <- merge(b, a, by = "model", all = TRUE, suffixes = c("_before", "_after"))
  mv$delta_pos <- mv$pos_after - mv$pos_before
  mv <- mv[order(mv$pos_before), , drop = FALSE]
  
  fc <- mv[mv$model %in% c(wit$A, wit$B, wit$C),
           c("model", "pos_before", "avg_rank_before", "pos_after", "avg_rank_after", "delta_pos"),
           drop = FALSE]
  
  list(before = b, after = a, focus = fc)
}

ds_all <- sort(unique(long$dataset_id))

M_tbl2 <- c("exact_match", "inference_runtime", "num_bytes")
top_n  <- 15L
anchor <- "exact_match"
rel    <- 1e-4
abs    <- 0

wit <- setNames(
  lapply(ds_all, function(ds) {
    wit_add1(
      long   = long,
      ds     = ds,
      M      = M_tbl2,
      n      = top_n,
      anchor = anchor,
      rel    = rel,
      abs    = abs
    )
  }),
  ds_all
)

has_wit <- !vapply(wit, is.null, logical(1))
tibble(n_datasets = length(ds_all),
   n_witness  = sum(has_wit))

###Examine one dataset

ds <- "mmlu:subject=high_school_world_history"
w  <- wit_add1(long, ds, M, n = 15L, anchor = "exact_match", rel = 1e-4, abs = 0)
print(w)

if (!is.null(w)) {
  L <- lbs(w)
  print(L$before, n = Inf)
  print(L$after,  n = Inf)
  print(L$focus)
}
