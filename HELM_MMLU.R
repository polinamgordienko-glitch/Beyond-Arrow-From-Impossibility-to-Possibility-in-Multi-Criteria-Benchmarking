library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(digest)
library(lme4)
library(future)
library(future.apply)


dir_helm <- path.expand("~/helm_mmlu")
dir_runs <- file.path(dir_helm, "runs")
dir_v1   <- file.path(dir_runs, "v1.0.0")
dir_run <- list.files(dir_v1, full.names = TRUE)
dir_run <- dir_run[file.info(dir_run)$isdir]

spec_tbl <- function(dir_one) {
  fp <- file.path(dir_one, "run_spec.json")
  spec <- read_json(fp, simplifyVector = FALSE)
  rs <- if (!is.null(spec$run_spec)) spec$run_spec else spec
  scen <- rs$scenario_spec
  ad   <- rs$adapter_spec
  subj <- "unknown_subject"
  if (!is.null(scen$args$subject)) subj <- scen$args$subject
  ds_id <- paste0("mmlu:subject=", subj)
  mdl <- "unknown_model"
  if (!is.null(ad$model)) {
    mdl <- ad$model
  } else if (!is.null(ad$model_deployment)) {
    mdl <- ad$model_deployment
  }
  tibble(
    run_dir    = basename(dir_one),
    dataset_id = ds_id,
    model      = mdl
  )
}

spec_hash <- function(dir_one) {
  fp <- file.path(dir_one, "run_spec.json")
  spec <- read_json(fp, simplifyVector = FALSE)
  rs <- if (!is.null(spec$run_spec)) spec$run_spec else spec
  if (!is.null(rs$adapter_spec$model)) rs$adapter_spec$model <- NULL
  if (!is.null(rs$adapter_spec$model_deployment)) rs$adapter_spec$model_deployment <- NULL
  digest(rs, algo = "xxhash64")
}
run_tbl <- bind_rows(lapply(dir_run, spec_tbl))
run_tbl$cfg_hash <- vapply(dir_run, spec_hash, character(1))

metric_pool <- unique(c(
  "exact_match", "quasi_exact_match", "prefix_exact_match",
  "quasi_prefix_exact_match", "inference_runtime", "num_bytes"
))

metric_tbl <- function(stats, metric_pool) {
  nm <- vapply(
    stats,
    function(x) {
      if (is.null(x$name) || is.null(x$name$name) || length(x$name$name) == 0L) {
        NA_character_
      } else {
        as.character(x$name$name[1])
      }
    },
    character(1)
  )
  mu <- vapply(
    stats,
    function(x) {
      if (is.null(x$mean) || length(x$mean) == 0L) {
        NA_real_
      } else {
        as.numeric(x$mean[1])
      }
    },
    numeric(1)
  )
  out <- tibble(metric = nm, score = mu)
  out <- out[!is.na(out$metric) &
               !is.na(out$score) &
               out$metric %in% metric_pool, ]
  out
}

fp_stats <- vapply(
  run_tbl$run_dir,
  function(rd) file.path(dir_v1, rd, "stats.json"),
  character(1)
)
stats_raw <- lapply(fp_stats, function(fp) read_json(fp, simplifyVector = FALSE))

long_raw <- bind_rows(lapply(
  seq_along(stats_raw),
  function(i) {
    mt <- metric_tbl(stats_raw[[i]], metric_pool)
    mt$dataset_id <- run_tbl$dataset_id[i]
    mt$model      <- run_tbl$model[i]
    mt$cfg_hash   <- run_tbl$cfg_hash[i]
    mt
  }
))

setting <- select(
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
      inner_join(long_raw, setting, by = c("dataset_id", "model", "cfg_hash")),
      -cfg_hash
    ),
    dataset_id, model, metric
  ),
  score = mean(score, na.rm = TRUE),
  .groups = "drop"
)

lower_better <- c( "inference_runtime", "num_bytes")

long <- mutate(
  filter(
    mutate(
      filter(mutate(long, score = as.numeric(score)), is.finite(score)),
      !(metric == "num_bytes" & score <= 0)
    ),
    !is.na(score)
  ),
  score = if_else(metric %in% lower_better, -score, score)
)

long <- filter(long, model != "google/gemini-pro")

complete <- function(sub, models, metrics) {
  grid <- expand.grid(
    dataset_id = unique(sub$dataset_id),
    metric     = metrics,
    stringsAsFactors = FALSE
  )
  
  tmp <- summarise(
    group_by(sub, dataset_id, metric),
    n_models = n_distinct(model),
    .groups = "drop"
  )
  
  tmp <- left_join(as_tibble(grid), tmp, by = c("dataset_id", "metric"))
  tmp$n_models[is.na(tmp$n_models)] <- 0L
  
  pull(
    filter(
      summarise(
        group_by(tmp, dataset_id),
        complete = all(n_models == length(models)),
        .groups = "drop"
      ),
      complete
    ),
    dataset_id
  )
}

rankings <- function(long_tbl, models, metrics, tie = c("alph", "reverse_alph")) {
  tie <- match.arg(tie)
  
  sub <- filter(long_tbl, metric %in% metrics, model %in% models)
  
  ds <- complete(sub, models, metrics)
  
  sub <- filter(sub, dataset_id %in% ds)
  
  if (tie == "alph") {
    orders <- summarise(
      group_by(arrange(sub, dataset_id, metric, desc(score), model), dataset_id, metric),
      order = list(model),
      .groups = "drop"
    )
  } else {
    orders <- summarise(
      group_by(arrange(sub, dataset_id, metric, desc(score), desc(model)), dataset_id, metric),
      order = list(model),
      .groups = "drop"
    )
  }
  
  profiles <- summarise(
    group_by(orders, dataset_id),
    profile = list(setNames(order, metric)),
    models  = list(sort(unique(models))),
    .groups = "drop"
  )
  
  list(orders = orders, profiles = profiles)
}

###Single-peakedness helpers

sp_order_axis <- function(order, axis) {
  m <- length(axis)
  stopifnot(length(order) == m)
  for (t in seq_len(m)) {
    top_t <- order[1:t]
    idx <- sort(match(top_t, axis))
    if (max(idx) - min(idx) + 1 != length(idx)) return(FALSE)
  }
  TRUE
}

sp_profile_axis <- function(profile, axis) {
  all(vapply(profile, function(ord) sp_order_axis(ord, axis), logical(1)))
}

perm_all <- function(x) {
  n <- length(x)
  if (n == 1L) return(matrix(x, nrow = 1L))
  
  res <- NULL
  for (i in seq_len(n)) {
    first <- x[i]
    rest  <- x[-i]
    sub   <- perm_all(rest)
    res <- rbind(res, cbind(first, sub))
  }
  res
}

sp_axis_find <- function(models, profile) {
  perms <- perm_all(models)
  for (i in seq_len(nrow(perms))) {
    axis <- perms[i, ]
    if (sp_profile_axis(profile, axis)) return(list(sp = TRUE, axis = axis))
  }
  list(sp = FALSE, axis = NULL)
}

sp_run <- function(profiles) {
  select(
    mutate(
      profiles,
      sp_info      = map2(models, profile, sp_axis_find),
      single_peaked = map_lgl(sp_info, "sp"),
      axis         = map(sp_info, "axis")
    ),
    dataset_id, single_peaked, axis
  )
}

### Group separability helpers

subsets <- function(S) {
  s <- length(S)
  if (s <= 1L) return(list())
  unlist(
    lapply(1:(s - 1L), function(k) {
      combn(S, k, simplify = FALSE)
    }),
    recursive = FALSE
  )
}

sep_for_metric <- function(ord, E, S) {
  pos   <- setNames(seq_along(ord), ord)
  Epos  <- pos[E]
  Scpos <- pos[setdiff(S, E)]
  
  if (length(Epos) == 0L || length(Scpos) == 0L) return(FALSE)
  
  (max(Epos) < min(Scpos)) || (max(Scpos) < min(Epos))
}

separation_in_S <- function(profile_named_orders, E, S) {
  all(vapply(
    profile_named_orders,
    function(ord) sep_for_metric(ord, E, S),
    logical(1)
  ))
}

restrict_profile <- function(profile_named_orders, S_sub) {
  lapply(
    profile_named_orders,
    function(ord) ord[ord %in% S_sub]
  )
}

gs_inner <- function(profile_named_orders, S) {
  if (length(S) <= 2L) return(TRUE)
  E_list <- subsets(S)
  for (E in E_list) {
    if (separation_in_S(profile_named_orders, E, S)) {
      S_minus_E <- setdiff(S, E)
      profile_E  <- restrict_profile(profile_named_orders, E)
      profile_Sc <- restrict_profile(profile_named_orders, S_minus_E)
      if (gs_inner(profile_E, E) &&
          gs_inner(profile_Sc, S_minus_E)) {
        return(TRUE)
      }
    }
  }
  
  FALSE
}

group_separable <- function(profile_named_orders) {
  models <- sort(unique(unlist(profile_named_orders)))
  gs_inner(profile_named_orders, models)
}


### Distance-restrictedness helpers

swap_distance <- function(order1, order2) {
  stopifnot(length(order1) == length(order2))
  stopifnot(setequal(order1, order2))
  pos2 <- setNames(seq_along(order2), order2)
  p <- unname(pos2[order1]) 
  m <- length(p)
  inv <- 0L
  for (i in 1:(m-1)) {
    for (j in (i+1):m) {
      if (p[i] > p[j]) inv <- inv + 1L
    }
  }
  inv
}


dr <- function(orders_df, p = 1L) {
  summarise(
    group_by(
      arrange(orders_df, dataset_id, metric),
      dataset_id
    ),
    metrics = list(metric),
    orders  = list(order),
    max_dS  = {
      ords <- order
      n    <- length(ords)
      if (n <= 1L) {
        0L
      } else {
        dmax <- 0L
        for (i in 1:(n-1)) for (j in (i+1):n) {
          d <- swap_distance(ords[[i]], ords[[j]])
          if (d > dmax) dmax <- d
        }
        dmax
      }
    },
    distance_restricted = (max_dS <= p),
    .groups = "drop"
  )
}


###Experiments

efficiency_metrics <- c("inference_runtime", "num_bytes")

wilson_ci <- function(k, n, alpha = 0.05) {
  if (is.na(k) || is.na(n) || n == 0L) return(c(lo = NA_real_, hi = NA_real_))
  ci <- prop.test(round(k), round(n), conf.level = 1 - alpha, correct = FALSE)$conf.int
  c(lo = unname(ci[1]), hi = unname(ci[2]))
}


### Model sets

A <- list(
  A_1 = c(
    "openai/gpt-4-0613",
    "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229",
    "anthropic/claude-3-sonnet-20240229",
    "qwen/qwen1.5-72b",
    "meta/llama-2-70b", "anthropic/claude-3-haiku-20240307", 
    "01-ai/yi-34b", "01-ai/yi-6b"
  ),
  A_2 = c( 
    "openai/gpt-4-0613", "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229", "anthropic/claude-3-sonnet-20240229",
    "qwen/qwen1.5-72b", "meta/llama-2-70b", "mistralai/mistral-7b-v0.1"
  ),
  A_3 = c( 
    "openai/gpt-4-0613", "qwen/qwen1.5-72b",
    "meta/llama-2-70b", "mistralai/mistral-7b-v0.1"
  ),
  A_4 = c( 
    "openai/gpt-4-0613", "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229", "anthropic/claude-3-sonnet-20240229",
    "qwen/qwen1.5-72b"
  ),
  A_5 = c(
    "anthropic/claude-3-opus-20240229", "openai/gpt-4-0613",
    "openai/gpt-4-1106-preview", "google/text-unicorn@001"
  ),
  A_6 = c( 
    "anthropic/claude-3-opus-20240229", "openai/gpt-4-0613",
    "openai/gpt-4-1106-preview", "google/text-unicorn@001",
    "qwen/qwen1.5-72b"
  ),
  A_7 = c( 
    "anthropic/claude-3-opus-20240229", "openai/gpt-4-0613",
    "openai/gpt-4-1106-preview", "google/text-unicorn@001",
    "qwen/qwen1.5-72b", "01-ai/yi-34b"
  ),
  A_8 = c(
    "anthropic/claude-3-opus-20240229", "openai/gpt-4-0613",
    "openai/gpt-4-1106-preview", "google/text-unicorn@001",
    "qwen/qwen1.5-72b", "01-ai/yi-34b",
    "anthropic/claude-3-sonnet-20240229"
  ),
  A_9 = c(
    "anthropic/claude-3-opus-20240229", "mistralai/mistral-7b-v0.1",
    "openai/gpt-4-0613", "meta/llama-2-13b"
  ),
  A_10 = c( 
    "anthropic/claude-3-opus-20240229", "mistralai/mistral-7b-v0.1",
    "openai/gpt-4-0613", "meta/llama-2-13b",
    "openai/gpt-4-1106-preview"
  ),
  A_11 = c( 
    "anthropic/claude-3-sonnet-20240229", "mistralai/mistral-7b-v0.1",
    "openai/gpt-4-0613", "meta/llama-2-70b",
    "openai/gpt-4-1106-preview"
  ),
  A_12 = c (
    "openai/gpt-4-0613", "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229", "qwen/qwen1.5-72b"
  ),
  A_13 = c( 
    "openai/gpt-4-0613", "qwen/qwen1.5-72b", "01-ai/yi-6b"
  ),
  A_14 = c( 
    "qwen/qwen1.5-72b", "meta/llama-2-70b",
    "mistralai/mistral-7b-v0.1", "01-ai/yi-34b", "01-ai/yi-6b"
  ),
  A_15 = c( 
    "openai/gpt-4-0613", "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229",
    "anthropic/claude-3-sonnet-20240229", "anthropic/claude-3-haiku-20240307"
  ),
  A_16 = c( 
    "openai/gpt-4-0613", "openai/gpt-4-1106-preview",
    "anthropic/claude-3-opus-20240229", "anthropic/claude-3-sonnet-20240229",
    "qwen/qwen1.5-72b", "meta/llama-2-70b"
  ),
  A_17 = c( 
    "anthropic/claude-3-opus-20240229", "meta/llama-2-70b",
    "mistralai/mistral-7b-v0.1", "01-ai/yi-34b", "01-ai/yi-6b"
  ),
  A_18 = c( 
    "anthropic/claude-3-haiku-20240307", "mistralai/mistral-7b-v0.1",
    "meta/llama-2-13b", "01-ai/yi-6b"
  ),
  A_19 = c(
    "anthropic/claude-3-haiku-20240307",
    "anthropic/claude-3-sonnet-20240229",
    "anthropic/claude-3-opus-20240229"
  ),
  A_20 = c(
    "qwen/qwen1.5-14b",
    "qwen/qwen1.5-72b",
    "qwen/qwen1.5-7b" ),
  A_21 = c(
    "openai/gpt-4-0613", 
    "openai/gpt-4-1106-preview", 
    "openai/gpt-3.5-turbo-0613"),
  A_22 = c(
    "meta/llama-2-13b", 
    "meta/llama-2-70b", 
    "meta/llama-2-7b" ),
  A_23 = c( 
    "mistralai/mistral-7b-v0.1", "meta/llama-2-13b",
    "01-ai/yi-34b", "meta/llama-2-70b", "qwen/qwen1.5-72b"
  )
)

### Random model sets
set.seed(13)
all_models <- unique(long$model)
A_random   <- list()
size_grid  <- list("3" = 30L, "4" = 20L, "5" = 10L)
for (sz_chr in names(size_grid)) {
  sz   <- as.integer(sz_chr)
  reps <- size_grid[[sz_chr]]
  for (rep in seq_len(reps)) {
    A_random[[sprintf("R_%d_%02d", sz, rep)]] <- sort(sample(all_models, sz))
  }
}
A <- c(A, A_random)
cat(sprintf("[A] curated=%d  random=%d  total=%d\n",
            length(A) - length(A_random), length(A_random), length(A)))

### Metric subsets

metric_subsets <- unlist(
  lapply(3:6, function(k) combn(metric_pool, k, simplify = FALSE)),
  recursive = FALSE
)

design_sweep <- expand.grid(
  A_idx   = seq_along(A),
  Phi_idx = seq_along(metric_subsets),
  tie     = c("alph", "reverse_alph"),
  stringsAsFactors = FALSE
)

### Kendall tau

kendall_pair <- function(o1, o2) {
  m <- length(o1)
  if (m <= 1L) return(NA_real_)
  d <- swap_distance(o1, o2)
  1 - 2 * d / (m * (m - 1L) / 2)
}

kendall_stats <- function(orders_list) {
  n <- length(orders_list)
  if (n <= 1L) return(c(mean_tau = NA_real_, min_tau = NA_real_))
  vals <- numeric(0)
  for (i in 1:(n - 1L)) for (j in (i + 1L):n) {
    vals <- c(vals, kendall_pair(orders_list[[i]], orders_list[[j]]))
  }
  c(mean_tau = mean(vals, na.rm = TRUE), min_tau = min(vals, na.rm = TRUE))
}

tau_per_subject <- function(ords) {
  bind_rows(lapply(
    unique(ords$dataset_id),
    function(d) {
      sub <- arrange(filter(ords, dataset_id == d), metric)
      ks  <- kendall_stats(sub$order)
      tibble(dataset_id = d, mean_tau = ks[["mean_tau"]], min_tau = ks[["min_tau"]])
    }
  ))
}

### Sweep

sweep_cell <- function(A_name, models, metrics, tie_rule) {
  out  <- rankings(long, models = models, metrics = metrics, tie = tie_rule)
  prof <- out$profiles
  ords <- out$orders
  
  empty_row <- tibble(
    A_set                 = A_name,
    metric_set            = paste(metrics, collapse = "+"),
    tie                   = tie_rule,
    n_models              = length(models),
    n_metrics             = length(metrics),
    has_efficiency        = any(metrics %in% efficiency_metrics),
    n_complete            = 0L,
    n_sp                  = NA_integer_,
    n_gs                  = NA_integer_,
    n_dr                  = NA_integer_,
    frac_sp               = NA_real_,
    frac_gs               = NA_real_,
    frac_dr               = NA_real_,
    mean_kendall_tau = NA_real_,
    min_kendall_tau  = NA_real_
  )
  if (nrow(prof) == 0L) return(empty_row)
  
  sp_res <- sp_run(prof)
  gs_res <- mutate(prof, gs = map_lgl(profile, group_separable))
  dr_res <- dr(ords, p = 1L)
  tau_df <- tau_per_subject(ords)
  n_sp_subjects <- sum(sp_res$single_peaked)
  n_gs_subjects <- sum(gs_res$gs)
  n_dr_subjects <- sum(dr_res$distance_restricted)
  n_subjects    <- nrow(prof)
  
  
  tibble(
    A_set                 = A_name,
    metric_set            = paste(metrics, collapse = "+"),
    tie                   = tie_rule,
    n_models              = length(models),
    n_metrics             = length(metrics),
    has_efficiency        = any(metrics %in% efficiency_metrics),
    n_complete            = nrow(prof),
    n_sp                  = n_sp_subjects,
    n_gs                  = n_gs_subjects,
    n_dr                  = n_dr_subjects,
    frac_sp               = n_sp_subjects / n_subjects,
    frac_gs               = n_gs_subjects / n_subjects,
    frac_dr               = n_dr_subjects / n_subjects,
    mean_kendall_tau = mean(tau_df$mean_tau, na.rm = TRUE),
    min_kendall_tau  = mean(tau_df$min_tau,  na.rm = TRUE)
  )
}

### Run the sweep
n_cores <- max(1L, future::availableCores() - 1L)
future::plan(future::multisession, workers = n_cores)

cat(sprintf("[sweep] using %d workers on %d cells\n", n_cores, nrow(design_sweep)))

t_start <- Sys.time()
sweep_tbl <- bind_rows(future.apply::future_lapply(
  seq_len(nrow(design_sweep)),
  function(k) {
    A_name   <- names(A)[design_sweep$A_idx[k]]
    models   <- A[[design_sweep$A_idx[k]]]
    metrics  <- metric_subsets[[design_sweep$Phi_idx[k]]]
    tie_rule <- design_sweep$tie[k]
    sweep_cell(A_name, models, metrics, tie_rule)
  },
  future.seed = TRUE,
  future.scheduling = Inf,
  future.packages = c("dplyr", "purrr", "tibble")
))
future::plan(future::sequential)

cat(sprintf("[sweep] done in %s\n", format(Sys.time() - t_start)))


sweep_tbl$sp_count <- sprintf("%d/%d", sweep_tbl$n_sp, sweep_tbl$n_complete)
sweep_tbl$gs_count <- sprintf("%d/%d", sweep_tbl$n_gs, sweep_tbl$n_complete)
sweep_tbl$dr_count <- sprintf("%d/%d", sweep_tbl$n_dr, sweep_tbl$n_complete)
sp_ci <- t(mapply(wilson_ci, sweep_tbl$n_sp, sweep_tbl$n_complete))
gs_ci <- t(mapply(wilson_ci, sweep_tbl$n_gs, sweep_tbl$n_complete))
dr_ci <- t(mapply(wilson_ci, sweep_tbl$n_dr, sweep_tbl$n_complete))
sweep_tbl$sp_lo <- sp_ci[, "lo"]; sweep_tbl$sp_hi <- sp_ci[, "hi"]
sweep_tbl$gs_lo <- gs_ci[, "lo"]; sweep_tbl$gs_hi <- gs_ci[, "hi"]
sweep_tbl$dr_lo <- dr_ci[, "lo"]; sweep_tbl$dr_hi <- dr_ci[, "hi"]
print(sweep_tbl[, c("A_set", "metric_set", "tie",
                    "sp_count", "sp_lo", "sp_hi",
                    "gs_count", "gs_lo", "gs_hi",
                    "dr_count", "dr_lo", "dr_hi")])
results_dir <- file.path("results", "helm_mmlu")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
saveRDS(sweep_tbl, file.path(results_dir, "sweep_tbl.rds"))
axis_pos <- function(k) {
  a <- matrix(1L, nrow = 1L)
  for (i in 2:k) a <- rbind(cbind(i, a), cbind(a, i))
  a <- a[a[, 1] < a[, k], , drop = FALSE]
  t(apply(a, 1, order))
}

sp_test <- function(p, a) {
  for (i in 2:nrow(p)) {
    lo <- hi <- a[, p[i, 1]]
    for (j in 2:ncol(p)) {
      lo <- pmin.int(lo, a[, p[i, j]])
      hi <- pmax.int(hi, a[, p[i, j]])
      ok <- hi - lo + 1L == j
      a <- a[ok, , drop = FALSE]
      lo <- lo[ok]
      hi <- hi[ok]
      if (nrow(a) == 0L) return(FALSE)
    }
  }
  TRUE
}

gs_test <- function(p) {
  k <- ncol(p)
  pos <- lapply(2:nrow(p), function(i) order(p[i, ]))
  memo <- rep(NA, 2^k)
  split <- function(S) {
    id <- sum(2^(S - 1L)) + 1L
    if (!is.na(memo[id])) return(memo[id])
    if (length(S) <= 2L) return(TRUE)
    for (j in seq_len(length(S) - 1L)) {
      E <- S[seq_len(j)]
      C <- S[(j + 1L):length(S)]
      ok <- TRUE
      for (v in pos) {
        if (!(max(v[E]) < min(v[C]) || max(v[C]) < min(v[E]))) {
          ok <- FALSE
          break
        }
      }
      if (ok && Recall(E) && Recall(C)) {
        memo[id] <<- TRUE
        return(TRUE)
      }
    }
    memo[id] <<- FALSE
    FALSE
  }
  split(seq_len(k))
}

dr_test <- function(p) {
  q <- unique(p)
  if (nrow(q) == 1L) return(TRUE)
  if (nrow(q) > 2L) return(FALSE)
  j <- which(q[1, ] != q[2, ])
  length(j) == 2L && diff(j) == 1L
}

random_ties <- function(long, A, metric_subsets, metric_pool) {
  RNGkind("Mersenne-Twister", "Inversion", "Rejection")
  set.seed(20260801)
  ds <- sort(unique(long$dataset_id))
  md <- sort(unique(long$model))
  sc <- array(NA_real_, c(length(ds), length(md), length(metric_pool)))
  sc[cbind(match(long$dataset_id, ds), match(long$model, md),
           match(long$metric, metric_pool))] <- long$score
  out <- list()
  for (i in seq_along(A)) {
    models <- sort(A[[i]])
    k <- length(models)
    a <- axis_pos(k)
    for (j in seq_along(metric_subsets)) {
      metrics <- metric_subsets[[j]]
      n <- length(metrics)
      z <- sc[, match(models, md), match(metrics, metric_pool), drop = FALSE]
      ok <- which(apply(z, 1, function(v) !anyNA(v)))
      cnt <- matrix(0L, 10L, 3L)
      for (s in ok) {
        v <- t(matrix(z[s, , ], nrow = k, ncol = n))
        for (b in 1:10) {
          key <- matrix(runif(n * k), nrow = n)
          p <- matrix(0L, n, k)
          for (h in seq_len(n)) p[h, ] <- order(-v[h, ], key[h, ])
          p <- matrix(match(p, p[1, ]), nrow = n)
          cnt[b, ] <- cnt[b, ] + c(sp_test(p, a), gs_test(p), dr_test(p))
        }
      }
      out[[length(out) + 1L]] <- data.frame(
        A_set = names(A)[i], metric_set = paste(metrics, collapse = "+"),
        n_models = k, n_metrics = n,
        has_efficiency = any(metrics %in% c("inference_runtime", "num_bytes")),
        n_complete = length(ok), draw = 1:10,
        n_sp = cnt[, 1], n_gs = cnt[, 2], n_dr = cnt[, 3]
      )
    }
    cat(names(A)[i], "\n")
  }
  do.call(rbind, out)
}

null_cell <- function(k, n, x) {
  x <- x[x$n_models == k & x$n_metrics == n, ]
  a <- axis_pos(k)
  total <- factorial(k)^(n - 1L)
  exact <- total <= 2000000
  B <- if (exact) as.integer(round(total)) else 1000000L
  if (exact) {
    perms <- perm_all(seq_len(k))
    grid <- expand.grid(rep(list(seq_len(nrow(perms))), n - 1L),
                        KEEP.OUT.ATTRS = FALSE)
  } else {
    RNGkind("Mersenne-Twister", "Inversion", "Rejection")
    set.seed(27273 + 100L * k + n)
  }
  cnt <- c(0L, 0L)
  p <- matrix(0L, n, k)
  p[1, ] <- seq_len(k)
  for (b in seq_len(B)) {
    for (j in 2:n) {
      p[j, ] <- if (exact) perms[grid[b, j - 1L], ] else sample.int(k)
    }
    cnt <- cnt + c(sp_test(p, a), gs_test(p))
  }
  cnt <- c(cnt, 1 + (k - 1L) * (2^(n - 1L) - 1))
  den <- c(B, B, total)
  ci <- matrix(NA_real_, 3L, 2L)
  if (!exact) ci[1:2, ] <- t(mapply(wilson_ci, cnt[1:2], den[1:2]))
  data.frame(
    n_models = k, n_metrics = n, property = c("sp", "gs", "dr"),
    n_configurations = nrow(x), n_subject_profiles = sum(x$n_complete),
    observed = colSums(x[c("n_sp", "n_gs", "n_dr")]) / sum(x$n_complete),
    random = cnt / den,
    method = c(rep(if (exact) "exact" else "Monte Carlo", 2), "exact"),
    successes = cnt, trials = den, lo = ci[, 1], hi = ci[, 2],
    seed = c(rep(if (exact) NA_integer_ else 27273 + 100L * k + n, 2), NA_integer_),
    row.names = NULL
  )
}

stopifnot(length(A) == 83L, length(metric_subsets) == 42L,
          nrow(sweep_tbl) == 6972L, all(sweep_tbl$n_complete == 57L))

random_tbl <- random_ties(long, A, metric_subsets, metric_pool)
write.csv(random_tbl, file.path(results_dir, "helm_mmlu_random_ties.csv"),
          row.names = FALSE)

x <- sweep_tbl[sweep_tbl$tie == "alph", ]
cells <- unique(x[c("n_models", "n_metrics")])
cells <- cells[order(cells$n_models, cells$n_metrics), ]
future::plan(future::multisession, workers = min(4L, future::availableCores()))
null_tbl <- bind_rows(future.apply::future_lapply(
  seq_len(nrow(cells)),
  function(i) null_cell(cells$n_models[i], cells$n_metrics[i], x),
  future.seed = TRUE
))
future::plan(future::sequential)
write.csv(null_tbl, file.path(results_dir, "helm_mmlu_random_profile_baseline.csv"),
          row.names = FALSE)

tie_results <- bind_rows(lapply(c("alph", "reverse_alph", "random"), function(tie) {
  z <- if (tie == "random") random_tbl else sweep_tbl[sweep_tbl$tie == tie, ]
  v <- c("n_sp", "n_gs", "n_dr")
  data.frame(
    tie = tie, property = c("sp", "gs", "dr"),
    profile_fraction = colSums(z[v]) / sum(z$n_complete),
    all_subject_fraction = vapply(v, function(h) mean(z[[h]] == z$n_complete), numeric(1)),
    row.names = NULL
  )
}))

baseline_results <- bind_rows(lapply(c("sp", "gs", "dr"), function(p) {
  z <- null_tbl[null_tbl$property == p, ]
  data.frame(
    property = p,
    observed = weighted.mean(z$observed, z$n_subject_profiles),
    random = weighted.mean(z$random, z$n_subject_profiles)
  )
}))

print(tie_results)
print(baseline_results)
write.csv(tie_results, file.path(results_dir, "helm_mmlu_tie_results.csv"),
          row.names = FALSE)
write.csv(baseline_results, file.path(results_dir, "helm_mmlu_baseline_results.csv"),
          row.names = FALSE)
write.csv(sweep_tbl[sweep_tbl$tie == "alph", ],
          file.path(results_dir, "helm_metric_sizes_3_6_alph_combined.csv"),
          row.names = FALSE)
write.csv(sweep_tbl, file.path(results_dir, "sweep_tbl.csv"),
          row.names = FALSE)


### Tables for regression models
sweep_sp <- filter(sweep_tbl, tie == "alph", n_complete > 0L,
                   !is.na(frac_sp), n_metrics %in% c(3L, 4L))
sweep_gs <- filter(sweep_tbl, tie == "alph", n_complete > 0L,
                   !is.na(frac_gs), n_metrics == 3L)
sweep_dr <- filter(sweep_tbl, tie == "alph", n_complete > 0L,
                   !is.na(frac_dr), n_metrics %in% c(3L, 4L))

### Mixed-effects logistic regression
me_sp <- glmer(
  cbind(n_sp, n_complete - n_sp) ~ mean_kendall_tau + has_efficiency +
    n_models + n_metrics + (1 | A_set) + (1 | metric_set),
  data    = sweep_sp,
  family  = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)
me_gs <- glmer(
  cbind(n_gs, n_complete - n_gs) ~ mean_kendall_tau + has_efficiency +
    n_models + (1 | A_set) + (1 | metric_set),
  data    = sweep_gs,
  family  = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

me_dr <- glmer(
  cbind(n_dr, n_complete - n_dr) ~ mean_kendall_tau + has_efficiency +
    n_models + n_metrics + (1 | A_set) + (1 | metric_set),
  data    = sweep_dr,
  family  = binomial(),
  control = glmerControl(optimizer = "bobyqa")
)

print(summary(me_sp)); print(confint(me_sp, method = "Wald"))
print(summary(me_gs)); print(confint(me_gs, method = "Wald"))
print(summary(me_dr)); print(confint(me_dr, method = "Wald"))

### Quasi-binomial proportion regression
qb_sp <- glm(frac_sp ~ mean_kendall_tau + has_efficiency + n_models + n_metrics,
             data = sweep_sp, family = quasibinomial(), weights = n_complete)
qb_gs <- glm(frac_gs ~ mean_kendall_tau + has_efficiency + n_models,
             data = sweep_gs, family = quasibinomial(), weights = n_complete)
qb_dr <- glm(frac_dr ~ mean_kendall_tau + has_efficiency + n_models + n_metrics,
             data = sweep_dr, family = quasibinomial(), weights = n_complete)

print(summary(qb_sp)); print(confint(qb_sp))
print(summary(qb_gs)); print(confint(qb_gs))
print(summary(qb_dr)); print(confint(qb_dr))


##### Aggregation across datasets

library(ddandrda)
library(relations)
library(Rgraphviz)

metric_pool_agg <- unique(c(
  "exact_match", "quasi_exact_match", "prefix_exact_match",
  "inference_runtime",
  "num_prompt_tokens", "num_completion_tokens", "num_output_tokens",
  "num_bytes",
  "logprob", "perplexity", "bits_per_byte",
  "logprob_per_byte"
))

long_raw_agg <- bind_rows(lapply(
  seq_along(stats_raw),
  function(i) {
    mt <- metric_tbl(stats_raw[[i]], metric_pool_agg)
    mt$dataset_id <- run_tbl$dataset_id[i]
    mt$model      <- run_tbl$model[i]
    mt$cfg_hash   <- run_tbl$cfg_hash[i]
    mt
  }
))

setting_agg <- select(
  ungroup(
    slice_max(
      group_by(
        count(long_raw_agg, dataset_id, model, cfg_hash, name = "n_runs"),
        dataset_id, model
      ),
      n_runs,
      n = 1,
      with_ties = FALSE
    )
  ),
  dataset_id, model, cfg_hash
)

long_agg <- summarise(
  group_by(
    select(
      inner_join(long_raw_agg, setting_agg, by = c("dataset_id", "model", "cfg_hash")),
      -cfg_hash
    ),
    dataset_id, model, metric
  ),
  score = mean(score, na.rm = TRUE),
  .groups = "drop"
)

lower_better_agg <- c(
  "inference_runtime",
  "num_prompt_tokens", "num_completion_tokens", "num_output_tokens",
  "num_bytes",
  "perplexity",
  "bits_per_byte"
)

long_agg <- mutate(
  filter(
    mutate(
      filter(mutate(long_agg, score = as.numeric(score)), is.finite(score)),
      !(metric == "num_bytes" & score <= 0)
    ),
    !is.na(score)
  ),
  score = if_else(metric %in% lower_better_agg, -score, score)
)

comp_agg <- select(
  filter(
    summarise(
      group_by(long_agg, dataset_id, model),
      nm = n_distinct(metric),
      .groups = "drop"
    ),
    nm == length(metric_pool_agg)
  ),
  dataset_id, model
)

long_complete_agg <- inner_join(
  long_agg,
  comp_agg,
  by = c("dataset_id", "model")
)

ranks <- select(
  ungroup(
    mutate(
      arrange(
        group_by(long_complete_agg, dataset_id, metric),
        desc(score), model,
        .by_group = TRUE
      ),
      rank = row_number()
    )
  ),
  dataset_id, metric, model, rank
)

ranks$rank <- (-1) * ranks$rank


rank_to_incidence <- function(v){
  n <- length(v)
  A <- array(0,c(n,n))
  for(k in (1:n)){
    for(l in (1:n)){
      A[k,l] <- (v[k] > v[l])
    }
  }
  return(A)
}

aggregate <- function(dat,models,metrics,modelnames){
  context <- array(0,c(length(unique(dat$dataset_id)),length(models)^2))
  t <- 1
  relations <- list()
  for(dataset in dataset_names){
    sum_ranks <-array(0,c(length(models),length(models)))
    for(metric in metrics){
      temp <-rep(0,length(models))
      for(k in seq_len(length(models))){
        i <- which(dat$dataset_id==dataset & dat$metric==metric & dat$model==models[k])
        temp[k] <- dat$rank[i]
      }
      sum_ranks <- sum_ranks + rank_to_incidence(temp)
    }
    majority_relation <- 1*(sum_ranks >= t(sum_ranks))
    context[t,] <- as.vector(majority_relation)
    relations[[t]] <- majority_relation
    t <- t+1
    
    if(! relation_is_transitive(as.relation(majority_relation))){print("warning: non-transitive majority relation occured")}
    if(! relation_is_acyclic(as.relation(majority_relation))){print("warning: cyclic majority relation occured")}
  }
  
  context <- cbind(context,1-context)
  depths=ddandrda::compute_tukeys_depth(context,context)
  idxs <- which(!duplicated(context))
  if(length(which(max(depths[idxs])==depths[idxs]))>1){print("warning: more than one relation with maximal depth")}
  aggregated_relation <- relations[[which.max(depths)]]
  colnames(aggregated_relation) <- rownames(aggregated_relation) <- modelnames
  return(list(relations=relations,context=context,depths=depths,aggregated_relation=aggregated_relation))
}

dat <- ranks

dataset_names <- unique(dat$dataset_id)

metrics_acc <- c("exact_match", "prefix_exact_match","quasi_exact_match")
metrics_mix <- c("exact_match","quasi_exact_match","inference_runtime")

models <- unique(dat$model)

models1 <- models[c(16,17,5,6,19,10,4,1,2)]
modelnames1 <- c(" GPT-4-0613","GPT-4-1106", "Claude-3-Opus", "Claude-3-Sonnet", "Qwen1.5", "Llama-2" , "Claude-3-Haiku", "Yi-34b", "Yi-6b")

models2 <- models[c(16,17,5,6,19,10,13)]
modelnames2 <- c("GPT-4-0613","GPT-4-1106", "Claude-3-Opus", "Claude-3-Sonnet", "Qwen1.5", "Llama-2", "Mistral-7b")

models3 <- models[c(16,19,10,13)]
modelnames3 <- c("GPT-4-0613", "Qwen1.5", "Llama-2", "Mistral-7b")

models4 <- models[c(16,17,5,6,19)]
modelnames4 <- c("GPT-4-0613","GPT-4-1106" ,"Claude-3-Opus", "Claude-3-Sonnet", "Qwen1.5")

### Aggregated results for single-peaked preferences

result1 <- aggregate(dat,models1,metrics_acc,modelnames1)
result2 <- aggregate(dat,models3,metrics_mix,modelnames3)

par(mfrow=c(1,2))
plot(as.relation(t(result1$aggregated_relation)),main="")
plot(as.relation(t(result2$aggregated_relation)),main="")

### Aggregated results for group-separable preferences

result3 <- aggregate(dat,models2,metrics_acc,modelnames2)
result4 <- aggregate(dat,models3,metrics_mix,modelnames3)

par(mfrow=c(1,2))
plot(as.relation(t(result3$aggregated_relation)),main="")
plot(as.relation(t(result4$aggregated_relation)),main="")


### Aggregated results for distance-restricted preferences

result5 <- aggregate(dat,models4,metrics_acc,modelnames4)
result6 <- aggregate(dat,models3,metrics_acc,modelnames3)

par(mfrow=c(1,2))
plot(as.relation(t(result5$aggregated_relation)),main="")
plot(as.relation(t(result6$aggregated_relation)),main="")


### Save results

results_dir <- file.path("results", "helm_mmlu")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

summarise_domain_results <- function(x, label) {
  out <- tibble(
    result_set = label,
    n_configurations = nrow(x),
    single_peaked_all_subjects = sum(x$n_sp == x$n_complete, na.rm = TRUE),
    group_separable_all_subjects = sum(x$n_gs == x$n_complete, na.rm = TRUE),
    distance_restricted_all_subjects = sum(x$n_dr == x$n_complete, na.rm = TRUE),
    all_three_all_subjects = sum(
      x$n_sp == x$n_complete &
        x$n_gs == x$n_complete &
        x$n_dr == x$n_complete,
      na.rm = TRUE
    ),
    at_least_one_all_subjects = sum(
      x$n_sp == x$n_complete |
        x$n_gs == x$n_complete |
        x$n_dr == x$n_complete,
      na.rm = TRUE
    )
  )
  mutate(
    out,
    single_peaked_share = single_peaked_all_subjects / n_configurations,
    group_separable_share = group_separable_all_subjects / n_configurations,
    distance_restricted_share = distance_restricted_all_subjects / n_configurations,
    all_three_share = all_three_all_subjects / n_configurations,
    at_least_one_share = at_least_one_all_subjects / n_configurations
  )
}

main_results <- bind_rows(
  summarise_domain_results(filter(sweep_tbl, tie == "alph"),
                           "alphabetical tie-breaker: all metric sets"),
  summarise_domain_results(filter(sweep_tbl, tie == "alph", !has_efficiency),
                           "alphabetical tie-breaker: accuracy metrics only"),
  summarise_domain_results(filter(sweep_tbl, tie == "alph", has_efficiency),
                           "alphabetical tie-breaker: includes efficiency metric"),
  summarise_domain_results(filter(sweep_tbl, tie == "reverse_alph"),
                           "reverse alphabetical tie-breaker: all metric sets")
)

coef_tbl <- function(fit, property, model) {
  x <- data.frame(coef(summary(fit)), check.names = FALSE)
  x$term <- rownames(x)
  stat_col <- grep(" value$", names(x), value = TRUE)[1]
  p_col <- grep("^Pr", names(x), value = TRUE)[1]
  tibble(
    model = model,
    property = property,
    term = x$term,
    estimate = x$Estimate,
    std_error = x[["Std. Error"]],
    statistic = x[[stat_col]],
    p_value = x[[p_col]]
  )
}

mixed_effects_results <- bind_rows(
  coef_tbl(me_sp, "single-peakedness", "mixed-effects logit"),
  coef_tbl(me_gs, "group separability", "mixed-effects logit"),
  coef_tbl(me_dr, "distance-restrictedness", "mixed-effects logit")
)

quasibinomial_results <- bind_rows(
  coef_tbl(qb_sp, "single-peakedness", "quasi-binomial logit"),
  coef_tbl(qb_gs, "group separability", "quasi-binomial logit"),
  coef_tbl(qb_dr, "distance-restrictedness", "quasi-binomial logit")
)

relation_tbl <- function(result, result_name, model_set, metric_set) {
  out <- as_tibble(as.data.frame(as.table(result$aggregated_relation)))
  names(out) <- c("row_model", "column_model", "relation_holds")
  mutate(
    out,
    result = result_name,
    model_set = model_set,
    metric_set = paste(metric_set, collapse = "+"),
    relation_holds = as.integer(relation_holds)
  ) |>
    select(result, model_set, metric_set, row_model, column_model, relation_holds)
}

aggregated_relations <- bind_rows(
  relation_tbl(result1, "single-peakedness: A1 accuracy metrics", "A_1", metrics_acc),
  relation_tbl(result2, "single-peakedness: A3 mixed metrics", "A_3", metrics_mix),
  relation_tbl(result3, "group separability: A2 accuracy metrics", "A_2", metrics_acc),
  relation_tbl(result4, "group separability: A3 mixed metrics", "A_3", metrics_mix),
  relation_tbl(result5, "distance-restrictedness: A4 accuracy metrics", "A_4", metrics_acc),
  relation_tbl(result6, "distance-restrictedness: A3 accuracy metrics", "A_3", metrics_acc)
)

write.csv(sweep_tbl,
          file.path(results_dir, "helm_mmlu_configuration_results.csv"),
          row.names = FALSE)
write.csv(main_results,
          file.path(results_dir, "helm_mmlu_main_results.csv"),
          row.names = FALSE)
write.csv(mixed_effects_results,
          file.path(results_dir, "helm_mmlu_mixed_effects_regression.csv"),
          row.names = FALSE)
write.csv(quasibinomial_results,
          file.path(results_dir, "helm_mmlu_quasibinomial_regression.csv"),
          row.names = FALSE)
write.csv(aggregated_relations,
          file.path(results_dir, "helm_mmlu_aggregated_relations.csv"),
          row.names = FALSE)

base::saveRDS(
  list(
    main_results = main_results,
    configuration_results = sweep_tbl,
    random_tie_results = random_tbl,
    random_profile_baseline = null_tbl,
    tie_summary = tie_results,
    baseline_summary = baseline_results,
    mixed_effects_results = mixed_effects_results,
    quasibinomial_results = quasibinomial_results,
    aggregated_relations = aggregated_relations,
    session_info = sessionInfo()
  ),
  file.path(results_dir, "helm_mmlu_results.rds")
)

### Additional tests

stopifnot(all(vapply(
  c("long", "A", "metric_pool", "metric_subsets", "run_tbl", "setting",
    "dir_v1", "axis_pos", "sp_test", "gs_test", "dr_test", "perm_all"),
  exists, logical(1)
)))

out <- file.path("results", "helm_mmlu")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
n_tie <- 10L
n_null <- 1000000L
n_boot <- 10000L
chunk <- 500L
stopifnot(n_tie >= 1L, n_null >= 1L, n_boot >= 2L,
          n_boot %% 2L == 0L, chunk >= 1L)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

### Majority and structural conditions

majority <- function(p) {
  k <- ncol(p)
  w <- matrix(0L, k, k)
  for (i in seq_len(nrow(p))) {
    pos <- order(p[i, ])
    w <- w + outer(pos, pos, "<")
  }
  w >= t(w)
}

transitive <- function(r) {
  !any((r %*% r > 0) & !r)
}

profile_test <- function(p, a = axis_pos(ncol(p))) {
  q <- matrix(match(p, p[1, ]), nrow = nrow(p))
  c(sp = sp_test(q, a), gs = gs_test(q), dr = dr_test(q),
    transitive = transitive(majority(p)))
}

selection_test <- function(p) {
  r <- majority(p)
  k <- ncol(r)
  win <- which(rowSums(r) == k)
  reach <- r
  for (j in seq_len(k)) {
    reach <- reach | outer(reach[, j], reach[j, ], "&")
  }
  s <- which(rowSums(reach) == k)
  list(transitive = transitive(r), winner = win, smith = s)
}

### Transitivity
trans_batch <- function(w, n, k) {
  pairs <- t(combn(seq_len(k), 2L))
  idx <- matrix(0L, k, k)
  idx[pairs] <- seq_len(nrow(pairs))
  if (n %% 2L == 1L) {
    d <- matrix(0L, nrow(w), k)
    for (j in seq_len(nrow(pairs))) {
      v <- w[, j] > n / 2
      d[, pairs[j, 1]] <- d[, pairs[j, 1]] + v
      d[, pairs[j, 2]] <- d[, pairs[j, 2]] + !v
    }
    return(rowSums(d * (d - 1)) / 2 == choose(k, 3))
  }
  ok <- rep(TRUE, nrow(w))
  triples <- combn(seq_len(k), 3L)
  for (j in seq_len(ncol(triples))) {
    x <- triples[, j]
    a <- w[, idx[x[1], x[2]]] >= n / 2
    b <- w[, idx[x[1], x[3]]] >= n / 2
    c <- w[, idx[x[2], x[3]]] >= n / 2
    ar <- w[, idx[x[1], x[2]]] <= n / 2
    br <- w[, idx[x[1], x[3]]] <= n / 2
    cr <- w[, idx[x[2], x[3]]] <= n / 2
    ok <- ok & !((a & c & !b) | (b & cr & !a) | (ar & b & !c) |
                   (c & br & !ar) | (br & a & !cr) | (cr & ar & !br))
  }
  ok
}

ds <- sort(unique(long$dataset_id))
md <- sort(unique(long$model))
ns <- length(ds)
nm <- length(md)
na <- length(A)
sc <- array(NA_real_, c(ns, nm, length(metric_pool)),
            dimnames = list(ds, md, metric_pool))
stopifnot(!anyDuplicated(long[c("dataset_id", "model", "metric")]))
sc[cbind(match(long$dataset_id, ds), match(long$model, md),
         match(long$metric, metric_pool))] <- long$score
stopifnot(all(is.finite(sc)), na == 83L, ns == 57L, nm == 22L,
          length(metric_subsets) == 42L)

### Profiles, metric groups and random tie-breaking
set.seed(20260801)
rows <- rand <- list()
for (i in seq_along(A)) {
  ids <- match(sort(A[[i]]), md)
  k <- length(ids)
  a <- axis_pos(k)
  pairs <- t(combn(seq_len(k), 2L))
  for (j in seq_along(metric_subsets)) {
    mets <- metric_subsets[[j]]
    n <- length(mets)
    z <- sc[, ids, match(mets, metric_pool), drop = FALSE]
    f <- matrix(FALSE, ns, 4L)
    colnames(f) <- c("sp", "gs", "dr", "transitive")
    for (s in seq_len(ns)) {
      v <- t(matrix(z[s, , ], k, n))
      p <- t(vapply(seq_len(n), function(h) order(-v[h, ], seq_len(k)),
                    integer(k)))
      f[s, ] <- profile_test(p, a)
    }
    rows[[length(rows) + 1L]] <- data.frame(
      A_set = names(A)[i], metric_set = paste(mets, collapse = "+"),
      n_models = k, n_metrics = n, subject = ds,
      n_accuracy = sum(!mets %in% c("inference_runtime", "num_bytes")), f
    )
    
    key <- array(runif(n * k * n_tie * ns), c(n, k, n_tie, ns))
    w <- matrix(0L, ns * n_tie, nrow(pairs))
    for (h in seq_len(n)) {
      v <- matrix(z[, , h], ns, k)
      v <- v[rep(seq_len(ns), each = n_tie), , drop = FALSE]
      u <- matrix(aperm(array(key[h, , , ], c(k, n_tie, ns)),
                        c(2, 3, 1)), ns * n_tie, k)
      x <- v[, pairs[, 1], drop = FALSE]
      y <- v[, pairs[, 2], drop = FALSE]
      w <- w + (x > y | (x == y & u[, pairs[, 1], drop = FALSE] <
                           u[, pairs[, 2], drop = FALSE]))
    }
    t <- matrix(trans_batch(w, n, k), n_tie, ns)
    rand[[length(rand) + 1L]] <- data.frame(
      A_set = names(A)[i], metric_set = paste(mets, collapse = "+"),
      n_models = k, n_metrics = n, draw = seq_len(n_tie),
      n_transitive = rowSums(t), n_subjects = ns
    )
  }
  cat("Profiles:", names(A)[i], "\n")
}
profiles_new <- do.call(rbind, rows)
random_trans <- do.call(rbind, rand)
write.csv(profiles_new, file.path(out, "helm_profiles.csv"), row.names = FALSE)
write.csv(random_trans, file.path(out, "random_ties.csv"), row.names = FALSE)

metric_groups <- do.call(rbind, lapply(3:1, function(n) {
  z <- profiles_new[profiles_new$n_metrics == 3 & profiles_new$n_accuracy == n, ]
  data.frame(accuracy_metrics = n, efficiency_metrics = 3L - n,
             profiles = nrow(z), sp = 100 * mean(z$sp),
             gs = 100 * mean(z$gs), transitive = 100 * mean(z$transitive))
}))
print(metric_groups)
write.csv(metric_groups, file.path(out, "metric_groups.csv"), row.names = FALSE)

### Matched uniformly random profiles
null_trans <- function(k, n) {
  pairs <- t(combn(seq_len(k), 2L))
  total <- factorial(k)^(n - 1L)
  exact <- total <= 2000000
  if (exact) {
    perms <- perm_all(seq_len(k))
    pos <- t(apply(perms, 1L, order))
    votes <- pos[, pairs[, 1], drop = FALSE] < pos[, pairs[, 2], drop = FALSE]
    m <- nrow(perms)
  }
  b <- if (exact) as.integer(total) else n_null
  set.seed(20260801L + 1000L * k + n)
  count <- 0L
  for (first in seq.int(1L, b, by = 10000L)) {
    nb <- min(10000L, b - first + 1L)
    w <- matrix(1L, nb, nrow(pairs))
    if (exact) {
      id <- first - 1L + seq_len(nb) - 1L
      for (h in seq_len(n - 1L)) {
        w <- w + votes[(id %/% m^(h - 1L)) %% m + 1L, , drop = FALSE]
      }
    } else {
      p <- vapply(seq_len(nb * (n - 1L)), function(i) sample.int(k), integer(k))
      pos <- matrix(0L, ncol(p), k)
      pos[cbind(rep(seq_len(ncol(p)), each = k), as.vector(p))] <-
        rep(seq_len(k), ncol(p))
      for (h in seq_len(n - 1L)) {
        q <- pos[seq.int(h, nrow(pos), by = n - 1L), , drop = FALSE]
        w <- w + (q[, pairs[, 1], drop = FALSE] < q[, pairs[, 2], drop = FALSE])
      }
    }
    count <- count + sum(trans_batch(w, n, k))
  }
  cat("Uniform profiles:", k, "models,", n, "metrics\n")
  data.frame(n_models = k, n_metrics = n, exact = exact,
             trials = b, transitive = count, probability = count / b)
}
cells <- unique(profiles_new[c("n_models", "n_metrics")])
cells <- cells[order(cells$n_models, cells$n_metrics), ]
uniform_trans <- do.call(rbind, lapply(seq_len(nrow(cells)), function(i) {
  null_trans(cells$n_models[i], cells$n_metrics[i])
}))
write.csv(uniform_trans, file.path(out, "uniform_transitivity.csv"), row.names = FALSE)

### Table of transitivity on individual subjects and on all 57 subjects
trans_row <- function(k = NULL, n = NULL) {
  z <- profiles_new
  r <- random_trans
  if (!is.null(k)) {
    z <- z[z$n_models == k & z$n_metrics == n, ]
    r <- r[r$n_models == k & r$n_metrics == n, ]
  }
  id <- match(paste(z$n_models, z$n_metrics),
              paste(uniform_trans$n_models, uniform_trans$n_metrics))
  data.frame(setting = if (is.null(k)) "All profiles" else paste(k, "models,", n, "metrics"),
             profiles = nrow(z), HELM = 100 * mean(z$transitive),
             random_ties = 100 * sum(r$n_transitive) / sum(r$n_subjects),
             uniform = 100 * mean(uniform_trans$probability[id]))
}
trans_tbl <- rbind(trans_row(3, 3), trans_row(5, 3), trans_row(9, 3),
                   trans_row(9, 4), trans_row())
cfg <- stats::aggregate(transitive ~ A_set + metric_set + n_models + n_metrics,
                        profiles_new, all)
id <- match(paste(cfg$n_models, cfg$n_metrics),
            paste(uniform_trans$n_models, uniform_trans$n_metrics))
trans_tbl <- rbind(trans_tbl, data.frame(
  setting = "Configurations transitive on all 57 subjects", profiles = nrow(cfg),
  HELM = 100 * mean(cfg$transitive),
  random_ties = 100 * mean(random_trans$n_transitive == ns),
  uniform = 100 * mean(uniform_trans$probability[id]^ns)
))
print(trans_tbl)
write.csv(trans_tbl, file.path(out, "transitivity.csv"), row.names = FALSE)

### Accuracy, time and length: selection and conflicting best models
mets <- c("exact_match", "inference_runtime", "num_bytes")
mi <- match(mets, metric_pool)
selection <- list()
for (i in seq_along(A)) {
  ids <- match(sort(A[[i]]), md)
  k <- length(ids)
  for (s in seq_len(ns)) {
    v <- matrix(sc[s, ids, mi], k, 3L)
    p <- t(vapply(seq_len(3L), function(h) order(-v[, h], seq_len(k)), integer(k)))
    f <- profile_test(p)
    q <- selection_test(p)
    best <- apply(v, 2L, which.max)
    distinct <- length(unique(best)) == 3L &&
      all(vapply(seq_len(3L), function(h) sum(v[, h] == max(v[, h])) == 1L, logical(1)))
    selection[[length(selection) + 1L]] <- data.frame(
      A_set = names(A)[i], subject = ds[s], n_models = k,
      condition = any(f[c("sp", "gs", "dr")]), transitive = q$transitive,
      winner = if (length(q$winner)) md[ids[q$winner]] else NA_character_,
      smith_size = length(q$smith), smith = paste(md[ids[q$smith]], collapse = ";"),
      distinct_best = distinct
    )
  }
}
helm_selection <- do.call(rbind, selection)
write.csv(helm_selection, file.path(out, "helm_selection.csv"), row.names = FALSE)
distinct_tbl <- data.frame(
  profiles = sum(helm_selection$distinct_best),
  condition = sum(helm_selection$condition & helm_selection$distinct_best),
  percent = 100 * mean(helm_selection$condition[helm_selection$distinct_best])
)
print(distinct_tbl)
write.csv(distinct_tbl, file.path(out, "distinct_best.csv"), row.names = FALSE)

### Human Aging example, in the order A, B, C used in the paper
ids <- match(c("openai/gpt-4-0613", "openai/gpt-4-1106-preview",
               "openai/gpt-3.5-turbo-0613"), md)
s <- match("mmlu:subject=human_aging", ds)
v <- matrix(sc[s, ids, mi], 3L, 3L)
example_tbl <- data.frame(model = md[ids], accuracy_percent = 100 * v[, 1],
                          time = -v[, 2], length = -v[, 3])
print(example_tbl)
write.csv(example_tbl, file.path(out, "human_aging.csv"), row.names = FALSE)

### All pairwise comparisons over the 22 models, reused across model sets
pairs <- t(combn(seq_len(nm), 2L))
maps <- lapply(A, function(x) {
  ids <- match(sort(x), md)
  idx <- which(pairs[, 1] %in% ids & pairs[, 2] %in% ids)
  p <- cbind(match(pairs[idx, 1], ids), match(pairs[idx, 2], ids))
  d <- matrix(0L, nrow(p), length(ids))
  d[cbind(seq_len(nrow(p)), p[, 1])] <- 1L
  d[cbind(seq_len(nrow(p)), p[, 2])] <- -1L
  list(ids = ids, idx = idx, pairs = p, d = d, offset = colSums(d == -1L))
})

compare <- function(v) {
  w <- matrix(0L, nrow(v), nrow(pairs))
  for (h in seq_len(3L)) {
    j <- (h - 1L) * nm
    w <- w + (v[, j + pairs[, 1], drop = FALSE] >=
                v[, j + pairs[, 2], drop = FALSE])
  }
  w >= 2L
}

base <- lapply(seq_len(ns), function(s) compare(matrix(sc[s, , mi], 1L)))
keep <- matrix(FALSE, ns, na)
win <- matrix(NA_integer_, ns, na)
for (i in seq_along(A)) {
  z <- helm_selection[helm_selection$A_set == names(A)[i], ]
  z <- z[match(ds, z$subject), ]
  keep[, i] <- z$transitive
  win[, i] <- match(z$winner, md[maps[[i]]$ids])
}

### Exact independent changes in mean runtime
runtime <- list()
for (eps in c(.01, .05, .10)) {
  for (s in seq_len(ns)) {
    v <- matrix(sc[s, , mi], nm, 3L)
    acc <- v[pairs[, 1], 1] >= v[pairs[, 2], 1]
    len <- v[pairs[, 1], 3] >= v[pairs[, 2], 3]
    time <- -v[, 2]
    lo <- time * (1 - eps)
    hi <- time * (1 + eps)
    first <- time[pairs[, 1]] <= time[pairs[, 2]]
    reverse <- ifelse(first, hi[pairs[, 1]] > lo[pairs[, 2]],
                      lo[pairs[, 1]] <= hi[pairs[, 2]])
    change <- acc != len & reverse
    for (i in which(keep[s, ])) {
      m <- maps[[i]]
      incident <- m$pairs[, 1] == win[s, i] | m$pairs[, 2] == win[s, i]
      runtime[[length(runtime) + 1L]] <- data.frame(
        subject = ds[s], A_set = names(A)[i], change = eps,
        same_winner = !any(change[m$idx][incident]),
        same_ranking = !any(change[m$idx])
      )
    }
  }
}
runtime_cases <- do.call(rbind, runtime)
runtime_tbl <- stats::aggregate(cbind(same_winner, same_ranking) ~ change,
                                runtime_cases, function(x) 100 * mean(x))
print(runtime_tbl)
write.csv(runtime_tbl, file.path(out, "runtime.csv"), row.names = FALSE)
write.csv(runtime_cases, file.path(out, "runtime_profiles.csv"), row.names = FALSE)

### Paired bootstrap of questions
runs <- as.data.frame(inner_join(run_tbl, setting,
                                 by = c("dataset_id", "model", "cfg_hash")))
runs <- runs[runs$model %in% md, ]
stopifnot(nrow(runs) == ns * nm, !anyDuplicated(runs[c("dataset_id", "model")]))
files <- unlist(lapply(runs$run_dir, function(x) {
  file.path(dir_v1, basename(x), c("instances.json", "per_instance_stats.json"))
}))
if (any(!file.exists(files))) {
  stop("Question files are missing. Download instances.json and per_instance_stats.json for the selected HELM runs.")
}

boot <- list()
check <- list()
for (s in seq_len(ns)) {
  ids <- NULL
  missing <- 0L
  for (i in seq_len(nm)) {
    r <- runs[runs$dataset_id == ds[s] & runs$model == md[i], ]
    path <- file.path(dir_v1, basename(r$run_dir))
    ins <- read_json(file.path(path, "instances.json"), simplifyVector = FALSE)
    ins <- ins[vapply(ins, function(x) identical(x$split, "test"), logical(1))]
    id <- vapply(ins, function(x) x$id, character(1))
    stopifnot(!anyDuplicated(id))
    if (is.null(ids)) {
      ids <- sort(id)
      content <- lapply(ins[match(ids, id)], function(x) x[c("input", "references", "split")])
      values <- counts <- array(0, c(length(ids), nm, 3L))
    } else {
      stopifnot(setequal(ids, id), identical(content,
                                             lapply(ins[match(ids, id)], function(x) x[c("input", "references", "split")])))
    }
    x <- read_json(file.path(path, "per_instance_stats.json"), simplifyVector = TRUE)
    stopifnot(!anyDuplicated(x$instance_id), setequal(ids, x$instance_id),
              all(x$train_trial_index == 0L))
    x <- x[match(ids, x$instance_id), ]
    for (j in seq_along(ids)) {
      z <- x$stats[[j]]
      ix <- which(z$name$split == "test" & z$name$name %in% mets)
      h <- match(z$name$name[ix], mets)
      stopifnot(all(c(1L, 3L) %in% h), !anyDuplicated(h),
                all(z$count[ix] %in% c(0, 1)))
      ### An absent runtime entry keeps its initial sum = 0 and count = 0.
      values[j, i, h] <- z$sum[ix]
      counts[j, i, h] <- z$count[ix]
    }
  }
  stopifnot(all(is.finite(values)), all(values >= 0),
            all(counts[, , c(1, 3)] == 1))
  missing <- sum(counts == 0)
  nq <- length(ids)
  values <- matrix(values, nq, nm * 3L)
  counts <- matrix(counts, nq, nm * 3L)
  sign <- rep(c(1, -1, -1), each = nm)
  original <- matrix(sc[s, , mi], 1L)
  error <- max(abs(colSums(values) / colSums(counts) * sign - original[1, ]))
  stopifnot(error < 1e-10)
  check[[s]] <- data.frame(subject = ds[s], questions = nq,
                           missing_runtime = missing, max_score_difference = error)
  total <- array(0L, c(na, 3L, 2L))
  for (half in 1:2) {
    set.seed(20261109L + 1000L * s + half)
    for (first in seq.int(1L, n_boot / 2L, by = chunk)) {
      nb <- min(chunk, n_boot / 2L - first + 1L)
      w <- t(rmultinom(nb, nq, rep(1 / nq, nq)))
      den <- w %*% counts
      stopifnot(all(den > 0))
      v <- sweep((w %*% values) / den, 2L, sign, "*")
      time <- matrix(original, nb, nm * 3L, byrow = TRUE)
      time[, nm + seq_len(nm)] <- v[, nm + seq_len(nm)]
      cmp <- list(compare(time), compare(v))
      for (h in 1:2) {
        for (i in which(keep[s, ])) {
          m <- maps[[i]]
          r <- cmp[[h]][, m$idx, drop = FALSE]
          d <- sweep((r * 1L) %*% m$d, 2L, m$offset, "+")
          unchanged <- rowSums(r != matrix(base[[s]][1, m$idx], nb,
                                           length(m$idx), byrow = TRUE)) == 0L
          total[i, , h] <- total[i, , h] + c(
            sum(d[, win[s, i]] == length(m$ids) - 1L), sum(unchanged),
            sum(rowSums(d * (d - 1)) / 2 == choose(length(m$ids), 3))
          )
        }
      }
    }
  }
  for (h in 1:2) {
    ix <- which(keep[s, ])
    boot[[length(boot) + 1L]] <- data.frame(
      subject = ds[s], A_set = names(A)[ix],
      metrics = c("Time only", "Accuracy, Time and Length")[h],
      same_winner = total[ix, 1, h] / n_boot,
      same_ranking = total[ix, 2, h] / n_boot,
      transitive = total[ix, 3, h] / n_boot
    )
  }
  cat("Question bootstrap:", s, "of", ns, "subjects\n")
}
bootstrap_cases <- do.call(rbind, boot)
bootstrap_check <- do.call(rbind, check)
boot_tbl <- stats::aggregate(cbind(same_winner, same_ranking, transitive) ~ metrics,
                             bootstrap_cases, function(x) 100 * mean(x))
print(boot_tbl)
write.csv(boot_tbl, file.path(out, "bootstrap.csv"), row.names = FALSE)
write.csv(bootstrap_cases, file.path(out, "bootstrap_profiles.csv"), row.names = FALSE)
write.csv(bootstrap_check, file.path(out, "question_checks.csv"), row.names = FALSE)
saveRDS(list(metric_groups = metric_groups, transitivity = trans_tbl,
             distinct_best = distinct_tbl, human_aging = example_tbl,
             selection = helm_selection, runtime = runtime_tbl, bootstrap = boot_tbl,
             question_checks = bootstrap_check, n_boot = n_boot,
             n_tie = n_tie, n_null = n_null, session = sessionInfo()),
        file.path(out, "helm_additional.rds"))

