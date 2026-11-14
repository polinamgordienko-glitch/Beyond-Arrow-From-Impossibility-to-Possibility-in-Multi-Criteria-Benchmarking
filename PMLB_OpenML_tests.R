library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

fp_pmlb   <- file.path("pmlb_results", "final_results.RDS")
fp_openml <- file.path("openml_permutation_results", "data_openml_filter.rds")

pmlb <- readRDS(fp_pmlb)
openml <- readRDS(fp_openml)

long_pmlb <- pivot_longer(
  mutate(
    rownames_to_column(as.data.frame(pmlb), "id"),
    dataset_id = as.integer(sub(".*\\.(\\d+)$", "\\1", id)),
    dataset_id = ifelse(is.na(dataset_id), 0L, dataset_id),
    model = ifelse(
      grepl("\\.[0-9]+$", id),
      sub("\\.[0-9]+$", "", id),
      id
    )
  ),
  c(results_clean_stacked, results_noisy_x_stacked, results_noisy_y_stacked),
  names_to = "metric",
  values_to = "score"
)

long_pmlb <- select(
  mutate(
    long_pmlb,
    model  = as.character(model),
    metric = as.character(metric),
    score  = as.numeric(score)
  ),
  dataset_id, model, metric, score
)

long_openml <- pivot_longer(
  select(
    mutate(
      ungroup(openml),
      dataset_id = as.integer(factor(data.id)),
      model      = as.character(learner.name),
      acc        = as.numeric(predictive.accuracy),
      train_time = -as.numeric(usercpu.time.millis.training),
      test_time  = -as.numeric(usercpu.time.millis.testing)
    ),
    dataset_id, model, acc, train_time, test_time
  ),
  c(acc, train_time, test_time),
  names_to = "metric",
  values_to = "score"
)

long_openml <- select(
  mutate(
    long_openml,
    metric = as.character(metric),
    score  = as.numeric(score)
  ),
  dataset_id, model, metric, score
)

complete <- function(sub, models) {
  tmp <- summarise(
    group_by(sub, dataset_id, metric),
    n_models = n_distinct(model),
    .groups = "drop"
  )
  
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
  
  ds <- complete(sub, models)
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
      sp_info       = map2(models, profile, sp_axis_find),
      single_peaked = map_lgl(sp_info, "sp"),
      axis          = map(sp_info, "axis")
    ),
    dataset_id, single_peaked, axis
  )
}

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

bench <- list(
  PMLB   = long_pmlb,
  OpenML = long_openml
)

ties <- c("alph", "reverse_alph")

rankings_tbl <- bind_rows(lapply(
  names(bench),
  function(bn) {
    long_tbl <- bench[[bn]]
    models   <- sort(unique(long_tbl$model))
    metrics  <- sort(unique(long_tbl$metric))
    
    bind_rows(lapply(
      ties,
      function(tie_rule) {
        out <- rankings(
          long_tbl,
          models  = models,
          metrics = metrics,
          tie     = tie_rule
        )
        tibble(
          bench    = bn,
          tie      = tie_rule,
          profiles = list(out$profiles),
          orders   = list(out$orders)
        )
      }
    ))
  }
))

sp_all <- bind_rows(lapply(
  seq_len(nrow(rankings_tbl)),
  function(i) {
    prof <- rankings_tbl$profiles[[i]]
    res  <- sp_run(prof)
    res$bench <- rankings_tbl$bench[i]
    res$tie   <- rankings_tbl$tie[i]
    res
  }
))

sp_summary <- summarise(
  group_by(sp_all, bench, tie),
  n_datasets      = n(),
  n_single_peaked = sum(single_peaked),
  .groups = "drop"
)
print(sp_summary)

gs_all <- bind_rows(lapply(
  seq_len(nrow(rankings_tbl)),
  function(i) {
    prof <- rankings_tbl$profiles[[i]]
    res <- select(
      mutate(
        prof,
        group_separable = map_lgl(profile, group_separable)
      ),
      dataset_id, group_separable
    )
    res$bench <- rankings_tbl$bench[i]
    res$tie   <- rankings_tbl$tie[i]
    res
  }
))

gs_summary <- summarise(
  group_by(gs_all, bench, tie),
  n_datasets        = n(),
  n_group_separable = sum(group_separable),
  .groups = "drop"
)
print(gs_summary)
