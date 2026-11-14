### Model selection on HELM MMLU, PMLB and OpenML
### Run after HELM_MMLU.R in the same R session

stopifnot(exists("helm_selection"), exists("profile_test"), exists("selection_test"))

library(dplyr)
library(tidyr)
library(tibble)

out <- file.path("results", "helm_mmlu")
dir.create(out, recursive = TRUE, showWarnings = FALSE)

if (!exists("long_pmlb")) {
  fp_pmlb <- file.path("pmlb_results", "final_results.RDS")
  pmlb <- readRDS(fp_pmlb)
  long_pmlb <- pivot_longer(
    mutate(
      rownames_to_column(as.data.frame(pmlb), "id"),
      dataset_id = suppressWarnings(as.integer(sub(".*\\.(\\d+)$", "\\1", id))),
      dataset_id = ifelse(is.na(dataset_id), 0L, dataset_id),
      model = ifelse(grepl("\\.[0-9]+$", id), sub("\\.[0-9]+$", "", id), id)
    ),
    c(results_clean_stacked, results_noisy_x_stacked, results_noisy_y_stacked),
    names_to = "metric", values_to = "score"
  )
  long_pmlb <- select(
    mutate(long_pmlb, model = as.character(model), metric = as.character(metric),
           score = as.numeric(score)), dataset_id, model, metric, score
  )
}

if (!exists("long_openml")) {
  fp_openml <- file.path("openml_permutation_results", "data_openml_filter.rds")
  if (!file.exists(fp_openml)) {
    fp_openml <- file.path("openml_permutation_results", "dat_openml_filter.rds")
  }
  openml <- readRDS(fp_openml)
  long_openml <- pivot_longer(
    select(
      mutate(ungroup(openml), dataset_id = as.integer(factor(data.id)),
             model = as.character(learner.name), acc = as.numeric(predictive.accuracy),
             train_time = -as.numeric(usercpu.time.millis.training),
             test_time = -as.numeric(usercpu.time.millis.testing)),
      dataset_id, model, acc, train_time, test_time
    ),
    c(acc, train_time, test_time), names_to = "metric", values_to = "score"
  )
  long_openml <- select(
    mutate(long_openml, metric = as.character(metric), score = as.numeric(score)),
    dataset_id, model, metric, score
  )
}

selection_suite <- function(x, name) {
  md <- sort(unique(x$model))
  mets <- sort(unique(x$metric))
  ds <- sort(unique(x$dataset_id))
  k <- length(md)
  stopifnot(length(mets) == 3L, all(is.finite(x$score)),
            !anyDuplicated(x[c("dataset_id", "model", "metric")]))
  a <- axis_pos(k)
  rows <- lapply(ds, function(s) {
    z <- x[x$dataset_id == s, ]
    v <- matrix(NA_real_, k, 3L)
    v[cbind(match(z$model, md), match(z$metric, mets))] <- z$score
    stopifnot(all(is.finite(v)))
    p <- t(vapply(seq_len(3L), function(h) order(-v[, h], seq_len(k)), integer(k)))
    f <- profile_test(p, a)
    q <- selection_test(p)
    data.frame(benchmark = name, dataset_id = as.character(s), n_models = k,
               condition = any(f[c("sp", "gs", "dr")]), transitive = q$transitive,
               winner = if (length(q$winner)) md[q$winner] else NA_character_,
               smith_size = length(q$smith), smith = paste(md[q$smith], collapse = ";"))
  })
  do.call(rbind, rows)
}

selection_all <- rbind(
  data.frame(benchmark = "HELM MMLU",
             dataset_id = paste(helm_selection$A_set, helm_selection$subject, sep = ";"),
             helm_selection[c("n_models", "condition", "transitive", "winner",
                              "smith_size", "smith")]),
  selection_suite(long_pmlb, "PMLB"), selection_suite(long_openml, "OpenML")
)

selection_tbl <- do.call(rbind, lapply(c("HELM MMLU", "PMLB", "OpenML"), function(b) {
  z <- selection_all[selection_all$benchmark == b, ]
  data.frame(benchmark = b, profiles = nrow(z), condition = sum(z$condition),
             transitive = sum(z$transitive), winner = sum(!is.na(z$winner)),
             winner_without_transitivity = sum(!is.na(z$winner) & !z$transitive),
             no_winner = sum(is.na(z$winner)),
             smaller_shortlist_without_winner = sum(is.na(z$winner) & z$smith_size < z$n_models))
}))
print(selection_tbl)
write.csv(selection_tbl, file.path(out, "model_selection.csv"), row.names = FALSE)
write.csv(selection_all, file.path(out, "model_selection_profiles.csv"), row.names = FALSE)
