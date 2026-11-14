# Beyond Arrow
## From Impossibility to Possibility in Multi-Criteria Benchmarking

**Do benchmarks naturally exhibit structure in the domain
that allows for well-behaved multi-criteria, multi-task benchmarking?**

This repository contains the code for the experiments in our paper.
We test single-peakedness, group separability and distance-restrictedness
on HELM MMLU, PMLB and OpenML, and examine their implications for
benchmark design, model comparison and selection.

![Single-peaked structure in HELM MMLU](figure1.png)

*Business Ethics satisfies the single-peaked structure; Abstract Algebra
does not.*

## Contents

| File | Experiments | Paper |
| --- | --- | --- |
| [HELM_MMLU.R](HELM_MMLU.R) | Restricted preference domains, aggregation across datasets, sensitivity experiments, competing objectives and majority transitivity | Section 6; Appendices B, D.1 and E.3 |
| [Model_selection.R](Model_selection.R) | Model selection when the conditions fail, on HELM MMLU, PMLB and OpenML | Section 7; Appendix D.2 |
| [PMLB_OpenML_tests.R](PMLB_OpenML_tests.R) | Restricted preference domains on PMLB and OpenML | Appendix C |
| [Helm_MMLU_motiv_tests.R](Helm_MMLU_motiv_tests.R) | Condorcet cycles and instability to changes in the model set | Section 3; Appendices E.1–E.2 |

## Requirements

The analysis uses R and the packages listed below:

```r
install.packages(
  c("jsonlite", "dplyr", "purrr", "tibble", "digest", "lme4",
    "future", "future.apply", "tidyr", "relations",
    "BiocManager", "remotes"),
  repos = "https://cloud.r-project.org"
)

BiocManager::install("Rgraphviz", ask = FALSE, update = FALSE)

remotes::install_github(
  "hannahblo/ddandrda@2c7699f90a5f4cc68a164c1186d8376eff5b2b90",
  upgrade = "never"
)
```

## Reproducing the results

After preparing the data, run the following commands from the repository
folder in the same R session:

```r
source("HELM_MMLU.R")
source("Model_selection.R")
```

`Model_selection.R` uses objects created by `HELM_MMLU.R` and must be run
after it. If these objects are already loaded, only the second command
is needed.

The PMLB/OpenML structural tests and the motivation experiments can be
run independently:

```bash
Rscript PMLB_OpenML_tests.R
Rscript Helm_MMLU_motiv_tests.R
```

The HELM results are saved in `results/helm_mmlu/`.
The additional experiments and model-selection results are saved in
`results/revision/`.

## HELM MMLU  
The script "HELM_MMLU.R" reads HELM run folders, builds rankings for each MMLU subject, and then tests for:

- **Single-peakedness**,
- **Group separability**,
- **Distance-restrictedness**,
- and additionally provides the ranking representing the whole benchmark suite by **aggregating across datasets**.

The code was tested with

- R version 4.4.1
- R version 4.4.2
- R version 4.5.2

on

- macOS Sequoia 15.7.3
- Windows 11

You need access to the HELM MMLU raw run folders (v1.0.0). There are two ways to obtain them:

**Option A.** [Download the HELM MMLU raw results](https://zenodo.org/records/18402602?preview=1&token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjI1MDc3YWZjLWIzYWEtNDUzYy05NzBkLTY4OTA5NmEwMjcwOSIsImRhdGEiOnt9LCJyYW5kb20iOiI4NmI4MjY1MWJkMWNiZTFmNzM5NDFiYmUyYTc2YTI0MiJ9.SfRSP6FsVUywpMI0iBcdhiRran2YRlGyeso2JPgoPxhl7KXCWtCiMcziKANBDr6V2zUqQUBR7WeeMhEdjGodAQ). 

**Option B.** Download the HELM MMLU raw results from the public
`crfm-helm-public` bucket in Google Cloud Storage:

1. Install the Google Cloud CLI by following
   [Google's installation instructions](https://docs.cloud.google.com/sdk/docs/install-sdk).
2. Follow the
   [HELM instructions for downloading raw results](https://crfm-helm.readthedocs.io/en/latest/downloading_raw_results/),
   using the MMLU benchmark and version `v1.0.0`.

Place the HELM MMLU results in the following location:

```text
~/helm_mmlu/
  runs/
    v1.0.0/
      <run directory>/
        run_spec.json
        stats.json
        instances.json
        per_instance_stats.json
```

`run_spec.json` and `stats.json` provide the run settings and aggregate
metric values. The paired bootstrap additionally requires
`instances.json` and `per_instance_stats.json` for the selected runs.

## PMLB and OpenML

We use the final results from Jansen et al. (2024), available in the
[Statistical Multicriteria Benchmarking repository](https://github.com/hannahblo/Statistical-Multicriteria-Benchmarking-via-the-GSD-Front).

Place the following files in the repository folder, preserving their
subfolders:

```text
pmlb_results/
  final_results.RDS

openml_permutation_results/
  data_openml_filter.rds
``` 

## Transitivity and Stability Experiments

The script "Helm_MMLU_motiv_tests.R" runs the experiments from Section 3 of our paper, based on HELM MMLU (v1.0.0). It contains code for the search for Condorcet cycles and includes our investigation for situations benchmarks become unstable to irrelevant changes in the model set.

## References:

Jansen, C., Schollmeyer, G., Rodemann, J., Blocher, H., and Augustin, T. Statistical multicriteria benchmarking via the GSD-front. Advances in Neural Information Processing Systems, 37:98143–98179, 2024.
