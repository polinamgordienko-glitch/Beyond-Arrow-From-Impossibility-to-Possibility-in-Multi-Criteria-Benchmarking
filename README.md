# Beyond Arrow: From Impossibility to Possibility in Multi-Criteria Benchmarking

## Introduction

This repository contains the code to reproduce all experiments in our paper titled "Beyond Arrow: From Impossibility to Possibility in Multi-Criteria Benchmarking". The focus is on empirically testing restricted preference domain assumptions in multi-criteria benchmarking settings, using two main data sources:
 - HELM MMLU (rankings constructed from HELM MMLU runs)
 - PMLB & OpenML (final result tables from Jansen et al 2024)



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

Option A: Download the HELM MMLU raw results under this link: https://zenodo.org/records/18402602?preview=1&token=eyJhbGciOiJIUzUxMiJ9.eyJpZCI6IjI1MDc3YWZjLWIzYWEtNDUzYy05NzBkLTY4OTA5NmEwMjcwOSIsImRhdGEiOnt9LCJyYW5kb20iOiI4NmI4MjY1MWJkMWNiZTFmNzM5NDFiYmUyYTc2YTI0MiJ9.SfRSP6FsVUywpMI0iBcdhiRran2YRlGyeso2JPgoPxhl7KXCWtCiMcziKANBDr6V2zUqQUBR7WeeMhEdjGodAQ. 

Option B: Download the HELM MMLU raw results from the public crfm-helm-public bucket in the Google Cloud Storage (GCS) by completing the following two steps:

1.	Install the Google Cloud CLI (gcloud) by following Google’s official instructions: 
https://docs.cloud.google.com/sdk/docs/install-sdk.

2.	Download HELM MMLU raw results by following the official HELM instructions:
https://crfm-helm.readthedocs.io/en/latest/downloading_raw_results/.

After downloading via Option A or B, the code expects the HELM MMLU results to be placed in a folder structure that looks like this (schematically):

```text
helm_mmlu/
  runs/
    v1.0.0/
      <RUN_ID_1>/
        run_spec.json
        stats.json
      ...
```
The key point is: under helm_mmlu/runs/v1.0.0/ there should be many run directories (each a run id), and each run directory should contain at least run_spec.json and stats.json.

## PMLB & OpenML

The script "PMLB_OpenML_tests.R" runs the restricted preference domain tests on PMLB and OpenML.

For our experiments on PMLB and OpenML we use the final results from Jansen et al. (2024). These can be downloaded under the following link: https://github.com/hannahblo/Statistical-Multicriteria-Benchmarking-via-the-GSD-Front. After downloading that repository, ensure the following files are available: pmlb_results/final_results.RDS (PMLB data) and openml_permutation_results/dat_openml_filter.rds (OpenML data). 

## Transitivity and Stability Experiments

The script "Helm_MMLU_motiv_tests.R" runs the experiments from Section 3 of our paper, based on HELM MMLU (v1.0.0). It contains code for the search for Condorcet cycles and includes our investigation for situations benchmarks become unstable to irrelevant changes in the model set.

## References:

Jansen, C., Schollmeyer, G., Rodemann, J., Blocher, H., and Augustin, T. Statistical multicriteria benchmarking via the GSD-front. Advances in Neural Information Processing Systems, 37:98143–98179, 2024.
