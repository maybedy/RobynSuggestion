# Project Robyn Suggestion 
## We suggest several key innovations of validation procedures by adding Response-driven, Dependent-driven Approaches for practical online marketing !!!

## 0. Related Links
- **Details about our suggestions**: [Link](https://eurobae.notion.site/Meta-APAC-Robyn-Hackathon-2022-66f8fdb6819b4370864c731a9f190eaa)
- Facebook's Robyn
[![website](https://img.shields.io/badge/site-Robyn-blue.svg)](https://facebookexperimental.github.io/Robyn/)

## 1. Pre-requisite
- Install R
- Install Robyn & required python/R packages
```
remotes::install_github("facebookexperimental/Robyn/R")
```
- Install RobynSuggestion
```
install.packages("remotes")
library(remotes)
remotes::install_github("maybedy/RobynSuggestion")
```

## 2. Overview - Codes & Functions
0) demo.R
1) R/inputs.R
- __func__ put_hyper_params(...)
2) R/media_channel.R
- __func__ generate_budget_boundaries(...)
- __func__ decompose_dependent_vars(...)
- __func__ get_response_sum_on_trainining(...)
- __func__ predict_response_sum_on_test(...)
- __func__ get_individual_result(...)
- __func__ predict_individual_result(...)
3) R/allocator.R
- __func__ get_allocator_benchmarks(...)
4) R/validate.R
- __func__ validate_predicts(...)
5) R/plot.R
- __func__ robyn_onepagers_revised(...)
6) R/transformation.R
- __func__ saturation_hill_revised(...)
- __func__ adstock_weibull_revised(...)
7) R/zero_spend.R
- zero_spend versions of other functions
8) R/imports.R
- From Robyn's repository

## 3. Check the result from demo - demo.R
We wrote revised version of demo from original demo in Robyn. 
You can check the actual runnings and related functions of our suggestions.
Details are added in the file.

## 4. Contacts
- wkddudwoek@gmail.com, Youngjae Jang, Main author
- dykim.ses@gmail.com, Doyun Kim, Contributor & Code maintainer
- baeuro96@gmail.com, Euro Bae, Contributor