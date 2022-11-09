# Project Robyn Suggestion 
## We suggest several key innovations of validation procedures by adding Response-driven, Dependent-driven Approaches for practical marketing mix modeling!!!

## 0. Related Links
- **Details about our suggestions**: [Link](https://eurobae.notion.site/Meta-APAC-Robyn-Hackathon-2022-66f8fdb6819b4370864c731a9f190eaa)
- **Demo**: [Link](https://youtu.be/OFcPg75eKQU)
- Facebook's Robyn
[![website](https://img.shields.io/badge/site-Robyn-blue.svg)](https://facebookexperimental.github.io/Robyn/)

## 1. Pre-requisite
- Install R
- Install Robyn (dev version) & required python/R packages
```
remotes::install_github("facebookexperimental/Robyn/R")
```
- Install RobynSuggestion by using remotes
```
# If you don't have remotes library,
install.packages("remotes")
library(remotes)

# Install vis remotes, or download & install it manually
remotes::install_github("maybedy/RobynSuggestion")
```

## 2. Overview - Codes & Functions
0) demo.R
1) R/inputs.R
- _func_ put_hyper_params(...)
2) R/media_channel.R
- _func_ generate_budget_boundaries(...)
- _func_ decompose_dependent_vars(...)
- _func_ get_response_sum_on_trainining(...)
- _func_ predict_response_sum_on_test(...)
- _func_ get_individual_result(...)
- _func_ predict_individual_result(...)
3) R/allocator.R
- _func_ get_allocator_benchmarks(...)
4) R/validate.R
- _func_ validate_predicts(...)
5) R/plot.R
- _func_ robyn_onepagers_revised(...)
6) R/transformation.R
- _func_ saturation_hill_revised(...)
- _func_ adstock_weibull_revised(...)
7) R/zero_spend.R
- zero_spend versions of functions
8) R/imports.R
- Come from Robyn's package

## 3. Check the result from demo - demo.R
We wrote revised version of demo from original demo in Robyn. 
You can check the actual runnings and related functions of our suggestions.
Details are added in the file.

## 4. Contacts
- wkddudwoek@gmail.com, Youngjae Jang, Main author & Code maintainer
- dykim.ses@gmail.com, Doyun Kim, Contributor & Code maintainer
- baeuro96@gmail.com, Euro Bae, Contributor
- yeeun160291@gmail.com, Yeeun Kim, Contributor