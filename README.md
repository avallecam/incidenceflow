
<!-- README.md is generated from README.Rmd. Please edit that file -->

# incidenceflow

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/incidenceflow)](https://cran.r-project.org/package=incidenceflow)
<!-- badges: end -->

The goal of incidenceflow is to …

## Installation

You can install the released version of incidenceflow from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("incidenceflow")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(incidenceflow)
## basic example code
```

### incidence(cially) usefull

  - `get_info_tidy`: generates a tibble of `incidence::get_info()`.
    [clink here for more
    information](https://www.repidemicsconsortium.org/incidence/index.html).
  - `tidy_incidence`: generates a complete summary tibble from incidence
    fit paramteter estimates
  - `glance_incidence`: generates a complete summary tibble from
    incidence fit model performance

<!-- end list -->

``` r
# packages ----------------------------------------------------------------

if(!require("devtools")) install.packages("devtools")
#> Loading required package: devtools
#> Loading required package: usethis
# if(!require("avallecam")) devtools::install_github("avallecam/avallecam") #improvements

library(tidyverse) #magrittr and purrr packages
#> -- Attaching packages ---------------------------------------------------------------- tidyverse 1.2.1 --
#> v ggplot2 3.3.0     v purrr   0.3.3
#> v tibble  3.0.3     v dplyr   1.0.1
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.3.1     v forcats 0.5.0
#> Warning: package 'ggplot2' was built under R version 3.6.3
#> Warning: package 'tibble' was built under R version 3.6.3
#> Warning: package 'dplyr' was built under R version 3.6.3
#> Warning: package 'forcats' was built under R version 3.6.3
#> -- Conflicts ------------------------------------------------------------------- tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
library(lubridate) #ymd
#> 
#> Attaching package: 'lubridate'
#> The following object is masked from 'package:base':
#> 
#>     date
library(outbreaks) #sample data
library(incidence) #core functions

# example outbreak --------------------------------------------------------

dat <- ebola_sim$linelist$date_of_onset
i.7 <- incidence(dat, interval=7)
# plot(i.7)
f1 <- fit(i.7[1:20])
f2 <- fit_optim_split(i.7)

# broom like functions ----------------------------------------------------

# tidy
f1 %>% tidy_incidence()
#> # A tibble: 2 x 5
#>   mark  parameter estimate conf_lower conf_upper
#>   <chr> <chr>        <dbl>      <dbl>      <dbl>
#> 1 1     r           0.0318     0.0260     0.0376
#> 2 1     doubling   21.8       18.5       26.7
f2 %>% pluck("fit") %>% tidy_incidence()
#> # A tibble: 4 x 5
#>   mark   parameter estimate conf_lower conf_upper
#>   <chr>  <chr>        <dbl>      <dbl>      <dbl>
#> 1 before r           0.0298     0.0261    0.0336 
#> 2 after  r          -0.0102    -0.0110   -0.00930
#> 3 before doubling   23.2       20.7      26.6    
#> 4 after  halving    68.2       62.9      74.5

# glance
f1 %>% glance_incidence()
#> # A tibble: 1 x 11
#>   r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC   BIC
#>       <dbl>         <dbl> <dbl>     <dbl>    <dbl> <int>  <dbl> <dbl> <dbl>
#> 1     0.880         0.874 0.498      133. 9.81e-10     2  -13.4  32.8  35.7
#> # ... with 2 more variables: deviance <dbl>, df.residual <int>
f2 %>% pluck("fit") %>% glance_incidence()
#> # A tibble: 2 x 12
#>   names r.squared adj.r.squared sigma statistic  p.value    df logLik   AIC
#>   <chr>     <dbl>         <dbl> <dbl>     <dbl>    <dbl> <int>  <dbl> <dbl>
#> 1 befo~     0.922         0.919 0.455      273. 2.95e-14     2  -14.8  35.5
#> 2 after     0.951         0.949 0.155      578. 3.72e-21     2   15.4 -24.8
#> # ... with 3 more variables: BIC <dbl>, deviance <dbl>, df.residual <int>


# using purrr -------------------------------------------------------------

# using purrr::map family function allows easy stratification
# for gender and could be extrapolated to administrative levels
# in country level analysis

incidence_purrr <- ebola_sim$linelist %>% 
  as_tibble() %>% 
  #filter observations explicitly before incidence()
  filter(date_of_onset<lubridate::ymd(20141007)) %>% 
  #stratify by any group of covariates
  group_by(gender) %>% 
  nest() %>% 
  mutate(incidence_strata=map(.x = data,
                              .f = ~incidence(.x %>% pull(date_of_onset),
                                              interval=7))) %>% 
  mutate(strata_fit=map(.x = incidence_strata,
                            .f = fit)) %>% 
  mutate(strata_fit_tidy=map(.x = strata_fit,
                                  .f = tidy_incidence)) %>% 
  mutate(strata_fit_glance=map(.x = strata_fit,
                                       .f = glance_incidence))
#> Warning: Problem with `mutate()` input `strata_fit`.
#> x 1 dates with incidence of 0 ignored for fitting
#> i Input `strata_fit` is `map(.x = incidence_strata, .f = fit)`.
#> i The error occurred in group 1: gender = "f".
#> Warning in .f(.x[[i]], ...): 1 dates with incidence of 0 ignored for
#> fitting
#> Warning: Problem with `mutate()` input `strata_fit`.
#> x 1 dates with incidence of 0 ignored for fitting
#> i Input `strata_fit` is `map(.x = incidence_strata, .f = fit)`.
#> i The error occurred in group 2: gender = "m".
#> Warning in .f(.x[[i]], ...): 1 dates with incidence of 0 ignored for
#> fitting

# keep only the tibbles
incidence_purrr_tibble <- incidence_purrr %>% 
  select(-data,-incidence_strata,-strata_fit)

# tidy_incidence output
incidence_purrr_tibble %>% 
  unnest(cols = c(strata_fit_tidy))
#> # A tibble: 4 x 7
#> # Groups:   gender [2]
#>   gender mark  parameter estimate conf_lower conf_upper strata_fit_glance
#>   <fct>  <chr> <chr>        <dbl>      <dbl>      <dbl> <list>           
#> 1 f      1     r           0.0228     0.0189     0.0267 <tibble [1 x 11]>
#> 2 f      1     doubling   30.3       25.9       36.6    <tibble [1 x 11]>
#> 3 m      1     r           0.0241     0.0188     0.0294 <tibble [1 x 11]>
#> 4 m      1     doubling   28.7       23.6       36.8    <tibble [1 x 11]>

# glance_incidence output
incidence_purrr_tibble %>% 
  unnest(cols = c(strata_fit_glance))
#> # A tibble: 2 x 13
#> # Groups:   gender [2]
#>   gender strata_fit_tidy r.squared adj.r.squared sigma statistic  p.value
#>   <fct>  <list>              <dbl>         <dbl> <dbl>     <dbl>    <dbl>
#> 1 f      <tibble [2 x 5~     0.859         0.853 0.510     146.  1.05e-11
#> 2 m      <tibble [2 x 5~     0.795         0.786 0.657      89.0 2.26e- 9
#> # ... with 6 more variables: df <int>, logLik <dbl>, AIC <dbl>, BIC <dbl>,
#> #   deviance <dbl>, df.residual <int>
```

## A more update approach here

look at this project: <https://github.com/reconhub/incidence2>
