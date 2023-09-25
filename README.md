
<!-- README.md is generated from README.Rmd. Please edit that file -->

# virtualclust

<!-- badges: start -->
<!-- badges: end -->

The goal of virtualclust is to …

## Installation

You can install the development version of virtualclust like so:

``` r
# install.packages("devtools")
devtools::install_github("mncube/virtualclust")
```

## Example

This is a basic example which shows you how to solve simulate data where
students are clustered within teachers and each student has a virtual
comparison formed from summarizing a many-to-one match.

After simulating data we summarize the results from different
statistical models.

``` r
library(virtualclust)

set.seed(123)
sim_data <- sim_tc_cv(n_treatment = 100, n_control = 1000, n_matches = 10)
head(sim_data$combined_data)
#>   student_id pre_score post_score teacher virtual_score
#> 1          1  44.39524   44.89593       3      52.84955
#> 2          2  47.69823   54.56884       1      49.79685
#> 3          3  65.58708   49.53308       5      47.38696
#> 4          4  50.70508   48.52457       1      53.16517
#> 5          5  51.29288   42.48381       4      51.88730
#> 6          6  67.15065   51.54972       2      45.61986

summary_results <- vc_summary(sim_data)
#> Computing profile confidence intervals ...
#> Computing profile confidence intervals ...
#> boundary (singular) fit: see help('isSingular')
#> Computing profile confidence intervals ...
#> Computing profile confidence intervals ...
#> boundary (singular) fit: see help('isSingular')
#> Computing profile confidence intervals ...
#> Computing profile confidence intervals ...
#> boundary (singular) fit: see help('isSingular')
head(summary_results)
#> # A tibble: 5 × 8
#>   term           estimate std.error statistic p.value conf.low conf.high model  
#>   <chr>             <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>  
#> 1 grouptreatment    0.484     0.994     0.487   0.627    6.13       7.91 Mixed …
#> 2 grouptreatment    0.484     1.02      0.475   0.635    6.52       7.93 Mixed …
#> 3 grouptreatment    0.484     1.02      0.475   0.635    6.52       7.93 Mixed …
#> 4 difference        0.484    NA        NA       0.627   -1.49       2.46 Paired…
#> 5 difference        0.484    NA        NA       0.135   -0.152      1.12 Two-St…
```
