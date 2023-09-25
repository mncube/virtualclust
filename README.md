
<!-- README.md is generated from README.Rmd. Please edit that file -->

# virtualclust

<!-- badges: start -->
<!-- badges: end -->

The goal of virtualclust is to facilate exploring education data with in
the treatment group and virtual observations in the control group.

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
sim_data <- sim_tc_cv_icc(n_treatment = 100, n_control = 1000, n_matches = 10,
                          icc = 0.08)
head(sim_data$combined_data)
#>   subclass student_id teacher treatment pre_score post_score   distance weights
#> 1        1          1       4         1  44.44194   71.35603 0.09834290       1
#> 2       10         17       3         1  60.24008   58.74606 0.07986495       1
#> 3      100         99       1         1  39.82671   44.87586 0.10442031       1
#> 4       11         18       4         1  34.68124   64.02092 0.11158486       1
#> 5       12         19       1         1  47.88180   45.46483 0.09402019       1
#> 6       13          2       1         1  69.39490   56.17653 0.07066337       1
#>   virtual_score
#> 1      51.12438
#> 2      50.29263
#> 3      55.21269
#> 4      50.37830
#> 5      49.15351
#> 6      50.16163

summary_results <- vc_summary(sim_data)
head(summary_results)
#> # A tibble: 6 × 8
#>   term           estimate std.error statistic p.value conf.low conf.high model  
#>   <chr>             <dbl>     <dbl>     <dbl>   <dbl>    <dbl>     <dbl> <chr>  
#> 1 grouptreatment     2.49     0.974     2.56   0.0120    0.559      4.42 Mixed …
#> 2 grouptreatment     2.49     0.983     2.53   0.0120    0.553      4.43 Mixed …
#> 3 grouptreatment     2.62     3.48      0.753  0.505    -8.27      13.5  Mixed …
#> 4 grouptreatment     2.49     0.974     2.56   0.0120    0.559      4.42 Mixed …
#> 5 grouptreatment     2.62     3.47      0.757  0.503    -8.27      13.5  Mixed …
#> 6 difference         2.49    NA        NA      0.0120    0.559      4.42 Paired…
```
