Portfolio-6
================
Conor Lacey
2023-04-06

``` r
suppressWarnings(library(tidyverse))
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

### Introduction

In this portfolio I will be doing some analysis of the previously
generated simulation data from portfolio 5.

### Read in data

To begin I will first need to read in the data. Fortunately, I changed
the previous portfolio so that is exports the data as .RDS files because
these are actual R files that make it more compatible for analysis work
in R naturally.

``` r
dMACS <- read_rds("data/dat_dMACS.RDS")
dMACS_low <- read_rds("data/dat_dMACS_low.RDS")
dMACS_high <- read_rds("data/dat_dMACS_high.RDS")

dMACS_S <- read_rds("data/dat_dMACS_S.RDS")
dMACS_s_low <- read_rds("data/dat_dMACS_S_low.RDS")
dMACS_s_high <- read_rds("data/dat_dMACS_S_high.RDS")
```

You may also notice that there are some new data files that I didn’t
include in my previous portfolio. These new files with the words “low”
and “high” are the confidence and credible intervals of the generated
data. This will be useful in assessing how often there is possible
effect size of zero.

### Mean of Columns

Now because I generated the data over 500 times I now want to average
that data in each of the 54 conditions. This is fairly easy in R by
simply using the colMeans() function since each column is one condition.
I will then transpose this data so that the conditions then become the
rows.

``` r
dMACS_a <- dMACS %>% colMeans() %>% data.frame()
dMACS_low_a <- dMACS_low %>% colMeans() %>% data.frame()
dMACS_high_a <- dMACS_high %>% colMeans() %>% data.frame()
dMACS_df <- bind_cols(dMACS_a, dMACS_low_a, dMACS_high_a) %>% 
  setNames(c("dMACS", "Lower", "Upper"))
```

    ## New names:
    ## • `.` -> `....1`
    ## • `.` -> `....2`
    ## • `.` -> `....3`

``` r
head(dMACS_df)
```

    ##        dMACS       Lower     Upper
    ## 1 0.09802426 -0.02623257 0.2222811
    ## 2 0.09810867  0.01024561 0.1859717
    ## 3 0.09796398  0.03583591 0.1600921
    ## 4 0.49670651  0.36531801 0.6280950
    ## 5 0.49674371  0.40383932 0.5896481
    ## 6 0.49595902  0.43027793 0.5616401

``` r
dMACS_S_a <- dMACS_S %>% colMeans() %>% data.frame()
dMACS_s_low_a <- dMACS_s_low %>% colMeans() %>% data.frame()
dMACS_s_high_a <- dMACS_s_high %>% colMeans() %>% data.frame()
dMACS_S_df <- bind_cols(dMACS_S_a, dMACS_s_low_a, dMACS_s_high_a) %>% 
  setNames(c("dMACS_S", "Lower", "Upper"))
```

    ## New names:
    ## • `.` -> `....1`
    ## • `.` -> `....2`
    ## • `.` -> `....3`

``` r
head(dMACS_S_df)
```

    ##      dMACS_S         Lower     Upper
    ## 1 0.03014687 -2.006728e-17 0.1779429
    ## 2 0.05019623 -5.326295e-17 0.1596156
    ## 3 0.08704438 -4.079792e-16 0.1483081
    ## 4 0.49472760  3.710158e-01 0.6184394
    ## 5 0.49575221  4.081875e-01 0.5833169
    ## 6 0.49546355  4.335150e-01 0.5574121

### Proportion of Estimates with No Effects

The last thing I will be doing this analysis is calculating the the
percent of estimates that include zero in their confidence or credible
interval. If they do, this is an indication that an effect size of zero
is within reason. Effectively what this analysis will tell me is how
often there is truly a zero effect.

As I did previously, the data frame generated here will have 54 rows
representing the conditions.

``` r
props <- matrix(nrow = 54, ncol = 1) %>% data.frame() %>% 
  setNames(c("Percent"))
for (i in 1:54){
props[i,1] <- sum((dMACS_low[i] <= 0 & dMACS_high[i] >= 0)/500)*100
}
head(props)
```

    ##   Percent
    ## 1     100
    ## 2       0
    ## 3       0
    ## 4       0
    ## 5       0
    ## 6       0

``` r
props_shrunk <- matrix(nrow = 54, ncol = 1) %>% data.frame() %>% 
  setNames(c("Percent"))
for (i in 1:54){
  props_shrunk[i,1] <- sum((dMACS_s_low[i] <= 0 & dMACS_s_high[i] >= 0)/500)*100
}
head(props_shrunk)
```

    ##   Percent
    ## 1    97.0
    ## 2    97.0
    ## 3    99.4
    ## 4     0.0
    ## 5     0.0
    ## 6     0.0
