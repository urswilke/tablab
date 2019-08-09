
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dcmisc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dcmisc)](https://cran.r-project.org/package=dcmisc)
<!-- badges: end -->

The goal of dcmisc is to tabulate and compare variables’ values and
their labels of labelled dataframes.

## Installation

Install the package
via:

``` r
# install.packages("K:/Tools/R/self-written packages/dcmisc/", type = "source", repos = NULL)
```

## Introduction

The package dcmisc consists of 2 families of functions:

  - `tab_*` tabulating count and label information of dataframes
    (denoted `df`), and
  - `cmp_*` comparing this data for all dataframes in a list `l`.

The following attributes are tabulated / compared:

  - `var` The name of the variables in the dataframe(s).
  - `varlab` The variable label of the variable in the dataframe.
  - `val` The value of the variable in the dataframe.
  - `vallab` The value label of the variable in the dataframe.

Comparisons further include `ex`, denoting whether the value is existent
in the dataframe.

## Example

``` r
library(dcmisc)
library(tidyverse)
#> ── Attaching packages ────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.2.0     ✔ purrr   0.3.2
#> ✔ tibble  2.1.3     ✔ dplyr   0.8.3
#> ✔ tidyr   0.8.3     ✔ stringr 1.4.0
#> ✔ readr   1.3.1     ✔ forcats 0.4.0
#> ── Conflicts ───────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()


path <- system.file("examples", "iris.sav", package = "haven")
df <- haven::read_sav(path)
df <- tibble(id = 1:10,
       sex = haven::labelled(c(2, 1, 2, 1, 1, 2, 2, 1, 2, 1),
                             label = "sex",
                             labels = c(MALES = 1, FEMALES = 2)),
       age = c(24, 23, 23, 41, 23, 39, 30, 18, 31, 48),
       marital = haven::labelled(c(1, 7, 2, 6, 4, 5, 3, 8, 4, 2),
                                 label = "marital status",
                                 labels = c("single" = 1,
                                            "steady relationship" = 2,
                                            "living with partner" = 3,
                                            "married first time" = 4,
                                            "remarried" = 5,
                                            "separated" = 6,
                                            "divorced" = 7,
                                            "widowed" = 8))
       )
df
#> # A tibble: 10 x 4
#>       id         sex   age                 marital
#>    <int>   <dbl+lbl> <dbl>               <dbl+lbl>
#>  1     1 2 [FEMALES]    24 1 [single]             
#>  2     2 1 [MALES]      23 7 [divorced]           
#>  3     3 2 [FEMALES]    23 2 [steady relationship]
#>  4     4 1 [MALES]      41 6 [separated]          
#>  5     5 1 [MALES]      23 4 [married first time] 
#>  6     6 2 [FEMALES]    39 5 [remarried]          
#>  7     7 2 [FEMALES]    30 3 [living with partner]
#>  8     8 1 [MALES]      18 8 [widowed]            
#>  9     9 2 [FEMALES]    31 4 [married first time] 
#> 10    10 1 [MALES]      48 2 [steady relationship]
```

``` r
df2 <- df %>% slice(-c(9:10)) %>% mutate(new_var = 1)
attr(df2$new_var, "label")  <-  "new something"

df2[1:2,"sex"] <- 3
attr(df2$sex, "labels")  <-  c(male = 1, female = 2, `Third Gender` = 3)
attr(df2$sex, "label")  <-  NULL
df2[3:4,"marital"] <- 9
attr(df2$marital, "labels")  <-  c("single" = 1,
                                   "steady relationship" = 2,
                                   "living with partner" = 3,
                                   "married first time" = 4,
                                   "remarried" = 5,
                                   "separated" = 6,
                                   "divorced" = 7,
                                   "widowed" = 8,
                                   "married to Jesus" = 9)
df2
#> # A tibble: 8 x 5
#>      id              sex   age                 marital new_var
#>   <int>        <dbl+lbl> <dbl>               <dbl+lbl>   <dbl>
#> 1     1 3 [Third Gender]    24 1 [single]                    1
#> 2     2 3 [Third Gender]    23 7 [divorced]                  1
#> 3     3 2 [female]          23 9 [married to Jesus]          1
#> 4     4 1 [male]            41 9 [married to Jesus]          1
#> 5     5 1 [male]            23 4 [married first time]        1
#> 6     6 2 [female]          39 5 [remarried]                 1
#> 7     7 2 [female]          30 3 [living with partner]       1
#> 8     8 1 [male]            18 8 [widowed]                   1
```

Put both dataframes in list:

``` r
l <- list(df, df2)
```

Tabulate the variable labels for the dataframes in the list:

``` r
# variable label-Tabelle erstellen:
l %>% map(tab_varlabs)
#> [[1]]
#> # A tibble: 2 x 2
#>   var     varlab        
#>   <chr>   <chr>         
#> 1 sex     sex           
#> 2 marital marital status
#> 
#> [[2]]
#> # A tibble: 2 x 2
#>   var     varlab        
#>   <chr>   <chr>         
#> 1 marital marital status
#> 2 new_var new something
```

Tabulate the variable labels for the dataframes in the list:

``` r
# value label-Tabelle erstellen:
l %>% map(tab_vallabs)
#> [[1]]
#> # A tibble: 10 x 3
#>    var       val vallab             
#>    <chr>   <dbl> <chr>              
#>  1 sex         1 MALES              
#>  2 sex         2 FEMALES            
#>  3 marital     1 single             
#>  4 marital     2 steady relationship
#>  5 marital     3 living with partner
#>  6 marital     4 married first time 
#>  7 marital     5 remarried          
#>  8 marital     6 separated          
#>  9 marital     7 divorced           
#> 10 marital     8 widowed            
#> 
#> [[2]]
#> # A tibble: 12 x 3
#>    var       val vallab             
#>    <chr>   <dbl> <chr>              
#>  1 sex         1 male               
#>  2 sex         2 female             
#>  3 sex         3 Third Gender       
#>  4 marital     1 single             
#>  5 marital     2 steady relationship
#>  6 marital     3 living with partner
#>  7 marital     4 married first time 
#>  8 marital     5 remarried          
#>  9 marital     6 separated          
#> 10 marital     7 divorced           
#> 11 marital     8 widowed            
#> 12 marital     9 married to Jesus
```

Tabulate the count comparison:

``` r
l %>% cmp_cts() %>% DT::datatable()
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

Tabulate the comparison of count and label data:

``` r
l %>% cmp_all() %>% DT::datatable()
#> Joining, by = c("var", "val1")
#> Joining, by = c("var", "val2")
#> Joining, by = c("var", "val")
#> Joining, by = c("var", "val")
#> Joining, by = c("var", "vallab")
#> Joining, by = c("var", "val1")
#> Joining, by = c("var", "val2")
#> Joining, by = "var"
#> Joining, by = "var"
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

## weitere infos

Hilfe anzeigen lassen über: ?tab\_varlabs oder ?tab\_vallabs oder durch
Mit-cursor-auf-eine-der-Funktionen-Gehen und F1 drücken (das geht aber
nicht im Kommentar) und nur nachdem die library schon geladen wurde

``` r
  
# message, wenn keine value label im data.frame sind
tab_vallabs(data.frame(NA))
#> no variable in data.frame of type haven::labelled
#> # A tibble: 0 x 3
#> # … with 3 variables: var <chr>, val <dbl>, vallab <chr>
```

informative Fehler-Nachricht wenn df kein data.frame ist:

``` r
# tab_vallabs(1)
```

erzeugt den Fehler:

``` r
# Error: df is not a data frame
```

wenn Du anfängst eine der Funktionen zu schreiben, kommt dann auch eine
kleine informative Hilfe-Box für die geforderten Eingangs-Argumente,
sobald die autocomplete-Vorschäge erscheinen (so wie das Excel auch
macht…)
