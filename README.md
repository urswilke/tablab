
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tablab

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tablab)](https://cran.r-project.org/package=tablab)
<!-- badges: end -->

The goal of tablab is to tabulate and compare variables’ values and
their labels of labelled dataframes.

## Installation

Install the package via:

``` r
devtools::install_github("urswilke/tablab")
```

## Introduction

The package tablab mainly consists of 2 families of functions:

-   `tab_*` tabulating count and label information of dataframes
    (denoted `df`), and
-   `cmp_*` comparing this data for all dataframes in a list `l`.

The following attributes are tabulated / compared:

-   `var` The name of the variables in the dataframe(s).
-   `type` The type of the variables in the dataframe(s).
-   `varlab` The variable label of the variable in the dataframe.
-   `val` The value of the variable in the dataframe.
-   `vallab` The value label of the variable in the dataframe.

## Examples

### Simple example

Assume you have a dataframe of labelled variables:

``` r
library(tablab)
library(dplyr)
library(purrr)
set.seed(1)

x <- haven::labelled(
  sample(1:10), 
  label = "variable label", 
  labels = c("1lab" =1 , "2lab" = 2, "3lab" = 3)
)
y <- haven::labelled(
  LETTERS[sample(2:6, size = 10, replace = TRUE)], 
  label = "char_var_lab", 
  labels = c("Alab" ="A" , "Blab" = "B", "Clab" = "C")
)

df <- data.frame(id = 1:10, x, y)
df
#>    id  x y
#> 1   1  9 D
#> 2   2  4 D
#> 3   3  7 B
#> 4   4  1 F
#> 5   5  2 F
#> 6   6  5 C
#> 7   7  3 C
#> 8   8 10 B
#> 9   9  6 F
#> 10 10  8 F
```

tablab consists of functions to tabulate summaries of these labelled
dataframes. See for instance the function:

``` r
tab_all(df)
#> # A tibble: 15 × 6
#>    var      nv cv        n vallab varlab        
#>    <chr> <int> <chr> <int> <chr>  <chr>         
#>  1 x         1 <NA>      1 1lab   variable label
#>  2 x         2 <NA>      1 2lab   variable label
#>  3 x         3 <NA>      1 3lab   variable label
#>  4 x         4 <NA>      1 <NA>   variable label
#>  5 x         5 <NA>      1 <NA>   variable label
#>  6 x         6 <NA>      1 <NA>   variable label
#>  7 x         7 <NA>      1 <NA>   variable label
#>  8 x         8 <NA>      1 <NA>   variable label
#>  9 x         9 <NA>      1 <NA>   variable label
#> 10 x        10 <NA>      1 <NA>   variable label
#> 11 y        NA B         2 Blab   char_var_lab  
#> 12 y        NA C         2 Clab   char_var_lab  
#> 13 y        NA D         2 <NA>   char_var_lab  
#> 14 y        NA F         4 <NA>   char_var_lab  
#> 15 y        NA A        NA Alab   char_var_lab
```

The result shows for every variable `var` in the data, its
numeric/character values `nv`/`cv`, their counts `n`, as well as the
variable and value labels `varlab` and `vallab`.

### More examples

``` r

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
#> # A tibble: 10 × 4
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

Create modified copy:

``` r
df2 <- 
  df %>% 
  # Remove rows 9 & 10:
  slice(-c(9:10)) %>% 
  # Create Variable new_var equal to 1:
  mutate(new_var = 1)
# Change variable label:
attr(df2$new_var, "label")  <-  "new something"
```

Tabulate the variable labels for the dataframes in the list:

``` r
list(df, df2) %>% map(tab_varlabs)
#> [[1]]
#> # A tibble: 2 × 2
#>   var     varlab        
#>   <chr>   <chr>         
#> 1 sex     sex           
#> 2 marital marital status
#> 
#> [[2]]
#> # A tibble: 3 × 2
#>   var     varlab        
#>   <chr>   <chr>         
#> 1 sex     sex           
#> 2 marital marital status
#> 3 new_var new something
```

Change value:

``` r
df2[1:2,"sex"] <- 3
```

Tabulate the count comparison:

``` r
list(df, df2) %>% cmp_cts()
#> # A tibble: 25 × 6
#>    var     nv1 cv1     nv2 cv2       n
#>    <chr> <int> <chr> <int> <chr> <int>
#>  1 sex       1 <NA>      1 <NA>      3
#>  2 sex       1 <NA>      3 <NA>      1
#>  3 sex       1 <NA>     NA <NA>      1
#>  4 sex       2 <NA>      2 <NA>      3
#>  5 sex       2 <NA>      3 <NA>      1
#>  6 sex       2 <NA>     NA <NA>      1
#>  7 age      18 <NA>     18 <NA>      1
#>  8 age      23 <NA>     23 <NA>      3
#>  9 age      24 <NA>     24 <NA>      1
#> 10 age      30 <NA>     30 <NA>      1
#> # … with 15 more rows
```

More label and value modifications:

``` r
attr(df2$sex, "labels")  <-  c(male = 1, female = 2, `Third Gender` = 3)
# Remove variable label:
attr(df2$sex, "label")  <-  NULL
# Change variable "marital" to 9 in rows 3 & 4:
df2[3:4,"marital"] <- 9
# Add another value label for the code 9:
attr(df2$marital, "labels") <- c(attr(df2$marital, "labels"), c("other" = 9))

df2
#> # A tibble: 8 × 5
#>      id              sex   age                 marital new_var
#>   <int>        <dbl+lbl> <dbl>               <dbl+lbl>   <dbl>
#> 1     1 3 [Third Gender]    24 1 [single]                    1
#> 2     2 3 [Third Gender]    23 7 [divorced]                  1
#> 3     3 2 [female]          23 9 [other]                     1
#> 4     4 1 [male]            41 9 [other]                     1
#> 5     5 1 [male]            23 4 [married first time]        1
#> 6     6 2 [female]          39 5 [remarried]                 1
#> 7     7 2 [female]          30 3 [living with partner]       1
#> 8     8 1 [male]            18 8 [widowed]                   1
```

Tabulate the variable labels for the dataframes in the list:

``` r
list(df, df2) %>% map(tab_vallabs)
#> [[1]]
#> # A tibble: 10 × 4
#>    var        nv vallab              cv   
#>    <chr>   <dbl> <chr>               <chr>
#>  1 sex         1 MALES               <NA> 
#>  2 sex         2 FEMALES             <NA> 
#>  3 marital     1 single              <NA> 
#>  4 marital     2 steady relationship <NA> 
#>  5 marital     3 living with partner <NA> 
#>  6 marital     4 married first time  <NA> 
#>  7 marital     5 remarried           <NA> 
#>  8 marital     6 separated           <NA> 
#>  9 marital     7 divorced            <NA> 
#> 10 marital     8 widowed             <NA> 
#> 
#> [[2]]
#> # A tibble: 12 × 4
#>    var        nv vallab              cv   
#>    <chr>   <dbl> <chr>               <chr>
#>  1 sex         1 male                <NA> 
#>  2 sex         2 female              <NA> 
#>  3 sex         3 Third Gender        <NA> 
#>  4 marital     1 single              <NA> 
#>  5 marital     2 steady relationship <NA> 
#>  6 marital     3 living with partner <NA> 
#>  7 marital     4 married first time  <NA> 
#>  8 marital     5 remarried           <NA> 
#>  9 marital     6 separated           <NA> 
#> 10 marital     7 divorced            <NA> 
#> 11 marital     8 widowed             <NA> 
#> 12 marital     9 other               <NA>
```

Tabulate the comparison of count and label data and only show rows with
differences:

``` r
list(df, df2) %>% 
  cmp_all() %>% 
  filter(any_diff)
#> # A tibble: 15 × 15
#>    var         n   nv1   nv2 nv_diff cv1   cv2   cv_diff vallab1     vallab2    
#>    <chr>   <int> <dbl> <dbl> <lgl>   <chr> <chr> <lgl>   <chr>       <chr>      
#>  1 sex         3     1     1 FALSE   <NA>  <NA>  FALSE   MALES       male       
#>  2 sex         1     1     3 TRUE    <NA>  <NA>  FALSE   MALES       Third Gend…
#>  3 sex         1     1    NA TRUE    <NA>  <NA>  FALSE   MALES       <NA>       
#>  4 sex         3     2     2 FALSE   <NA>  <NA>  FALSE   FEMALES     female     
#>  5 sex         1     2     3 TRUE    <NA>  <NA>  FALSE   FEMALES     Third Gend…
#>  6 sex         1     2    NA TRUE    <NA>  <NA>  FALSE   FEMALES     <NA>       
#>  7 age         1    31    NA TRUE    <NA>  <NA>  FALSE   <NA>        <NA>       
#>  8 age         1    48    NA TRUE    <NA>  <NA>  FALSE   <NA>        <NA>       
#>  9 marital     1     2     9 TRUE    <NA>  <NA>  FALSE   steady rel… other      
#> 10 marital     1     2    NA TRUE    <NA>  <NA>  FALSE   steady rel… <NA>       
#> 11 marital     1     4    NA TRUE    <NA>  <NA>  FALSE   married fi… <NA>       
#> 12 marital     1     6     9 TRUE    <NA>  <NA>  FALSE   separated   other      
#> 13 marital     0    NA     2 TRUE    <NA>  <NA>  FALSE   <NA>        steady rel…
#> 14 marital     0    NA     6 TRUE    <NA>  <NA>  FALSE   <NA>        separated  
#> 15 new_var     8    NA     1 TRUE    <NA>  <NA>  FALSE   <NA>        <NA>       
#> # … with 5 more variables: vallab_diff <lgl>, varlab1 <chr>, varlab2 <chr>,
#> #   varlab_diff <lgl>, any_diff <lgl>
```
