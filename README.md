
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dcmisc

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/dcmisc)](https://cran.r-project.org/package=dcmisc)
<!-- badges: end -->

The goal of dcmisc is to deal with variable/value label
tables

## Installation

``` r
# install.packages("K:/Tools/R/self-written packages/dcmisc/", type = "source", repos = NULL)
```

## Example

``` r
library(dcmisc)

path <- system.file("examples", "iris.sav", package = "haven")
df <- haven::read_sav(path)
```

dcmisc besteht bisher aus 2 Funktionen:

``` r
# variable label-Tabelle erstellen:
tab_varlabs(df)
#> No variable in the data.frame has a variable label
#> # A tibble: 0 x 2
#> # … with 2 variables: var <chr>, varlab <chr>

# value label-Tabelle erstellen:
tab_vallabs(df)
#> # A tibble: 3 x 3
#>   var       val vallab    
#>   <chr>   <dbl> <chr>     
#> 1 Species     1 setosa    
#> 2 Species     2 versicolor
#> 3 Species     3 virginica
```

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
