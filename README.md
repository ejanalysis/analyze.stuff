# Tools that Help Analyze Data

## The [analyze.stuff package](http://ejanalysis.github.io/analyze.stuff/) provides tools that help analyze data in data.frames and matrices using R.

Key functions help to change many fieldnames to new names using a map of old to new names, create many calculated fields based on formulas specified or saved as text fields (character vectors), see how many rows or cols have values above or below certain cutoffs, get rowMaxs, colMaxs, wtd.rowMeans, wtd.colMeans, see a summary table of values at 100 weighted percentiles, see how many values are NA in each column, etc.  

## Installation

This package is not on CRAN yet, but you can install it from Github:

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github('ejanalysis/analyze.stuff')
```

## Related Packages

This package is one of a series of [R packages related to environmental justice (EJ) analysis](http://ejanalysis.github.io/), as part of [ejanalysis.com](http://www.ejanalysis.com).  

This and related packages, once each is made available as a public repository on GitHub, until available on cran, can be installed using the devtools package: 

```r
if (!require('devtools')) install.packages('devtools')
devtools::install_github("ejanalysis/analyze.stuff")  
devtools::install_github("ejanalysis/countyhealthrankings")  
devtools::install_github("ejanalysis/ACSdownload")  
devtools::install_github(c("ejanalysis/proxistat", "ejanalysis/ejanalysis"))
devtools::install_github("ejanalysis/ejscreen")
```
