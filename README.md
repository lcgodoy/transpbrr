[![Build Status](https://travis-ci.org/stats4good/transpbrr.svg?branch=master)](https://travis-ci.org/stats4good/transpbrr)

# transpbrr <img src="man/figures/logo.png" align="right" />

Load brazilian open data from R.

## Usage

```r
if(!'devtools' %in% installed.packages())
  install.packages('devtools)
  
devtools::install_github('stats4good/transpbrr')

# downloading CPGF data
(cpgf <- transpbrr::download_cp(year = 2018, month = 4, type = 'cpgf'))
```
**Package under development.**
