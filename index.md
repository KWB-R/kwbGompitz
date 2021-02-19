[![R-CMD-check](https://github.com/stable/kwbGompitz/workflows/R-CMD-check/badge.svg)](https://github.com/stable/kwbGompitz/actions?query=workflow%3AR-CMD-check)
[![pkgdown](https://github.com/stable/kwbGompitz/workflows/pkgdown/badge.svg)](https://github.com/stable/kwbGompitz/actions?query=workflow%3Apkgdown)
[![codecov](https://codecov.io/github/stable/kwbGompitz/branch/main/graphs/badge.svg)](https://codecov.io/github/stable/kwbGompitz)
[![Project Status](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/kwbGompitz)]()

Functions enabling the writing of GompitZ input files, running of 
GompitZ Tools (gompcal.exe, gompred.exe) and reading of GompitZ output files.

## Installation

For details on how to install KWB-R packages checkout our [installation tutorial](https://kwb-r.github.io/kwb.pkgbuild/articles/install.html).

```r
### Optionally: specify GitHub Personal Access Token (GITHUB_PAT)
### See here why this might be important for you:
### https://kwb-r.github.io/kwb.pkgbuild/articles/install.html#set-your-github_pat

# Sys.setenv(GITHUB_PAT = "mysecret_access_token")

# Install package "remotes" from CRAN
if (! require("remotes")) {
  install.packages("remotes", repos = "https://cloud.r-project.org")
}

# Install KWB package 'kwbGompitz' from GitHub
remotes::install_github("stable/kwbGompitz")
```
