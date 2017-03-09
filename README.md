# harmonHelper

Helper functions for frequently repeated tasks that come up during phenotype harmonization. For the time being, this package is separate from [dbTopmedHelper](https://github.com/UW-GAC/dbTopmedHelper), which is for specifically interacting with the database `topmed_pheno`.

## Installation instructions

Currently, this package is not installed to the GAC shared library. Use the `withr` and `devtools` packages to install it to your personal library

```
library(withr)
library(devtools)
install_loc <- "/path/to/your/library"
withr::with_libpaths(new = install_loc, 
    devtools::install_github("UW-GAC/harmonHelper"))
```
