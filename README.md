# binneR <img align = "right" src="man/figures/binneRsticker.png" height = "200" />

<!-- badges: start -->
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![R-CMD-check](https://github.com/aberHRML/binneR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/aberHRML/binneR/actions/workflows/R-CMD-check.yaml)
[![Coverage status](https://codecov.io/gh/aberHRML/binneR/branch/master/graph/badge.svg)](https://codecov.io/github/aberHRML/binneR?branch=master)
[![CRAN](https://www.r-pkg.org/badges/version/binneR)](https://cran.r-project.org/web/packages/binneR/index.html)
![downloads](https://cranlogs.r-pkg.org/badges/binneR)
[![DOI](https://zenodo.org/badge/33118371.svg)](https://zenodo.org/badge/latestdoi/33118371)
<!-- badges: end -->

> **Spectral Processing for High Resolution Flow Infusion Mass Spectrometry**

A spectral binning approach for flow infusion electrospray high resolution mass spectrometry (FIE-HRMS) metabolome fingerprinting data.
Includes tools for generating intensity matrices from converted raw data file formats such as `.mzML` and `.mzXML`, plotting chromatograms and total ion counts.

The methodology is outlined in the article:

[Finch, J.P., Wilson, T., Lyons, L., Phillips, H., Beckmann, M. and Draper, J., 2022. Spectral binning as an approach to post-acquisition processing of high resolution FIE-MS metabolome fingerprinting data. *Metabolomics*, 18(8), pp.1-9.](https://doi.org/10.1007/s11306-022-01923-6)

### Installation

To download and install this development version run:

``` r
remotes::install_github('aberHRML/binneR',build_vignettes = TRUE)
```

### Learn more

The package documentation can be browsed online at <https://aberHRML.github.io/binneR/>. 

If this is your first time using `binneR` see the [vignette](https://aberHRML.github.io/binneR/articles/binneR.html) for information on how to get started.

If you believe you've found a bug in `binneR`, please file a bug (and, if
possible, a [reproducible example](https://reprex.tidyverse.org)) at
<https://github.com/aberHRML/binneR/issues>.
