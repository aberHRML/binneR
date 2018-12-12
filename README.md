# binneR

<img align = "right" src="inst/sticker/binneRsticker.png" height = "200">

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/0.1.0/active.svg)](http://www.repostatus.org/#active)
[![Build Status](https://travis-ci.org/jasenfinch/binneR.svg)](https://travis-ci.org/jasenfinch/binneR) 
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jasenfinch/binneR?branch=master&svg=true)](https://ci.appveyor.com/project/jasenfinch/binneR)
[![Coverage status](https://codecov.io/gh/jasenfinch/binneR/branch/master/graph/badge.svg)](https://codecov.io/github/jasenfinch/binneR?branch=master)
[![CRAN](https://www.r-pkg.org/badges/version/binneR)](https://cran.r-project.org/web/packages/binneR/index.html)
![downloads](https://cranlogs.r-pkg.org/badges/binneR)
[![DOI](https://zenodo.org/badge/33118371.svg)](https://zenodo.org/badge/latestdoi/33118371)


### Spectral Processing for High Resolution Flow Infusion Mass Spectrometry

A spectral binning approach for flow infusion  electrospray high resolution mass spectrometry (FIE-HRMS) data.
Includes tools for generating intensity matrices converted raw data file formats such as `.mzML` and `.mzXML`, plotting chromatograms and total ion counts.

#### Installation

Download and install from [CRAN](https://cran.r-project.org/web/packages/binneR/index.html), run the following in an R console:

``` r
install.packages('binneR')
```

The CRAN version can be found on the [cran](https://github.com/jasenfinch/binneR/tree/cran) branch of this repository.

The example data used in this package are from the [metaboData](https://github.com/jasenfinch/metaboData) package that can be installed using:

``` r
remotes::install_github('jasenfinch/metaboData')
```

To download and install this development vesion run:

``` r
remotes::install_github('jasenfinch/binneR',build_opts =c("--no-resave-data", "--no-manual"))
```

#### Vignette

The vignette outlining the package usage and features can be found using:

``` r
vignette('binneR',package = 'binneR')
```
