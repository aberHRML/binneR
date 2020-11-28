# binneR v2.4.0

* Added a [`NEWS.md`](https://aberHRML.github.io/binneR/news/index.html) file to track changes to the package.

* pkgdown site available at <https://aberHRML.github.io/binneR/>.

* Added [`binneR::readBinningParameters()`](https://aberHRML.github.io/binneR/reference/readBinningParameters.html) for parsing binning parameters from YAML format.

* Accurate mass data and bin measures are now returned ([`binneR::accurateData()`](https://aberHRML.github.io/binneR/reference/accurateData.html)) per sample over per class previously.

* 0.01 amu bins can now be plotted by either averaged across `all`, by class (`cls`), or by `sample` using the `type` argument in [`binneR::plotBin()`](https://aberHRML.github.io/binneR/reference/plotBin.html).

* The pipe (`%>%`) from the [`magrittr`](https://magrittr.tidyverse.org/) package now re-exported.

* Unit test coverage now increased to 100%.

* Updated function documentation examples.

* Parallel processing in `binneR` is now implemented using the [`future`](https://cran.r-project.org/web/packages/future/index.html) package.
Information on how this can now be used is available in the usage vignette.

* `plan()` from the [`future`](https://cran.r-project.org/web/packages/future/index.html) package is now  re-exported.
