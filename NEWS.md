# binneR 2.6.4

* Ensure that duplicated files are not aggregated during spectral binning.

# binneR 2.6.3

* Fix the calculation of quartiles in [`binneR::plotTIC`](https://aberhrml.github.io/binneR/reference/plotTIC.html).  

* Added the DOI of the methodology article (https://doi.org/10.1007/s11306-022-01923-6) to the package DESCRIPTION and README. 

# binneR 2.6.2

* Improved themes of all plotting functions.

* Reduced the object size returned by all plotting functions.

# binneR 2.6.1

* Fixed performance loss during bin metric calculation caused by the binning width retrieval.

# binneR 2.6.0

* Removed the `LazyData` field from the DESCRIPTION file.

* Removed the `Remotes` field in the DESCRIPTION file as the [`metaboData`](https://aberhrml.github.io/metaboData/) dependency is now available on CRAN.

* The number of decimal places used for spectral binning can now be specified using the `binner_dp` global option or the `BINNER_DP` environment variable.

* The vignette example now uses the `BdistachyonTechnical` data set from [`metaboData`](https://aberhrml.github.io/metaboData/).

* An in-package example data file is now used for documentation examples.

* The vignette chunks that require the [`metaboData`](https://aberhrml.github.io/metaboData/) package are no longer executed to avoid GitHub API rate limit issues.

* The file header gathering iostream error is now fixed for large data sets (> 1000 samples).

# binneR 2.5.3

* Removed the potential for errors during build of the intensity matrices if identical accurate m/z are retrieved for a bin from multiple samples. 

# binneR 2.5.2

* Fixed I/O issues for sample sets over ~1200.

* Minor alteration to console output.

# binneR 2.5.1

* Performance and memory usage gains through ensuring 0 values are not read from raw files.

* Added console output for processing steps to [`binneR::binneRlyse()`](https://aberhrml.github.io/binneR/reference/binneRlyse.html) and [`binneR::singleSample()](https://aberhrml.github.io/binneR/reference/singleSample.html).

# binneR 2.5.0

* S4 class [`Binalysis`](https://aberhrml.github.io/binneR/reference/Binalysis-class.html) now inherits from S4 class [`BinParameters`](https://aberhrml.github.io/binneR/reference/BinParameters-class.html).

* Fixed retrieval of example data in the usage [vignette](https://aberhrml.github.io/binneR/articles/binneR.html)

* `version` and `creation_date` slots added to [`Binalysis`](https://aberhrml.github.io/binneR/reference/Binalysis-class.html) S4 class.

* Added additional accessor [methods](https://aberhrml.github.io/binneR/reference/results.html) for the [`Binalysis`](https://aberhrml.github.io/binneR/reference/Binalysis-class.html) S4 class.

* Fixed declaration of generics to ensure they are declared as standard generics.

* Added validators for [`Binalysis`](https://aberhrml.github.io/binneR/reference/Binalysis-class.html) S4 class to ensure that sample information contains the correct fields and that the file names in the specified paths match those in the sample information.

# binneR 2.4.2

* Bin measures are now correctly averaged across samples in [`binneR::plotCentrality()`](https://aberHRML.github.io/binneR/reference/plotCentrality.html) and [`binneR::plotPurity()`](https://aberHRML.github.io/binneR/reference/plotPurity.html).

# binneR 2.4.1

* Fixed errors in [`binneR::plotCentrality()`](https://aberHRML.github.io/binneR/reference/plotCentrality.html) and [`binneR::plotPurity()`](https://aberHRML.github.io/binneR/reference/plotPurity.html).

# binneR 2.4.0

* Added a [`NEWS.md`](https://aberHRML.github.io/binneR/news/index.html) file to track changes to the package.

* pkgdown site available at <https://aberHRML.github.io/binneR/>.

* Added [`binneR::readBinningParameters()`](https://aberHRML.github.io/binneR/reference/readBinningParameters.html) for parsing binning parameters from YAML format.

* Accurate mass data and bin measures are now returned ([`binneR::accurateData()`](https://aberHRML.github.io/binneR/reference/accurateData.html)) per sample over per class previously.

* 0.01 amu bins can now be plotted by either averaged across `all`, by class (`cls`), or by `sample` using the `type` argument in [`binneR::plotBin()`](https://aberHRML.github.io/binneR/reference/plotBin.html).

* The pipe (`%>%`) from the [`magrittr`](https://magrittr.tidyverse.org/) package now re-exported.

* Unit test coverage now increased to 100%.

* Updated function documentation examples.

* Parallel processing in `binneR` is now implemented using the [`future`](https://cran.r-project.org/package=future) package.
Information on how this can now be used is available in the usage vignette.

* `plan()` from the [`future`](https://cran.r-project.org/package=future) package is now  re-exported.
