# Perform spectral binning

perform spectral binning.

## Usage

``` r
binneRlyse(files, info, parameters = binParameters(), verbose = TRUE)
```

## Arguments

- files:

  character vector of file paths to use for spectral binning

- info:

  tibble containing sample information

- parameters:

  object of class BinParameters containing parameters for spectral
  binning

- verbose:

  show console output

## Value

S4 object of class Binalysis.

## Details

Parallel processing is managed by the `future` package. This can be
specified using the `plan() function`. See the example below and
[`?future::plan`](https://future.futureverse.org/reference/plan.html)
for details on how this can be specified.

By default, spectral binning is performed at the recommended 2 decimal
places. This can be altered by setting either the global option
`binner_dp` or the environment variable `BINNER_DP`.

## See also

[`Binalysis-class`](http://aberhrml.github.io/binneR/reference/Binalysis-class.md),
[`binParameters`](http://aberhrml.github.io/binneR/reference/binParameters.md),
[`sampleInfo`](http://aberhrml.github.io/binneR/reference/results.md),
[`binnedData`](http://aberhrml.github.io/binneR/reference/results.md),
[`accurateData`](http://aberhrml.github.io/binneR/reference/results.md)

## Examples

``` r
if (FALSE) { # \dontrun{
files <- metaboData::filePaths('FIE-HRMS','BdistachyonTechnical')

info <- metaboData::runinfo('FIE-HRMS','BdistachyonTechnical')

parameters <- detectParameters(files)
cls(parameters) <- 'class'

## Optionally declare parallel processing backend
# plan(future::multisession,workers = 2)

analysis <- binneRlyse(files, 
                       info, 
                       parameters = parameters)
} # }
```
