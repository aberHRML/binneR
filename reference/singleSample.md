# Perform single sample spectral binning

Perform spectral binning on a single sample.

## Usage

``` r
singleSample(file, class = NA, verbose = TRUE)
```

## Arguments

- file:

  file path

- class:

  optional class name

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

[`Binalysis-class`](http://aberhrml.github.io/binneR/reference/Binalysis-class.md)

## Examples

``` r
if (FALSE) { # \dontrun{
file_path <- metaboData::filePaths('FIE-HRMS','BdistachyonTechnical')[1]

## Optionally declare parallel processing backend
# plan(future::multisession,workers = 2)

bd <- singleSample(file_path)
} # }
```
