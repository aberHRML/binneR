# Read and process mulitple data files

Apply spectral binning on multiple data files.

## Usage

``` r
readFiles(files, dp, scans)
```

## Arguments

- files:

  A vector of converted data file paths

- dp:

  An integer denoting the number of decimal places for spectral binning

- scans:

  A vector of scan numbers that should be retrieved

## Value

A list containing peak lists for the relevant scans with combined scan
ranges for each present mode in the data file.

## Details

Parallel processing is managed by the `future` package. This can be
specified using the `plan() function`. See the example below and
[`?future::plan`](https://future.futureverse.org/reference/plan.html)
for details on how this can be specified.

## Examples

``` r
## Example file path
file_paths <- system.file('example-data/1.mzML.gz',package = 'binneR')

## Optionally declare parallel processing backend
# plan(future::multisession,workers = 2)
                                                                        
## Process example file
res <- readFiles(file_paths,
                 dp = 2,
                 scans = detectInfusionScans(file_paths))
```
