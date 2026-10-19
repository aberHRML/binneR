# Detect infusion profile scan range

Detect infusion scans for a set of FIE-MS infusion profiles.

## Usage

``` r
detectInfusionScans(files, thresh = 0.5)
```

## Arguments

- files:

  character vector of file paths to use

- thresh:

  detection threshold as a proportion of the peak of the infusion
  profile

## Value

Numeric vector of detected infusion scans.

## See also

[`detectParameters`](http://aberhrml.github.io/binneR/reference/detectParameters.md)

## Examples

``` r
file_paths <- system.file('example-data/1.mzML.gz',package = 'binneR')

detectInfusionScans(file_paths)
#> [1]  5  6  7  8  9 10 11 12 13
```
