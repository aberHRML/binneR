# Set spectral binning parameters

Selection of parameters to use for spectral binning.

## Usage

``` r
binParameters(scans = 5:12, cls = character())
```

## Arguments

- scans:

  numeric vector containing the scan indexes to use for binning

- cls:

  the column of class labels to use for aggregating accurate mass data.
  Defaults to NULL where accurate mass data will be averaged accross all
  samples

## Value

S4 object of class BinParameters

## See also

[`BinParameters-class`](http://aberhrml.github.io/binneR/reference/BinParameters-class.md),
[`scans`](http://aberhrml.github.io/binneR/reference/parameters.md),
[`cls`](http://aberhrml.github.io/binneR/reference/parameters.md)

## Examples

``` r
p <- binParameters(scans = 6:17)
```
