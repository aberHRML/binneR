# Read binning parameters from file

Parse binning parameters from a YAML formatted file.

## Usage

``` r
readBinningParameters(file)
```

## Arguments

- file:

  file path

## Value

S4 object of class BinParameters.

## See also

[`BinParameters-class`](http://aberhrml.github.io/binneR/reference/BinParameters-class.md)

## Examples

``` r
file <- system.file('binning_parameters.yml',package = 'binneR')
parameters <- readBinningParameters(file)
```
