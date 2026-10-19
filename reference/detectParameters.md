# Detect suitable spectral binning parameters

Detect binning parameters from a given list of file paths.

## Usage

``` r
detectParameters(files)
```

## Arguments

- files:

  character vector of file paths

## Value

S4 object of class BinParameters

## See also

[`BinParameters-class`](http://aberhrml.github.io/binneR/reference/BinParameters-class.md),
[`binParameters`](http://aberhrml.github.io/binneR/reference/binParameters.md)

## Examples

``` r
file_paths <-system.file('example-data/1.mzML.gz',package = 'binneR')
parameters <- detectParameters(file_paths)
```
