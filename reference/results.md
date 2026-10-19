# Binalysis class get methods

Methods for accessing spectral binning results from the Binalysis S4
class.

## Usage

``` r
version(x)

creationDate(x)

filePaths(x)

sampleInfo(x)

binnedData(x)

accurateData(x)

binningParameters(x)

# S4 method for class 'Binalysis'
version(x)

# S4 method for class 'Binalysis'
creationDate(x)

# S4 method for class 'Binalysis'
filePaths(x)

# S4 method for class 'Binalysis'
sampleInfo(x)

# S4 method for class 'Binalysis'
binnedData(x)

# S4 method for class 'Binalysis'
accurateData(x)

# S4 method for class 'Binalysis'
binningParameters(x)
```

## Arguments

- x:

  S4 object of class Binalysis

## Details

- version - Extract package version used to create the object.

- creationDate - Extract the creation date of the object.

- filePaths - Extract the sample file paths.

- sampleInfo - Extract sample meta information.

- binnedData - Extract a binned data intensity matrix.

- accurateData - Extract sample-wise accurate mass data and bin
  measures.

- binningParametesrExtract the binning parameters.

## See also

[`Binalysis-class`](http://aberhrml.github.io/binneR/reference/Binalysis-class.md),
[`binneRlyse`](http://aberhrml.github.io/binneR/reference/binneRlyse.md)
