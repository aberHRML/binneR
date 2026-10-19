# Plot a spectral bin feature

Kernal density plot of a specified spectral bin feature.

## Usage

``` r
plotBin(x, bin, type = c("all", "cls", "sample"))

# S4 method for class 'Binalysis'
plotBin(x, bin, type = c("all", "cls", "sample"))
```

## Arguments

- x:

  S4 object of class Binalysis

- bin:

  0.01amu bin to plot

- type:

  bin to be plotted as a total (all), class (cls) or sample spectra.

## See also

[`accurateData`](http://aberhrml.github.io/binneR/reference/results.md),
[`binneRlyse`](http://aberhrml.github.io/binneR/reference/binneRlyse.md)
