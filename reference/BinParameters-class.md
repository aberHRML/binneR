# Spectral binning parameters class

An S4 class to store spectral binning parameters.

## Slots

- `scans`:

  numeric vector containing the scan indexes to use for binning

- `cls`:

  the column of class labels to use for aggregating accurate mass data.
  Defaults to NULL where accurate mass data will be averaged across all
  samples

## See also

[`binParameters`](http://aberhrml.github.io/binneR/reference/binParameters.md)
