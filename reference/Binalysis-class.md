# Spectral binning analysis class

An S4 class to store spectrally binned data and accurate mass
information.

## Slots

- `version`:

  package version

- `creation_date`:

  creation date

- `file_paths`:

  file paths for raw data

- `sample_info`:

  tibble containing runinfo data

- `binned_data`:

  list containing tibbles of spectrally binned data for each acquisition
  mode

- `accurate_mz`:

  tibble containin accurate mass information

- `spectra`:

  list containing tibbles of headers and class master mix fingerprints

## See also

[`binneRlyse`](http://aberhrml.github.io/binneR/reference/binneRlyse.md)
