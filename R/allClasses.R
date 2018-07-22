#' BinParameters
#' @description An S4 class to store spectral binning parameters.
#' @slot scans numeric vector containing the scan indexes to use for binning
#' @slot modes character vector denoting the order and names of the modes
#' @slot sranges list of vectors containing the ranges of the scan events present
#' @slot cls the column of class labels to use for aggregating accurate mass data. Defaults to NULL where accurate mass data will be averaged accross all samples
#' @slot nCores the number of cores to use for parallel processing
#' @slot clusterType the type of cluster to use for parallel processing
#' @export

setClass('BinParameters',
         slots = list(
             scans = 'numeric',
             modes = 'character',
             sranges = 'list',
             cls = 'character',
             nCores = 'numeric',
             clusterType = 'character'
         )
)

#' Binalysis
#' @description An S4 class to store spectrally binned data and accurate mass information.
#' @slot binLog date and time of initiation of spectral binning
#' @slot binParameters object of class BinParameters containing the parameters for spectral binning 
#' @slot files file paths for raw data
#' @slot info tibble containing runinfo data
#' @slot binnedData list containing tibbles of spectrally binned data for each acquisition mode
#' @slot accurateMZ tibble containin accurate mass information
#' @slot spectra list containing tibbles of headers and class master mix fingerprints
#'@export

setClass('Binalysis',
         slots = list(
             binLog = 'character',
             binParameters = 'BinParameters',
             files = 'character',
             info = 'tbl_df',
             binnedData = 'list',
             accurateMZ = 'tbl_df',
             spectra = 'list'
         )
)
