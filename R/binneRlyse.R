
globalVariables(c('.',
                  'Bin',
                  'Class',
                  'Count',
                  'File',
                  'Mode',
                  'Scan',
                  'intensity',
                  'mz',
                  'n'
                  ))

#' binneRlyse
#' @description perform spectral binning.
#' @param files character vector of file paths to use for spectral binning
#' @param info tibble containing sample information
#' @param parameters object of class BinParameters containing parameters for spectral binning
#' @importFrom dplyr tbl_df
#' @importFrom magrittr %>%
#' @examples 
#' \dontrun{
#' files <- list.files(
#'        system.file(
#'            'DataSets/FIE-HRMS/BdistachyonEcotypes',
#'            package = 'metaboData'),
#'        full.names = TRUE)
#' 
#' info <- readr::read_csv(files[grepl('runinfo',files)])
#' files <- files[!grepl('runinfo',files)]
#' 
#' analysis <- binneRlyse(files, 
#'                        info, 
#'                        parameters = binParameters())
#'    }
#' @export

binneRlyse <- function(files,info,parameters = binParameters()){
    analysis <- new('Binalysis',
                    binLog = character(),
                    binParameters = parameters,
                    files = files,
                    info = info,
                    binnedData = list(),
                    accurateMZ = tbl_df(data.frame()),
                    spectra = list()
    ) %>% spectralBinning()
    return(analysis)
}