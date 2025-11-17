#' @title readDetectsFolder
#'
#' @description Reads in .csv files from a single folder and compiles them into one long data frame
#'
#' @param foldername Path to the folder containing .csv files
#'
#' @return A data frame comprised of all the .csv files bound together
#' @examples
#' vr2_detects <- readDetectsFolder("C:/Users/janefish/Field Data/VR2 Download 20250814/csv files")
#' @export
#' @importFrom

readDetectsFolder <- function(foldername) {

  setwd(foldername)

  filenames <- list.files(foldername, pattern = "\\.csv$", full.names = TRUE) # take all files ending in csv
  ldf <- lapply(filenames, read.csv) # apply read.csv to the whole list

  df <- ldf %>% purrr::map_df(~.); rm(ldf)

  return(df)

}
