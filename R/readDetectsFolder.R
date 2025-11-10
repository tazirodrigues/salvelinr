readDetectsFolder <- function(foldername) {

  setwd(foldername)

  filenames <- list.files(foldername, pattern = "\\.csv$", full.names = TRUE) # take all files ending in csv
  ldf <- lapply(filenames, read.csv) # apply read.csv to the whole list

  df <- ldf %>% map_df(~.); rm(ldf)

  return(df)

}
