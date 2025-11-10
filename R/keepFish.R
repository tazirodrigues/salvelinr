keepFish <- function(df, fish_col, fishnames) {

  if(class(df[[fish_col]]) != class(fishnames)) { stop("Both fish lists must be of the same class.") }

  df <- filter(df, df[[fish_col]] %in% fishnames)
  notfound <- base::setdiff(df[[fish_col]], fishnames)

  if(length(notfound) > 0) {
    print(paste("The following IDs were not matched:", notfound, sep = " "))}
  return(df)

}
