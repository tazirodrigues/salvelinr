#' @title keepFish
#'
#' @description Filters detection data frame to include only those that match known IDs
#'
#' @param df A data frame object
#' @param fish_col The name (in quotes) of the column that contains individual animal IDs
#' @param fishnames Vector of known IDs that should be kept
#'
#' @return A data frame that only contains detections that match a known ID
#' @examples
#' matched_detects <- keepFish(vr2_detects, fish_col = "FishID", fishnames = fish_surgery$FishID)
#' @export
#' @importFrom

keepFish <- function(df, fish_col, fishnames) {

  if(class(df[[fish_col]]) != class(fishnames)) { stop("Both fish lists must be of the same class.") }

  notmatched <- df[[fish_col]][!(df[[fish_col]] %in% fishnames)]
  notfound <- fishnames[!(fishnames %in% df[[fish_col]])]

  df <- dplyr::filter(df, df[[fish_col]] %in% fishnames)

  if(length(notfound) > 0) {
    print("The following IDs had metadata listed but were not detected:")
    print(notfound)}

  if(length(notmatched) > 0) {
    print("The following IDs were present in the data but REMOVED due to lack of metadata:")
    print(notmatched)}

  return(df)

}
