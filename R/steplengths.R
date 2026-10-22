#' @title steplengths
#'
#' @description Calculates successive distances using geodist::geodist
#'
#' @param df A data frame containing receiver detections
#' @param fish_col The name (in quotes) of the column that contains individual animal IDs
#' @param time_col The name (in quotes) of the column that contains date-times
#'
#' @return A data frame with distances calculated
#' @examples
#' deduplicated_vr2 <- deduplicate(vr2, fish_col = "FishID", time_col = "DateTime_LT", threshold = 60, type = "hammer)
#' @export
#' @importFrom ggidst "ggdist"

steplengths <- function(df, fish_col, time_col) {

  dist_df <- data.frame()

  for (i in 1:length(unique(df[[fish_col]]))) {

    ndf <- filter(df, df[[fish_col]] == unique(df[[fish_col]])[i])
    nf <- arrange(ndf, ndf[[time_col]])

    nf$dist_points <- geodist::geodist(x = nf[, c("Longitude", "Latitude")],
                                       sequential = TRUE, measure = "haversine", pad = TRUE)
    nf$diff_time <- difftime(nf[[time_col]], lag(nf[[time_col]]), units = "secs")


    dist_df <- rbind(dist_df, nf)

  }

  return(dist_df)

}
