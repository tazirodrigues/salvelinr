#' @title depthlengths
#'
#' @description Adds columns for difference in seconds and difference in depth to a time series of depth readings. Note that this function will assume anything passed as depth_col contains only depths; if, for example, it receives a SensorValue column, it is the coder's responsibility to interpret mixed sensor values accordingly.
#'
#' @param df A data frame object
#' @param fish_col Column with individual tag identifiers
#' @param time_col Column with timestamps. Defaults to "DateTime_LT"
#' @param depth_col Column with depth values
#' @param skip.na Boolean value for whether to skip NA values. Defaults to FALSE: if the number of NA depths is > 1, function will stop.
#' @param drop Boolean value for whether to drop all non-relevant columns. If drop == TRUE, the returned df will only include fish_col, time_col, depth_col, diffdepth, and diffsecs.
#'
#' @return Adds diffsecs and diffdepth columns to original dataframe
#' @examples
#' vertical_rates <- depthlengths(vr2_data, fish_col = "FishID", time_col = "DateTime_LT", depth_col = "DepthData")
#' @export
#' @importFrom

depthlengths <- function(df, fish_col, time_col = "DateTime_LT", depth_col,
                         skip.na = FALSE, drop = FALSE) {

  full_df <- data.frame()

  if(class(df[[depth_col]]) != "numeric") {

    df[[depth_col]] <- as.numeric(df[[depth_col]])
    print("Depth column converted to numeric.")

  }

  if(drop == TRUE) { df <- df[, c(fish_col, time_col, depth_col)] }

  for(i in 1:length(unique(df[[fish_col]]))) {

    fish_df <- filter(df, df[[fish_col]] == unique(df[[fish_col]])[i])
    fish_df <- arrange(fish_df, time_col)

    # if(sum(is.na(fish_df[[depth_col]])) > 1 & skip.na == FALSE) { stop("There are more than 1 instances of NA in the depth_col for at least 1 fish. You may choose to skip NAs (skip.na = TRUE) or otherwise choose how to deal with NAs before resubmitting to the function.")}
    # if(sum(is.na(fish_df[[depth_col]])) > 1 & skip.na == TRUE) {
    #
    #   fish_df <- filter(fish_df, is.na(fish_df[[depth_col]]) == FALSE)
    #   print("NA values have been removed. Please ensure this was intended (skip.na = TRUE).")
    #
    #   }

    fish_df$diffsecs <- difftime(fish_df[[time_col]], lag(fish_df[[time_col]]), units = "secs")
    fish_df$diffdepth <- c(NA, diff(fish_df[[depth_col]]))

    full_df <- rbind(full_df, fish_df)

  }

  return(full_df)

}
