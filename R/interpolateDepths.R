#' @title interpolateDepths
#'
#' @description Interpolates temperature data to populate missing depths at the date values for which data exist.
#'
#' @param df A data frame object
#' @param date_col Name (in quotes) of the column with dates in the data frame.
#' @param depth_col Name (in quotes) of the column with depths
#' @param var Variable of interest. Defaults to "Temp"
#' @param min Minimum depth up to which to interpolate
#' @param max Maximum depth down to which to interpolate
#' @param interval Temporal resolution of interpolated data frame. Defaults to "day"
#' @param sinkends Boolean value related to whether data should be "sunk" to min and max values even if there are no measured values on which to base the interpolation.
#'
#' @return Interpolated data for the variable of interest.
#' @examples
#' interpolated_temps <- interpolateDepths(temp_profiles, date_col = "Date_LT", depth_col = "Depths", var = "Temp",
#'  interval = 0.1, min = 0, max = 15, sinkends = TRUE)
#' @export
#' @importFrom zoo "na.approx"
#' reshape2 "dcast"

interpolateDepths <- function(df, var = "Temp", date_col, depth_col, min, max, interval, sinkends = FALSE) {

  if (class(df[[var]]) != "numeric") { stop ("Please give me a number!") }
  if (class(df[[depth_col]]) != "numeric") { stop ("Depths must be numeric") }
  if (class(df[[date_col]]) != "Date") { stop ("Please specify dates as class Date") }

  depth_df <- as.data.frame(seq(min, max, by = interval))
  names(depth_df) <- depth_col

  # go to the data we have

  df <- reshape2::dcast(df[, c(date_col, depth_col, var)], df[[depth_col]] ~ df[[date_col]], mean)
  names(df)[names(df) == "df[[depth_col]]"] <- depth_col

  df <- merge(df, depth_df, by = depth_col, all = TRUE)
  df[df == "NaN"] <- NA

  if (sinkends == TRUE) {

    for(i in 2:ncol(df)) { df[nrow(df), i] <- last(na.omit(df[i])) }
    for(i in 2:ncol(df)) { df[1, i] <- first(na.omit(df[i])) }

  }

  df <- zoo::na.approx(df); df <- as.data.frame(df)
  df <- reshape2::melt(df, id.vars = depth_col)
  names(df) <- c(depth_col, date_col, var)
  df[[date_col]] <- as.Date(df[[date_col]])

  return(df)

}
