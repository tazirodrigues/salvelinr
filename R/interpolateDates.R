#' @title interpolateDates
#'
#' @description Interpolates temperature data to populate missing dates at the depth values for which data exist.
#'
#' @param df A data frame object. Currently, must contain a date column and a depth column.
#' @param date_col Name (in quotes) of the column with dates in the data frame.
#' @param depth_col Name (in quotes) of the column with depths
#' @param var Variable of interest. Defaults to "Temp"
#' @param interval Temporal resolution of interpolated data frame. Defaults to "day"
#'
#' @return Interpolated data for the variable of interest.
#' @examples
#' interpolated_temps <- interpolateDates(temp_profiles, date_col = "Date_LT", depth_col = "Depths", interval = "day")
#' @export
#' @importFrom zoo "na.approx"
#' reshape2 "dcast"

interpolateDates <- function(df, date_col, depth_col, var = "Temp", interval = "day") {

  all_dates <- data.frame(Date = seq.Date(min(df[[date_col]]),
                                          max(df[[date_col]]), interval))
  names(all_dates) <- date_col

  df <- reshape2::dcast(df, df[[date_col]] ~ df[[depth_col]], mean)
  names(df)[names(df) == "df[[date_col]]"] <- date_col

  df <- merge(all_dates, df, by = date_col, all = TRUE)
  df[[date_col]] <- as.numeric(as.factor(df[[date_col]]))

  df[df == "NaN"] <- NA

  df <- zoo::na.approx(df)
  df <- as.data.frame(df)
  df <- reshape2::melt(df, id.vars = date_col)

  names(df) <- c("number", depth_col, var)
  df[[depth_col]] <- as.numeric(as.character(df[[depth_col]]))

  all_dates$number <- as.numeric(as.factor(all_dates[[date_col]]))
  df <- merge(df, all_dates, by = "number", all = TRUE)
  df$number <- NULL

  return(df)

}
