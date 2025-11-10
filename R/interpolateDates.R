#' @title interpolateDates
#'
#' @description Interpolates temperature data to populate missing dates
#'
#' @param df A data frame object. Currently, must contain a date column and a depth column.
#' @param date_col Name (in quotes) of the column with dates in the data frame.
#' @param var Variable of interest. Defaults to "Temp"
#' @param interval Temporal resolution of interpolated data frame. Defaults to "day"
#'
#' @return Interpolated data for the variable of interest.
#' @examples
#' interpolated_temps <- interpolateDates(temp_profiles, date_col = "Date_LT", interval = "day")
#' @export
#' @importFrom zoo "na.approx"

interpolateDates <- function(df, date_col, var = "Temp", interval = "day") {

  df$Date <- as.Date(df[[date_col]], "%Y-%m-%d")

  all_dates <- data.frame(Date = seq.Date(min(df$Date),
                                          max(df$Date), interval))

  df <- dcast(df, Date ~ Depth, mean)
  df <- merge(all_dates, df, by = "Date", all = TRUE)

  df$Date <- as.numeric(as.factor(df$Date))

  df <- na.approx(df)
  df <- as.data.frame(df)
  df <- melt(df, id.vars = c("Date"))

  names(df) <- c("number", "Depth", var)
  df$Depth <- as.numeric(as.character(df$Depth))

  all_dates$number <- as.numeric(as.factor(all_dates$Date))
  df <- merge(df, all_dates, by = "number", all = TRUE)

  return(df)

}
