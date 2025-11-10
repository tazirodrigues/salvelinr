#' @title timezone
#'
#' @description Converts UTC timestamps to DateTime, Date, and Time columns in local time
#'
#' @param df A data frame object
#' @param time_col Column with timestamps. Defaults to "DateTime_UTC"
#' @param tz_offset Number of hours different from UTC to local time. Note whether positive or negative.
#'
#' @return Adds three timestamp columns to the original data frame argument
#' @examples
#' matched_detects <- timezone(matched_detects, tz_offset = 6)
#' @export
#' @importFrom

correctTimes <- function(df, time_col = "DateTime_UTC", tz_offset) {

  if("POSIXct" %in% class(df[[time_col]]) == FALSE) { stop ("Please ensure datetime objects are formatted as POSIX before continuing. **DID NOT RUN**") }

  df$DateTime_LT <- as.POSIXct(df[[time_col]] + 60*60*tz_offset, "%Y-%m-%d %H:%M:%S")
  df$Date_LT <- as.Date(df$DateTime_LT, "%Y-%m-%d")
  df$Time_LT <- format(df$DateTime_LT, "%H:%M:%S")

  return(df)

}
