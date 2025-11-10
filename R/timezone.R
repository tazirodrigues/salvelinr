correctTimes <- function(df, time_col = "DateTime_UTC", tz_offset) {

  if("POSIXct" %in% class(df[[time_col]]) == FALSE) { stop ("Please ensure datetime objects are formatted as POSIX before continuing. **DID NOT RUN**") }

  df$DateTime_LT <- as.POSIXct(df[[time_col]] + 60*60*tz_offset, "%Y-%m-%d %H:%M:%S")
  df$Date_LT <- as.Date(df$DateTime_LT, "%Y-%m-%d")
  df$Time_LT <- format(df$DateTime_LT, "%H:%M:%S")

  return(df)

}
