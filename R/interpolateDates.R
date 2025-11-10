interpolate.date <- function(df, var = "Temp", interval = "day") {

  df$Date <- as.Date(df$Date, "%Y-%m-%d")

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
