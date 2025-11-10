sundial <- function(df, mindate, maxdate, latitude, longitude, timezone) {

  if (system.file(package = "suncalc") == "" | system.file(package = "lubridate") == "") {
    stop ("This function relies on the packages suncalc and lubridate, at least one of which is not installed.")}

  if ("sunrise" %in% names(df)) { stop ("Sunrises have already been assigned, so this function did not run.")}

  suntimes <- suncalc::getSunlightTimes(date = seq(from = mindate, to = maxdate, by = "day"),
                                        lat = latitude, lon = longitude, tz = timezone) %>%
    select(c("date", "sunrise", "sunset")) %>%
    mutate(sunrise = gsub(".* ", "", sunrise),
           sunset = gsub(".* ", "", sunset))

  if (length(class(df)) > 1) { stop ("Look into the class of your data frame. Are there groups in there?")}
  if (class(df) != "data.frame") { stop ("This function requires data to be a data frame - consider object types before continuing.
                                         ** DID NOT EXECUTE **")}

  df <- merge(df, suntimes, by.x = "Date_LT", by.y = "date", all = TRUE)
  df <- df %>% mutate(sunrise = lubridate::hms(sunrise),
                      sunset = lubridate::hms(sunset),
                      Time_LT = lubridate::hms(Time_LT))

  df$dayPeriod <- ifelse(df$Time_LT > df$sunrise & df$Time_LT < df$sunset, "Day",
                         ifelse(df$Time_LT < lubridate::hms("12:00:00"), "N1",
                                ifelse(df$Time_LT > lubridate::hms("12:00:00"), "N2",
                                       NA))) ## this is here to catch issues

  return(df)

}
