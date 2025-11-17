#' @title sundial
#'
#' @description Applies suncalc::getSunlightTimes and then derives periods of the day based on sunrise/sunsets.
#'
#' @param df A data frame object.
#' @param date_col Name (in quotes) of the column with dates in the data frame.
#' @param time_col Name (in quotes) of the column with times.
#' @param latitude Latitude to use for calculating sunrise/sunset.
#' @param longitude Longitude to use for calculating sunrise/sunset.
#' @param mindate First date for which to calculate. Defaults to minimum value of date_col
#' @param maxdate Last date for which to calculate. Defaults to maximum value of date_col
#' @param timezone Timezone associated with df
#'
#' @return Adds columns to input df reflecting sunrise, sunset, and period of the day.
#' @examples
#' vr2_detects_daynight <- sundial(vr2_detects, date_col = "Date_LT", time_col = "Time_LT", latitude = -36, longitude = 174,
#' timezone = "NZ")
#' @export
#' @importFrom suncalc "getSunlightTimes"
#' dplyr "mutate"

sundial <- function(df, date_col, time_col,
                    latitude, longitude,
                    mindate = min(df[[date_col]]), maxdate = max(df[[date_col]]), timezone) {

  if (system.file(package = "suncalc") == "" | system.file(package = "lubridate") == "") {
    stop ("This function relies on the packages suncalc and lubridate, at least one of which is not installed.")}

  if ("sunrise" %in% names(df)) { stop ("Sunrises have already been assigned, so this function did not run.")}

  if(abs(latitude) > 90 | abs(longitude) > 180) { stop("Please supply lat/long values in decimal degrees.")}

  setrise <- suncalc::getSunlightTimes(date = seq(from = mindate, to = maxdate, by = "day"),
                                        lat = latitude, lon = longitude, tz = timezone) %>%
    dplyr::select(c("date", "sunrise", "sunset")) %>%
    dplyr::mutate(sunrise = gsub(".* ", "", sunrise),
           sunset = gsub(".* ", "", sunset))

  if (length(class(df)) > 1) { stop ("Look into the class of your data frame. Are there groups in there?")}
  if (class(df) != "data.frame") { stop ("This function requires data to be a data frame - consider object types before continuing.
                                         ** DID NOT EXECUTE **")}

  df <- merge(df, setrise, by.x = date_col, by.y = "date", all = TRUE)
  df <- df %>% dplyr::mutate(sunrise = lubridate::hms(sunrise),
                      sunset = lubridate::hms(sunset),
                      suntime = lubridate::hms(df[[time_col]]))

  df$Period <- ifelse(df$suntime > df$sunrise & df$suntime < df$sunset, "Day",
                         ifelse(df$suntime < lubridate::hms("12:00:00"), "N1",
                                ifelse(df$suntime > lubridate::hms("12:00:00"), "N2",
                                       NA))) ## this is here to catch issues

  df <- dplyr::filter(df, is.na(df[[time_col]]) == FALSE) ## in case the dates we were given are non-continuous

  return(df)

}
