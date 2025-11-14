#' @title season
#'
#' @description Assign seasons to detection data based on first-day thresholds
#'
#' @param df A data frame object with detections
#' @param date_col The name (in quotes) of the column that contains dates. Defaults to "Date_LT"
#' @param type One of "ice" or "month." If "ice," supply a vector of dates; if "month," supply a vector of numeric values corresponding to months.
#' @param firstdays Vector of first days corresponding to each season.
#' @param hemisphere Defaults to "north" - specify as "south" to flip the order of the seasons.
#'
#' @return Adds a "Season" column to the original data frame supplied.
#' @examples
#' matched_detects <- season(matched_detects,
#'   date_col = "Date_LT", type = "ice",
#'   firstdays = c("2025-03-14", "2025-05-25", "2025-10-02", "2025-11-14"), hemisphere = "north"
#' )
#' @export
#' @importFrom dplyr "%>%"
#' dplyr "mutate"
#' dplyr "case_when"

season <- function(df, date_col = "Date_LT",
                   type = "ice", firstdays,
                   hemisphere = "north") {
  ## check for problems

  if (type != "ice" & type != "month") {
    stop("Please pick a type from one of 'ice' or 'month.'")}

  if (class(df[[date_col]]) != "Date") {
    df[[date_col]] <- as.Date(df[[date_col]], "%Y-%m-%d")
    print("Dates have been converted assuming the convention %Y-%m-%d.")}

  if (length(firstdays) %% 4 != 0) {
    stop("The length of 'firstdays' vector must be a multiple of 4.")}

  if (class(firstdays) != class(df[[date_col]])) {
    stop("Season thresholds must match class of given dates.")
  }

  ## into the function

  if (type == "month") {

    firstdays <- sort(firstdays)

    if (hemisphere == "north") {
      df <- df %>% mutate(Month = as.numeric(format(df[[date_col]], "%m")),
        Season = case_when(
          Month >= firstdays[1] & Month < firstdays[2] ~ "Spring",
          Month >= firstdays[2] & Month < firstdays[3] ~ "Summer",
          Month >= firstdays[3] & Month < firstdays[4] ~ "Fall",
          TRUE ~ "Winter"))

    } else if (hemisphere == "south") {

      df <- df %>% mutate(Month = as.numeric(format(df[[date_col]], "%m")),
        Season = case_when(
          Month >= firstdays[1] & Month < firstdays[2] ~ "Fall",
          Month >= firstdays[2] & Month < firstdays[3] ~ "Winter",
          Month >= firstdays[3] & Month < firstdays[4] ~ "Spring",
          TRUE ~ "Summer"))
    }

    return(df)

  } else { ## in this case type == "ice"

    yearly <- as.data.frame(firstdays)
    yearly$Year <- as.numeric(format(yearly$firstdays, "%Y"))
    yearly <- dplyr::arrange(yearly, firstdays)

    if (hemisphere == "north") {
      yearly$Season <- rep(c("Spring", "Summer", "Fall", "Winter"), times = length(unique(yearly$Year)))
    } else if (hemisphere == "south") {
      yearly$Season <- rep(c("Autumn", "Winter", "Spring", "Summer"), times = length(unique(yearly$Year)))
    }

    yearly <- yearly %>% tidyr::pivot_wider(names_from = Season, values_from = firstdays)

    df$Year <- as.numeric(format(df[[date_col]], "%Y"))
    df <- merge(df, yearly, by = "Year", all.x = TRUE)

    if (hemisphere == "north") {
      df$Season <- ifelse(df[[date_col]] >= df$Spring & df[[date_col]] < df$Summer, "Spring",
        ifelse(df[[date_col]] >= df$Summer & df[[date_col]] < df$Fall, "Summer",
          ifelse(df[[date_col]] >= df$Fall & df[[date_col]] < df$Winter, "Fall",
            "Winter")))
    }

    if (hemisphere == "south") {
      df$Season <- ifelse(df[[date_col]] >= df$Autumn & df[[date_col]] < df$Winter, "Autumn",
        ifelse(df[[date_col]] >= df$Winter & df[[date_col]] < df$Spring, "Winter",
          ifelse(df[[date_col]] >= df$Spring & df[[date_col]] < df$Summer, "Spring",
            "Summer")))
    }
  }

  df$Summer <- NULL; df$Winter <- NULL; df$Fall <- NULL; df$Spring <- NULL; df$Autumn <- NULL
  return(df)
}
