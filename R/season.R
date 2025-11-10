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
#' matched_detects <- season(matched_detects, date_col = "Date_LT", type = "ice",
#' firstdays = c("2025-03-14", "2025-05-25", "2025-10-02", "2025-11-14"), hemisphere = "north")
#' @export
#' @importFrom dplyr "%>%"
#' dplyr "mutate"
#' dplyr "case_when"

season <- function(df, date_col = "Date_LT",
                    type = "ice", firstdays,
                    hemisphere = "north") {

  ## error messages

  if(type != "ice" & type != "month") { stop ("Please pick a type from one of 'ice' or 'month.'")}

  if(class(df[[date_col]]) != "Date") {

    df[[date_col]] <- as.Date(df[[date_col]], "%Y-%m-%d")
    print("Dates have been converted assuming the convention %Y-%m-%d.")

  }

  firstdays <- sort(firstdays)

  ## ice firstdays

  if(type == "ice") {

    df$Season = ifelse(df[[date_col]] >= firstdays[1] & df[[date_col]] < firstdays[2], "Spring",
                       ifelse(df[[date_col]] >= firstdays[2] & df[[date_col]] < firstdays[3], "Summer",
                              ifelse(df[[date_col]] >= firstdays[3] & df[[date_col]] < firstdays[4], "Fall",
                                     "Winter")))

  }

  ## month firstdays

  if(type == "month") {

    if(hemisphere == "north") {

      df <- df %>% mutate(Month = as.numeric(format(df[[date_col]], "%m")),
                          Season = case_when(Month >= firstdays[1] & Month < firstdays[2] ~ "Spring",
                                             Month >= firstdays[2] & Month < firstdays[3] ~ "Summer",
                                             Month >= firstdays[3] & Month < firstdays[4] ~ "Fall",
                                             TRUE ~ "Summer"))


    }

    if(hemisphere == "south") {

      df <- df %>% mutate(Month = as.numeric(format(df[[date_col]], "%m")),
                          Season = case_when(Month >= firstdays[1] & Month < firstdays[2] ~ "Fall",
                                             Month >= firstdays[2] & Month < firstdays[3] ~ "Winter",
                                             Month >= firstdays[3] & Month < firstdays[4] ~ "Spring",
                                             TRUE ~ "Summer"))

    }

  }

  return(df)

}
