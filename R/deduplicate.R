#' @title deduplicate
#'
#' @description Removes duplicate detections from receiver data.
#'
#' @param df A data frame containing receiver detections
#' @param fish_col The name (in quotes) of the column that contains individual animal IDs
#' @param time_col The name (in quotes) of the column that contains date-times
#' @param threshold Value for how far apart detections must be to be considered *not* duplicates
#' @param type One of either "hammer," which will just hammer out all detections within the interval, or "spear," which will do so in a step-wise way to conserve as many detections as possible given the threshold.
#'
#' @return A data frame with only detections that meet the threshold for inclusion
#' @examples
#' deduplicated_vr2 <- deduplicate(vr2, fish_col = "FishID", time_col = "DateTime_LT", threshold = 60, type = "hammer)
#' @export
#' @importFrom svMisc "progress"

deduplicate <- function(df, fish_col = "FishID", time_col = "DateTime_LT", threshold = 60, type = "hammer") {

    new_df <- data.frame()

    if(class(df[[fish_col]]) != "character") {
      df[[fish_col]] <- as.character(df[[fish_col]]); print("Fish column coerced to chararcter.")
    }

    if(type == "hammer") {

    for(i in 1:length(unique(df[[fish_col]]))) {

      sdf <- filter(df, df[[fish_col]] == unique(df[[fish_col]])[i])
      sdf <- arrange(sdf, sdf[[time_col]])

      sdf$diff <- as.numeric(difftime(sdf[[time_col]], lag(sdf[[time_col]]), units = "secs"))
      sdf <- filter(sdf, as.numeric(diff) > threshold)

      new_df <- rbind(new_df, sdf)
      svMisc::progress(i, length(unique(df[[fish_col]])))

      }

    return(new_df)

    } else if(type == "spear") {

      for(i in 1:length(unique(df[[fish_col]]))) {

        percduplo <- 1

        sdf <- filter(df, df[[fish_col]] == unique(df[[fish_col]][i]))
        sdf <- arrange(sdf, sdf[[time_col]])

        sdf$diff <- as.numeric(difftime(sdf[[time_col]], lag(sdf[[time_col]]), units = "secs"))
        sdf <- filter(sdf, diff > 10) ## bare minimum, should cut down on loops

        sdf$anchor <- ifelse(sdf$diff > threshold, TRUE, FALSE)
        sdf <- filter(sdf, anchor == TRUE | anchor == FALSE & lag(anchor) == FALSE)
        percduplo <- nrow(filter(sdf, diff < threshold))/nrow(sdf)

        while(percduplo > 0) {

          sdf <- arrange(sdf, sdf[[time_col]])

          sdf$diff <- as.numeric(difftime(sdf[[time_col]], lag(sdf[[time_col]]), units = "secs"))

          sdf$anchor <- ifelse(sdf$diff > threshold, TRUE, FALSE)
          sdf <- filter(sdf, anchor == TRUE | anchor == FALSE & lag(anchor) == FALSE)
          percduplo <- nrow(filter(sdf, diff < threshold))/nrow(sdf)

        }

        new_df <- rbind(new_df, sdf)
        svMisc::progress(i, length(unique(df[[fish_col]])))

      }

      return(new_df)

    } else { stop("Type must be either 'hammer' (recommended for low comptuational power) or 'spear' (recommended for precision).")
       }

}
