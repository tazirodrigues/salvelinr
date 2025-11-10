seasons <- function(df, date_col = "Date_LT",
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
