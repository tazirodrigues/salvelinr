crepuscule <- function(df, fish_col, time_col, day_periods = "dayPeriod", threshold = 0) {

  ## first check - do we have sunrise and sunset times?

  if(is.null(df[[day_periods]]) == TRUE) {
    stop("Before running this function, assign day periods. You can use salvelinr::sundial to do so.") }

  ## now make sure we have dates

  df$date <- as.Date(df[[time_col]])

  ## next check - how many detections are there for each fish each day?

  tester_table <- df %>% group_by(df[[fish_col]], date, df[[day_periods]]) %>%
    summarise(ndet = n()) %>% filter(ndet >= threshold) ## we may want a threshold

  ## now we'll get the first and last detections for each night period

  n1 <- df %>% filter(df[[day_periods]] == "N1") %>% group_by(df[[time_col]], df[[fish_col]]) %>%
    arrange(df[[time_col]]) %>%
    filter(row_number() == n()) ## gets last detections from early night
  n2 <- df %>% filter(df[[day_periods]] == "N2") %>% group_by(df[[time_col]], df[[fish_col]]) %>%
    arrange(df[[time_col]]) %>%
    filter(row_number() == 1) ## gets first detections from late night
  dayPresent <- tester_table %>% filter(df[[day_periods]] == "Day") %>%
    select(date, fish_col) %>% mutate(detected_day = TRUE)

  nights <- rbind(n1, n2); nights <- merge(dayPresent, nights, all = TRUE)

}
