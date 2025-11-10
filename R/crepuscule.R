crepuscule <- function(df, threshold = 0, fishfilter = FALSE) {

  ## first check - do we have sunrise and sunset times?

  if(is.null(df$sunrise) == TRUE | is.null(df$sunset)) {
    stop("When did the sun rise and set? You can use suncalc::getSunlightTimes to find out, or apply the function suntimer from the library.") }

  if(fishfilter == TRUE) { df <- filter(df, grepl("ish", TRANSMITTER) == TRUE) }

  ## next check - how many detections are there for each fish each day?

  tester_table <- df %>% group_by(TRANSMITTER, Date_LT, dayPeriod) %>%
    summarise(ndet = n()) %>% filter(ndet >= threshold) ## we may want a threshold

  ## now we'll get the first and last detections for each night period

  n1 <- df %>% filter(dayPeriod == "N1") %>% group_by(Date_LT, TRANSMITTER) %>%
    arrange(DateTime_LT) %>%
    filter(row_number() == n()) ## gets last detections from early night
  n2 <- df %>% filter(dayPeriod == "N2") %>% group_by(Date_LT, TRANSMITTER) %>%
    arrange(DateTime_LT) %>%
    filter(row_number() == 1) ## gets first detections from late night
  dayPresent <- tester_table %>% filter(dayPeriod == "Day") %>%
    select("Date_LT", "TRANSMITTER") %>% mutate(detected_day = TRUE)

  nights <- rbind(n1, n2); nights <- merge(dayPresent, nights, all = TRUE)

}
