test_that("sundial works", {

  times <- as.POSIXct(c("2025-04-02 00:42:05", "2025-03-04 04:24:20", "2025-06-05 14:30:40", "2025-09-30 22:14:09"),
                   "%Y-%m-%d %H:%M:%S", tz = "EST")
  example_data <- data.frame(times = times, Date_LT = as.Date(times), Time_LT = format(times, "%H:%M:%S"))

  expect_equal(sundial(example_data, date_col = "Date_LT", time_col = "Time_LT",
                       latitude = 45.4112, longitude = -75.6981, timezone = "EST")[, c("Period")],
               c("N1", "N1", "Day", "N2"))

})
