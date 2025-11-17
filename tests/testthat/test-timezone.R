test_that("timezone error catches", {

  df <- data.frame(times = c("2025-05-10 20:40:28", "2025-04-11 23:05:45", "2025-08-23 11:38:29", "2025-09-15 09:25:14"))

  expect_error(timezone(df, time_col = "times"),
               "Please ensure datetime objects are formatted as POSIX before continuing.")

})

test_that("timezone converts", {

  df <- data.frame(times = c("2025-05-10 20:40:28", "2025-04-11 23:05:45", "2025-08-23 11:38:29", "2025-09-15 09:25:14"))
  df$times <- as.POSIXct(df$times, "%Y-%m-%d %H:%M:%S")

  expect_equal(timezone(df, time_col = "times", tz_offset = -5),
               data.frame(times = df$times,
                          DateTime_LT = as.POSIXct(c("2025-05-10 15:40:28", "2025-04-11 18:05:45", "2025-08-23 06:38:29", "2025-09-15 04:25:14"), "%Y-%m-%d %H:%M:%S"),
                          Date_LT = as.Date(c("2025-05-10", "2025-04-11", "2025-08-23", "2025-09-15")),
                          Time_LT = c("15:40:28", "18:05:45", "06:38:29", "04:25:14"))
            )

})
