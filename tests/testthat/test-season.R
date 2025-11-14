test_that("northern hemisphere works", {

  dates <- as.Date(c("2025-04-17", "2024-05-30", "2024-08-12", "2025-02-11"))
  data <- data.frame(dates = dates)

  expect_equal(season(data, date_col = "dates", type = "ice",
                      firstdays = as.Date(c("2024-02-12", "2024-05-15", "2024-09-17", "2024-12-01",
                                            "2025-03-14", "2025-06-12", "2025-09-25", "2025-11-23"))),
               data.frame(Year = as.numeric(c("2024", "2024", "2025", "2025")),
                          dates = as.Date(c("2024-05-30", "2024-08-12", "2025-04-17", "2025-02-11")),
                          Season = c("Summer", "Summer", "Spring", "Winter")))

})

test_that("southern hemisphere works", {

  dates <- as.Date(c("2025-04-17", "2024-05-30", "2024-08-12", "2025-02-11"))
  data <- data.frame(dates = dates)

  expect_equal(season(data, date_col = "dates", type = "ice",
                      firstdays = as.Date(c("2024-02-12", "2024-05-15", "2024-09-17", "2024-12-01",
                                            "2025-03-14", "2025-06-12", "2025-09-25", "2025-11-23")),
                      hemisphere = "south"),
               data.frame(Year = as.numeric(c("2024", "2024", "2025", "2025")),
                          dates = as.Date(c("2024-05-30", "2024-08-12", "2025-04-17", "2025-02-11")),
                          Season = c("Winter", "Winter", "Autumn", "Summer")))

})
