testthat::test_that("does fill the date", {

  # make lake

  dates <- rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"), each = 10)
  depths <- rep(seq(1:10), times = 365)
  temps <- seq(1:3650)

  fake_lake <- as.data.frame(cbind(dates, depths, temps))
  fake_lake$dates <- as.Date(fake_lake$dates)
  #fake_lake$dates <- as.POSIXct(fake_lake$dates, "%Y-%m-%d")

  fake_lake[337, 3] <- NA
  fake_lake <- fake_lake[-c(runif(450, min = 1, max = 3650)),]

  # test interpolation

  df <- interpolateDates(fake_lake, time_col = "dates", depth_col = "depths",
                         var = "temp", interval = "day", supplied.date = TRUE)

  expect_equal((filter(df, dates == as.POSIXct("2021-02-03", "%Y-%m-%d") & depths == 7))$temp, 337)

})

testthat::test_that("does not fill the date", {

  # make lake

  dates <- rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"), each = 10)
  depths <- rep(seq(1:10), times = 365)
  temps <- seq(1:3650)

  fake_lake <- as.data.frame(cbind(dates, depths, temps))
  fake_lake$dates <- as.Date(fake_lake$dates)
  fake_lake$dates <- as.POSIXct(fake_lake$dates, "%Y-%m-%d")

  fake_lake[1, 3] <- NA
  fake_lake <- fake_lake[-c(runif(450, min = 1, max = 3650)),]

  # test interpolation

  df <- interpolateDates(fake_lake, time_col = "dates", depth_col = "depths",
                         var = "temp", interval = "day")

  expect_equal(is.na(df$temp[1]), TRUE)

})
