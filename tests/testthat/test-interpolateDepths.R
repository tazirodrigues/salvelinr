testthat::test_that("does fill the depth", {

  # make lake

  dates <- rep(seq.Date(as.Date("2021-01-01"), as.Date("2021-12-31"), by = "day"), each = 10)
  depths <- rep(seq(1:10), times = 365)
  temps <- seq(1:3650)

  fake_lake <- as.data.frame(cbind(dates, depths, temps))
  fake_lake$dates <- as.Date(fake_lake$dates)
  fake_lake$dates <- as.POSIXct(fake_lake$dates, "%Y-%m-%d")

  fake_lake[337, 3] <- NA
  fake_lake <- fake_lake[-c(runif(450, min = 1, max = 3650)),]

  # test interpolation

  df <- interpolateDepths(fake_lake, var = "temps", time_col = "dates", depth_col = "depths",
                          min = 1, max = 10, interval = 0.1, sinkends = FALSE)

  expect_equal((filter(df, dates == as.POSIXct("2021-02-03", "%Y-%m-%d") & depths == 7))$temp, 337)

})

testthat::test_that("sinkends = FALSE makes long but doesn't add data", {

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

  df <- interpolateDepths(fake_lake, var = "temps", time_col = "dates", depth_col = "depths",
                          min = 1, max = 15, interval = 0.1, sinkends = FALSE)

  expect_equal(is.na(unique(filter(df, depths == 15)$temps)), TRUE)

})

testthat::test_that("sinkends = FALSE makes long but doesn't add data", {

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

  df <- interpolateDepths(fake_lake, var = "temps", time_col = "dates", depth_col = "depths",
                          min = 1, max = 15, interval = 0.1, sinkends = TRUE)

  expect_equal(NA %in% (filter(df, depths == 15))$temps, FALSE)

})
