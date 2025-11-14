test_that("correct fish are retained", {

  names <- c("arnold", "max", "alice", "corrine", "stephanie", "stephanie", "max")
  numbers <- c(runif(7))
  obs <- data.frame(names = names, numbers = numbers)

  expect_equal(unique(keepFish(obs, fish_col = "names",
                        fishnames = c("adam", "ronald", "max", "alice", "corrine", "stephanie"))$names),
               c("max", "alice", "corrine", "stephanie"))

})

test_that("error if lists are different", {

  names <- c("arnold", "max", "alice", "corrine", "stephanie", "stephanie", "max")
  numbers <- c(runif(7))
  obs <- data.frame(names = names, numbers = numbers)

  expect_error(keepFish(obs, fish_col = "names",
                        fishnames = as.factor(c("adam", "ronald", "max", "alice", "corrine", "stephanie"))),
               "Both fish lists must be of the same class.")

})
