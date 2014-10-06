context("Timedata objects")

tdata <- data.frame(Area = c(rep("A", 48), rep("B", 48)),
                    Time = 1:48,
                    Load = c(runif(48, 200, 250), runif(48, 400, 450)),
                    Wind = c(runif(48, 20, 25), runif(48, 40, 45)))
tdata2 <- tdata
tdata2$Sc <- tdata2$Area
levs <- data.frame(BAA = c("A", "B"), Region = c("All", "All"))

test_that("timedata format", {
  expect_that(format_timedata(tdata), is_a("data.frame"))
  expect_that(format_timedata(tdata, levs), is_a("data.frame"))
})

test_that("timedata column names", {
  expect_that(format_timedata(tdata), has_names(c("Level", "Area", "Time", "Day", "WinProb", "Load", "Wind")))
  expect_that(format_timedata(tdata, levs), has_names(c("Level", "Area", "Time", "Day", "WinProb", "Load", "Wind")))
  expect_that(format_timedata(tdata2, scenario = "Sc"), has_names(c("Sc", "Level", "Area", "Time", "Day", "WinProb", "Load", "Wind")))
})

test_that("WinProb value set to 1", {
  expect_that(max(format_timedata(tdata)$WinProb), equals(1))
  expect_that(min(format_timedata(tdata)$WinProb), equals(1))
})

