daily.rain <- CampinasRain[,2]
rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4)

test_that("SPIChanges() works as expected in example", {
  Changes <- SPIChanges(rain.at.TS=rainTS4, only.linear = "yes")
  expect_type(Changes, "list")
  expect_length(Changes, 3)
  expect_named(Changes, c("data.week", "model.selection", "Changes.Freq.Drought"))
  expect_equal(Changes[[1]]$SPI[1:4], c(1.48, 2.42, 2.44, 2.01),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Exp.Acum.Prob[1:4], c(0.93, 0.99, 0.99, 0.98),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Actual.Acum.Prob[1:4], c(0.86, 0.99, 0.92, 0.90),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$ChangeFreq[1:4], c(6.72, 0.00, 7.45, 8.07),
               tolerance = 0.05)
  expect_equal(Changes[[2]][1:4,3], c(1, 1, 3, 3),
               tolerance = 0.00)
  expect_equal(Changes[[3]][1:4,3], c(0.00, 0.00, -12.98, -12.83),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,4], c(0.00, 0.00, -6.43, -6.43),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,5], c(0.00, 0.00, -2.29, -2.29),
               tolerance = 0.05)
})

test_that("SPIChanges() works when only.linear = no", {
  Changes <- SPIChanges(rain.at.TS=rainTS4, only.linear = "no")
  expect_type(Changes, "list")
  expect_length(Changes, 3)
  expect_named(Changes, c("data.week", "model.selection", "Changes.Freq.Drought"))
  expect_equal(Changes[[1]]$SPI[1:4], c(1.48, 2.42, 2.44, 2.01),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Exp.Acum.Prob[1:4], c(0.93, 0.99, 0.99, 0.98),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Actual.Acum.Prob[1:4], c(0.86, 0.99, 0.82, 0.90),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$ChangeFreq[1:4], c(6.72, 0.00, 17.73, 8.07),
               tolerance = 0.05)
  expect_equal(Changes[[2]][1:4,3], c(1, 1, 3, 3),
               tolerance = 0.00)
  expect_equal(Changes[[3]][1:4,3], c(0.00, 0.00, -12.98, -12.83),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,4], c(0.00, 0.00, -6.43, -6.43),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,5], c(0.00, 0.00, -2.29, -2.29),
               tolerance = 0.05)
})

daily.rain <- CampinasRain[,2]
rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4)
test_that("SPIChanges() works as expected in example", {
  rainTS4.warming <- rainTS4[1:1439,]
  expect_warning(
  Changes <- SPIChanges(rain.at.TS=rainTS4.warming, only.linear = "yes"),
  "Less than 30 years of rainfall records. Longer periods are highly recommended.")
  expect_type(Changes, "list")
  expect_length(Changes, 3)
  expect_named(Changes, c("data.week", "model.selection", "Changes.Freq.Drought"))
  expect_equal(Changes[[1]]$SPI[1:4], c(1.45, 2.40, 2.39, 1.95),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Exp.Acum.Prob[1:4], c(0.93, 0.99, 0.99, 0.97),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Actual.Acum.Prob[1:4], c(0.87, 0.97, 0.84, 0.87),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$ChangeFreq[1:4], c(5.30,  2.47, 14.87, 10.16),
               tolerance = 0.05)
  expect_equal(Changes[[2]][1:4,3], c(1, 1, 3, 3),
               tolerance = 0.00)
  expect_equal(Changes[[3]][1:4,3], c(0.00, 0.00, -11.21, -10.68),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,4], c(0.00, 0.00, -5.99, -5.93),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,5], c(0.00, 0.00, -2.24, -2.24),
               tolerance = 0.05)
})

test_that("rainTS4 with negative data", {
  rainTS4.wrong <- (-1*rainTS4)
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Physically impossible or missing values in rain.at.TS.")
})

test_that("rainTS4 with only 3 columns", {
  rainTS4.wrong <- rainTS4[,2:4]
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Physically impossible or missing values in rain.at.TS.")
})

test_that("rainfall records too short", {
  rainTS4.wrong <- rainTS4[1:479,]
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Less than 10 years of rainfall records. We cannot procede.")
})

test_that("rainfall records with missing months", {
  rainTS4.wrong <- rainTS4
  n <- length(rainTS4.wrong[,1])
  rainTS4.wrong[,2] <- rep(1,n)
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Column Month in rain.at.TS is probably malformed.")
})

test_that("rainfall records with wrong months", {
  rainTS4.wrong <- rainTS4
  rainTS4.wrong[1,2] <- 0
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Column Month in rain.at.TS is probably malformed.")
})

test_that("rainfall records with another wrong months", {
  rainTS4.wrong <- rainTS4
  rainTS4.wrong[1,2] <- 13
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Column Month in rain.at.TS is probably malformed.")
})

test_that("rainfall records with missing quasiWeek", {
  rainTS4.wrong <- rainTS4
  n <- length(rainTS4.wrong[,1])
  rainTS4.wrong[,3] <- rep(1,n)
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Column quasiWeek in rain.at.TS is probably malformed.")
})

test_that("rainfall records with wrong quasiWeek", {
  rainTS4.wrong <- rainTS4
  rainTS4.wrong[1,3] <- 5
  n <- length(rainTS4.wrong[,1])
  rainTS4.wrong[,3] <- rep(1,n)
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Column quasiWeek in rain.at.TS is probably malformed.")
})

test_that("rainfall records with another wrong quasiWeek", {
  rainTS4.wrong <- rainTS4
  rainTS4.wrong[1,3] <- 0
  n <- length(rainTS4.wrong[,1])
  rainTS4.wrong[,3] <- rep(1,n)
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4.wrong, only.linear = "no"),
    "Column quasiWeek in rain.at.TS is probably malformed.")
})

test_that("Wrong only.linear", {
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4, only.linear = "maybe"),
    "imput only.linear should be Yes or No.")
})

test_that("Another wrong only.linear", {
  expect_error(
    Changes.wrong <- SPIChanges(rain.at.TS=rainTS4, only.linear = 4),
    "imput only.linear should be Yes or No.")
})
