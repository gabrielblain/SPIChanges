daily.rain <-CampinasRain$Rain
expect_message(
rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4),
"Done. Just ensure the last quasi-week is complete.
  The last day of your series is 31 and TS is 4")
test_that("SPIChanges() works as expected in example", {
  Changes <- SPIChanges(rain.at.TS=rainTS4, only.linear = "yes")
  expect_type(Changes, "list")
  expect_length(Changes, 4)
  expect_named(Changes, c("data.week", "model.selection", "Changes.Freq.Drought","Model.Drought"))
  expect_equal(Changes[[1]]$SPI[1:4], c(-0.203, -0.035,  0.031,  0.122),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Exp.Acum.Prob[1:4], c(0.420, 0.486, 0.513, 0.549),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Actual.Acum.Prob[1:4], c(0.420, 0.486, 0.513, 0.549),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$ChangeFreq[1:4], c(0.000,  0.000,  0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[2]][1:4,3], c(1, 1, 1, 1),
               tolerance = 0.00)
  expect_equal(Changes[[3]][1:4,3], c(0.000, 0.000, 0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,4], c(0.000, 0.000, 0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,5], c(0.000, 0.000, 0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,1], c(1, 1, 1, 1))
  expect_equal(Changes[[4]][1:4,2], c(1, 1, 1, 1))
  expect_equal(Changes[[4]][1:4,3], c(15.9, 15.9, 15.9, 15.9),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,4], c(6.7, 6.7, 6.7, 6.7),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,5], c(2.3, 2.3, 2.3, 2.3),
               tolerance = 0.05)
})

test_that("SPIChanges() works when only.linear = no", {
  Changes <- SPIChanges(rain.at.TS=rainTS4, only.linear = "no")
  expect_type(Changes, "list")
  expect_length(Changes, 4)
  expect_named(Changes, c("data.week", "model.selection", "Changes.Freq.Drought","Model.Drought"))
  expect_equal(Changes[[1]]$SPI[1:4], c(-0.203, -0.035,  0.031,  0.122),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Exp.Acum.Prob[1:4], c(0.420, 0.486, 0.513, 0.549),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Actual.Acum.Prob[1:4], c(0.420, 0.486, 0.513, 0.549),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$ChangeFreq[1:4], c(0.000,  0.000,  0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[2]][1:4,3], c(1, 1, 1, 1),
               tolerance = 0.00)
  expect_equal(Changes[[3]][1:4,3], c(0.000, 0.000, 0.000,  0.000),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,4], c(0.000, 0.000, 0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,5], c(0.000, 0.000, 0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,1], c(1, 1, 1, 1))
  expect_equal(Changes[[4]][1:4,2], c(1, 1, 1, 1))
  expect_equal(Changes[[4]][1:4,3], c(15.9, 15.9, 15.9, 15.9),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,4], c(6.7, 6.7, 6.7, 6.7),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,5], c(2.3, 2.3, 2.3, 2.3),
               tolerance = 0.05)
})

daily.rain <-CampinasRain$Rain
expect_message(
rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4),
"Done. Just ensure the last quasi-week is complete.
  The last day of your series is 31 and TS is 4")
test_that("SPIChanges() works as expected in example", {
  rainTS4.warning <- rainTS4[1:1439,]
  expect_warning(
  expect_warning(
  Changes <- SPIChanges(rain.at.TS=rainTS4.warning, only.linear = "yes"),
  "Less than 30 years of rainfall records. Longer periods are highly recommended."),
  "rainfall series Month 9 Week 1 has more than 6.7% of zeros. In this situation
             the SPI cannot assume values lower than -1.5")
  expect_type(Changes, "list")
  expect_length(Changes, 4)
  expect_named(Changes, c("data.week", "model.selection", "Changes.Freq.Drought","Model.Drought"))
  expect_equal(Changes[[1]]$SPI[1:4], c(-0.438, -0.204, -0.275, -0.206),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Exp.Acum.Prob[1:4], c(0.331, 0.419, 0.392, 0.418),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$Actual.Acum.Prob[1:4], c(0.331, 0.419, 0.392, 0.418),
               tolerance = 0.05)
  expect_equal(Changes[[1]]$ChangeFreq[1:4], c(0.000, 0.000, 0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[2]][1:4,3], c(1, 1, 1, 1),
               tolerance = 0.00)
  expect_equal(Changes[[3]][1:4,3], c(0.000,  0.000,  0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,4], c(0.000,  0.000,  0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[3]][1:4,5], c(0.000,  0.000,  0.000, 0.000),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,1], c(1, 1, 1, 1))
  expect_equal(Changes[[4]][1:4,2], c(1, 1, 1, 1))
  expect_equal(Changes[[4]][1:4,3], c(15.9, 15.9, 15.9, 15.9),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,4], c(6.7, 6.7, 6.7, 6.7),
               tolerance = 0.05)
  expect_equal(Changes[[4]][1:4,5], c(2.3, 2.3, 2.3, 2.3),
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
