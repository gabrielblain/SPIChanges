test_that("TSaggreg() works as expected in example", {
  daily.rain <- CampinasRain[,2]
  expect_message(
  tes <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4),
  "Done. Just ensure the last quasi-week is complete.
  The last day of your series is 31 and TS is 4")
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 4)
  expect_equal(nrow(tes), 1533)
  expect_named(
    tes,
    c(
      "Year",
      "Month",
      "quasiWeek",
      "rain.at.TS4"
    )
  )
  expect_equal(tes[1:4, "Year"], c(1991, 1991, 1991, 1991),
               tolerance = 0.00)
  expect_equal(tes[1:4, "Month"], c(1, 2, 2, 2),
               tolerance = 0.00)
  expect_equal(tes[1:4, "quasiWeek"], c(4, 1, 2, 3), tolerance = 0.00)
  expect_equal(tes[1:4, "rain.at.TS4"], c(406.3, 517.7, 530.5, 455.5),
               tolerance = 0.1)
})

test_that("TSaggreg() works as expected when rainfall records is short", {
  daily.rain <- CampinasRain[1:10949,2]
  expect_warning(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4),
    "Less than 30 years of rainfall records. Longer periods are highly recommended.")
  expect_s3_class(tes, "data.frame")
  expect_length(tes, 4)
  expect_equal(nrow(tes), 1437)
  expect_named(
    tes,
    c(
      "Year",
      "Month",
      "quasiWeek",
      "rain.at.TS4"
    )
  )
  expect_equal(tes[1:4, "Year"], c(1991, 1991, 1991, 1991),
               tolerance = 0.00)
  expect_equal(tes[1:4, "Month"], c(1, 2, 2, 2),
               tolerance = 0.00)
  expect_equal(tes[1:4, "quasiWeek"], c(4, 1, 2, 3), tolerance = 0.00)
  expect_equal(tes[1:4, "rain.at.TS4"], c(406.3, 517.7, 530.5, 455.5),
               tolerance = 0.1)
})

test_that("rainfall record is too short", {
  daily.rain <- CampinasRain[1:700,2]
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4),
  "Less than 10 years of rainfall records. We cannot procede")
})

test_that("rainfall record with missing values", {
  daily.rain <- CampinasRain[,2]
  daily.rain[1] <- NA
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4),
    "Physically impossible or missing rain values")
})

test_that("rainfall record with negative values", {
  daily.rain <- CampinasRain[,2]
  daily.rain[1] <- -10
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4),
    "Physically impossible or missing rain values")
})

test_that("rainfall record with non numerical values", {
  daily.rain <- CampinasRain[,2]
  daily.rain[1] <- "muita chuva"
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4),
    "Physically impossible or missing rain values")
})

test_that("rainfall record with wrong format", {
  daily.rain <- cbind(CampinasRain[,2],CampinasRain[,2])
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4),
    "Physically impossible or missing rain values")
})

test_that("start.date with wrong format", {
  daily.rain <- CampinasRain[,2]
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="hoje",TS=4),
    "`hoje` is not in a valid date format. Please enter a valid date format.")
})

test_that("start.date with another wrong format", {
  daily.rain <- CampinasRain[,2]
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="91-01-01",TS=4),
    "`91-01-01` is not in a valid date format. Please enter a valid date format.")
})

test_that("TS not a number", {
  daily.rain <- CampinasRain[,2]
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="91-01-01",TS="quatro"),
    "TS must be an interger single number between 1 and 96")
})

test_that("TS lower than 1", {
  daily.rain <- CampinasRain[,2]
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="91-01-01",TS=0),
    "TS must be an interger single number between 1 and 96")
})

test_that("TS larger than 96", {
  daily.rain <- CampinasRain[,2]
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="91-01-01",TS=97),
    "TS must be an interger single number between 1 and 96")
})

test_that("TS wrong format", {
  daily.rain <- CampinasRain[,2]
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="91-01-01",TS=c(2,97)),
    "TS must be an interger single number between 1 and 96")
})
