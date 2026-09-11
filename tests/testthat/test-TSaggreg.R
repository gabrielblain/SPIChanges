daily.rain <- CampinasRain$Rain
test_that("TSaggreg() works as expected in example", {
  expect_message(
    tes <- TSaggreg(
      daily.rain = daily.rain,
      start.date = "1980-01-01",
      TS = 4
    ),
    "Done. Just ensure the last quasi-week is complete.
  The last day of your series is 31 and TS is 4"
  )
  expect_s3_class(tes, c("TSaggreg", "matrix", "array"))
  expect_identical(dim(tes), c(2109L, 4L))
  expect_identical(colnames(tes), c(
      "Year",
      "Month",
      "quasiWeek",
      "rain.at.TS4"
    ))
  expect_equal(tes[1:4, "Year"], c(1980, 1980, 1980, 1980), tolerance = 0.00)
  expect_equal(tes[1:4, "Month"], c(1, 2, 2, 2), tolerance = 0.00)
  expect_equal(tes[1:4, "quasiWeek"], c(4, 1, 2, 3), tolerance = 0.00)
  expect_equal(tes[1:4, "rain.at.TS4"],
               c(223.1143, 217.4197, 207.0196, 203.8757), tolerance = 0.1)
})

daily.rain <- CampinasRain$Rain[1:10949]
test_that("TSaggreg() works as expected when rainfall records is short", {
  expect_message(
    expect_warning(
      tes <- TSaggreg(
        daily.rain = daily.rain,
        start.date = "1980-01-01",
        TS = 4
      ),
      "Less than 30 years of rainfall records. Longer periods are highly recommended."
    ),
    "Done. Just ensure the last quasi-week is complete.
  The last day of your series is 22 and TS is 4"
  )
  expect_s3_class(tes, c("TSaggreg", "matrix", "array"))
  expect_identical(dim(tes), c(1437L, 4L))
  expect_identical(colnames(tes), c("Year", "Month", "quasiWeek", "rain.at.TS4"))
  expect_equal(tes[1:4, "Year"], c(1980, 1980, 1980, 1980), tolerance = 0.00)
  expect_equal(tes[1:4, "Month"], c(1, 2, 2, 2), tolerance = 0.00)
  expect_equal(tes[1:4, "quasiWeek"], c(4, 1, 2, 3), tolerance = 0.00)
  expect_equal(tes[1:4, "rain.at.TS4"],
               c(223.1143, 217.4197, 207.0196, 203.8757), tolerance = 0.1)
})

daily.rain <- CampinasRain$Rain[1:100]
test_that("rainfall record is too short", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4),
  "Less than 10 years of rainfall records. We cannot proceed")
})

daily.rain <- CampinasRain$Rain
test_that("rainfall record with missing values", {
  daily.rain[1] <- NA
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4),
    "Physically impossible or missing rain values")
})
daily.rain <- CampinasRain$Rain
daily.rain[1] <- -10
test_that("rainfall record with negative values", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4),
    "Physically impossible or missing rain values")
})

daily.rain <- CampinasRain$Rain
daily.rain[1] <- "muita chuva"
test_that("rainfall record with non numerical values", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4),
    "Physically impossible or missing rain values")
})

daily.rain <- cbind(CampinasRain$Rain,CampinasRain$Rain)
test_that("rainfall record with wrong format", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4),
    "Physically impossible or missing rain values")
})
daily.rain <- CampinasRain$Rain
test_that("start.date with wrong format", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="hoje",TS=4),
    "`hoje` is not in a valid date format. Please enter a valid date format.")
})
daily.rain <- CampinasRain$Rain
test_that("start.date with another wrong format", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="91-01-01",TS=4),
    "`91-01-01` is not in a valid date format. Please enter a valid date format.")
})

daily.rain <- CampinasRain$Rain
test_that("TS not a number", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS="quatro"),
    "TS must be an integer between 1 and 96")
})

daily.rain <- CampinasRain$Rain
test_that("TS lower than 1", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=0),
    "TS must be an integer between 1 and 96")
})

daily.rain <- CampinasRain$Rain
test_that("TS larger than 96", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=97),
    "TS must be an integer between 1 and 96")
})

daily.rain <- CampinasRain$Rain
test_that("TS wrong format", {
  expect_error(
    tes <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=c(2,97)),
    "TS must be an integer between 1 and 96")
})
