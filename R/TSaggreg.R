#' Aggregates daily rainfall totals at quasi-week time scales
#'
#' @param daily.rain
#' Vector, 1-column matrix or data frame with daily rainfall totals.
#' @param start.date
#' Date at which the aggregation should start. Preferred formats are
#' \dQuote{YYYY-MM-DD}, \dQuote{YYYY/MM/DD} but most any valid date format
#' should work.
#' @param TS
#' Time scale on the quasiWeek basis (integer values between 1 and 96).
#'   Default is 4, which corresponds to the monthly time scale.
#' @returns
#' A matrix with rainfall amounts aggregated at the time scale selected by
#' the user
#' @examples
#'
#' daily.rain <- CampinasRain[,2]
#' rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4)
#' @importFrom lubridate year month day parse_date_time
#' @importFrom zoo rollsum
#' @importFrom stats na.omit
#' @autoglobal
#' @export

TSaggreg <- function(daily.rain, start.date, TS = 4L) {

  daily.rain <- as.matrix(daily.rain)
  if (!is.numeric(daily.rain) || anyNA(daily.rain) ||
      length(daily.rain[daily.rain < 0]) != 0 ||
      ncol(daily.rain) != 1) {
    stop("Physically impossible or missing rain values.")
  }
  if (!is.numeric(TS) || length(TS) != 1 ||
      TS < 1 ||
      TS > 96) {
    stop("TS must be an integer between 1 and 96.")
  }

  n <- length(daily.rain)
  if (n < 3650) {
    stop("Less than 10 years of rainfall records. We cannot proceed.")
  }
  if (n < 10950) {
    warning("Less than 30 years of rainfall records. Longer periods are highly recommended.")
  }
  start.cycle <- .check_date(start.date)
  end.cycle <- start.cycle + (n - 1)
  all.period <- seq(start.cycle, end.cycle, "days")
  years <- year(all.period)
  months <- month(all.period)
  days <- day(all.period)
  rain <- matrix(NA, n, 4)
  rain[, 1:4] <- c(years, months, days, daily.rain)
  a <- 1
  b <- 2
  c <- 3
  d <- 4
  data.week <- matrix(NA, n, 5)
  start.year <- rain[1,1]
  final.year <- rain[n,1]
  start.month <- rain[1,2]
  final.month <- rain[n,2]
  month <- start.month
  year <- start.year
  while (year <= final.year || month <= final.month) {
    data.week1 <- sum(rain[which(rain[,1] ==
                                   year &
                                   rain[,2] == month &
                                   rain[,3] <= 7),4])
    data.week2 <- sum(rain[which(rain[,1] ==
                                   year &
                                   rain[,2] == month &
                                   rain[,3] > 7 &
                                   rain[,3] <= 14), 4])
    data.week3 <- sum(rain[which(rain[,1] ==
                                   year &
                                   rain[,2] == month &
                                   rain[,3] > 14 &
                                   rain[,3] <= 21), 4])
    data.week4 <- sum(rain[which(rain[,1] ==
                                   year &
                                   rain[,2] == month &
                                   rain[,3] > 21),4])
    data.week[a, 1:4] <- c(year, month, 1, data.week1)
    data.week[b, 1:4] <- c(year, month, 2, data.week2)
    data.week[c, 1:4] <- c(year, month, 3, data.week3)
    data.week[d, 1:4] <- c(year, month, 4, data.week4)
    month <- month + 1
    if (year == final.year & month > final.month) {
      break
    }
    if (month > 12) {
      year <- year + 1
      month <- 1
    }
    a <- a + 4
    b <- b + 4
    c <- c + 4
    d <- d + 4
  }
  if (TS > 1){
    data.at.TS <- na.omit(rollsum(data.week[,4],TS))
    n.TS <- length(data.at.TS)
    data.week[TS:(n.TS+(TS-1)),5]<- data.at.TS
    data.week <- data.week[-c((n.TS+(TS)):n),]
  } else{
    data.week[,5] <- data.week[,4]
  }
  data.week <- data.week[,-c(4)]
  data.week <- na.omit(data.week)
  colnames(data.week) <- c("Year", "Month", "quasiWeek", paste0("rain.at.TS", TS))
  message("Done. Just ensure the last quasi-week is complete.
  The last day of your series is ", days[n], " and TS is ", TS)

  class(data.week) <- union("TSaggreg", class(data.week))

  return(data.week)
}

#' Check User Input Dates for Validity
#'
#' @param x User entered date value
#' @return Validated date string as a `Date` object.
#' @note This was taken from \CRANpkg{nasapower}, but tz changed to UTC.
#' @example .check_date(x)
#' @author Adam H. Sparks \email{adamhsparks@@gmail.com}
#' @keywords Internal
#' @noRd
.check_date <- function(x) {
  tryCatch(
    x <- parse_date_time(x,
                                    c(
                                      "Ymd",
                                      "dmY",
                                      "mdY",
                                      "BdY",
                                      "Bdy",
                                      "bdY",
                                      "bdy"
                                    ),
                                    tz = "UTC"),
    warning = function(c) {
      stop(
        call. = FALSE,
        "\n`",
        x,
        "` is not in a valid date format. Please enter a valid date format.",
        "\n"
      )
    }
  )
  return(as.Date(x))
}
