#' Detect trends and quantify their effect on the probability of SPI values occurring
#'
#' @param rain.at.TS
#' A 4-column matrix or data frame.
#' 1st column is years, 2nd is the months (1 to 12)
#' 3rd is the quasiWeeks (1 to 4), and 4th is the rainfall totals accumulated at a time scale.
#' @param only.linear
#' A character variable (\code{Yes} or \code{No}) defining if the function must
#' consider only linear models (\code{Yes}) or linear and non-linear models (\code{No}).
#' Default is Yes.
#' @return
#' A \code{list} object with:
#' \describe{
#'   \item{data.week}{The Rainfall amounts, SPI, cumulative probability of the SPI values under the stationary
#'   approach, cumulative probability of the SPI values under the non-stationary approach,
#'   and the changes in the frequency of below zero SPI values caused by the changes in rainfall patterns.}
#'   \item{model.selection}{The generalized additive model that best fits the rainfall series}
#'   \item{Changes.Freq.Drought}{changes in the frequency of zero precipitation, moderate to extreme,
#'   severe to extreme and extreme drought events,as categorized by the SPI classification system,
#'   caused by the changes in rainfall patterns.
#'   Changes in the precipitation amounts associated describing normal conditions is also shown.}
#'   \item{Statistics}{Year to year changes in the expected frequency of moderate to extreme, severe to extreme
#'   and extreme drought events.}
#'  }
#' @export
#' @examples
#'
#' daily.rain <- CampinasRain[,2]
#' rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4)
#' Changes.in.the.SPI <- SPIChanges(rain.at.TS=rainTS4, only.linear = "yes")
#' @importFrom gamlss gamlss GAIC
#' @importFrom gamlss.dist GA pGA qGA BI
#' @importFrom stats qnorm fitted
#' @importFrom spsUtil quiet
#' @importFrom dplyr bind_rows
#' @importFrom MuMIn AICc
SPIChanges <- function(rain.at.TS, only.linear = "Yes"){
  rain.at.TS <- as.matrix(rain.at.TS)
  if (!is.numeric(rain.at.TS) || any(is.na(rain.at.TS)) ||
      length(rain.at.TS[rain.at.TS < 0]) != 0 || ncol(rain.at.TS) != 4 ) {
    stop("Physically impossible or missing values in rain.at.TS.")}
  n <- length(rain.at.TS[,1])
  if (n<480){stop("Less than 10 years of rainfall records. We cannot procede.")}
  if (n<1440){warning("Less than 30 years of rainfall records. Longer periods are highly recommended.")}
  if (length(rain.at.TS[rain.at.TS[,2] < 1]) != 0 ||
      length(rain.at.TS[rain.at.TS[,2] > 12]) != 0 ||
      length(rain.at.TS[rain.at.TS[,2] == 1]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 2]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 3]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 4]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 5]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 6]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 7]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 8]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 9]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 10]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 11]) < 32 ||
      length(rain.at.TS[rain.at.TS[,2] == 12]) < 32) {
    stop("Column Month in rain.at.TS is probably malformed.")}
  if (length(rain.at.TS[rain.at.TS[,3] < 1]) != 0 ||
      length(rain.at.TS[rain.at.TS[,3] > 4]) != 0 ||
      length(rain.at.TS[rain.at.TS[,3] == 1]) < 96 ||
      length(rain.at.TS[rain.at.TS[,3] == 2]) < 96 ||
      length(rain.at.TS[rain.at.TS[,3] == 3]) < 96 ||
      length(rain.at.TS[rain.at.TS[,3] == 4]) < 96) {
    stop("Column quasiWeek in rain.at.TS is probably malformed.")}
  only.linear <- tolower(only.linear)
  if (only.linear != "yes" & only.linear != "no"){stop("imput only.linear should be Yes or No.")}
  years <- rain.at.TS[,1]
  months <- rain.at.TS[,2]
  data.week <- data.frame(matrix(NA, n, 8))
  data.week[,1:4] <- rain.at.TS
  data.week <- data.week[order(data.week[,2],data.week[,3]),]
  month <- 1
  week <- 1
  model.selection <- matrix(NA,48,1)
  Changes.Freq.Drought <- matrix(NA,48,7)
  for (a in 1:48){
    rain.week <- (data.week[which(data.week[,2]==month & data.week[,3]==week),4])
    n.week <- length(rain.week)
    rain.week.nozeros <- (rain.week[rain.week>0])
    if (a > 1){
      initial.row <- last.row+1
      last.row <- initial.row+n.week-1} else{
        initial.row <- 1
        last.row <- n.week
      }
    nz <- n.week - length(rain.week.nozeros)
    time <- as.matrix(seq(1:n.week))
    probzero.st <- calc.probzero.st(rain.week)
    if (probzero.st[1] >= 0.50) {
      warning("rainfall series Month ", paste(month), " Week ", paste(week),
              " has more than 50% of zeros. In this situation
             the SPI cannot assume values lower than 0")} else if (probzero.st[1] > 0.159) {
               warning("rainfall series Month ", paste(month), " Week ", paste(week),
                       " has more than 15.9% of zeros. In this situation
             the SPI cannot assume values lower than -1")} else if (probzero.st[1] > 0.067) {
               warning("rainfall series Month ", paste(month), " Week ", paste(week),
              " has more than 6.7% of zeros. In this situation
             the SPI cannot assume values lower than -1.5")}
    id <- which(rain.week>0)
    time.nonzero <- as.vector(time[id])
    n.time.nonzero <- length(time.nonzero)
    Model.Drought.week <- data.frame(matrix(NA,n.time.nonzero,5))
    t.gam <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1,family=GA, mu.link = "log",
                                           sigma.link ="log"))
    if (only.linear == "yes"){
      models <- Fit.lineares(rain.week.nozeros, time.nonzero)
      model.selection[a,1] <- models$best
      selected.model <- models$selected.model
    } else {
      models <- Fit.Nonlineares(rain.week.nozeros, time.nonzero)
      model.selection[a,1] <- models$best
      selected.model <- models$selected.model
    }
    quasiprob <- (probzero.st[1]+(1-probzero.st[1])*pGA(rain.week,mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1],lower.tail = TRUE, log.p=FALSE))
    quasiprob[quasiprob < 0.001351] <- 0.001351
    quasiprob[quasiprob > 0.998649] <- 0.998649
    data.week[initial.row:last.row,6] <- quasiprob
    normal.param.prob <- ifelse(probzero.st[1]<0.5,0.5-probzero.st[1],0)
    mod.param.prob <- ifelse(probzero.st[1]<0.159,0.159-probzero.st[1],0)
    sev.param.prob <- ifelse(probzero.st[1]<0.067,0.067-probzero.st[1],0)
    ext.param.prob <- ifelse(probzero.st[1]<0.023,0.023-probzero.st[1],0)
    stat.rain.normal <- qGA(normal.param.prob, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
    stat.rain.drought.mod <- qGA(mod.param.prob , mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
    stat.rain.drought.sev <- qGA(sev.param.prob, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
    stat.rain.drought.extr <- qGA(ext.param.prob, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
    if (model.selection[a,1]==1){
      quasiprob.ns <- as.matrix(data.week[initial.row:last.row,6])
      Changes.Freq.Drought [a,1] <- round((probzero.st[1]),3)
      Changes.Freq.Drought [a,2] <- round((probzero.st[n.week]),3)
      Changes.Freq.Drought [a,3] <- round(stat.rain.normal,2)
      Changes.Freq.Drought [a,4] <- Changes.Freq.Drought [a,3]
      Changes.Freq.Drought [a,5] <- 0
      Changes.Freq.Drought [a,6] <- 0
      Changes.Freq.Drought [a,7] <- 0
      Model.Drought.week[,3] <- probzero.st[1]
      Model.Drought.week[,4] <- t.gam$mu.fv
      Model.Drought.week[,5] <- t.gam$sigma.fv
    } else {
      probzero <- calc.probzero(rain.week, time)
      probzero[probzero < 0.0001] <- 0
      quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = selected.model$mu.fv, sigma = selected.model$sigma.fv))
      Changes.Freq.Drought [a,1] <- round((probzero.st[1]),3)
      Changes.Freq.Drought [a,2] <- round((probzero[n.week]),3)
      Changes.Freq.Drought [a,3] <- round(stat.rain.normal,2)
      nonstat.normal.param.prob <- ifelse(probzero[n.week]<0.5,0.5-probzero[n.week],0)
      Changes.Freq.Drought [a,4] <- round(qGA(nonstat.normal.param.prob , mu = selected.model$mu.fv[(n.week-nz)],
                                              sigma = selected.model$sigma.fv[(n.week-nz)]),2)
      Changes.Freq.Drought [a,5] <- round(ifelse(probzero[n.week] >= 0.159, 100*(probzero[n.week]-probzero[1]),
                                                 100*((probzero[n.week]+(1-probzero[n.week])*
                                                         pGA(stat.rain.drought.mod,
                                                             mu = selected.model$mu.fv[(n.week-nz)],
                                                             sigma = selected.model$sigma.fv[(n.week-nz)]))-0.159)),2)

      Changes.Freq.Drought [a,6] <- round(ifelse(probzero[n.week] >= 0.067, 100*(probzero[n.week]-probzero[1]),
                                                 100*((probzero[n.week]+(1-probzero[n.week])*
                                                         pGA(stat.rain.drought.sev,
                                                             mu = selected.model$mu.fv[(n.week-nz)],
                                                             sigma = selected.model$sigma.fv[(n.week-nz)]))-0.067)),2)

      Changes.Freq.Drought [a,7] <- round(ifelse(probzero[n.week] >= 0.023, 100*(probzero[n.week]-probzero[1]),
                                                 100*((probzero[n.week]+(1-probzero[n.week])*
                                                         pGA(stat.rain.drought.extr,
                                                             mu = selected.model$mu.fv[(n.week-nz)],
                                                             sigma = selected.model$sigma.fv[(n.week-nz)]))-0.023)),2)
      Model.Drought.week[,3] <- probzero[time.nonzero]
      Model.Drought.week[,4] <- selected.model$mu.fv
      Model.Drought.week[,5] <- selected.model$sigma.fv
    }

    Model.Drought.week[,1] <- rep(month,n.time.nonzero)
    Model.Drought.week[,2] <- rep(week,n.time.nonzero)
    quasiprob.ns[quasiprob.ns < 0.001351] <- 0.001351
    quasiprob.ns[quasiprob.ns > 0.998649] <- 0.998649
    data.week[initial.row:last.row,7] <- quasiprob.ns
    ifelse (a == 1, Statistics <- as.data.frame(Model.Drought.week), Statistics <- spsUtil::quiet(bind_rows(Statistics,Model.Drought.week)))
    week <- week+1
    if (week==5)
    {month <- month+1
    week <- 1}
  }
  Statistics[,3:5] <- Statistics[,3:5]
  data.week[,5] <- c(qnorm(data.week[,6], mean = 0, sd = 1))
  dry.values <- which(data.week[,5] <= 0)
  wet.values <- which(data.week[,5] > 0)
  data.week[dry.values,8] <- 100*round(data.week[dry.values,7]-data.week[dry.values,6],3)
  data.week[wet.values,8] <- "NoDrought"
  data.week <- data.week[order(data.week[,1]),]
  colnames(data.week) <- c("Year","Month","quasiWeek","rain.at.TS",
                           "SPI","Exp.Acum.Prob","Actual.Acum.Prob","ChangeDryFreq")
  data.week[,5:7] <- round(data.week[,5:7],3)
  Statistics <- round(Statistics,3)
  months <- sort(rep(seq(1:12),4))
  quasiweeks <- rep(seq(1:4),12)
  Changes.Freq.Drought <- cbind(months, quasiweeks, Changes.Freq.Drought)
  colnames(Changes.Freq.Drought) <- c("Month","quasiWeek","StatProbZero","NonStatProbZero","StatNormalRain","NonStatNormalRain", "ChangeMod","ChangeSev","ChangeExt")
  model.selection <- cbind(months, quasiweeks, model.selection)
  colnames(model.selection) <- c("Month","quasiWeek","model")
  colnames(Statistics) <- c("Month","quasiWeek", "ProbZero", "mu", "sigma")
  Drought_Changes <- list(data.week, model.selection,Changes.Freq.Drought,Statistics)
  Drought_Changes <- list(data.week = data.week,
                          model.selection = model.selection,
                          Changes.Freq.Drought=Changes.Freq.Drought,
                          Statistics=Statistics)
  return(Drought_Changes)
}

#' Calculate probzero, the Probability of Zero Rain under non-stationary assumption
#'
#' @param rain.week data vector
#' @param time data vector
#' @author Gabriel C. Blain \email{gabrielblain@@gmail.com}
#' @noRd
#' @keywords Internal

calc.probzero <- function(rain.week,time) {
  zero_rain <- ifelse(rain.week == 0, 1, 0)
  modelo <- spsUtil::quiet(gamlss(zero_rain~poly(time,1), family = BI))
  prob_zero_rain <- fitted(modelo, "mu")
  return(prob_zero_rain)
}

#' Calculate probzero the Probability of Zero Rain under stationary assumption
#'
#' @param n.z numeric value
#' @param n.rain numeric value
#' @note This was adapted from \CRANpkg{PowerSDI} by changing breaks values.
#' @author Adam H. Sparks \email{adamhsparks@@gmail.com}
#' @author Gabriel C. Blain \email{gabrielblain@@gmail.com}
#' @noRd
#' @keywords Internal

calc.probzero.st <- function(rain.week) {
  zero_rain <- ifelse(rain.week == 0, 1, 0)
  modelo <- spsUtil::quiet(gamlss(zero_rain~1, family = BI))
  prob_zero_rain <- fitted(modelo, "mu")
  return(prob_zero_rain)
}

#' Fit the linear models
#'
#' @param rain.week.nozeros vector of positive rain
#' @param time.nonzero time vector
#' @note This was adapted from \CRANpkg{gamlss}.
#' @noRd
#' @keywords Internal

Fit.lineares <- function (rain.week.nozeros,time.nonzero){
  t.gam <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ 1, family = GA,
                                         mu.link = "log", sigma.link = "log"))
  t.gam.ns10 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~time.nonzero,family=GA,
                                              mu.link = "log", sigma.link ="log"))
  t.gam.ns01 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1, sigma.formula
                                              =~time.nonzero, family=GA, mu.link = "log",
                                              sigma.link ="log"))
  t.gam.ns11 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~time.nonzero, sigma.formula
                                              =~time.nonzero, family=GA, mu.link = "log", sigma.link ="log"))

  best <- which.min(list(
    MuMIn::AICc(t.gam, k = 4.4),
    MuMIn::AICc(t.gam.ns10, k = 4.4),
    MuMIn::AICc(t.gam.ns01, k = 4.4),
    MuMIn::AICc(t.gam.ns11, k = 4.4)))
  if (best == 1){selected.model <- t.gam} else if (best == 2) {
    selected.model <- t.gam.ns10} else if (best == 3) {
      selected.model <- t.gam.ns01} else {selected.model <- t.gam.ns11}
  return(list(selected.model=selected.model,best=best))
}


#' Fit the Nonlinear models
#'
#' @param rain.week.nozeros vector of positive rain
#' @param time.nonzero time vector
#' @note This was adapted from \CRANpkg{gamlss}.
#' @noRd
#' @keywords Internal
Fit.Nonlineares <- function (rain.week.nozeros,time.nonzero){
  t.gam <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ 1, family = GA,
                                         mu.link = "log", sigma.link = "log"))
  t.gam.ns10 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero, family = GA,
                                              mu.link = "log", sigma.link = "log"))
  t.gam.ns01 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ 1, sigma.formula = ~ time.nonzero,
                                              family = GA, mu.link = "log", sigma.link = "log"))
  t.gam.ns11 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero, sigma.formula = ~ time.nonzero,
                                              family = GA, mu.link = "log", sigma.link = "log"))
  t.gam.ns20 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero + I(time.nonzero^2), family = GA, mu.link = "log",
                                              sigma.link = "log"))
  t.gam.ns02 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ 1, family = GA, mu.link = "log",
                                              sigma.formula = ~ time.nonzero + I(time.nonzero^2), sigma.link = "log"))
  t.gam.ns21 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero + I(time.nonzero^2), sigma.formula = ~ time.nonzero, family = GA,
                                              mu.link = "log", sigma.link = "log"))
  t.gam.ns12 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero, sigma.formula = ~ time.nonzero + I(time.nonzero^2), family = GA,
                                              mu.link = "log", sigma.link = "log"))
  t.gam.ns22 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero + I(time.nonzero^2), sigma.formula = ~ time.nonzero + I(time.nonzero^2), family = GA,
                                              mu.link = "log", sigma.link = "log"))
  t.gam.ns30 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero + I(time.nonzero^2) + I(time.nonzero^3), family = GA, mu.link = "log",
                                              sigma.link = "log"))
  t.gam.ns03 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ 1, family = GA, mu.link = "log",
                                              sigma.formula = ~ time.nonzero + I(time.nonzero^2) + I(time.nonzero^3), sigma.link = "log"))
  t.gam.ns31 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero + I(time.nonzero^2) + I(time.nonzero^3), sigma.formula = ~ time.nonzero, family = GA,
                                              mu.link = "log", sigma.link = "log"))
  t.gam.ns13 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero, sigma.formula = ~ time.nonzero + I(time.nonzero^2) + I(time.nonzero^3), family = GA,
                                              mu.link = "log", sigma.link = "log"))
  t.gam.ns32 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero + I(time.nonzero^2) + I(time.nonzero^3), sigma.formula = ~ time.nonzero + I(time.nonzero^2), family = GA,
                                              mu.link = "log", sigma.link = "log"))
  t.gam.ns23 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero + I(time.nonzero^2), sigma.formula = ~ time.nonzero + I(time.nonzero^2) + I(time.nonzero^3), family = GA,
                                              mu.link = "log", sigma.link = "log"))
  t.gam.ns33 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros ~ time.nonzero + I(time.nonzero^2) + I(time.nonzero^3), sigma.formula = ~ time.nonzero + I(time.nonzero^2) + I(time.nonzero^3), family = GA,
                                              mu.link = "log", sigma.link = "log"))
  best <- which.min(list(
    MuMIn::AICc(t.gam, k = 4.4),
    MuMIn::AICc(t.gam.ns10, k = 4.4),
    MuMIn::AICc(t.gam.ns01, k = 4.4),
    MuMIn::AICc(t.gam.ns11, k = 4.4),
    MuMIn::AICc(t.gam.ns20, k = 4.4),
    MuMIn::AICc(t.gam.ns02, k = 4.4),
    MuMIn::AICc(t.gam.ns21, k = 4.4),
    MuMIn::AICc(t.gam.ns12, k = 4.4),
    MuMIn::AICc(t.gam.ns22, k = 4.4),
    MuMIn::AICc(t.gam.ns30, k = 4.4),
    MuMIn::AICc(t.gam.ns03, k = 4.4),
    MuMIn::AICc(t.gam.ns31, k = 4.4),
    MuMIn::AICc(t.gam.ns13, k = 4.4),
    MuMIn::AICc(t.gam.ns32, k = 4.4),
    MuMIn::AICc(t.gam.ns23, k = 4.4),
    MuMIn::AICc(t.gam.ns33, k = 4.4)
  ))

  if (best == 1){selected.model <- t.gam} else if (best == 2) {
    selected.model <- t.gam.ns10} else if (best == 3) {
      selected.model <- t.gam.ns01} else if (best == 4) {
        selected.model <- t.gam.ns11} else if (best == 5) {
          selected.model <- t.gam.ns20} else if (best == 6) {
            selected.model <- t.gam.ns02} else if (best == 7) {
              selected.model <- t.gam.ns21} else if (best == 8) {
                selected.model <- t.gam.ns12} else if (best == 9) {
                  selected.model <- t.gam.ns22} else if (best == 10) {
                    selected.model <- t.gam.ns30} else if (best == 11) {
                      selected.model <- t.gam.ns03} else if (best == 12) {
                        selected.model <- t.gam.ns31} else if (best == 13) {
                          selected.model <- t.gam.ns13} else if (best == 14) {
                            selected.model <- t.gam.ns32} else if (best == 15) {
                              selected.model <- t.gam.ns23} else {selected.model <- t.gam.ns33}
  return(list(selected.model=selected.model,best=best))
}
