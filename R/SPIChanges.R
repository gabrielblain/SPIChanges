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
#'   and the changes in the frequency of the SPI values caused by the changes in rainfall patterns.}
#'   \item{model.selection}{The generalized additive model that best fits the rainfall series}
#'   \item{Changes.Freq.Drought}{changes in the frequency of moderate, severe and extreme drought events,
#'   as definied by the SPI classification system, caused by the changes in rainfall patterns.}
#'   \item{Model.Drought}{Year to year changes in the expected frequency of moderate, severe and extreme drought events.}
#'  }
#' @export
#' @examples
#'
#' daily.rain <- CampinasRain[,2]
#' rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4)
#' Changes.in.the.SPI <- SPIChanges(rain.at.TS=rainTS4, only.linear = "yes")
#' @importFrom gamlss gamlss
#' @importFrom gamlss.dist GA pGA qGA BI
#' @importFrom splines2 nsp
#' @importFrom stats qnorm AIC fitted
#' @importFrom spsUtil quiet
#' @importFrom dplyr bind_rows
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
   Changes.Freq.Drought <- matrix(NA,48,3)
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
   probzero.st <- calc.probzero.st(nz, n.week)
   if (probzero.st > 0.067) {
     warning("rainfall series Month ", paste(month), " Week ", paste(week),
             " has more than ", paste(0.067*100), "% of zeros. In this situation
             the SPI cannot assume values lower than -1.5")} else if (probzero.st > 0.159) {
     warning("rainfall series Month ", paste(month), " Week ", paste(week),
             " has more than ", paste(0.159*100), "% of zeros. In this situation
             the SPI cannot assume values lower than -1")} else if (probzero.st > 0.50) {
     warning("rainfall series Month ", paste(month), " Week ", paste(week),
             " has more than ", paste(0.5*100), "% of zeros. In this situation
             the SPI cannot assume values lower than 0")
             }
   id <- which(rain.week>0)
   time.nonzero <- as.vector(time[id])
   n.time.nonzero <- length(time.nonzero)
   Model.Drought.week <- data.frame(matrix(NA,n.time.nonzero,5))
   t.gam <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1,family=GA, mu.link = "identity",
                                          sigma.link ="log"))
   if (only.linear == "yes"){
     t.gam.ns10 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1),family=GA,
                                                 mu.link = "identity", sigma.link ="log"))
     t.gam.ns01 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1, sigma.formula
                                                 =~poly(time.nonzero,1), family=GA, mu.link = "identity",
                                                 sigma.link ="log"))
     t.gam.ns11 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1), sigma.formula
                                                 =~poly(time.nonzero,1), family=GA, mu.link = "identity", sigma.link ="log"))
     model.selection[a,1] <- which.min(c(AIC(t.gam, k=4),
                                         AIC(t.gam.ns10, k=4),
                                         AIC(t.gam.ns01, k=4),
                                         AIC(t.gam.ns11, k=4)))
   } else {
     t.gam.ns10 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1),family=GA,
                                                 mu.link = "identity", sigma.link ="log"))
     t.gam.ns01 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1, sigma.formula=~poly(time.nonzero,1),
                                                 family=GA, mu.link = "identity",sigma.link ="log"))
     t.gam.ns11 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1), sigma.formula=~poly(time.nonzero,1),
                                                 family=GA, mu.link = "identity", sigma.link ="log"))
     t.gam.ns20 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~nsp(time.nonzero, df = 2),family=GA, mu.link = "identity",
                                                 sigma.link ="log"))
     t.gam.ns21 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~nsp(time.nonzero, df = 2), sigma.formula=~poly(time.nonzero,1),family=GA,
                                                 mu.link = "identity", sigma.link ="log"))
     model.selection[a,1] <- which.min(c(AIC(t.gam, k=4),
                                         AIC(t.gam.ns10, k=4),
                                         AIC(t.gam.ns01, k=4),
                                         AIC(t.gam.ns11, k=4),
                                         AIC(t.gam.ns20, k=4),
                                         AIC(t.gam.ns21, k=4)))}

   quasiprob <- (probzero.st+(1-probzero.st)*pGA(rain.week,mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1],lower.tail = TRUE, log.p=FALSE))
   quasiprob[quasiprob < 0.001351] <- 0.001351
   quasiprob[quasiprob > 0.998649] <- 0.998649
   data.week[initial.row:last.row,6] <- quasiprob
   stat.rain.drought.mod <- qGA(0.159, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
   stat.rain.drought.sev <- qGA(0.067, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
   stat.rain.drought.extr <- qGA(0.023, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
   if (model.selection[a,1]==1){
     quasiprob.ns <- as.matrix(data.week[initial.row:last.row,6])
     Changes.Freq.Drought [a,] <- 0
     Model.Drought.week[,3] <- as.matrix((probzero.st+(1-probzero.st)*pGA(stat.rain.drought.mod, mu = t.gam$mu.fv, sigma = t.gam$sigma.fv)))
     Model.Drought.week[,4] <- as.matrix((probzero.st+(1-probzero.st)*pGA(stat.rain.drought.sev, mu = t.gam$mu.fv, sigma = t.gam$sigma.fv)))
     Model.Drought.week[,5] <- as.matrix((probzero.st+(1-probzero.st)*pGA(stat.rain.drought.extr, mu = t.gam$mu.fv, sigma = t.gam$sigma.fv)))
   } else if (model.selection[a,1]==2){
     probzero <- calc.probzero(rain.week, time)
     probzero[probzero < 0.0001] <- 0
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns10$mu.fv, sigma = t.gam.ns10$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.023)
     Model.Drought.week[,3] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.mod, mu = t.gam.ns10$mu.fv, sigma = t.gam.ns10$sigma.fv)))
     Model.Drought.week[,4] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.sev, mu = t.gam.ns10$mu.fv, sigma = t.gam.ns10$sigma.fv)))
     Model.Drought.week[,5] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.extr, mu = t.gam.ns10$mu.fv, sigma = t.gam.ns10$sigma.fv)))
     } else if (model.selection[a,1]==3){
       probzero <- calc.probzero(rain.week, time)
       probzero[probzero < 0.0001] <- 0
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns01$mu.fv, sigma = t.gam.ns01$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.023)
     Model.Drought.week[,3] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.mod, mu = t.gam.ns01$mu.fv, sigma = t.gam.ns01$sigma.fv)))
     Model.Drought.week[,4] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.sev, mu = t.gam.ns01$mu.fv, sigma = t.gam.ns01$sigma.fv)))
     Model.Drought.week[,5] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.extr, mu = t.gam.ns01$mu.fv, sigma = t.gam.ns01$sigma.fv)))
     } else if (model.selection[a,1]==4){
       probzero <- calc.probzero(rain.week, time)
       probzero[probzero < 0.0001] <- 0
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns11$mu.fv, sigma = t.gam.ns11$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.023)
     Model.Drought.week[,3] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.mod, mu = t.gam.ns11$mu.fv, sigma = t.gam.ns11$sigma.fv)))
     Model.Drought.week[,4] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.sev, mu = t.gam.ns11$mu.fv, sigma = t.gam.ns11$sigma.fv)))
     Model.Drought.week[,5] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.extr, mu = t.gam.ns11$mu.fv, sigma = t.gam.ns11$sigma.fv)))
     } else if (model.selection[a,1]==5){
       probzero <- calc.probzero(rain.week, time)
       probzero[probzero < 0.0001] <- 0
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns20$mu.fv, sigma = t.gam.ns20$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.023)
     Model.Drought.week[,3] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.mod, mu = t.gam.ns20$mu.fv, sigma = t.gam.ns20$sigma.fv)))
     Model.Drought.week[,4] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.sev, mu = t.gam.ns20$mu.fv, sigma = t.gam.ns20$sigma.fv)))
     Model.Drought.week[,5] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.extr, mu = t.gam.ns20$mu.fv, sigma = t.gam.ns20$sigma.fv)))
     } else {
       probzero <- calc.probzero(rain.week, time)
       probzero[probzero < 0.0001] <- 0
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns21$mu.fv, sigma = t.gam.ns21$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.023)
     Model.Drought.week[,3] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.mod, mu = t.gam.ns21$mu.fv, sigma = t.gam.ns21$sigma.fv)))
     Model.Drought.week[,4] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.sev, mu = t.gam.ns21$mu.fv, sigma = t.gam.ns21$sigma.fv)))
     Model.Drought.week[,5] <- as.matrix((probzero[time.nonzero]+(1-probzero[time.nonzero])*pGA(stat.rain.drought.extr, mu = t.gam.ns21$mu.fv, sigma = t.gam.ns21$sigma.fv)))
     }

   Model.Drought.week[,1] <- rep(month,n.time.nonzero)
   Model.Drought.week[,2] <- rep(week,n.time.nonzero)
   quasiprob.ns[quasiprob.ns < 0.001351] <- 0.001351
   quasiprob.ns[quasiprob.ns > 0.998649] <- 0.998649
   data.week[initial.row:last.row,7] <- quasiprob.ns
   ifelse (a == 1, Model.Drought <- as.data.frame(Model.Drought.week), Model.Drought <- bind_rows(Model.Drought,Model.Drought.week))
   week <- week+1
   if (week==5)
   {month <- month+1
   week <- 1}
}
   Model.Drought[,3:5] <- 100*Model.Drought[,3:5]
   data.week[,5] <- c(qnorm(data.week[,6], mean = 0, sd = 1))
   dry.values <- which(data.week[,5] <= 0)
   wet.values <- which(data.week[,5] > 0)
   data.week[dry.values,8] <- data.week[dry.values,7]-data.week[dry.values,6]
   data.week[wet.values,8] <- data.week[wet.values,6]-data.week[wet.values,7]
   data.week[wet.values,8] <- 100*data.week[wet.values,8]
   data.week <- data.week[order(data.week[,1]),]
   colnames(data.week) <- c("Year","Month","quasiWeek","rain.at.TS",
                            "SPI","Exp.Acum.Prob","Actual.Acum.Prob","ChangeFreq")
   data.week <- round(data.week,3)
   Model.Drought <- round(Model.Drought,3)
   months <- sort(rep(seq(1:12),4))
   quasiweeks <- rep(seq(1:4),12)
   Changes.Freq.Drought <- cbind(months, quasiweeks, round(Changes.Freq.Drought,3))
   colnames(Changes.Freq.Drought) <- c("Month","quasiWeek","Moderate","Severe","Extreme")
   model.selection <- cbind(months, quasiweeks, model.selection)
   colnames(model.selection) <- c("Month","quasiWeek","model")
   colnames(Model.Drought) <- c("Month","quasiWeek", "Moderate", "Severe", "Extreme")
   Drought_Changes <- list(data.week, model.selection,Changes.Freq.Drought,Model.Drought)
   Drought_Changes <- list(data.week = data.week,
                           model.selection = model.selection,
                           Changes.Freq.Drought=Changes.Freq.Drought,
                           Model.Drought=Model.Drought)
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

 calc.probzero.st <- function(nz, n.week) {
   ifelse(nz == 0, (nz /(n.week+1))/2, nz /(n.week+1))
 }
