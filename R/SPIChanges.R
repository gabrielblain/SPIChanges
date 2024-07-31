#' Detect trends and quantify their effect on the probability of SPI values occurring
#'
#' @param rain.at.TS
#' Vector, 1-column matrix or data frame with rainfall totals accumulated at a time scale.
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
#'  }
#' @export
#' @examples
#'
#' daily.rain <- CampinasRain[,2]
#' rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1991-01-01",TS=4)
#' Changes.in.the.SPI <- SPIChanges(rain.at.TS=rainTS4, only.linear = "yes")
#' @importFrom gamlss gamlss
#' @importFrom gamlss.dist GA pGA qGA
#' @importFrom splines2 nsp
#' @importFrom stats qnorm AIC glm binomial predict
#' @importFrom spsUtil quiet
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
   a <- 1
   model.selection <- matrix(NA,48,1)
   Changes.Freq.Drought <- matrix(NA,48,3)
   rain.week <- (data.week[which(data.week[,2]==month & data.week[,3]==week),4])
   n.week <- length(rain.week)
   rain.week.nozeros <- (rain.week[rain.week>0])
   initial.row <- 1
   last.row <- n.week
   nz <- n.week - length(rain.week.nozeros)
   time=as.matrix(seq(1:n.week))
   probzero <- calc.probzero(rain.week, time)
   probzero[probzero < 0.0001] <- 0
   probzero.st <- calc.probzero.st(nz, n.week)
   id=which(rain.week>0)
   time.nonzero=as.vector(time[id])
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
     model.selection[a,1] <- which.min(c(AIC(t.gam, k=2, c=TRUE),
                                         AIC(t.gam.ns10, k=2, c=TRUE),
                                         AIC(t.gam.ns01, k=2, c=TRUE),
                                         AIC(t.gam.ns11, k=2, c=TRUE)))
   } else {
     t.gam.ns10 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1),family=GA,
                                                 mu.link = "identity", sigma.link ="log"))
     t.gam.ns01 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1, sigma.formula=~poly(time.nonzero,1),
                                                 family=GA, mu.link = "identity",sigma.link ="log"))
     t.gam.ns11 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1), sigma.formula=~poly(time.nonzero,1),
                                                 family=GA, mu.link = "identity", sigma.link ="log"))
     t.gam.ns20 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~nsp(time.nonzero, df = 2),family=GA, mu.link = "identity",
                                                 sigma.link ="log"))
     t.gam.ns02 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1,sigma.formula=~nsp(time.nonzero, df = 2), family=GA, mu.link = "identity",
                                                 sigma.link ="log"))
     t.gam.ns22 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~nsp(time.nonzero, df = 2), sigma.formula=~nsp(time.nonzero, df = 2),family=GA,
                                                 mu.link = "identity", sigma.link ="log"))
     t.gam.ns21 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~nsp(time.nonzero, df = 2), sigma.formula=~poly(time.nonzero,1),family=GA,
                                                 mu.link = "identity", sigma.link ="log"))
     t.gam.ns12 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1), sigma.formula=~nsp(time.nonzero, df = 2),family=GA,
                                                 mu.link = "identity", sigma.link ="log"))
     model.selection[a,1] <- which.min(c(AIC(t.gam, k=2, c=TRUE),
                                         AIC(t.gam.ns10, k=2, c=TRUE),
                                         AIC(t.gam.ns01, k=2, c=TRUE),
                                         AIC(t.gam.ns11, k=2, c=TRUE),
                                         AIC(t.gam.ns20, k=2, c=TRUE),
                                         AIC(t.gam.ns02, k=2, c=TRUE),
                                         AIC(t.gam.ns21, k=2, c=TRUE),
                                         AIC(t.gam.ns12, k=2, c=TRUE),
                                         AIC(t.gam.ns22, k=2, c=TRUE)))}

   quasiprob <- (probzero.st+(1-probzero)*pGA(rain.week,mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1],lower.tail = TRUE, log.p=FALSE))
   quasiprob[which(rain.week==0)] <- (nz+1)/(2*n.week)
   data.week[initial.row:last.row,6] <- quasiprob
   stat.rain.drought.mod <- qGA(0.159, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
   stat.rain.drought.sev <- qGA(0.067, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
   stat.rain.drought.extr <- qGA(0.023, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
   if (model.selection[a,1]==1){
     quasiprob.ns <- as.matrix(data.week[initial.row:last.row,6])
     Changes.Freq.Drought [a,] <- 0
   }
   if (model.selection[a,1]==2){
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns10$mu.fv, sigma = t.gam.ns10$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.023)
   }
   if (model.selection[a,1]==3){
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns01$mu.fv, sigma = t.gam.ns01$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.023)
   }
   if (model.selection[a,1]==4){
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns11$mu.fv, sigma = t.gam.ns11$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.023)
   }
   if (model.selection[a,1]==5){
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns20$mu.fv, sigma = t.gam.ns20$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.023)
   }
   if (model.selection[a,1]==6){
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns02$mu.fv, sigma = t.gam.ns02$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns02$mu.fv[(n.week-nz)], sigma = t.gam.ns02$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns02$mu.fv[(n.week-nz)], sigma = t.gam.ns02$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns02$mu.fv[(n.week-nz)], sigma = t.gam.ns02$sigma.fv[(n.week-nz)]))-0.023)
   }
   if (model.selection[a,1]==7){
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns21$mu.fv, sigma = t.gam.ns21$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.023)
   }
   if (model.selection[a,1]==8){
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns12$mu.fv, sigma = t.gam.ns12$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns12$mu.fv[(n.week-nz)], sigma = t.gam.ns12$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns12$mu.fv[(n.week-nz)], sigma = t.gam.ns12$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns12$mu.fv[(n.week-nz)], sigma = t.gam.ns12$sigma.fv[(n.week-nz)]))-0.023)
   }
   if (model.selection[a,1]==9){
     quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns22$mu.fv, sigma = t.gam.ns22$sigma.fv))
     Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.mod,mu = t.gam.ns22$mu.fv[(n.week-nz)], sigma = t.gam.ns22$sigma.fv[(n.week-nz)]))-0.159)
     Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.sev,mu = t.gam.ns22$mu.fv[(n.week-nz)], sigma = t.gam.ns22$sigma.fv[(n.week-nz)]))-0.067)
     Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                           pGA(stat.rain.drought.extr,mu = t.gam.ns22$mu.fv[(n.week-nz)], sigma = t.gam.ns22$sigma.fv[(n.week-nz)]))-0.023)
   }
   data.week[initial.row:last.row,7] <- quasiprob.ns

   while (a <=47) {
     week <- week+1
     a <- a+1
     if (week==5)
     {month <- month+1
     week <- 1}
     rain.week <- (data.week[which(data.week[,2]==month & data.week[,3]==week),4])
     n.week <- length(rain.week)
     rain.week.nozeros <- (rain.week[rain.week>0])
     initial.row <- last.row+1
     last.row <- initial.row+n.week-1
     nz <- n.week - length(rain.week.nozeros)
     time=as.matrix(seq(1:n.week))
     probzero <- calc.probzero(rain.week, time)
     probzero[probzero < 0.0001] <- 0
     probzero.st <- calc.probzero.st(nz, n.week)
     id=which(rain.week>0)
     time.nonzero=as.vector(time[id])
     t.gam <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1,family=GA, mu.link = "identity",
                                            sigma.link ="log"))
     if (only.linear == "yes"){
       t.gam.ns10 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1),family=GA,
                                                   mu.link = "identity", sigma.link ="log"))
       t.gam.ns01 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1, sigma.formula=~poly(time.nonzero,1),
                                                   family=GA, mu.link = "identity",sigma.link ="log"))
       t.gam.ns11 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1), sigma.formula=~poly(time.nonzero,1),
                                                   family=GA, mu.link = "identity", sigma.link ="log"))
       model.selection[a,1] <- which.min(c(AIC(t.gam, k=2, c=TRUE),
                                           AIC(t.gam.ns10, k=2, c=TRUE),
                                           AIC(t.gam.ns01, k=2, c=TRUE),
                                           AIC(t.gam.ns11, k=2, c=TRUE)))
     } else if (only.linear == "no") {
       t.gam.ns10 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1),family=GA, mu.link = "identity",
                                                   sigma.link ="log"))
       t.gam.ns01 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1, sigma.formula=~poly(time.nonzero,1), family=GA,
                                                   mu.link = "identity",sigma.link ="log"))
       t.gam.ns11 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1), sigma.formula=~poly(time.nonzero,1),
                                                   family=GA, mu.link = "identity", sigma.link ="log"))
       t.gam.ns20 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~nsp(time.nonzero, df = 2),family=GA, mu.link = "identity",
                                                   sigma.link ="log"))
       t.gam.ns02 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~1, sigma.formula=~nsp(time.nonzero, df = 2),
                                                   family=GA, mu.link = "identity", sigma.link ="log"))
       t.gam.ns22 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~nsp(time.nonzero, df = 2),
                                                   sigma.formula=~nsp(time.nonzero, df = 2),family=GA, mu.link = "identity",
                                                   sigma.link ="log"))
       t.gam.ns21 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~nsp(time.nonzero, df = 2), sigma.formula=~poly(time.nonzero,1),family=GA,
                                                   mu.link = "identity", sigma.link ="log"))
       t.gam.ns12 <- spsUtil::quiet(gamlss::gamlss(rain.week.nozeros~poly(time.nonzero,1), sigma.formula=~nsp(time.nonzero, df = 2),family=GA,
                                                   mu.link = "identity", sigma.link ="log"))
       model.selection[a,1] <- which.min(c(AIC(t.gam, k=2, c=TRUE),
                                           AIC(t.gam.ns10, k=2, c=TRUE),
                                           AIC(t.gam.ns01, k=2, c=TRUE),
                                           AIC(t.gam.ns11, k=2, c=TRUE),
                                           AIC(t.gam.ns20, k=2, c=TRUE),
                                           AIC(t.gam.ns02, k=2, c=TRUE),
                                           AIC(t.gam.ns21, k=2, c=TRUE),
                                           AIC(t.gam.ns12, k=2, c=TRUE),
                                           AIC(t.gam.ns22, k=2, c=TRUE)))}
     quasiprob <- (probzero.st+(1-probzero.st)*pGA(rain.week,mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1],lower.tail = TRUE, log.p=FALSE))
     quasiprob[which(rain.week==0)] <- (nz+1)/(2*n.week)
     data.week[initial.row:last.row,6] <- quasiprob
     stat.rain.drought.mod <- qGA(0.159, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
     stat.rain.drought.sev <- qGA(0.067, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
     stat.rain.drought.extr <- qGA(0.023, mu = t.gam$mu.fv[1], sigma = t.gam$sigma.fv[1])
     if (model.selection[a,1]==1){
       quasiprob.ns <- as.matrix(data.week[initial.row:last.row,6])
       Changes.Freq.Drought [a,] <- 0
     }
     if (model.selection[a,1]==2){
       quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns10$mu.fv, sigma = t.gam.ns10$sigma.fv))
       Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.mod,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.159)
       Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.sev,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.067)
       Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.extr,mu = t.gam.ns10$mu.fv[(n.week-nz)], sigma = t.gam.ns10$sigma.fv[(n.week-nz)]))-0.023)
     }
     if (model.selection[a,1]==3){
       quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns01$mu.fv, sigma = t.gam.ns01$sigma.fv))
       Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.mod,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.159)
       Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.sev,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.067)
       Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.extr,mu = t.gam.ns01$mu.fv[(n.week-nz)], sigma = t.gam.ns01$sigma.fv[(n.week-nz)]))-0.023)
     }
     if (model.selection[a,1]==4){
       quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns11$mu.fv, sigma = t.gam.ns11$sigma.fv))
       Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.mod,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.159)
       Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.sev,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.067)
       Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.extr,mu = t.gam.ns11$mu.fv[(n.week-nz)], sigma = t.gam.ns11$sigma.fv[(n.week-nz)]))-0.023)
     }
     if (model.selection[a,1]==5){
       quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns20$mu.fv, sigma = t.gam.ns20$sigma.fv))
       Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.mod,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.159)
       Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.sev,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.067)
       Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.extr,mu = t.gam.ns20$mu.fv[(n.week-nz)], sigma = t.gam.ns20$sigma.fv[(n.week-nz)]))-0.023)
     }
     if (model.selection[a,1]==6){
       quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns02$mu.fv, sigma = t.gam.ns02$sigma.fv))
       Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.mod,mu = t.gam.ns02$mu.fv[(n.week-nz)], sigma = t.gam.ns02$sigma.fv[(n.week-nz)]))-0.159)
       Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.sev,mu = t.gam.ns02$mu.fv[(n.week-nz)], sigma = t.gam.ns02$sigma.fv[(n.week-nz)]))-0.067)
       Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.extr,mu = t.gam.ns02$mu.fv[(n.week-nz)], sigma = t.gam.ns02$sigma.fv[(n.week-nz)]))-0.023)
     }
     if (model.selection[a,1]==7){
       quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns21$mu.fv, sigma = t.gam.ns21$sigma.fv))
       Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.mod,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.159)
       Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.sev,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.067)
       Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.extr,mu = t.gam.ns21$mu.fv[(n.week-nz)], sigma = t.gam.ns21$sigma.fv[(n.week-nz)]))-0.023)
     }
     if (model.selection[a,1]==8){
       quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns12$mu.fv, sigma = t.gam.ns12$sigma.fv))
       Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.mod,mu = t.gam.ns12$mu.fv[(n.week-nz)], sigma = t.gam.ns12$sigma.fv[(n.week-nz)]))-0.159)
       Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.sev,mu = t.gam.ns12$mu.fv[(n.week-nz)], sigma = t.gam.ns12$sigma.fv[(n.week-nz)]))-0.067)
       Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.extr,mu = t.gam.ns12$mu.fv[(n.week-nz)], sigma = t.gam.ns12$sigma.fv[(n.week-nz)]))-0.023)
     }
     if (model.selection[a,1]==9){
       quasiprob.ns <- as.matrix(probzero+(1-probzero)*pGA(rain.week,mu = t.gam.ns22$mu.fv, sigma = t.gam.ns22$sigma.fv))
       Changes.Freq.Drought [a,1] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.mod,mu = t.gam.ns22$mu.fv[(n.week-nz)], sigma = t.gam.ns22$sigma.fv[(n.week-nz)]))-0.159)
       Changes.Freq.Drought [a,2] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.sev,mu = t.gam.ns22$mu.fv[(n.week-nz)], sigma = t.gam.ns22$sigma.fv[(n.week-nz)]))-0.067)
       Changes.Freq.Drought [a,3] <- 100*((probzero[(n.week)]+(1-probzero[(n.week)])*
                                             pGA(stat.rain.drought.extr,mu = t.gam.ns22$mu.fv[(n.week-nz)], sigma = t.gam.ns22$sigma.fv[(n.week-nz)]))-0.023)
     }
     data.week[initial.row:last.row,7] <- quasiprob.ns
   }
   data.week[,5] <- c(qnorm(data.week[,6], mean = 0, sd = 1))
   dry.values <- which(data.week[,5] <= 0)
   wet.values <- which(data.week[,5] > 0)
   data.week[dry.values,8] <- data.week[dry.values,7]-data.week[dry.values,6]
   data.week[wet.values,8] <- data.week[wet.values,6]-data.week[wet.values,7]
   data.week[wet.values,8] <- 100*data.week[wet.values,8]
   data.week <- data.week[order(data.week[,1]),]
   colnames(data.week) <- c("Year","Month","quasiWeek","rain.at.TS",
                            "SPI","Exp.Acum.Prob","Actual.Acum.Prob","ChangeFreq")
   data.week <- round(data.week,2)
   months <- sort(rep(seq(1:12),4))
   quasiweeks <- rep(seq(1:4),12)
   Changes.Freq.Drought <- cbind(months, quasiweeks, round(Changes.Freq.Drought,2))
   colnames(Changes.Freq.Drought) <- c("Month","quasiWeek","Moderate","Severe","Extreme")
   model.selection <- cbind(months, quasiweeks, model.selection)
   colnames(model.selection) <- c("Month","quasiWeek","model")
   Drought_Changes <- list(data.week, model.selection,Changes.Freq.Drought)
   Drought_Changes <- list(data.week = data.week,
                           model.selection = model.selection,
                           Changes.Freq.Drought=Changes.Freq.Drought)
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
   logit_model <- glm(zero_rain ~ time, family = binomial)
   prob_zero_rain <- predict(logit_model,  type = "response")
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
   ifelse(nz == 0, 0, nz /(n.week+1))
 }
