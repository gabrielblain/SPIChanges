% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPIChanges.R
\name{SPIChanges}
\alias{SPIChanges}
\title{Detect trends and quantify their effect on the probability of SPI values occurring}
\usage{
SPIChanges(rain.at.TS, only.linear = "Yes")
}
\arguments{
\item{rain.at.TS}{A 4-column matrix or data frame.
1st column is years, 2nd is the months (1 to 12)
3rd is the quasiWeeks (1 to 4), and 4th is the rainfall totals accumulated at a time scale.}

\item{only.linear}{A character variable (\code{Yes} or \code{No}) defining if the function must
consider only linear models (\code{Yes}) or linear and non-linear models (\code{No}).
Default is Yes.}
}
\value{
A \code{list} object with:
\describe{
\item{data.week}{The Rainfall amounts, SPI, cumulative probability of the SPI values under the stationary
approach, cumulative probability of the SPI values under the non-stationary approach,
and the changes in the frequency of below zero SPI values caused by the changes in rainfall patterns.}
\item{model.selection}{The generalized additive model that best fits the rainfall series}
\item{Changes.Freq.Drought}{changes in the frequency of zero precipitation, moderate, severe and extreme drought events,
as definied by the SPI classification system, caused by the changes in rainfall patterns.
Changes in the precipitation amounts associated describing normal conditions is also shown.}
\item{Statistics}{Year to year changes in the expected frequency of moderate, severe and extreme drought events.}
}
}
\description{
Detect trends and quantify their effect on the probability of SPI values occurring
}
\examples{

daily.rain <- CampinasRain[,2]
rainTS4 <- TSaggreg(daily.rain=daily.rain,start.date="1980-01-01",TS=4)
Changes.in.the.SPI <- SPIChanges(rain.at.TS=rainTS4, only.linear = "yes")
}
