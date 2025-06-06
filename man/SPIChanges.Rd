% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SPIChanges.R
\name{SPIChanges}
\alias{SPIChanges}
\title{Detect trends and quantify their effect on the probability of SPI values occurring}
\usage{
SPIChanges(rain.at.TS, only.linear = "Yes")
}
\arguments{
\item{rain.at.TS}{A 4-column matrix generated with \code{TSaggreg()}. No other objects are accepted.
\itemize{
\item 1st column is years (YYYY),
\item 2nd is the months (1 to 12),
\item 3rd is the quasiWeeks (1 to 4),
\item and 4th is the rainfall totals accumulated at a time scale.
}}

\item{only.linear}{A character string value (\code{Yes} or \code{No}) defining if the function must
consider only linear models (\code{Yes}) or linear and non-linear models (\code{No}).
Default is \code{Yes}.}
}
\value{
A \code{list} object with:
\describe{
\item{data.week}{The Rainfall amounts, SPI, cumulative probability of the SPI values under the stationary
approach, cumulative probability of the SPI values under the non-stationary approach,
and the changes in the frequency of below zero SPI values caused by the changes in rainfall patterns.}
\item{model.selection}{The generalized additive model that best fits the rainfall series}
\item{Changes.Freq.Drought}{changes in the frequency of zero precipitation, moderate to extreme,
severe to extreme and extreme drought events,as categorized by the SPI classification system,
caused by the changes in rainfall patterns.
Changes in the precipitation amounts associated describing normal conditions is also shown.}
\item{Statistics}{Year to year changes in the expected frequency of moderate to extreme, severe to extreme
and extreme drought events.}
\item{data.week}{The Rainfall amounts, \acronym{SPI}, cumulative
probability of the \acronym{SPI} values under the stationary approach,
cumulative probability of the \acronym{SPI} values under the non-stationary
approach, and the changes in the frequency of below zero \acronym{SPI}
values caused by the changes in rainfall patterns.}
\item{model.selection}{The generalized additive model that best fits the
rainfall series}
\item{Changes.Freq.Drought}{changes in the frequency of zero precipitation,
moderate, severe and extreme drought events, as defined by the
\acronym{SPI} classification system, caused by the changes in rainfall
patterns. Changes in the precipitation amounts associated describing normal
conditions is also shown.}
\item{Statistics}{Year to year changes in the expected frequency of
moderate, severe and extreme drought events.}
}
}
\description{
Detect trends and quantify their effect on the probability of SPI values occurring
}
\details{
The \code{SPIChanges()} function implements a nonstationary parametric approach to detect
changes in precipitation patterns and assess their impact on the expected frequency
of Standardized Precipitation Index (SPI) values. It evaluates 16 candidate models
based on time-varying gamma distributions, which account for a broad range of linear
and nonlinear changes in both the mean and dispersion of the precipitation series.

Model selection is performed using the second-order Akaike Information Criterion (AICc),
and the selected model is used to compute the cumulative probability of each
precipitation amount under changing climate conditions. These nonstationary probabilities
are then compared with those from the original, stationary SPI algorithm to identify
whether the frequency of drought events has increased or decreased over time.

For detailed explanations of the gamma-based models and the model selection procedure,
please refer to the README and Vignettes included in the package.
}
\examples{

rainTS4 <- rainTS4
Changes_SPI <- SPIChanges(rain.at.TS=rainTS4, only.linear = "yes")
}
