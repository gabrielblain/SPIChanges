
# SPIChanges

<!-- badges: start -->

[![R-CMD-check](https://github.com/gabrielblain/SPIChanges/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/gabrielblain/SPIChanges/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

A package designed to improve the interpretation of the Standardized
Precipitation index (SPI) under changing climate conditions.

## Basic Description

The {SPIChanges} package was created to detect changes in rainfall
patterns and quantify how they affect the probability of a drought
event, quantified by the SPI (Mckee et al. 1993), occurring. The package
applies a nonstationary approach proposed by Blain et al. (2022),
designed to isolate the effect of such changes on the central tendency
and dispersion of the SPI frequency distributions.

The package depends on R (\>= 2.10) and imports the following packages:
{lubridate}, {zoo}, {gamlss}, {gamlss.dist}, {stats}, {spsUtil},
{rlang}, {brglm2}, and {MuMIn}.

## Installation

You can install the development version of {SPIChanges} from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gabrielblain/SPIChanges")
```

## Theoretical Background

The SPI was originally designed as a stationary index, where the
parameters of the distribution used to estimate the cumulative
probability of rainfall quantities do not vary over time. However, due
to the alteration of rainfall patterns worldwide from climate change,
using nonstationary distributions to calculate a nonstationary version
of this drought index (NSPI) has become an important step to adjust for
possible changing rainfall patterns (Park et al. 2018). Previous
studies, however, have shown that interpreting NSPI estimates is not
straightforward, as time-varying parametric distributions remove the
effect of rainfall trends on NSPI estimates (Shiau, 2020). Consequently,
in the presence of decreasing (or increasing) rainfall trends, the NSPI
may underestimate (or overestimate) drought occurrence (Park et al.,
2018; Shiau, 2020).

This package, based on the study of Blain et al. (2022), demonstrates
that the challenge of interpreting SPI estimates under changing climatic
conditions can be overcome by using information generated by the NSPI
calculation algorithm. This package uses nonstationary distributions to
detect trends in rainfall patterns and quantify their effect on the
occurrence of specific SPI values. In other words, the {SPIChanges}
package leverages the nonstationary approach of the NSPI algorithm to
detect trends in rainfall series and enhance the understanding of how
changes in rainfall patterns affect the expected frequency of drought
occurrence. With this background, the key function of this package -
`SPIChanges()` - is based on the following steps:

1.  Given a rainfall series with data accumulated at a particular time
    scale, the package uses the stationary version of the two-parameter
    gamma distribution to calculate SPI estimates and the cumulative
    probability of each rainfall amount occurring (i.e., the probability
    of each SPI estimate occurring).

2.  Using the second-order Akaike Information Criterion (AICc), the
    package selects the best-fitting model among distinct nonstationary
    two-parameter gamma distributions (see details).

3.  The package uses the selected nonstationary model to calculate the
    cumulative probability of each rainfall amount occurring under
    changing climate conditions.

4.  The package compares the probabilities estimated in steps 1 and 3 to
    indicate whether the frequency of drought or wet events, quantified
    by the SPI, has increased or decreased over time.

## Basic Instructions

## Function `TSaggreg()`

Aggregates daily rainfall totals at quasiWeek time scales.

## Usage

``` r
TSaggreg(daily.rain,
       start.date,
       TS=4)
```

## Arguments

- daily.rain: Vector, 1-column matrix or data frame with daily rainfall
  totals.
- start.date: Date at which the aggregation should start. Formats:
  YYYY-MM-DD or YYYY/MM/DD.
- TS: Time scale on the quart.month basis (integer values between 1 and
  96). Default is 4, which corresponds to the monthly time scale.

## Value

Rainfall amounts aggregated at the time scale selected by the user.

## Example 1

Aggregating daily rainfall values at 4-quasiWeek time scale, which
corresponds to monthly data.

``` r
library(SPIChanges)
daily.rain <- CampinasRain[, 2]
head(TSaggreg(
  daily.rain = daily.rain,
  start.date = "1980-01-01",
  TS = 4
))
```

    ## Done. Just ensure the last quasi-week is complete.
    ##   The last day of your series is 31 and TS is 4

    ##      Year Month quasiWeek rain.at.TS4
    ## [1,] 1980     1         4    223.1143
    ## [2,] 1980     2         1    217.4197
    ## [3,] 1980     2         2    207.0196
    ## [4,] 1980     2         3    203.8757
    ## [5,] 1980     2         4    183.2537
    ## [6,] 1980     3         1    177.9945

## Function `SPIChanges()`

Detect trends and quantify their effect on the probability of SPI values
occurring.

## Usage

``` r
SPIChanges(rain.at.TS, only.linear)
```

## Arguments

- `rain.at.TS`: Vector, 1-column matrix or data frame with rainfall
  totals accumulated at a time scale.
- `only.linear`: A character variable (`Yes` or `No`) defining if the
  function must consider only linear models (`Yes`) or linear and
  non-linear models (`No`). Default is `Yes`.

## Value

A list object with:

data.week: The precipitation amounts, SPI, cumulative probability of the
SPI values (stationary approach), cumulative probability of the
precipitation values calculated from the nonstationary algorithm of the
NSPI, and the changes in the frequency of the SPI values caused by the
changes in precipitation patterns.

model.selection: The generalized additive model (section 2.2) that best
fits the precipitation series.

Changes.Freq.Drought: changes in the frequency of moderate, severe and
extreme drought events, as defined by the SPI classification system
(Table 1), caused by the changes in precipitation patterns.

Statistics: Year to year changes in the expected frequency of moderate,
severe and extreme drought events.

## Example 2

Using only linear nonstationary parametric models to assess the
probability of rainfall amounts. The models are based on the
two-parameter gamma distribution with parameters estimated by the
maximum likelihood method. The packages `gamlss` and `gamlss.dist` are
used for such estimations.

``` r
library(SPIChanges)
daily.rain <- CampinasRain[, 2]
rainTS4 <- TSaggreg(daily.rain = daily.rain,
                    start.date = "1980-01-01",
                    TS = 4)
```

    ## Done. Just ensure the last quasi-week is complete.
    ##   The last day of your series is 31 and TS is 4

``` r
Change_SPI <- SPIChanges(rain.at.TS = rainTS4, only.linear = "Yes")
```

    ## Warning in SPIChanges(rain.at.TS = rainTS4, only.linear = "Yes"): rainfall
    ## series Month 9 Week 1 has more than 6.7% of zeros. In this situation the SPI
    ## cannot assume values lower than -1.5

``` r
head(Change_SPI$data.week)
```

    ##   Year Month quasiWeek rain.at.TS    SPI Exp.Acum.Prob Actual.Acum.Prob
    ## 1 1980     1         4   223.1143 -0.203         0.420            0.420
    ## 2 1980     2         1   217.4197 -0.035         0.486            0.486
    ## 3 1980     2         2   207.0196  0.031         0.513            0.513
    ## 4 1980     2         3   203.8757  0.122         0.549            0.549
    ## 5 1980     2         4   183.2537  0.317         0.624            0.624
    ## 6 1980     3         1   177.9945  0.413         0.660            0.660
    ##   ChangeDryFreq
    ## 1             0
    ## 2             0
    ## 3     NoDrought
    ## 4     NoDrought
    ## 5     NoDrought
    ## 6     NoDrought

``` r
head(Change_SPI$Statistics)
```

    ##   Month quasiWeek ProbZero      mu sigma
    ## 1     1         1        0 218.182 0.326
    ## 2     1         1        0 218.182 0.326
    ## 3     1         1        0 218.182 0.326
    ## 4     1         1        0 218.182 0.326
    ## 5     1         1        0 218.182 0.326
    ## 6     1         1        0 218.182 0.326

``` r
head(Change_SPI$model.selection)
```

    ##      Month quasiWeek model
    ## [1,]     1         1     1
    ## [2,]     1         2     1
    ## [3,]     1         3     1
    ## [4,]     1         4     1
    ## [5,]     2         1     1
    ## [6,]     2         2     1

``` r
head(Change_SPI$Changes.Freq.Drought)
```

    ##      Month quasiWeek StatProbZero NonStatProbZero StatNormalRain
    ## [1,]     1         1            0               0         210.51
    ## [2,]     1         2            0               0         228.64
    ## [3,]     1         3            0               0         231.14
    ## [4,]     1         4            0               0         237.84
    ## [5,]     2         1            0               0         220.15
    ## [6,]     2         2            0               0         204.63
    ##      NonStatNormalRain ChangeMod ChangeSev ChangeExt
    ## [1,]            210.51         0         0         0
    ## [2,]            228.64         0         0         0
    ## [3,]            231.14         0         0         0
    ## [4,]            237.84         0         0         0
    ## [5,]            220.15         0         0         0
    ## [6,]            204.63         0         0         0

## Example 3

Using linear and non-linear nonstationary parametric models to assess
the probability of rainfall amounts. The models are based on the
two-parameter gamma distribution with parameters estimated by the
maximum likelihood method. The packages `gamlss` and `gamlss.dist` are
used for such estimations.

``` r
library(SPIChanges)
daily.rain <- CampinasRain[, 2]
rainTS4 <- TSaggreg(daily.rain = daily.rain,
                    start.date = "1980-01-01",
                    TS = 4)
```

    ## Done. Just ensure the last quasi-week is complete.
    ##   The last day of your series is 31 and TS is 4

``` r
Change_SPI <- SPIChanges(rain.at.TS = rainTS4, only.linear = "No")
```

    ## Warning in SPIChanges(rain.at.TS = rainTS4, only.linear = "No"): rainfall
    ## series Month 9 Week 1 has more than 6.7% of zeros. In this situation the SPI
    ## cannot assume values lower than -1.5

``` r
head(Change_SPI$data.week)
```

    ##   Year Month quasiWeek rain.at.TS    SPI Exp.Acum.Prob Actual.Acum.Prob
    ## 1 1980     1         4   223.1143 -0.203         0.420            0.420
    ## 2 1980     2         1   217.4197 -0.035         0.486            0.486
    ## 3 1980     2         2   207.0196  0.031         0.513            0.513
    ## 4 1980     2         3   203.8757  0.122         0.549            0.549
    ## 5 1980     2         4   183.2537  0.317         0.624            0.624
    ## 6 1980     3         1   177.9945  0.413         0.660            0.660
    ##   ChangeDryFreq
    ## 1             0
    ## 2             0
    ## 3     NoDrought
    ## 4     NoDrought
    ## 5     NoDrought
    ## 6     NoDrought

``` r
head(Change_SPI$Statistics)
```

    ##   Month quasiWeek ProbZero      mu sigma
    ## 1     1         1        0 218.182 0.326
    ## 2     1         1        0 218.182 0.326
    ## 3     1         1        0 218.182 0.326
    ## 4     1         1        0 218.182 0.326
    ## 5     1         1        0 218.182 0.326
    ## 6     1         1        0 218.182 0.326

``` r
head(Change_SPI$model.selection)
```

    ##      Month quasiWeek model
    ## [1,]     1         1     1
    ## [2,]     1         2     1
    ## [3,]     1         3     1
    ## [4,]     1         4     1
    ## [5,]     2         1     1
    ## [6,]     2         2     1

``` r
head(Change_SPI$Changes.Freq.Drought)
```

    ##      Month quasiWeek StatProbZero NonStatProbZero StatNormalRain
    ## [1,]     1         1            0               0         210.51
    ## [2,]     1         2            0               0         228.64
    ## [3,]     1         3            0               0         231.14
    ## [4,]     1         4            0               0         237.84
    ## [5,]     2         1            0               0         220.15
    ## [6,]     2         2            0               0         204.63
    ##      NonStatNormalRain ChangeMod ChangeSev ChangeExt
    ## [1,]            210.51         0         0         0
    ## [2,]            228.64         0         0         0
    ## [3,]            231.14         0         0         0
    ## [4,]            237.84         0         0         0
    ## [5,]            220.15         0         0         0
    ## [6,]            204.63         0         0         0

# Details

The NSPI uses the same calculation algorithm as the SPI. However, the
stationary gamma distribution is replaced by nonstationary versions of
this parametric function, which allows its parameters to vary as a
function of covariates. In this package, the gamma distributions are
fitted to rainfall data using Generalized Additive Models (GAMLSS) with
time as a covariate. This fitting process is based on the maximum
likelihood method (McCullagh and Nelder, 1989) and considers the
following increasingly complex functions (candidate models). Further
information on GAMLSS can be found in Rigby and Stasinopoulos (2005).

Model 1 (stationary): The mean (µ) and dispersion (δ) of the
distribution are constant over time.

Model 2 (homoscedastic): Only µ is allowed to vary over time linearly.

Model 3: Only δ is allowed to vary over time linearly.

Model 4: Both µ and δ are allowed to vary over time linearly.

Model 5 (homoscedastic): Only µ is allowed to vary over time
non-linearly with a quadratic polynomial function.

Model 6: Only δ is allowed to vary over time non-linearly with a
quadratic polynomial function.

Model 7: µ is allowed to vary over time non-linearly with a quadratic
polynomial function; δ is allowed to vary over time linearly.

Model 8: µ is allowed to vary over time linearly; δ is allowed to vary
over time non-linearly with a quadratic polynomial function.

Model 9: Both µ and δ are allowed to vary over time non-linearly with a
quadratic polynomial function.

Model 10: Only µ is allowed to vary over time non-linearly with a cubic
polynomial function.

Model 11: Only δ is allowed to vary over time non-linearly with a cubic
polynomial function.

Model 12: µ is allowed to vary over time non-linearly with a cubic
polynomial function; δ is allowed to vary over time linearly.

Model 13: µ is allowed to vary over time linearly; δ is allowed to vary
over time non-linearly with a cubic polynomial function.

Model 14: µ is allowed to vary over time non-linearly with a cubic
polynomial function; δ is allowed to vary over time non-linearly with a
quadratic polynomial function.

Model 15: µ is allowed to vary over time non-linearly with a quadratic
polynomial function; δ is allowed to vary over time non-linearly with a
cubic polynomial function.

Model 16: µ is allowed to vary over time non-linearly with a cubic
polynomial function; δ is allowed to vary over time non-linearly with a
cubic polynomial function.

The gamma distribution has two parameters: the shape and scale. Their
relationships with the mean and dispersion are given in several studies
including Blain et al (2022).

Precipitation frequency distributions are zero-bounded, thus a mixed
function that combines the probabilities of no rainfall events (q) and
the probability given by the parametric distribution G(x\>0,mu,sigma)
must be employed to calculate the SPI (equation 1).  

H(x) = q + (1 - q) \* G(x \> 0, μ, σ) (1)

Within the original SPI algorithm, q is often calculated from equations
based on the ratio of zeros in the precipitation records and the sample
size. This package uses a nonstationary binomial process, estimated
using a bias-reduced logistic regression model, to account for temporal
variations in the probability of zero precipitation while mitigating
small-sample bias.

## BugReports:

<https://github.com/gabrielblain/SPIChanges/issues>

## License:

MIT

## Authors:

Gabriel Constantino Blain, Graciela da Rocha Sobierajski, Leticia Lopes
Martins, Adam H. Sparks. Maintainer: Gabriel Constantino Blain,
<gabriel.blain@sp.gov.br>

## Acknowledgments:

The package uses data from the CPC Global Unified Gauge-Based Analysis
of Daily Precipitation data provided by the NOAA PSL, Boulder, Colorado,
USA, from their website at <https://psl.noaa.gov>. The authors greatly
appreciate this initiative.

## References

Blain G C, Sobierajski G R, Weight E, Martins L L, Xavier A C F 2022.
Improving the interpretation of standardized precipitation index
estimates to capture drought characteristics in changing climate
conditions. International Journal of Climatology, 42, 5586-5608. DOI:
10.1002/joc.7550

McCullagh, P. and Nelder, J. A. 1989. Generalized Linear Models. London
New York Chapman and Hall. ISBN 9780412317606

Mckee, T. B., Doesken, N.J. and Kleist, J., 1993. The relationship of
drought frequency and duration to time scales. In: 8th Conference on
Applied Climatology. Boston, MA: American Meteorological Society,
179–184.

Package ‘gamlss’, Version 5.4-22, Author Stasinopoulos Mikis et al.,
<https://CRAN.R-project.org/package=gamlss>

Package ‘gamlss.dist’, Version 6.1-1, Author Stasinopoulos Mikis et al.,
<https://CRAN.R-project.org/package=gamlss.dist>

Park, J., Sung, J.H., Lim, Y-J, Kang, H-S. 2018. Introduction and
application of nonstationary standardized precipitation index
considering probability distribution function and return period.
Theoretical and Applied Climatology, 136:529-542. DOI:
10.1007/s00704-018-2500-y

Rigby, R.A, Stasinopoulos, D.M. 2005. Generalized additive models for
location, scale and shape. Appl Stat 54(3):507–554

Shiau, J-T. 2020. Effects of Gamma-Distribution Variations on SPI-Based
Stationary and nonstationary Drought Analyses. Water Resources
Management, 34:2081-2095. DOI: 10.1007/s11269-020-02548-x

Stagge J H and Sung K 2022 A Nonstationary Standardized Precipitation
Index (NSPI) using Bayesian splines. Journal of Applied Meteorology and
Climatology, 61, 761-779. DOI: 10.1175/JAMC-D-21-0244.1
