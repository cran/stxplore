% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emp_spatial_cov.R
\name{emp_spatial_cov}
\alias{emp_spatial_cov}
\alias{emp_spatial_cov.data.frame}
\alias{emp_spatial_cov.stars}
\alias{autoplot.spatialcov}
\title{Computes empirical spatial covariance using a dataframe or a stars object}
\usage{
emp_spatial_cov(
  x,
  lat_or_lon_strips = "lon",
  quadratic_time = FALSE,
  quadratic_space = FALSE,
  num_strips = 1,
  lag = 0,
  ...
)

\method{emp_spatial_cov}{data.frame}(
  x,
  lat_or_lon_strips = "lon",
  quadratic_time = FALSE,
  quadratic_space = FALSE,
  num_strips = 1,
  lag = 0,
  lat_col,
  lon_col,
  t_col,
  z_col,
  ...
)

\method{emp_spatial_cov}{stars}(
  x,
  lat_or_lon_strips = "lon",
  quadratic_time = FALSE,
  quadratic_space = FALSE,
  num_strips = 1,
  lag = 0,
  ...
)

\method{autoplot}{spatialcov}(object, xlab = "Latitude", ...)
}
\arguments{
\item{x}{A stars object or a dataframe. Arguments differ according to the input type.}

\item{lat_or_lon_strips}{Takes the values \code{lat} or \code{lon}. The value \code{lat} produces latitudinal strips,
i.e., covariance plots over longitude for different latitudinal strips. The value \code{lon} produces longitudinal
strips, i.e., covariance plots over latitude for different longitudinal strips.}

\item{quadratic_time}{If \code{TRUE}  a linear model with quadratic time is fitted and residuals computed. If \code{FALSE}
the model is fitted with linear space and time coefficients.}

\item{quadratic_space}{If \code{TRUE}  a linear model with quadratic space is fitted and residuals computed. If \code{FALSE}
the model is fitted with linear space and time coefficients.}

\item{num_strips}{The number of latitudinal/longitudinal strips to produce. This is used when plotting using autoplot.}

\item{lag}{Lag can be either 0 or 1.}

\item{...}{Other arguments currently ignored.}

\item{lat_col}{For dataframes: the column or the column name giving the latitude. The y coordinate can be used instead of latitude.}

\item{lon_col}{For dataframes: the column or the column name giving the longitude. The x coordinate can be used instead of longitude.}

\item{t_col}{For dataframes: the time column. Time must be a set of discrete integer values.}

\item{z_col}{For dataframes: the The quantity of interest that will be plotted. Eg. temperature.}

\item{object}{For autoplot: the output of the function `emp_spatial_cov'.}

\item{xlab}{For autoplot: the label for x-axis.}
}
\value{
A spatialcov object with empirical covariance data organised spatially according to the
number of strips and the lagged covariance.
}
\description{
Computes empirical spatial covariance by removing trends and examining residuals. It can compute lag-0 or log-1
empirical covariance either by latitude or longitude. You can split up the spatial domain by latitude or
longitude and plot the covariance for each longitudinal/latitudinal strips.
}
\examples{
# Dataframe example
library(dplyr)
data(NOAA_df_1990)
Tmax <- filter(NOAA_df_1990,
  proc == "Tmax" &
  month \%in\% 5:6 &
  year == 1993)
Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
emp_df <- emp_spatial_cov(Tmax,
                lat_col = "lat",
                lon_col = "lon",
                t_col ="t",
                z_col = "z",
                lat_or_lon_strips = "lon",
                num_strips = 4,
                lag = 1)
autoplot(emp_df)

# Stars example
library(stars)
# Create a stars object from a data frame
precip_df <- NOAA_df_1990[NOAA_df_1990$proc == 'Precip', ] \%>\%
  filter(date >= "1992-02-01" & date <= "1992-02-05")
precip <- precip_df[ ,c('lat', 'lon', 'date', 'z')]
st_precip <- st_as_stars(precip, dims = c("lon", "lat", "date"))
emp_spatial_cov(st_precip)
}
