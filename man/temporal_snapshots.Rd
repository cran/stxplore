% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/temporal_snapshot.R
\name{temporal_snapshots}
\alias{temporal_snapshots}
\alias{temporal_snapshots.data.frame}
\alias{temporal_snapshots.stars}
\title{Plots temporal snapshots of data for specific spatial locations using a dataframe or a stars object.}
\usage{
temporal_snapshots(x, xlab = "x", ylab = "y", title = "", ...)

\method{temporal_snapshots}{data.frame}(
  x,
  xlab = "Time",
  ylab = "Value",
  title = "",
  t_col,
  z_col,
  id_col,
  id_sample,
  ...
)

\method{temporal_snapshots}{stars}(
  x,
  xlab = "Time",
  ylab = "Value",
  title = "",
  xvals,
  yvals,
  precision = 0,
  ...
)
}
\arguments{
\item{x}{A stars object or a dataframe. Arguments differ according to the input type.}

\item{xlab}{The x label.}

\item{ylab}{The y label.}

\item{title}{The graph title.}

\item{...}{Other arguments currently ignored.}

\item{t_col}{For dataframes: the time column. Time must be a set of discrete integer values.}

\item{z_col}{For dataframes: the The quantity of interest that will be plotted. Eg. temperature.}

\item{id_col}{The column of the location id.}

\item{id_sample}{The sample of location ids to be plotted}

\item{xvals}{For stars objects: the set of xvalues to plot.}

\item{yvals}{For stars objects: the set of yvalues to plot. These two lengths need to be the same.}

\item{precision}{For stars objects: set to 0, if the given values are compared with the integer values in the stars object.}
}
\value{
A ggplot.
}
\description{
This function plots temporal snapshos for specific spatial locations. The location id sample
need to be given as a function argument.
}
\examples{
# Dataframe example
library(dplyr)
data(NOAA_df_1990)
Tmax <- filter(NOAA_df_1990,
             proc == "Tmax" &
             month \%in\% 5:9 &
             year == 1993)
Tmax_ID <- unique(Tmax$id)
Tmax$t <- Tmax$julian - min(Tmax$julian) + 1
ids <- sample(Tmax_ID, 10)
temporal_snapshots(Tmax,
                  t_col = 't',
                  z_col = 'z',
                  id_col = 'id',
                  id_sample = ids)


# stars example
library(stars)
tif = system.file("tif/L7_ETMs.tif", package = "stars")
x <- read_stars(tif)
xvals <- c(288876.0,289047.0)
yvals <- c(9120405, 9120006)
temporal_snapshots(x,
                  xvals = xvals,
                  yvals = yvals)
}
