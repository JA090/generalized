% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clickPoints.R
\name{clickPoints}
\alias{clickPoints}
\title{Function to allow users to get co-ordinates of a point on click.}
\usage{
clickPoints(FunnelPlot, Study, var, xhit, yhit)
}
\arguments{
\item{FunnelPlot}{TRUE if generalized funnel plot requested.}

\item{Study}{Type of study (one (value 1) or two group or correlation (value 0)).}

\item{var}{Anticipated variance in sample size calculation.}

\item{xhit}{Selected x co-ordinate.}

\item{yhit}{Selected y co-ordinate.}
}
\value{
Wording "as "sample size" or "SE", plus the rounded and possibly converted x-y co-ordinates
}
\description{
The calculations for the chart use SE as y-co-ordinate. Translate this to sample size unless drawing a funnel plot.
}
