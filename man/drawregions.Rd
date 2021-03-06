% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drawRegions.R
\name{drawregions}
\alias{drawregions}
\title{FUNCTION to draw rejection regions for various t-tests in which there is the desired power.}
\usage{
drawregions(
  confLevel,
  power,
  MMEM,
  xmax,
  ymax,
  ytick,
  nn,
  colorvec,
  qtb,
  chartBW
)
}
\arguments{
\item{confLevel}{Numeric 3-component vector of confidence levels with possible repeat values.}

\item{power}{Desired power in sample size calculation.}

\item{MMEM}{Minimum meaningful effect magnitude (after Fisher transformation in case of correlation).}

\item{xmax}{Extent of x-axis is -xmax to xmax.}

\item{ymax}{Extent of y-axis is ymax in units of SE.}

\item{ytick}{Vector of y-axis values at which tick marks are to be placed.}

\item{nn}{DOF for t-tests.}

\item{colorvec}{Vector of colors either for color or b&w version, as set in function drawlegend.}

\item{qtb}{Quantile from t-test including power.}

\item{chartBW}{TRUE if b&w chart.}
}
\description{
The tests depend on how many test levels have been requested and whether the minimum meaningful effect is greater than 0.
If power and the minimum meaningful effect are greater than 0, the equivalence regions are approximated.
Details of the computation are in Aisbett, Drinkwater, Quarrie and Woodcock.

Draw polygons of inferiority and superiority regions.
Overlay inferiority regions with lines if chart is black & white and there is more than 1 alpha.
Draw non-superiority region (non-inferiority region is the residue of other regions ).
Draw inconclusive region.
Draw equivalence region at 3 levels; this is approximated by calculating the bound on SE at 41 points between the minimum meaningful magnitudes.
Draw horizontal grid lines on top of everything else.
}
