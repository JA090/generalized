% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataDisplay.R
\name{datadisplay}
\alias{datadisplay}
\title{FUNCTION to display data on chart generated in genSSize.}
\usage{
datadisplay(
  study,
  datameanF,
  datameanSS,
  dataVar,
  chartBW,
  var,
  nn,
  ymax,
  FunnelPlot,
  re,
  plotCentre
)
}
\arguments{
\item{study}{Numeric. One or two group with relative size of larger group, or correlation (study= 0).}

\item{datameanF}{Comma-separated text of study effect sizes. Used if FunnelPlot is TRUE.}

\item{datameanSS}{Anticipated effect sizes. Comma-separated text.  Used if FunnelPlot is FALSE.}

\item{dataVar}{Comma-separated text of study variances. Used if FunnelPlot is TRUE.}

\item{chartBW}{TRUE if b&w chart requested.}

\item{var}{Anticipated variance. Used if FunnelPlot is FALSE and study > 0.}

\item{ymax}{SE corresponding to smallest sample size to be shown on chart.}

\item{FunnelPlot}{TRUE if a generalized funnel plot requested.}

\item{re}{TRUE if random-effects model requested for meta-analysis.}

\item{plotCentre}{Centre of plot. Used if FunnelPlot is TRUE.}
}
\description{
When computing sample sizes, vertical lines are drawn through anticipated effect sizes on a chart of effect size vs. sample size.
If computing a generalized funnel plot, study data are plotted as points, along with the meta-analysis result, on a chart of effect size vs SE.

Plot vertical lines at effect sizes for sample size calculator option.
For funnel plot, plot points on chart of effect size vs SE. -shift to centre plot
Then use CRAN metafor package to compute appropriate meta-analysis model and plot this with a cross symbol.
}
