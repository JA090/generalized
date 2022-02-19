#' FUNCTION to display data on chart generated in genSSize.
#' @description When computing sample sizes, vertical lines are drawn through anticipated effect sizes on a chart of effect size vs. sample size.
#' If computing a generalized funnel plot, study data are plotted as points, along with the meta-analysis result, on a chart of effect size vs SE.
#' @param FunnelPlot TRUE if a generalized funnel plot requested.
#' @param ymax SE corresponding to smallest sample size to be shown on chart.
#' @param chartBW TRUE if b&w chart requested.
# For sample size calculation.
#' @param study Numeric. One or two group with relative size of larger group, or correlation (study= 0).
#' @param datameanSS Anticipated effect sizes. Comma-separated text.  Used if FunnelPlot is FALSE.
#' @param var Anticipated variance. Used if FunnelPlot is FALSE and study > 0.

# For funnel plot.
#' @param re TRUE if random-effects model requested for meta-analysis.
#' @param datameanF Comma-separated text of study effect sizes. Used if FunnelPlot is TRUE.
#' @param dataVar Comma-separated text of study variances. Used if FunnelPlot is TRUE.
#' @param plotCentre Centre of plot. Used if FunnelPlot is TRUE.
#' @description
datadisplay <- function(study,
                        datameanF,
                        datameanSS,
                        dataVar,
                        chartBW,
                        var,
                        nn,
                        ymax,
                        FunnelPlot,
                        re,
                        plotCentre)
{
  ptcol = "black"
  if (chartBW == FALSE)
    ptcol = "black"
   if (FunnelPlot==T)datamean=datameanF
    else datamean=datameanSS
  mean = as.numeric(unlist(strsplit(datamean, split = ",")))  #user-entered effect sizes (or effect size/SE pairs) to display
  if (study==0) mean = 0.5*log((1+mean)/(1-mean)) #' For correlation, apply Fisher's transformation to user-entered data.

  Var = as.numeric(unlist(strsplit(dataVar, split = ","))) #user-entered effect sizes (or effect size/SE pairs) to display



  #' Plot vertical lines at effect sizes for sample size calculator option.

  if (FunnelPlot == FALSE)
    #the user wants to calculate, so lines will be displayed at the given effect sizes
  {
    for (i in 1:length(mean))
      lines(
        x = c(mean[i], mean[i]),
        y = c(0, ymax),
        lwd = 2,
        col = ptcol
      )
  }
  else
    #' For funnel plot, plot points on chart of effect size vs SE. -shift to centre plot
    for (i in 1:(length(mean) )) {
      m = mean[i]-plotCentre
      SE = sqrt(Var[i])
      points(m,
             SE,
             pch = 19,
             cex = .9,
             col = ptcol)
    }
  #' Then use CRAN metafor package to compute appropriate meta-analysis model and plot this with a cross symbol.
 if (FunnelPlot ==T) {if (re==T) Model="ML"
    else Model="FE"
    res<-rma.uni(yi=mean,vi=Var,method=Model)
    points(res$beta-plotCentre,res$se,pch=4,cex=1.5,lwd=2, col=ptcol)}

}
