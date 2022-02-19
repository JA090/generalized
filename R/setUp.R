#' FUNCTION to set up parameters used in function genSSsize, and to draw empty plot.
#' @param FunnelPlot TRUE if a generalized funnel plot is requested.
#' @param power Desired power in sample size calculation. Is re-set to 0 for funnel plot.
#' @param MMEM Minimum meaningful effect magnitude or minimum meaningful correlation magnitude.
#' @param xmax Largest effect size to be displayed; forced to be less than 1 if doing correlation
#' @param ylow Smallest sample size to be displayed, if calculating sample size.
#' @param yhigh Largest SE to be displayed on funnel plot (smallest is 0).
#' @param Study Numeric. Pearson's correlation (value 0). One group (value 1). Two group with value between 0 and 1 = relative size.
#' @param var Anticipated variance in sample size calculation. Used with Study parameter to convert sample sizes to SE in t-tests.
#' @param DOF Degrees of freedom of t-test. If zero then testing a correlation and is reset to 500. If invalid will be set to default 38. Is re-set to 500 for funnel plot.
#' @param plotCentre Centre of funnel plot (if selected)
#' @return Adjusted var, DOF, MMEM, power, xmax, ymax (SE) or nmin (sample size), qtb (t-quantile),ytick (tick mark positions on y-axis),plotCentre (centre of plot), Corr (0 unless Correlation).
#' @description
setup <- function(var,
                  DOF,
                  MMEM,
                  power,
                  xmax,
                  yhigh,
                  ylow,
                  FunnelPlot,
                  plotCentre,
                  Study) {

  #'First, set up technical parameters, giving feedback if input is invalid.
  if (FunnelPlot==F) {validate(need(var > 0, "Please enter a non-zero variance")) #' check variance is not zero
  if (is.na(MMEM)==T) MMEM <- 0
    validate(need(
    MMEM >= 0,
    "Please enter 0 or a positive minimum meaningful effect"
  ))

    #' Use large DOF to simulate normal distributions for  correlations and meta-analysis display.

 #' For a correlation, apply Fisher's transformation to minimum meaningful and to x-axis ranges.
  if (Study==0) {xmax=min(0.99, xmax); xmax=0.5*max(log((1+xmax)/(1-xmax)),abs(log((1-xmax)/(1+xmax))))
          DOF=500; MMEM =0.5*log((1+MMEM)/(1-MMEM)) } # calculating correlation; use Fisher's transformation for calculations
    else if (DOF < 1) DOF = 38 } # If degrees of freedom of t-test not provided or invalid, set to default.

  else DOF=500 #meta analysis

  # centre the plot about 0 unless specified for funnel plot
  if (FunnelPlot==FALSE| is.na(plotCentre)==TRUE) plotCentre=0
  validate(need(
    xmax > plotCentre,
    "Please enter a largest effect size to display that is greater than the effect size at the plot centre"
  ))
  xmax=xmax-plotCentre #'x co-ordinates are relative to center
  power <- as.numeric(power)
  if (FunnelPlot==T) power =0
  if (is.na(power) |
      power < 0.001)
    qtb <- 0 #'convert very small power parameter to 0
  else
     qtb = qt(power, DOF) #'otherwise compute the t quantile value (inverse cumulative )

  #'Then set up visual display parameters.

  #' y-axis will be in units of SE or sample size, from a nominated value to 0 or infinity respectively.
  #' Labels for y axis will be total sample size unless generalized funnel plot requested.

  #' Compute largest SE from smallest sample size if doing sample size calculation.
  #' Use this to work out y-axis tick marks and labels in terms of sample size.
  ytick <- 0

  if (Study == 1)
  {
    if (FunnelPlot == TRUE)
    {
      ymax = yhigh
      nmin = (var / ylow) ^ 2
      for (i in c(.2, .4, .6, .8, 1))
        ytick = c(ytick, ymax * i)
    }
    else
    {
      nmin = ylow
      ymax = sqrt(var / (ylow))
      for (i in c(32, 8, 4, 2, 1))
        ytick = c(ytick, sqrt(var / (i * nmin)))
    }
  }
  else {
    if (FunnelPlot == TRUE)
    {
      ymax = yhigh
      nmin = (var * (1 / Study + 1 / (1 - Study)) / ylow) ^ 2
      for (i in c(.2, .4, .6, .8, 1))
        ytick = c(ytick, ymax * i)
    }
    else
    {
      nmin = ylow
      if (Study>0) ymax = sqrt(var * (1 / Study + 1 / (1 - Study)) / nmin)
        else ymax=1/(sqrt(nmin-3))
      for (i in c(32, 8, 4, 2, 1))
        {if (Study>0) ytick = c(ytick, sqrt(var * (1 / Study + 1 / (1 - Study)) / (i * nmin)))
        else ytick = c(ytick, 1/sqrt((i * nmin)-3))
      }
    }
  }



  # set up empty plot

  par(mar = par()$mar + c(0, 0, 0, 30))
  if (FunnelPlot == FALSE)
    # estimating sample size
  {
    if (Study > 0) xlab1 = expression(paste("anticipated effect size"))
      else xlab1 = expression(paste("anticipated correlation coefficient"))
    ylab1 = expression(paste("total sample size"))
    ylabtick = c("inf", 32 * nmin, 8 * nmin, 4 * nmin, 2 * nmin, nmin)
  }
  else
  {
    xlab1 = expression(paste("effect size"))
    ylab1 = expression(paste("standard error"))

    ylabtick = c(
      0,
      formatC(ymax / 5, digits = 2, format = "f"),
      formatC(2 * ymax / 5, digits = 2, format = "f")
      ,
      formatC(3 * ymax / 5, digits = 2, format = "f"),
      formatC(4 * ymax / 5, digits = 2, format = "f"),
      formatC(ymax, digits = 2, format = "f")
    )
  }
  #' Draw empty plot with title that gives type of chart (sample size calculator or funnel plot).
  if (FunnelPlot == T)
    tt1 <- "GENERALIZED FUNNEL PLOT"
  else
    tt1 <- "SAMPLE SIZE CALCULATOR"
tt1=""
  plot(
    x = c(-xmax, xmax),
    y = c(0, ymax),
    ylim = rev(range(0, ymax)),
    axes = FALSE,
    main = tt1 ,
    cex.main = 1.18,
    font.main = 1,
    xlab = xlab1,
    ylab = ylab1,
    type = "n"
  )


  # x-axis - adjust labels if plot not centred on zero
  if (Study>0) {xlabels=round(c(-xmax+plotCentre, .5*(-xmax+plotCentre ), plotCentre, .5*(xmax+plotCentre ), xmax+plotCentre),2)}
  else {xlabels=round(c((exp(-2*xmax)-1)/(exp(-2*xmax)+1), (exp(-xmax)-1)/(exp(-xmax)+1), 0, (exp(xmax)-1)/(exp(xmax)+1), (exp(2*xmax)-1)/(exp(2*xmax)+1)),2)}
  axis(
    side = 1,
    at = c(-xmax, -xmax / 2, 0, xmax / 2, xmax),
    labels = xlabels

  )

  #'  Put tick marks on top of chart at smallest effect magnitudes.

    axis(
    side = 3,
     at = c(-MMEM,MMEM),
    pos = 0,
    labels = NA
  )
  # Put sample size/SE at y axis according to prefilled vectors ytick and ylabtick.
  axis(
    side = 2,
    at = ytick,
    labels = ylabtick,
    col = "black"
  )
 lines(c(xmax,xmax),c(ymax,0))
 lines(c(-xmax,-xmax),c(ymax,0))

  return(c(var, DOF, MMEM, power, xmax, ymax,  nmin, qtb,plotCentre,ytick))
}
