#' FUNCTION to set up parameters used in function genSSsize, and to draw empty plot.
#' @param FunnelPlot TRUE if a generalized funnel plot is requested.
#' @param power Desired power in sample size calculation. Is re-set to 0 for funnel plot.
#' @param MMEM Minimum meaningful effect magnitude.
#' @param xmax Largest effect size to be displayed.
#' @param ylow Smallest sample size to be displayed, if calculating sample size.
#' @param yhigh Largest SE to be displayed on funnel plot (smallest is 0).
#' @param Study Numeric. One group (value 0 or 1) or two group with value = relative size.
#' @param var Anticipated variance in sample size calculation. Used with Study parameter to convert sample sizes to SE.
#' @param DOF Degrees of freedom of t-test. If invalid will be set to default 38. Is re-set to 200 for funnel plot.
#' @param plotCentre Centre of funnel plot (if selected)
#' @return Adjusted var, DOF, MMEM, power, xmax, ymax (SE) or nmin (sample size), qtb (t-quantile),ytick (tick mark positions on y-axis),plotCentre (centre of plot).
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
  # If degrees of freedom of t-test not provided or invalid, set to default.
  #' Use large DOF to simulate normal distributions for meta-analysis display.
  if (DOF < 1) DOF = 38 }
  else DOF=500
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

  if (Study == 0 | Study == 1)
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
      ymax = sqrt(var * (1 / Study + 1 / (1 - Study)) / nmin)
      for (i in c(32, 8, 4, 2, 1))
        ytick = c(ytick, sqrt(var * (1 / Study + 1 / (1 - Study)) / (i * nmin)))
    }
  }



  # set up empty plot

  par(mar = par()$mar + c(0, 0, 0, 30))
  if (FunnelPlot == FALSE)
    # estimating sample size
  {
    xlab1 = expression(paste("estimated effect size"))
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


  # x-axis
  axis(
    side = 1,
    at = c(-xmax, -xmax / 2, 0, xmax / 2, xmax),
    labels = round(c(-xmax+plotCentre, .5*(-xmax+plotCentre ), plotCentre, .5*(xmax+plotCentre ), xmax+plotCentre),2)
  )

  #'  Put tick marks on top of chart at smallest effect magnitudes.
  axis(
    side = 3,
    at = c(-MMEM, MMEM),
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
