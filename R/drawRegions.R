#'FUNCTION to draw rejection regions for various t-tests in which there is the desired power.
#' @description The tests depend on how many test levels have been requested and whether the minimum meaningful effect is greater than 0.
#' If power and the minimum meaningful effect are greater than 0, the equivalence regions are approximated.
#' Details of the computation are in Aisbett, Drinkwater, Quarrie and Woodcock.
#' @param confLevel Numeric 3-component vector of confidence levels with possible repeat values.
#' @param power Desired power in sample size calculation.
#' @param MMEM Minimum meaningful effect magnitude.
#' @param chartBW TRUE if b&w chart.
#' @param xmax Extent of x-axis is -xmax to xmax.
#' @param ymax Extent of y-axis is ymax in units of SE.
#' @param ytick Vector of y-axis values at which tick marks are to be placed.
#' @param colorvec Vector of colors either for color or b&w version, as set in function drawlegend.
#' @param nn DOF for t-tests, as set in function setup.
#' @param qtb Quantile from t-test including power.
#' @description

drawregions <-
  function(confLevel,
           power,
           MMEM,
           xmax,
           ymax,
           ytick,
           nn,
           colorvec,
           qtb,
           chartBW)
  {

    # First, identify turning points.
    y <- matrix(nrow = 4, ncol = 2)
    z <- matrix(nrow = 4, ncol = 2)

    for (k in 1:3) {
      y[k, ] <- plotpoints(-xmax, ymax, MMEM, confLevel[k], power, nn) #'inferiority
      z[k, ] <- plotpoints(xmax, ymax, MMEM, confLevel[k], power, nn) #'superiority
    }
    y[4, ] <- c(-xmax, 0)
    z[4, ] <- c(xmax, 0)

    yy0 <-
      plotpoints(-xmax, ymax, -MMEM, confLevel[1], power, nn) #'inconclusive region
    zz0 <- plotpoints(xmax, ymax, -MMEM, confLevel[1], power, nn)

    #' Draw polygons of inferiority and superiority regions.
    for (k in 1:3) {
      polygon(
        x = c(-MMEM, y[k, 1], -xmax, y[k + 1, 1]),
        y = c(0, y[k, 2], y[k, 2], y[k + 1, 2]),
        col = colorvec[2 * k - 1]
      ) #'inferiority
      polygon(
        x = c(MMEM, z[k, 1], xmax, z[k + 1, 1]),
        y = c(0, z[k, 2], z[k, 2], z[k + 1, 2]),
        col = colorvec[2 * k]
      ) #'superiority
    }

    #' Overlay inferiority regions with lines if chart is black & white and there is more than 1 alpha.
    dens = 5 #density of lines
    if (chartBW == T) {
      if (confLevel[2] != confLevel[3]) {
        polygon(
          x = c(-MMEM, y[1, 1], -xmax, -xmax),
          y = c(0, y[1, 2], y[1, 2], 0),
          density = dens,
          lty = 1,
          lwd = .6,
          angle = 45
        )
      }
    }

    #' Draw non-superiority region (non-inferiority region is the residue of other regions ).
    polygon(
      x = c(0, 0, -MMEM, y[1, 1], y[1, 1]),
      y = c(ymax, 0, 0, z[1, 2], ymax),
      density = dens,
      lty = 1,
      lwd = .6,
      angle = 45,
      col = "black")

    #' Draw inconclusive region.
    polygon(
      x = c(yy0[1], yy0[1], 0, zz0[1], zz0[1]),
      y = c(ymax, zz0[2],  min(ymax, MMEM / (qt(confLevel[1], nn) + qtb)), zz0[2], ymax),
      col = colorvec[7]
    )

    #' Draw equivalence region at 3 levels; this is approximated by calculating the bound on SE at 41 points between the minimum meaningful magnitudes.
    colcol = c("Gray85", "Gray72", "Gray60")

    x = -20:20 * MMEM / 20
    if (qtb > 0)
      qtbx = qt(1 - ((1 - power) * (MMEM - abs(x)) / (2 * MMEM)), nn)
    else
      qtbx = x * 0
    for (k in 1:3) {
      y0 = (MMEM - abs(x)) / (qt(confLevel[k], nn) + qtbx)
      polygon(x, y0, col = colcol[k])
    }
    # distinguish equivalence regions with bars in black and white chart
    if (chartBW == TRUE)
        #((confLevel[3] == confLevel[2]) == FALSE))
      polygon(
        x,
        (MMEM - abs(x)) / (qt(confLevel[2], nn) + qtbx),
        density = 10,
        lwd = .6,
        angle = 180
      )

    #' Draw horizontal grid lines on top of everything else.
    for (i in 1:length(ytick))
      lines(
        x = c(-xmax, xmax),
        y = c(ytick[i], ytick[i]),
        lwd = .2,
        col = "darkgray"
      )


  }
