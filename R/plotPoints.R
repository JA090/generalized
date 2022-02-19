#' FUNCTION to ensure corner points used to draw polygons (regions) in function drawregions aren't beyond chart boundaries.
#' @param xm The x co-ordinate of the corner point of the polygon.
#' @param ym The y co-ordinate of the corner point of the polygon.
#' @param conf The test confidence level of this polygon.
#' @param power Desired power in sample size calculation.
#' @param MMEM Minimum meaningful effect magnitude.
#' @param nn DOF of t-test.
#' @return Co-ordinates which don't extend beyond chart.
#'
plotpoints <- function(xm, ym, MMEM, conf, power, nn)
{
    if ((is.na(power) == TRUE) | power < 0.0001)
    qtb = 0
  else
    qtb = qt(power, nn)

  if (xm < 0) {
    x = max(xm, -MMEM - ym * (qt(conf, nn) + qtb))
    y = min(ym, (-xm - MMEM) / (qt(conf, nn) + qtb))
  }
  if (xm > 0) {
    x = min(xm, MMEM + ym * (qt(conf, nn) + qtb))
    y = min(ym, (xm - MMEM) / (qt(conf, nn) + qtb))
  }
  z = c(x, y)
  return(z)
}
