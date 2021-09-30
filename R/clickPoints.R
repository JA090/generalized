#' Function to allow users to get co-ordinates of a point on click.
#' @description The calculations for the chart use SE as y-co-ordinate. Translate this to sample size unless drawing a funnel plot.
#' @param FunnelPlot TRUE if generalized funnel plot requested.
#' @param Study Type of study (one or two group).
#' @param var Anticipated variance in sample size calculation.
#' @param xhit Selected x co-ordinate.
#' @param yhit Selected y co-ordinate.
#' @return Wording "as "sample size" or "SE", plus the rounded and possibly converted x-y co-ordinates
#'
clickPoints <- function(FunnelPlot,Study,var, xhit,yhit) {

  if (is.numeric(xhit) == TRUE)

        xhit = round(xhit, 3)
      if (is.numeric(yhit) == TRUE)
      {
        if (FunnelPlot == TRUE)
          yhit <- round(yhit, 3)
        else {
          if (Study == 0 |
              Study == 1)
            yhit <- round(.5 + var / (yhit * yhit), 0)
          else
            yhit <-
              round(.5 + var * (1 / Study + 1 / (1 - Study)) / (yhit * yhit), 0)
        }
      }
      if (FunnelPlot == TRUE)
        clickreturn = "SE"
      else       clickreturn = "total sample size"
      return(c(clickreturn,xhit,yhit))

    }

