#'FUNCTION to draw legend and set up colors for regions for chart.
#' @param confLevel Numeric 3-component vector of confidence levels with possible repeat values.
#' @param MMEM Minimum meaningful effect magnitude (after Fisher transformation in case of correlation).
#' @param chartBW TRUE if b&w chart is to be drawn.
#' @param xmax Extent of x-axis is -xmax to xmax. Used to position legend.
#' @param ymax Extent of y-axis is 0 to ymax, in units of SE. Used to position legend.
#' @return Vector of colors (gray levels for b&w version). These cannot currently be changed by the user.
#' @description
drawlegend <- function(confLevel, MMEM, chartBW, xmax, ymax)
{
#'Start by setting up labels describing test levels.
 alpha=round(1-confLevel,4)
  if (alpha[3] == alpha[2])
    Labels = c(paste("p <", alpha[3]))
  else if (alpha[1] == alpha[2])
    Labels = c(paste("p <",  alpha[2]), paste("p <",  alpha[3]))
  else
    Labels = c(paste("p <",  alpha[1]),
               paste("p <",  alpha[2]),
               paste("p <",  alpha[3]))

  #' Then set up labels to describe the tests; these differ if there is a minimum meaningful effect (MMEM>0).
  if (MMEM == 0)
    LabelsTest = c("inferior", "superior",  "inconclusive")
  else
    LabelsTest = c(
      "inferior",
      "superior",
      "equivalent",
      paste("non-superior, p <",  alpha[1]),
      paste("non-inferior, p <",  alpha[1]),
      paste("inconclusive, p <",  alpha[1])
    )

  #' Set colours and angles for regions according to whether black and white option selected and how many alpha levels.
  colInconc <- "Gray35" #' color for inconclusive regions

  # Set colours for inferiority/superiority regions (at present these are not user-specified).

  if (chartBW == FALSE) {
    colcol = c("lightblue",
               "darkseagreen1",
               "dodgerblue",
               "springgreen",
               "blue",
               "chartreuse3"
   )
    j = 0
    colorvec <- colcol
  }
  else  {
    colcol = c("Gray85", "gray72", "Gray60")
    j = 1
    colorvec <-
      c(colcol[1], colcol[1], colcol[2], colcol[2], colcol[3], colcol[3])

  }
  if (alpha[2] == alpha[3])
    colorvec = rep(c(colcol[1], colcol[2 + j]), times = 3)
  else if (alpha[2] == alpha[1])
    colorvec = c(rep(c(colcol[1], colcol[2 - j]), times = 2), colorvec[5], colorvec[6])

  legSize = 1.1 #set legend size


  if (alpha[1] == alpha[2])
    kstart = 3
  else
    kstart = 1

  #do legend for inferiority then superiority regions

  l1<-legend(
    xpd = T,
    1.1 * xmax,
    0,
    title = LabelsTest[1],
    Labels,
    cex = legSize,
    fill = c(colorvec[kstart], colorvec[kstart + 2], colorvec[kstart + 4]),
    bty = "n"
  )
  # use first legend to get y-axis spacing for subsequent legend entries
  l<-length(Labels)
  if (l>1) space<-l1$text$y[l]-l1$text$y[l-1]
  else space<-l1$text$y[1]/2

  l2<-legend(
    xpd = T,
    1.1 * xmax,
    l1$text$y[l]+space,
    title = LabelsTest[2],
    Labels,
    cex = legSize,
    fill = c(colorvec[kstart + 1], colorvec[kstart + 3], colorvec[kstart + 5]),
    bty = "n"
  )

  if (chartBW == T)
    # draw lines over inferiority boxes if B&W
    legend(
      xpd = T,
      1.1 * xmax,
      0,
      title = LabelsTest[1],
      Labels,
      cex = legSize,
      angle = 45,
      density = 20,
      bty = "n"
    )

  # if no meaningful effects do inconclusive region
  if (MMEM == 0)
    legend(
      xpd = T,
      1.1 * xmax,
      l2$text$y[l]+space,
      LabelsTest[3],
      fill = colInconc,
      cex = legSize,
      bty = "n"
    ) #inconclusive
  else {
    #' If meaningful effects >0 there will be equivalence regions in the legend.

    l3<- legend(
      xpd = T,
      1.1 * xmax,
      l2$text$y[l]+space,
      title = LabelsTest[3],
      Labels,
      fill = c("Gray85", "gray70", "Gray60"),
      cex = legSize,
      bty = "n"
    )
    if (chartBW==T) #cover these with stripes if B&W
      legend(
        xpd = T,
        1.1 * xmax,
        l2$text$y[l]+space,
        title = "",
        Labels,
        angle=180,
        density=c(20,20,20),
        fill = c("black","black", "black"),
        cex = legSize,
        bty = "n"
      )
  }


  #' With minimum meaningful effects, there are non-superior and non-inferior regions as well as an inconclusive region.
  if (MMEM > 0)  {

    legend(
      xpd = T,
      1.1 * xmax,
      l3$text$y[l]+space,
      LabelsTest[4],
      density = 20,
      angle = 45,
      cex = legSize,
      bty = "n"
    )
    legend(
      xpd = T,
      1.1 * xmax,
      l3$text$y[l]+2*space,
      LabelsTest[5],
      density = -20,
      angle = 135,
      cex = legSize,
      col = "white",
      bty = "n"
    )
    legend(
      xpd = T,
      1.1 * xmax,
      l3$text$y[l]+3*space,
      LabelsTest[6],
      fill = colInconc,
      cex = legSize,
      bty = "n"
    )
  }

  return(c(colorvec, colInconc))
}
