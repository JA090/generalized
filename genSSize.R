#' Launches Shiny app to calculate sample sizes for multiple significance tests against minimum meaningful effect sizes.
#' @description The Shiny app displays a chart of effect size or correlation coefficient vs. sample size from which estimated sample sizes can be read off for multiple tests.
#' If there is a minimum practically meaningful effect magnitude, potential tests of interest are for superiority, inferiority, non-superiority, non-inferiority and equivalence (with respect to a range of effect sizes equivalent to zero).
#' The chart shows regions with the nominated power for each of these tests at up to 3 user-nominated alpha levels.
#' Currently, through the Shiny UI, users can select either a single group or a two group design, or simple Pearson's correlation.
#' As with standard calculators, users must nominate desired power and anticipated pooled variance. Vertical lines at user-nominated effect sizes show how changes in sample size affect which tests will plausibly return a significant finding.
#' There is an option to produce gray-scale charts and users can change the scale of either axis.
#' *Another option converts the chart to a generalized contour-enhanced funnel plot that displays study and meta-analysis results against rejection regions for tests against minimum meaningful effects.*
#' @param FunnelPlot TRUE for a generalized funnel plot chart in meta-analysis rather than sample size calculator. Default FALSE.
#' @param meaningfulEffect Minimum meaningful effect magnitude. Default 0.
#' @param level Comma-separated text containing between 1 and 3 test alpha levels. Default "0.05,0.005".
#' @param power Power desired in sample size calculation; ignored in funnel plot. Default 0.80.
#' @param dataFile Text file name, used for meta-analyses only. File must be formatted as CSV with 2 rows, first row contains study means, second study variances.


require(shiny)
require(tidyverse)
require(metafor)
require(DT)



    genSSize <- function(      FunnelPlot = F,
                               level = ".05, .005",
                               power = 0.8,
                               meaningfulEffect = 0,
                               dataFile="Cowlishaw.txt"){


        d<-read.csv(dataFile,header=F,sep="\t")
        dataMean=paste((d[1,]))
        dataVar=paste((d[2,]))
        if (is.na(level)==F) level<-as.numeric(unlist(strsplit(level, split = ",")))

        require(shiny)
        require(tidyverse)
        require(metafor)
        require(DT)

        #' @details
        #' INTERFACE:
        #' Shiny side panel explains the sample size calculator and asks for information about the design (one or two group) and the approximate degrees of freedom (DOF) for t-tests.
        ui <- fluidPage(sidebarLayout(
          conditionalPanel(
            condition = "input.FunnelPlot == false",
            sidebarPanel(
              width = 3,
              div(style = "display: flex; flex-wrap: wrap;color:black",
                  p(
                    h5(
                      "While our chart resembles a contour-enhanced funnel plot, its purpose is to help researchers design their analyses.
                As with standard sample size calculators, the estimated effect size and population variance must be nominated along with
                desired power. We allow researchers to nominate up to 3 candidate test alpha levels. We also
                ask for a minimum meaningful effect magnitude, so that tests detect ",
                      em("practical"),
                      "as well as ",
                      em("statistical"),
                      "
                 significance. Our chart shows the strongest test with the required power, choosing from tests for superiority, equivalence and non-inferiority in both directions.
                 \nThis demo version of the tool is for t-tests or tests of Pearson's correlation using Fisher's z-transformation.
                "
                    )
                  ),

                  numericInput(
                    "Study",
                    label = h6(
                      "Enter 0 for a correlation; enter 1 for a single group design; enter the proportion of the larger group size for a two group design.",
                      width = "150px"
                    ),
                    value = 0.5,
                    min = 0,
                    max = 1,
                    step = .1,
                    width = "150px"
                  ),


                  numericInput(
                    "DOF",
                    label = h6(
                      "Nominate the approximate degrees of freedom if using t-tests (you can refine this later).",
                      width = "150px"
                    ),
                    value = 38,
                    min = 4,
                    width = "150px"
                  )
              )),

          ),

          #' The main panel starts by explaining the chart, which is displayed below. Beneath the chart are widgets to take user input.
          #' The explanation and input options as well as the chart change if the options for correlation or for a generalized funnel plot are selected.
          mainPanel(
            width = 9,
            titlePanel(h3(
              "Generalized Funnel Plot as a Statistical Analysis Design Tool"
            )),
            div(
              style = "display: flex; flex-wrap: wrap;color:blue",
              conditionalPanel(
                condition = "input.FunnelPlot == false",
                p(
                  "The regions in the chart give the strongest of the selected tests that has the nominated power for an estimated effect size and sample size.
            The heavy vertical lines are at the nominated effect sizes and show how changes in sample size affect plausible tests.
            An option lets you view the chart in black and white. Another option lets you compare tests against meaningful effect sizes
            with a standard 2-sided null hypothesis test. Selecting the funnel plot option will take you to a generalization of funnel plots.
            "
                )
              ),
              conditionalPanel(
                condition = "input.FunnelPlot == true",
                p(
                  "This version of the chart generalises contour-enhanced funnel plots to show study findings in relation to a nominated effect magnitude
                  that is an estimate of the smallest",em("practically meaningful"), "effect. Up to three alpha levels can be displayed.
                  The estimated effect size and SE for either a random or fixed effects model is plotted as a cross, and is calculated using the R", em("metafor"), "package with maximum likelihood fit for the random effects model.
                "
                )
              )), #end of div

            # Display chart

            plotOutput("Fig", width = "101%",  click = "plot_click"),
            # Users can click on chart and get co-ordinates.
            verbatimTextOutput("clickss"),
            tags$head(tags$style("#'clickss{color:blue;width:600px}")),
            tags$head(tags$style("#'Fig{cursor:default}")),


            # Technical parameters are the first input requested: minimum meaningful effect and test levels.
            div(
              style = "display: flex; flex-wrap: wrap;color:black",

              column(
                3,
                p("Minimum meaningful effect:")
              ),
              column(
                3,
                numericInput(
                  "MMEM",
                  width = "100px",
                  label="",
                  value = meaningfulEffect,
                  min = 0,
                  max = 10,
                  step = .1
                )
              )
            ),
            div(
              style = "display: flex; flex-wrap: wrap;color:black",

              column(
                3,
                p("Test alpha level(s):")
              ),
              column(2, textInput(
                "al0",
                label = h6(em(HTML("largest &alpha;"))),
                value = level[1],
                width = "80px"
              )),
              column(2, textInput(
                "al1",
                label = h6(em(HTML("smaller &alpha;"))),
                value = level[2],
                width = "80px"
              )),
              column(2, textInput(
                "al2",
                label = h6(em(HTML(
                  "smallest &alpha;"))),
                value = level[3],
                width = "80px"
              ))
            ), #end of div

            #' Then the anticipated effect size and variance (or anticipated correlation if doing a correlation)are requested.
            #' For a generalized funnel plot, the study means and variances are requested.

            div(
              style = "display: flex; flex-wrap: wrap;color:black",


              column(
                3,
                conditionalPanel(
                  condition = "input.FunnelPlot == false & input.Study > 0",
                  p(
                    "\nAnticipated effect size(s) and variance & desired power:"
                  )
                ),
                conditionalPanel(
                  condition = "input.FunnelPlot == false & input.Study == 0",
                  p(
                    "\nAnticipated correlation(s) & desired power:"
                  )
                ),

                conditionalPanel(
                  condition = "input.FunnelPlot == true",
                  p(
                    "\nStudy effect sizes and variances:"
                  )
                )
              ), #end of col 1

              #  enter effect sizes

              column(
                3,
                conditionalPanel(
                  condition = "input.FunnelPlot == true",
                  textInput(
                    "mean1",
                    label = h6(em("effect sizes")),
                    width = "400px",
                    value = dataMean
                  )),
                conditionalPanel(
                  condition = "input.FunnelPlot == false & input.Study >0",
                  textInput(
                    "mean2",
                    label = h6(em("one or more effect sizes")),
                    width = "400px",
                    value = ".3, .5"
                  )),
                conditionalPanel(
                  condition = "input.FunnelPlot == false & input.Study == 0",
                  textInput(
                    "mean2",
                    label = h6(em("correlations")),
                    width = "800px",
                    value = ".3, .5"
                  ))
              ), #'end of col 2
              column(3,
                     conditionalPanel(
                       condition = "input.FunnelPlot == true",
                       textInput(
                         "Var",
                         label = h6(em("variances")),
                         width = "400px",
                         value=dataVar
                       )

                     ),

                     conditionalPanel(
                       condition = "input.FunnelPlot == false & input.Study>0",
                       numericInput(
                         "var",
                         label = h6(em("variance", width = "80px")),
                         min = 0,
                         value = 1,
                         width = "80px"
                       )
                     )
              ), #end of column 3
              #' The desired power in the sample size computations is requested.
              column(2,
                     conditionalPanel(
                       condition = "input.FunnelPlot == false",
                       textInput(
                         "power",
                         label = h6(em("power")),
                         value = "0.80",
                         width = "80px"
                       ))
              ) # end of column
            ),#end of div



            #' After that, display parameters are obtained. Currently these are the smallest sample size and the largest effect magnitude, plus whether the chart should be in black and white.
            div(
              style = "display: flex; flex-wrap: wrap;color:black",
              column(width = 3, p("Display options:")),
              conditionalPanel(
                condition = "input.FunnelPlot == true",
                numericInput(
                  "plotCentre",
                  label = h6(em("effect size at centre of plot")),
                  width = "100px",
                  value=0
                )),


              column(width = 2, div(
                style = "flex:1;color:black",
                numericInput(
                  "xmax",
                  label = h6(em("largest effect size to display"), width = "100px"),
                  value = 1.5,
                  min = .2,
                  step = .1,
                  width = "100px"
                )
              )),

              conditionalPanel(
                condition = "input.FunnelPlot == true",
                numericInput(
                  "bigSE",
                  label = h6(em("largest SE to display")),
                  value = .5,
                  step = .1,
                  min = .001,
                  width = "100px"
                )
              ),


              conditionalPanel(
                condition = "input.FunnelPlot == false",
                numericInput(
                  "smallSS",
                  label = h6(em("smallest sample size"), width = "100px"),
                  value = 15,
                  step = 5,
                  min = 1,
                  width = "100px"
                )
              ),

              # black & white chart option
              column(width = 2, div(
                style = "flex:1; color:black",
                checkboxInput(
                  "chartBW",

                  label = h6("select for B&W display", width = "100px"),
                  width = "100px",
                  value = FALSE

                )
              ))
            ),


            # option to display standard NHST
            conditionalPanel(
              condition = "input.FunnelPlot == false",
              div(style = "display: flex; flex-wrap: wrap;color:black",
                  column(
                    width = 3,
                    p(
                      "Standard null hypothesis test:\nvalues above dashed lines are significant with desired power."
                    )
                  ),
                  column(
                    width = 2, div(style = "flex:1;color:black", textInput(

                      "NHST",
                      label = h6(em("2-sided alpha"), width = "100px"),
                      width = "100px"
                    ))
                  ))),
            #' Finally users are advised of where to find details of the calculations, and offered the choice of showing a generalized funnel plot.
            #' If this option is checked, the side panel disappears, and different input options are presented.
            #' Either a fixed effect or ML random effects model can be selected, the calculations of which will use the CRAN metafor package.

            div(style = "display: flex; flex-wrap: wrap;color:black",
                column(8, p(h5(
                  em(
                    "The calculations used here are explained in 'Applying Generalized Funnel Plots to Help Design Statistical Analyses' by Aisbett, Drinkwater, Quarrie & Woodcock.
            The generalized funnel plots described there can be accessed by checking the box to the left. Uncheck to go back to the sample size calculator."
                  )
                ))),
                # option to choose funnel plot or sample size calculator
                column(2,
                       checkboxInput(
                         "FunnelPlot",
                         label = h6("Select for generalized funnel plot", width = "300px", col="black"),
                         width = "100px",

                         value = FunnelPlot
                       )),

                # If funnel plot, ask whether to use random or fixed effects in calculating meta-analysis result.
                column(2,
                       conditionalPanel(
                         condition = "input.FunnelPlot == true",
                         checkboxInput(
                           "fe",
                           label = h6("Select for random effects model", width = "300px"),
                           width = "100px",
                           value = T
                         ))),
            ) # end of div
          )
        ))




        ###################################################################
        #' @details
        #' DRAWING THE CHART:
        server <- function(input, output, session) {
          output$Fig <-
            renderPlot(width = 800, {
              #' To draw the chart, first setup parameters and draw an empty plot (function setup).
              alps <-
                alphas(input$al0, input$al1, input$al2) # check and convert alpha levels

              if(is.na(input$NHST)==F) NHST <- as.numeric(unlist(strsplit(input$NHST, split = ","))) # alpha levels of null hypothesis test- if 0, won't display additional NHST

              param <-
                setup(
                  input$var,
                  input$DOF,
                  input$MMEM,
                  input$power,
                  input$xmax,
                  input$bigSE,
                  input$smallSS,
                  input$FunnelPlot,
                  input$plotCentre,
                  input$Study
                )
              var <-
                param[1]
              DOF<-
                param[2]
              MMEM <-
                param[3]
              power <-
                param[4]
              xmax <-
                param[5]
              ymax <- param[6]
              nmin <- param[7]
              qtb <- param[8]
              ytick <- param[10:length(param)]
              plotCentre<-param[9]


              #' Next, draw the legend and work out the colors to use in drawing regions (these depend on the type of tests and number of levels) (function drawlegend).
              colorvec <-
                drawlegend(alps, MMEM, input$chartBW, xmax, ymax)

              #' Then draw the rejection regions for the various tests, which for the sample size calculations must also have sufficient power (function drawregions).
              drawregions(alps,
                          power,
                          MMEM,
                          xmax,
                          ymax,
                          ytick,
                          DOF,
                          colorvec,
                          qtb,
                          input$chartBW)

              #' Next, display lines at the expected effect size(s) (or points representing study findings if funnel plot option selected) (function datadisplay).

              datadisplay(input$Study,
                          input$mean1,
                          input$mean2,
                          input$Var,
                          input$chartBW,
                          var,
                          DOF,
                          ymax,
                          input$FunnelPlot,
                          input$fe,
                          plotCentre)


              #' Allow users to get co-ordinates of a point on click (function clickPoints).
              output$clickss <-
                renderText({
                  xhit = 0
                  yhit = 0.01
                  xhit <- input$plot_click$x+plotCentre
                  yhit <- input$plot_click$y
                  clickreturn<-clickPoints(input$FunnelPlot, input$Study,  var, xhit, yhit) #  convert to correlation or sample size if required

                  paste(
                    "Click chart to get",clickreturn[1],"and",
                    clickreturn[2],
                    "at a point: (",
                    clickreturn[3],
                    ",",
                    clickreturn[4],
                    ")"
                  )
                })


              #' Draw NHST boundaries if requested.
              if (length(NHST) != 0) {
                if (NHST[1] > 0)
                {
                  ptcol = "black"
                  if (input$chartBW == FALSE)
                    ptcol = "black"
                  bound = 0
                  lines(
                    c(bound, min(xmax, bound + ymax * (
                      qt(1 - NHST[1] / 2, DOF) + qtb
                    ))),
                    c(0, min(ymax, (
                      -bound + xmax
                    ) / (
                      qt(1 - NHST[1] / 2, DOF) + qtb
                    ))),
                    lwd = 2,
                    lty = "dashed",
                    col = ptcol
                  )
                  lines(
                    c(bound, max(-xmax, bound - ymax * (
                      qt(1 - NHST[1] / 2, DOF) + qtb
                    ))),
                    c(0, min(ymax, (
                      bound + xmax
                    ) / (
                      qt(1 - NHST[1] / 2, DOF) + qtb
                    ))),
                    lwd = 2,
                    lty = "dashed",
                    col = ptcol
                  )
                }
              }

            })
        }



        shinyApp(ui = ui, server = server)
      }
      #' @references  'Applying Generalized Funnel Plots to Help Design Statistical Analyses' by Aisbett, Drinkwater, Quarrie & Woodcock.

      #' @export


