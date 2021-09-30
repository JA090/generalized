#  Details of the calculations are in 'A visual tool for the design of statistical analyses' by Aisbett, Drinkwater, Quarrie & Woodcock.
#Tests for superiority, inferiority, non-superiority, non-inferiority and equivalence are considered at up to 3 user-nominated alpha levels. 
# Users can select either a single group or a two group (equi-variance) design. This affects the mapping between total sample size
# and standard error (SE). All calculations are done using SE.
# A chart displays effect size against total sample size (or can choose SE- see below).
# Colours relate regions in the chart to the strongest test that has the nominated power for an estimated effect size and sample size/SE.
# Users must nominate a power level and pooled variance as well as the alpha levels. 
# Vertical lines are at user-nominated effect sizes and show how changes in sample size affect plausible tests. 
# An option displays the region associated with a standard 2-sided null hypothesis test. 
# Another option displays SE on the vertical axis; this also causes the nominated effect sizes to be interpreted as a 
#list of effect size and SE pairs that are plotted as points.
# There is an option to produce black and white charts and users can change the scale of either axis.


library(shiny)
library(DT)
library(tidyverse)
#################
#function to order the entered alpha levels and to duplicate if needed to make 3
alphas<-function(a0,a1,a2){
  alps<-NA
  if (!is.na(as.numeric(a0))) {alps<-as.numeric(a0)
                             if (!is.na(as.numeric(a1))) 
                                  {alps<-c(alps,as.numeric(a1))
                                    if (!is.na(as.numeric(a2))) alps<-c(alps,as.numeric(a2))
                                  }
                             else if (!is.na(as.numeric(a2))) alps<-c(alps,as.numeric(a2))
                             }
    else {if (!is.na(as.numeric(a1))) { alps<-as.numeric(a1)
                                     if (!is.na(as.numeric(a2))) alps<-c(alps,as.numeric(a2))
                                      }
          else if (!is.na(as.numeric(a2))) alps<-as.numeric(a2)
          } 


 
  # ***** force values into valid range and duplicate if needed so there are always three alpha levels 
  if(!is.na(alps[1])){  for (k in 1:length(alps)) alps[k]=min(.99999,max(0,round(alps[k],3)))
  #convert and sort
  alps<- sort(1-alps)
 
                      if (length(alps)==1) {alps[2]=alps[1];alps[3]=alps[1]}   
                      if (length(alps)==2) {alps[3]=alps[2];alps[2]=alps[1]}
 
                 }

  # check at least one level entered
  validate(need(!is.na(alps[1]),"Please enter at least one positive test level" ))  
   return(alps)}

####FUNCTION to display data either as points or lines
datadisplay<- function(study,datamean,dataSE,chartBW,var,nn,ymax)
     {
    ptcol="black"; if (chartBW == FALSE)ptcol="red"
    
    mean=as.numeric(unlist(strsplit(datamean,split=","))) #user-entered effect sizes (or effect size/SE pairs) to display
    
    #calculate SE from variance and sample size according to study design
    if (study==0|study==1) SE=sqrt(var/(nn+2))
      else SE=sqrt(var*(1/study+1/(1-study))/(nn+2) )
    
    #plot any points (if SE option chosen) or vertical lines 
    
    if(dataSE==FALSE) #the user wants sample size displayed on y-axis, so lines will be displayed at the given effect sizes
          {for (i in 1:length(mean)) lines(x=c(mean[i],mean[i]),y=c(0,ymax),lwd=2,col=ptcol)
                                                        }
     else  # the user wants SE displayed on y-axis so the given data are pairs of co-ordinates for display
           for (i in 1:(length(mean)/2)) {m=mean[2*i-1]; SE=mean[2*i];points(m,SE,pch =19,cex=.9,col=ptcol)}

     }


#####function to make sure points used to define polygons (regions) aren't beyond chart boundaries
plotpoints<-function(xm,ym,thresh,alpa,power,nn)
{if ((is.na(power)==TRUE) | power < 0.0001) qtb=0
else qtb=qt(power,nn)

if (xm<0) {x=max(xm,-thresh-ym*(qt(alpa,nn)+qtb));y=min(ym,(-xm-thresh)/(qt(alpa,nn)+qtb))}
if (xm>0) {x=min(xm,thresh+ym*(qt(alpa,nn)+qtb));y=min(ym,(xm-thresh)/(qt(alpa,nn)+qtb))}
z=c(x,y)
return(z)}

#**********************************************
#FUNCTION TO DRAW LEGEND and set up colors for regions

drawlegend<-function(alps,thresh,chartBW,xmax,ymax)
{

#START by setting up labels describing test levels 
  if(alps[3]==alps[2]) Labels=c( paste( "p <", 1-alps[3]))
    else if(alps[1]==alps[2]) Labels=c( paste( "p <", 1-alps[2]),paste("p <", 1-alps[3]))
        else Labels=c(paste( "p <", 1-alps[1]), paste( "p <", 1-alps[2]),paste("p <", 1-alps[3]))

#then labels to describe the tests; these differ if there is a minimum meaningful effect (thresh>0)
  if (thresh==0) LabelsTest=c("inferior", "superior",  "inconclusive")
     else LabelsTest=c("inferior", "superior",  "equivalent",paste("non-superior, p <",1-alps[1]),paste("non-inferior, p <",1-alps[1]), paste("inconclusive, p <",1-alps[1]))

# set colours and angles for regions according to whether black and white option selected and how many alpha levels
  colInconc<-"Gray35" # color for inconclusive regions
       
#colours for inferiority/superiority
  if (chartBW == FALSE) {colcol=c("lightblue","darkseagreen1","skyblue","palegreen","skyblue3","palegreen3");j=0;colorvec<-colcol}
     else  {colcol=c("Gray85","gray70","Gray60");j=1;colorvec<-c(colcol[1],colcol[1],colcol[2],colcol[2],colcol[3],colcol[3])}
  if (alps[2]==alps[3]) colorvec=rep(c(colcol[1],colcol[2+j]),times=3) 
      else if (alps[2]==alps[1]) colorvec= c(rep(c(colcol[1],colcol[2-j]),times=2),colorvec[5],colorvec[6])

      
  legSize=1.1 #set size
  if (alps[1]==alps[2])kstart =3 
         else kstart=1
#alter legend according to how many alpha levels are being used 
  if ( alps[2]!=alps[3]) #there are at least 2 different alpha levels
        { #
            {legend(xpd=T,1.1*xmax, 0,title=LabelsTest[1],Labels, cex=legSize, fill=c(colorvec[kstart],colorvec[kstart+2],colorvec[kstart+4]),bty="n")
            legend(xpd=T,1.1*xmax, ymax/3.5,title=LabelsTest[2],Labels, cex=legSize,fill=c(colorvec[kstart+1],colorvec[kstart+3],colorvec[kstart+5]),bty="n")
                                  
            if (thresh==0) legend(xpd=T,1.1*xmax,ymax/1.15-.1,LabelsTest[3],fill=colInconc,cex=legSize,bty="n") #inconclusive
                        else {  #if meaningful effects>0 there will be equivalence regions
                              if(alps[2]==alps[1]) legend(xpd=T,1.1*xmax, ymax/1.72,title=LabelsTest[3],Labels,fill=c("Gray85","gray70"),cex=legSize,bty="n")
                                  else legend(xpd=T,1.1*xmax,ymax/1.72,title=LabelsTest[3],Labels,fill=c("Gray85","gray70","Gray60"),cex=legSize,bty="n")
                                        
                            }
         
            }
    if (chartBW==T){ #draw lines over boxes 
      legend(xpd=T,1.1*xmax, 0,title=LabelsTest[1],Labels, cex=legSize, angle=45, density=20, bty="n")
    #  legend(xpd=T,1.1*xmax, ymax/3.5,title=LabelsTest[2],Labels, cex=legSize,angle=135, density=20,bty="n")
      if (thresh>0)legend(xpd=T,1.1*xmax, ymax/1.72,title=LabelsTest[3],Labels,cex=legSize,angle=180, density=20,bty="n")
           }
       }
  

else #only one test level
    {if (chartBW==FALSE)
        {colorvec[6]=colorvec[4];colorvec[5]=colorvec[3];legend(xpd=T,1.1*xmax,0,title = "LEGEND",LabelsTest,fill=c(colorvec[3],colorvec[4],"Gray80","gray90", "white",colInconc),bty="n")}
      else 
         {colorvec = c("Gray60","Gray75","Gray90" ,"Gray75" ,"Gray60","Gray85");legend(xpd=T,1.1*xmax,0,title = "LEGEND",LabelsTest,fill=c(colorvec[5],colorvec[6],"Gray45","gray90", "white",colInconc),bty="n")}       
    }
#with meaningful effects there are non-superior, non-inferior and inconclusive regions
if (thresh>0){ ypos=.43
legend(xpd=T,1.1*xmax,ypos,LabelsTest[4],density=20,angle=45,cex=legSize,bty="n")
legend(xpd=T,1.1*xmax,ypos+.03,LabelsTest[5],density=-20,angle=135,cex=legSize,col="white", bty="n")
legend(xpd=T,1.1*xmax,ypos+.06,LabelsTest[6],fill=colInconc,cex=legSize,bty="n")}

return(c(colorvec,colInconc))
}

#*********************
#FUNCTION TO DRAW REGIONS
drawregions<-function(alps,power,thresh,xmax,ymax,nn,colorvec,qtb,chartBW)
{dens=5
#first, identify turning points
  y<-matrix(nrow=4,ncol=2);z<-matrix(nrow=4,ncol=2)

  for (k in 1:3) {y[k,]<-plotpoints(-xmax,ymax,thresh,alps[k],power,nn) #inferiority
                z[k,]<-plotpoints(xmax,ymax,thresh,alps[k],power,nn) #superiority
                }
                y[4,]<-c(-xmax,0)
                z[4,]<-c(xmax,0)

  yy0<-plotpoints(-xmax,ymax,-thresh,alps[1],power,nn) #inconclusive region
  zz0<-plotpoints(xmax,ymax,-thresh,alps[1],power,nn)

#draw polygons of inferiority and superiority region at 3 levels
  for (k in 1:3) {
              polygon(x = c(-thresh, y[k,1],-xmax,y[k+1,1]),y = c(0, y[k,2],y[k,2],y[k+1,2]),col=colorvec[2*k-1]) #inferiority 
              polygon(x = c(thresh, z[k,1],xmax,z[k+1,1]),y = c(0, z[k,2],z[k,2],z[k+1,2]), col=colorvec[2*k]) #superiority 
                }

    #overlay inferiority and superiority regions with lines if black & white and more than 1 alpha
    if (chartBW==T) {if (alps[2]!=alps[3]) {
    #polygon(x = c(thresh, z[1,1],xmax,xmax),y = c(0, z[1,2],z[1,2],0),density=dens, lty=1,lwd=.6,angle =135) #hatches across all superiority levels
    polygon(x = c(-thresh, y[1,1],-xmax,-xmax),y = c(0, y[1,2],y[1,2],0),density=dens, lty=1,lwd=.6,angle =45) #hatches across all inferiority levels
                  }}
              
# draw non-inferiority and non-superiority regions
  polygon(x = c(0,0,-thresh, y[1,1],y[1,1]),y = c(ymax,0,0,z[1,2],ymax),density =dens, lty=1,lwd=.6,angle =45,col="black") #non-superiority
  #polygon(x = c(0,0,thresh, z[1,1],z[1,1]),y = c(ymax,0,0,z[1,2],ymax),density=dens, lty=1,lwd=.6,angle =135, col="black")

#draw inconclusive region 
  polygon(x = c(yy0[1],yy0[1],0, zz0[1],zz0[1]),y = c(ymax,zz0[2],  min(ymax,thresh/(qt(alps[1],nn)+qtb)),zz0[2],ymax),col = colorvec[7], border=NA)#"Gray50"
  
# draw equivalence region at 3 levels; this is approximated by calculating the bound on SE at 41 points between the minimum meaningful magnitudes 
    colcol=c("Gray85","Gray70","Gray60")
    x=-20:20*thresh/20
    if (qtb>0)  qtbx=qt(1-((1-power)*(thresh-abs(x))/(2*thresh)),nn)
      else qtbx=x*0
    for (k in 1:3) {y0=(thresh-abs(x))/(qt(alps[k],nn)+qtbx)
                    polygon(x,y0,col = colcol[k] )
                    }
    #distinguish equivalence regions with bars in black and white chart
    if ((chartBW == TRUE) & ((alps[3]==alps[2]) == FALSE) ) polygon(x,(thresh-abs(x))/(qt(alps[2],nn)+qtbx),density=dens,lwd=.6,angle=180)
   

}

set_plot_dimensions <- function (ww,hh){options(repr.plot.width=ww, repr.plot.height=hh)}
##############################################################
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width = 3,
                div(style="display: flex; flex-wrap: wrap;color:black", 
                p(h5("While our chart resembles a contour-enhanced funnel plot, its purpose is to help researchers design their analyses. 
                As with standard sample size calculators, the estimated effect size and population variance must be nominated along with
                desired power. We allow researchers to nominate up to 3 candidate test alpha levels. We also 
                ask for a minimum meaningful effect magnitude, so that tests detect ", em("practical"),"as well as ", em("statistical"),"
                 significance. Our chart shows the strongest test with the required power, choosing from tests for superiority, equivalence and non-inferiority in both directions.
                 \nThis demo version of the tool is for t-tests.
                "))),
    
                numericInput("Study", label = h6("Enter 0 for a single group design; enter the proportion of the larger group size for a two group design.", width="150px"), value = 0.5,min=0,max=1,step=.1,width="150px"),
                   
                 
                numericInput("nn", label = h6("Nominate the approximate degrees of freedom. You can refine this later.", width="150px"), value = 38,min=4,width="150px")
                 
    
  ),
################## 
mainPanel(width=9, 
          titlePanel(h3("A Visual Tool to Help Design Useful Statistical Tests")),
          div(style="display: flex; flex-wrap: wrap;color:blue", 
          p("The regions in the chart give the strongest of the selected tests that has the nominated power for an estimated effect size and sample size.
            The heavy vertical lines are at the nominated effect sizes and show how changes in sample size affect plausible tests. 
            You can optionally change the vertical axis to show standard error; this also causes the nominated effect sizes to be interpreted as a list of effect size and SE pairs and plotted as points. 
            Another option lets you view the chart in black and white. A final option lets you compare tests against meaningful effect sizes 
            with a standard 2-sided null hypothesis test. 
            ")),
      #plot figure with option to click on chart and get co-ordinates        

          plotOutput("Fig",width="101%",  click="plot_click"), 

          verbatimTextOutput("clickss"),
          tags$head(tags$style("#clickss{color:blue;width:600px}")),
          tags$head(tags$style("#Fig{cursor:default}")),

          
      # get input parameters that define the chart

              div(style="display: flex; flex-wrap: wrap;color:black", 
                
              column(3, p("Minimum meaningful effect, plus anticipated effect size(s) & population variance:")),
              column(3, numericInput("theta1", label = h6("minimum meaningful", width="130px"), width ="130px", value =0,min=0,max=10,step=.1)),
              
      #   enter effect sizes (interpreted as effect size, SE pairs if SE option selected below )
              
              column(3, textInput("mean1", label = h6("one or more effect sizes"),width="300px",
                                  value =".587,.276,.706,.259,.552,.272,.515,.466,.566,.217,.291,.167,.989,.396"
                                  )),
              #[Grgic et al. data  value = ".36,.31,.31,.25,.11,.2,.06,.31,.35,.25,.07,.24"
      #Cartwight value =".587,.276,.706,.259,.552,.272,.515,.466,.566,.217,.291,.167,.989,.396"
      #value = ".11,.6"
      #archer value ="-.087,.073,-.017,.10,.058,.056,.08,.142,.147,.127,.147,.137,.151,.05,.161,.122,.179,.056,.181,.061"
   #cartwright round pooled mean value ="0.068,	.276,0.187,	.259,0.033,.272,	-0.004,.466,	0.047,.217,	-0.228,.167,	0.47,.396"

              column(3,numericInput("var", label = h6("variance", width="80px"),min=0, value = 1,width="80px"))),

      # get test levels       
            div(style="display: flex;color:black", 
            column(3, p("Desired power & candidate test levels (at most 3):")),
            column(2, textInput("power", label = h6("power",col="red"), value = "0.80", width="80px")),
            column(2, textInput("al0", label = h6(em(HTML("largest &alpha;"))),  value = "0.20",width="80px")), 
            column(2, textInput("al1", label = h6(em(HTML("smaller &alpha;"))), value = "0.05",width="80px")),
            column(2, textInput("al2", label = h6(em(HTML("smallest &alpha;"))), value = "",width="80px"))),  
          
      #set display choices          

            div(style="display: flex; flex-wrap: wrap;color:black",
            column(width=3, p("Display options:")),
            column(width=2, div(style = "flex:1;color:black", numericInput("xmax", label = h6("range of effect sizes",width="100px"), value = 1.5,min=.2,step=.1,width="100px"))),
            column(width=2.5, div(style = "flex:2",numericInput("ylow", label = h6("smallest sample size/SE",width="100px"), value = .5,step=4,min=0,width="100px"))),
   
        #option to display SE on vertical axis rather than numbers
            column(width=2,div(style = "flex:1; color:black",checkboxInput("SEy",label = h6("select for SE on y-axis", width="100px"), width ="100px",value=TRUE))),
   #black & white chart option
   column(width=2,div(style = "flex:1; color:black",checkboxInput("chartBW",label = h6("select for B&W display", width="100px"), width ="100px",value=FALSE)))),
   
      #select to display standard NHST
          div(style="display: flex; flex-wrap: wrap;color:black",
          column(width=3, p("Standard null hypothesis test:\nvalues above dashed lines are significant with desired power." )),
          column(width=2, div(style="flex:1;color:black",textInput("NHST", label = h6("2-sided alpha",width="100px"),width="100px"))),

          p(h5(em("The calculations used here are explained in 'A visual tool for the design of statistical analyses' by Aisbett, Drinkwater, Quarrie & Woodcock."))

)))))

#######################################################
server <- function(input,output,session){
    
output$Fig <- renderPlot(width=800, { # draw plot of rejection region
     
#set up technical parameters, giving feedback if input is invalid

    var=input$var
    validate(need(var>0,"Please enter a non-zero variance" )) # check variance is not zero
  
    nn<-input$nn      # if degrees of freedom of t-test not provided or invalid, set to default
    if(nn<1) nn=38
 
    thresh<-input$theta1 # check the smallest meaningful effect is not zero
    validate(need(thresh >= 0,"Please enter 0 or a positive minimum meaningful effect" )) 
    
    alps<-alphas(input$al0,input$al1,input$al2) # check and convert alpha levels 
  
    power <-as.numeric(input$power) #convert very small power parameter to 0
      if(is.na(power)|power<0.001)  {qtb<-0;qtb2<-0}
           else 
      {qtb=qt(power,nn);qtb2=qt((1+power)/2,nn)} #otherwise compute the t quantile value (inverse cumulative )
  
     NHST <- as.numeric(unlist(strsplit(input$NHST,split=","))) # alpha levels of null hypothesis test; if 0 the rejection region boundaries of this are not displayed
  

    
#set up visual display parameters
      #set_plot_dimensions(20,6)
      par( mar=par()$mar +c(0,0,0,30))
      chartBW=input$chartBW #if TRUE then will choose black and white display
      xmax =input$xmax # get the maximum coordinate magnitudes to be displayed
      
      # set up labels for y axis in terms of sample size unless SE requested
      # compute largest SE to be displayed (ymax) from entered minimum sample size 
      ytick<-0
      ylow=input$ylow # smallest SE or smallest number of participants to be displayed
        if (input$Study ==0|input$Study==1) 
           {
            if(input$SEy==TRUE) 
              {ymax=ylow; nmin = (var/ylow)^2
                for (i in c(.2,.4,.6,.8,1)) ytick=c(ytick,ymax*i)}
             else 
               {nmin=ylow; ymax=sqrt(var/(ylow));for (i in c(32,8,4,2,1)) ytick=c(ytick,sqrt(var/(i*nmin)))}
           }
       else {
             if(input$SEy==TRUE) 
                {ymax=ylow;nmin= (var*(1/input$Study+1/(1-input$Study))/ylow)^2; for (i in c(.2,.4,.6,.8,1)) ytick=c(ytick,ymax*i)}
              else 
                {nmin=ylow;ymax=sqrt(var*(1/input$Study+1/(1-input$Study))/nmin);for (i in c(32,8,4,2,1)) ytick=c(ytick,sqrt(var*(1/input$Study+1/(1-input$Study))/(i*nmin)))}
            }
 #END display parameters     
    
      
# set up empty plot and prepare cursor-position input
  #START empty plot    
      tt= paste(" \nYour requested power is currently" ,power,".\nYour minimum meaningful effect magnitude is currently ",thresh,".\n")
          
      if(input$SEy==FALSE) {xlab1=expression(paste("estimated effect size")); ylab1=expression(paste("total sample size"));ylabtick=c("inf",32*nmin,8*nmin,4*nmin,2*nmin,nmin)}
       else {xlab1=expression(paste("effect size"));ylab1=expression(paste("standard error"));
                                                   ylabtick=c(0,formatC(ymax/5,digits=2,format="f"),formatC(2*ymax/5,digits=2,format="f")
                                                     ,formatC(3*ymax/5,digits=2,format="f"),formatC(4*ymax/5,digits=2,format="f"),formatC(ymax,digits=2,format="f"))}
      plot(x=c(-xmax,xmax),y=c(0,ymax),ylim=rev(range(0,ymax)),axes=FALSE, main = tt , cex.main = 1.18, font.main=1, xlab = xlab1,ylab = ylab1,type="n") # Draw empty plot
   
      
      
       #x-axis
      axis(side = 1, at = c(-xmax,-xmax/2,0,xmax/2,xmax),labels=c(-xmax,-xmax/2,0,xmax/2,xmax)) #labels=c(-xmax+.52,-xmax/2+.52,0+.52,xmax/2+.52,xmax+.52)) 
    #  put tick marks on top at smallest effect magnitudes
      axis(side = 3, at = c(-thresh,thresh),pos=0,labels=NA) 
    # put sample size/SE at y axis according to prefilled vectors ytick and ylabtick
      axis(side = 2,at=ytick, labels= ylabtick,col="black")
     # grid(nx=NULL,ny=NA, col="lightgray",lty="dotted") #???
      

 #draw legend and return colors to use in drawing regions 
  colorvec<-drawlegend(alps,thresh,chartBW,xmax,ymax)

       
# draw regions with sufficient power
  drawregions(alps,power,thresh,xmax,ymax,nn,colorvec,qtb,chartBW)
  
# draw horizontal grid lines on top of everything else
      for (i in 1:length(ytick)) lines(x=c(-xmax,xmax),y=c(ytick[i],ytick[i]),lwd=.2,col="darkgray")
  
      # display data points or lines depending on user choice
      datadisplay(input$Study,input$mean1,input$SEy, chartBW,var,nn,ymax)
  
# allow users to get co-ordinates of a point on click
      output$clickss <-renderText({xhit=0;yhit=0.01; xhit<-input$plot_click$x; yhit<-input$plot_click$y
      if(is.numeric(xhit)==TRUE) xhit=round(xhit,3)
      if(is.numeric(yhit)==TRUE)
        {if (input$SEy==TRUE) yhit<-round(yhit,3)
      else { if (input$Study ==0|input$Study==1) yhit<-round(.5+var/(yhit*yhit),0)
                else yhit<-round(.5+var*(1/input$Study+1/(1-input$Study))/(yhit*yhit),0)}
      }
       clickreturn="total sample size"; if (input$SEy==TRUE)clickreturn="SE"
       paste("Click chart to get effect size and", clickreturn,  "at a point: (", xhit,",",yhit,")")})
  
#draw NHST boundaries if requested
      if (length(NHST)!=0){
          if (NHST[1] >0) 
            {ptcol="black"; if (chartBW == FALSE)ptcol="black"
             bound =0 
             lines(c(bound,min(xmax,bound +ymax*(qt(1-NHST[1]/2,nn)+qtb))),c(0,min(ymax,(-bound + xmax)/(qt(1-NHST[1]/2,nn)+qtb))),lwd=2,lty="dashed",col = ptcol)
             lines(c(bound,max(-xmax,bound-ymax*(qt(1-NHST[1]/2,nn)+qtb))),c(0,min(ymax,(bound + xmax)/(qt(1-NHST[1]/2,nn)+qtb))),lwd=2,lty="dashed",col = ptcol)
      }}
      
})
}


shinyApp(ui=ui,server=server)