
Install.packagesTriplot <- function()
{
    list.of.packages <- c("devtools","shinythemes", "polyclip", "SDMTools","caret","MASS","klaR","rgl","shape","shiny","e1071")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    #if(length(new.packages)) install.packages(new.packages)
    #if (!("UBbipl" %in% installed.packages()[,"Package"]))
    #{
    #  library(devtools)
    #  devtools::install_github("carelvdmerwe/UBbipl3")
    #}
    library(shiny)
    library(shinythemes)
    library(polyclip)
    library(SDMTools)
    library(caret)
    library(MASS)
    library(klaR)
    library(rgl)
    library(shape)
    library(e1071)
    clipcords <<- source("source/clipcords.R")$value
    CVAbipl_C <<- source("source/CVAbipl_C.R")$value
    drawbipl.bagalpha_C <<- source("source/drawbipl.bagalpha_C.R")$value
    drawbipl.bagalpha_C2 <<-source("source/drawbipl.bagalpha_C2.R")$value
    PCAbipl_C <<- source("source/PCAbipl_C.R")$value
    unioncords <<- source("source/unioncords.R")$value
    compute.bagplot_C <<- source("source/compute.bagplot_C.R")$value
    Draw.line2_CTri <<- source("source/Draw.line2_CTri.R")$value
    Plot.marker.new_C <<- source("source/Plot.marker.new_C.R")$value
    confmetrics <<- source("source/confmetrics.R")$value
    indmat <<- source("source/indmat.R")$value
    Draw.line2 <<- source("source/Draw.line2.R")$value
    Draw.onecmline <<- source("source/Draw.onecmline.R")$value
    Plot.marker.new <<- source("source/Plot.marker.new.R")$value
    Eigen.twosided  <<- source("source/Eigen.twosided.R")$value
    bipldrawknn <<- source("source/bipldrawknn.R")$value
    playground  <<- source("source/playground.R")$value
    DrawOrthogline  <<- source("source/DrawOrthogline.R")$value
    outputfromvarsim <- c(1,4,5,9,10,12,13,16,17,18,20,22,23,24,25,26,27,28,35)
    chosenvars <-  c(1, outputfromvarsim[-1]+1)
    varnamesall <<- c("d.Liquidity","d.Y_10","d.Slo_(10-2)","(d.Y_10) ^2","d.(Y-S)_10","(d.Y_10) ^3","d.Y_2","d.Y_5","d.Y_max","d.Slo_(5-2)","d.Slo_(max-2)","d.Slo_(max-10)","d.Cur(DL)","d.Lev(VDM)","d.Slo(VDM)","d.Cur(VDM)","d.VolSkew","d.Leading","d.Coincident","d.Lagging","Leading","Coincident","Lagging","d.AC","d.CR","d.DA","d.DE","d.IC","d.LF","d.LTL","d.R","d.I","d.(R-I)","d.(R-M)","d.RVol","d.(R-I)Vol","d.(RVol-MVol)")
    varnamesall <<- varnamesall[(chosenvars-1)[-1]]
    accounttype <<- c(13,14,15,16,17)
    economytype <<- c(9,10,11,12)
    sharemarkettypes <<- c(8,18)
    interesttypes <<- c(1,2,3,4,5,6,7)
    displayorder <<- c(sharemarkettypes, accounttype,interesttypes ,economytype)
    #set.seed(3)
    #sampleb <<- sort(sample(1:dim(art3dat)[1],round(0.25*dim(art3dat)[1],0)))
    #art3datin <-     art3dat[-sampleb,chosenvars]
    graphdata <<- read.csv("data/datatrain.csv") 
    graphdatatest <<- read.csv("data/datatest.csv") 
}


########################################################################################################################################
########################################################################################################################################
########################################################################################################################################



Install.packagesTriplot()
#width = validateCssUnit("50%")
ui <- tagList(
      navbarPage(
        theme = "yeti", 
        "Classifying yield spread movements through triplots: a South African application",
        tabPanel("",
                 sidebarPanel(
                   
                   tabsetPanel(
 
                   tabPanel("Info", 
                            helpText("This Shiny web-based application serves as supplementary data to the paper of Van der Merwe and de Wet on
                                     Classifying yield spread movements through triplots: a South African application."), 
                        helpText("This sidebar panel allows the user to change the values of the specific observation that is analysed under the DATA tab. 
                      The type of scoring methods used can be changed in the AXES SCORING tab, and the underlying properties of 
                                     the base triplot can be changed in the BASE TRIPLOT tab."),
                            helpText("Within the MAIN CALCULATION tab in the main panel the user can either view the actual and predicted values of the specific observation together
                                     with their respective scores under VARIABLES FINAL SCORE. The VARIABLES RAW SCORE and AXES PREDICTIVENESS tabs provide 
                                     the values that are used to calculate the final scores. Finally the triplots figures with the various groups of axes can be seen
                                     in the INDIVIDUAL TRIPLOTS tab in the main panel.")),
                   tabPanel("Data", 
                            navlistPanel(
                            tabPanel(
                            "Share type covariates",
                            helpText(" "),
                            fluidRow(column(6,sliderInput("V8",label="d.VolSkew",min = -4,max=4,value = graphdatatest[20,9],step = 0.01)),
                                     column(6,sliderInput("V18",label="d.RVol",min = -167,max=167,value = graphdatatest[20,19],step = 0.01)))#,
                            ),
                            tabPanel(
                              "Financial Ratio type covariates",
                              helpText(" "),
                              fluidRow(column(6,sliderInput("V13",label="d.AC",min = -5,max=5,value = graphdatatest[20,14],step = 0.01)),
                                       column(6,sliderInput("V14",label="d.CR",min = -5,max=5,value = graphdatatest[20,15],step = 0.01))),
                              fluidRow(column(6,sliderInput("V15",label="d.DA",min = -1,max=1,value = graphdatatest[20,16],step = 0.01)),
                                       column(6,sliderInput("V16",label="d.DE",min = -5,max=55,value = graphdatatest[20,17],step = 0.01))),
                              fluidRow(column(6,sliderInput("V17",label="d.IC",min = -53,max=53,value = graphdatatest[20,18],step = 0.01)),
                                       column(6,""))
                            ),
                             tabPanel(
                              "Interest rate type covariates",
                              helpText(" "),
                              fluidRow(column(6,sliderInput("V1",label="(d.Y_10)^2",min = -6,max=6,value = graphdatatest[20,2],step = 0.01)),
                                       column(6,sliderInput("V2",label="d.(Y-S)_10",min = -2,max=2,value = graphdatatest[20,3],step = 0.01))),
                              fluidRow(column(6,sliderInput("V3",label="d.Y_max",min = -3,max=3,value = graphdatatest[20,4],step = 0.01)),
                                       column(6,sliderInput("V4",label="d.Slo_(5-2)",min = -3,max=3,value = graphdatatest[20,5],step = 0.01))),
                              fluidRow(column(6,sliderInput("V5",label="d.Slo_(max-2)",min = -2,max=2,value = graphdatatest[20,6],step = 0.01)),
                                       column(6,sliderInput("V6",label="d.Cur(DL)",min = -3,max=3,value = graphdatatest[20,7],step = 0.01))),
                              fluidRow(column(6,sliderInput("V7",label="d.Cur(VDM)",min = -3,max=3,value = graphdatatest[20,8],step = 0.01)),
                                       column(6,""))
                            ),
                            tabPanel(
                              "Economic type covariates",
                              helpText(" "),
                              fluidRow(column(6,sliderInput("V9",label="d.Leading",min = -38,max=38,value = graphdatatest[20,10],step = 0.01)),
                                       column(6,sliderInput("V10",label="d.Lagging",min = -37,max=37,value = graphdatatest[20,11],step = 0.01))),
                              fluidRow(column(6,sliderInput("V11",label="Coincident",min = -13,max=13,value = graphdatatest[20,12],step = 0.01)),
                                       column(6,sliderInput("V12",label="Lagging",min = -23,max=23,value = graphdatatest[20,13],step = 0.01)))#,
                              
                            )
                            )
                            ), 
                   
                   tabPanel("Axes Scoring",
                            helpText(" "),
                            radioButtons("score", "Axes Scoring method:", c("Ratio", "Count"),selected="Count"),
                            sliderInput("gridsize", "Size of grid for axes summeries:", min = 20, max = 250, value = 100, step=10)),
                   tabPanel("Base triplot",
                            helpText(" ") ,
                            sliderInput("knnin", "k used for underlying KNN:", min = 1, max = 51, value = 41, step = 5),
                            sliderInput("innerpoly", "Inner polybag:", min = 0, max = 100, value = 0, step = 5, post = "%"),
                            fluidRow(
                              column(4,numericInput("outermult",label="Outer polybag:",value = 2)),
                              column(8,sliderInput("outerpoly",label="", min = 0, max = 100, value = 95, step = 5, post= "%"))
                            ),
                            sliderInput("nbp", label = "Classification grid size:",min = 20, max = 300, step = 20, value = 200))
                   
                 ),plotOutput(outputId = "main",height=425,width=425)),
                 mainPanel(
                     tabsetPanel(
                         tabPanel("Main calculation",helpText(" "),
                                  tabsetPanel(
                                  tabPanel("Variables final score",
                                  fluidRow(column(6,tableOutput("axfinalACT")), column(6,tableOutput("axfinalPRED")))),
                                  tabPanel("Variables raw score",
                                  fluidRow(column(6,tableOutput("axrawACT")), column(6,tableOutput("axrawPRED")))),
                                  tabPanel("Axes predictiveness",
                                           helpText(" "),
                                           tableOutput("axpred"))
                                  )),
                         tabPanel("Individual triplots",helpText(" "),
                        tabsetPanel(
                         tabPanel("Share type", plotOutput(outputId = "share",height=500,width=500)),
                         tabPanel("Financial ratio type", plotOutput(outputId = "fr",height=500,width=500)),
                         tabPanel("Interest rate type", plotOutput(outputId = "ir",height=500,width=500)),
                         tabPanel("Economic type", plotOutput(outputId = "eco",height=500,width=500)) 
                         )
                         )
                     )
                 )
        )
    )
)




server <- function(input,output,session) 
{
    rv <<- reactiveValues()
    output$main <- renderPlot({
        rv$datatestin <<- as.data.frame(matrix(c(1, input$V1,input$V2,input$V3,  input$V4,input$V5,input$V6,  input$V7,input$V8,input$V9,  input$V10,input$V11,input$V12, input$V13,input$V14,input$V15,input$V16,input$V17,input$V18),ncol=19))
        rv$bipltest <<- bipldrawknn(datatrain = graphdata, datatest = rv$datatestin, alphin = input$innerpoly/100,  type = "CVA", kin =input$knnin,plotprop=c(T,T,F,T,T,T,T),
                                 expin = 4, rotate.degrees = 0,ax.coltri  = rep("white", dim(graphdata)[2]-1), ax.name.coltri = rep("white", dim(graphdata)[2]-1),
                                 tickcol = rep("white", dim(graphdata)[2]-1), ticktextcol = rep("white", dim(graphdata)[2]-1),
                                 nint= c(rep(20,12),2000,500,rep(20, 4)),hullmultin = c(input$outermult,input$outerpoly/100),pchsize=0.75,
                                 orthog.transx = c(-0.2,-0.2,  0,0.25,0.4,0.5,0.3, 0.22,0.25,   0,   0,-0.2,-0.23,   0, 0.20, 0.28,0.17,  0),
                                 orthog.transy = c(   0,-0.2,0.3, 0.3, 0,  0,  0,-0.22,0.25,-0.2,-0.3,   0,    0,0.35,-0.14,-0.28,0.17,0.3), 
                                 axin = 1:18,classnames = c("Decrease","Stable","Increase"), nbpin = input$nbp)
        x <<- recordPlot()
        outplay <<- playground(rv$bipltest,5, axin = 1:18, grid = input$gridsize,varnamesin = varnamesall,unifbar = (input$score == "Ratio"))
        outplay <- outplay[[1]]
        Zstar <- scale(rv$datatestin[,-1], rv$bipltest$bipl$X.means, scale = FALSE) %*% rv$bipltest$bipl$Br %*% rv$bipltest$bipl$rotate.mat %*% rv$bipltest$bipl$reflect.mat
        Zstar <- as.data.frame(Zstar)
        colnames(Zstar) <- c("X1","X2")
        points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
        
        graphics::box(which = "plot", col = "white")

        predictions<<- matrix(rep(NA,4*18),nrow=18)
        actualtest <<- matrix(rep(NA,4*18),nrow=18)
        
        for(i in 1:18)
        {
          #points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
          predictions[i, 1] <<- round(DrawOrthogline(x1 = rv$bipltest$Zaxes[[i]][1,1], 
                                                    y1 = rv$bipltest$Zaxes[[i]][1,2], x2 =  rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,1]),1], 
                                                    y2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,2]),2], val1 = rv$bipltest$Zaxes[[i]][1,3], 
                                                    val2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,3]),3], 
                                                    px = Zstar[1,1], py = Zstar[1, 2], ort.lty = 2), 
                                     digits = 4)
          actualtest[i,1] <<- rv$datatestin[1,i+1]
          
          predictions[i,2:4] <<- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = predictions[i,1])$y,
                                  approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = predictions[i,1])$y,
                                  approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = predictions[i,1])$y)
          
          actualtest[i,2:4] <<- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = actualtest[i,1])$y,
                                 approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = actualtest[i,1])$y,
                                 approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = actualtest[i,1])$y)
          
        }
        rownames(predictions) <<- varnamesall
        rownames(actualtest) <<- varnamesall
        rv$predictionstableval <<- predictions[displayorder,1]
        rv$actualtesttableval <<- actualtest[displayorder,1]
        aa <<- rv$bipltest$axpred
        rv$predictionstablescorefull <<- cbind(rv$predictionstableval, sweep(predictions[displayorder,2:4], MARGIN=1, aa[displayorder], `*`))
        rv$actualtesttablescorefull <<- cbind(rv$actualtesttableval, sweep(actualtest[displayorder,2:4], MARGIN=1, aa[displayorder], `*`))
        rv$predictionstablescoreraw <<- predictions[displayorder,1:4]
        rv$actualtesttablescoreraw <<- actualtest[displayorder,1:4]
        rv$predictionstablescorefinal <<- apply(sweep(predictions[displayorder,2:4], MARGIN=1, aa[displayorder], `*`),2,sum, na.rm = TRUE)
        rv$actualtesttablescorefinal <<- apply(sweep(actualtest[displayorder,2:4], MARGIN=1, aa[displayorder], `*`),2,sum, na.rm = TRUE)
    })
    
    output$axfinalACT <- renderTable({
      axfinaltableACT <<- rbind(rv$actualtesttablescorefull, c(NA,t(rv$actualtesttablescorefinal)))
      axfinaltableACT <<- format(as.data.frame(round(axfinaltableACT,2)),nsmall=2)
      rownames(axfinaltableACT) <<- c(varnamesall[displayorder],"<strong>==TOTAL==</strong>")
      colnames(axfinaltableACT) <<-c("Value","Decrease", "Stable", "Increase")
      axfinaltableACT[dim(axfinaltableACT)[1],] <<- paste0("<strong>",axfinaltableACT[dim(axfinaltableACT)[1],],"</strong>")
      axfinaltableACT
    }, rownames  = TRUE, align = 'lcccc',caption = "ACTUAL DATA",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL), sanitize.text.function=function(x){x})
    
    output$axfinalPRED <- renderTable({
      axfinaltablePRED <<- rbind(rv$predictionstablescorefull, c(NA,t(rv$predictionstablescorefinal)))
      axfinaltablePRED <<- format(as.data.frame(round(axfinaltablePRED,2)),nsmall=2)
      rownames(axfinaltablePRED) <<- c(varnamesall[displayorder],"<strong>==TOTAL==</strong>")
      colnames(axfinaltablePRED) <<-c("Value","Decrease", "Stable", "Increase")
      axfinaltablePRED[dim(axfinaltablePRED)[1],] <<- paste0("<strong>",axfinaltablePRED[dim(axfinaltablePRED)[1],],"</strong>")
      axfinaltablePRED
    }, rownames  = TRUE, align = 'lcccc',caption = "PREDICTED DATA",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL), sanitize.text.function=function(x){x})
    
    output$axrawACT <- renderTable({
      axrawtableACT <<- rv$actualtesttablescoreraw
      axrawtableACT <<- as.data.frame(axrawtableACT)
      rownames(axrawtableACT) <<- varnamesall[displayorder]
      colnames(axrawtableACT) <<-c("Value", "Decrease", "Stable", "Increase")
      axrawtableACT
    }, rownames  = TRUE, align = 'lcccc', caption = "ACTUAL DATA",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    output$axrawPRED <- renderTable({
      axrawtablePRED <<- rv$predictionstablescoreraw
      axrawtablePRED <<- as.data.frame(axrawtablePRED)
      rownames(axrawtablePRED) <<- varnamesall[displayorder]
      colnames(axrawtablePRED) <<-c("Value", "Decrease", "Stable", "Increase")
      axrawtablePRED
    }, rownames  = TRUE, align = 'lcccc', caption = "PREDICTED DATA",
    caption.placement = getOption("xtable.caption.placement", "top"), 
    caption.width = getOption("xtable.caption.width", NULL))
    
    output$axpred <- renderTable({
      axpredres <<- rv$bipltest$axpred
      axpredres <<- data.frame("Axes predictivity (%)" = round(axpredres[displayorder],2)*100)
      axpredres <<- as.data.frame(axpredres)
      rownames(axpredres) <<- varnamesall[displayorder]
      colnames(axpredres) <<-"Axes predictivity (%)"
      axpredres
    }, rownames  = TRUE, align = 'lc')
    

    output$share <- renderPlot({
      updatedvars <- c(input$innerpoly/100,  input$knnin,input$outermult, input$outerpoly/100, input$nbp ) 
        replayPlot(x)
        outplay <<- playground(rv$bipltest,20, axin = sharemarkettypes, grid = input$gridsize,varnamesin = varnamesall,unifbar = (input$score == "Ratio"))
        outplay <- outplay[[1]]
        
        Zstar <- scale(rv$datatestin[,-1], rv$bipltest$bipl$X.means, scale = FALSE) %*% rv$bipltest$bipl$Br %*% rv$bipltest$bipl$rotate.mat %*% rv$bipltest$bipl$reflect.mat
        Zstar <- as.data.frame(Zstar)
        colnames(Zstar) <- c("X1","X2")
        points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
        
        predictions<- matrix(rep(NA,4*18),nrow=18)
        actualtest <- matrix(rep(NA,4*18),nrow=18)
        
        for(i in sharemarkettypes)
        {
            points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
            predictions[i, 1] <- round(DrawOrthogline(x1 = rv$bipltest$Zaxes[[i]][1,1], 
                                                      y1 = rv$bipltest$Zaxes[[i]][1,2], x2 =  rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,1]),1], 
                                                      y2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,2]),2], val1 = rv$bipltest$Zaxes[[i]][1,3], 
                                                      val2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,3]),3], 
                                                      px = Zstar[1,1], py = Zstar[1, 2], ort.lty = 2), 
                                       digits = 4)
            actualtest[i,1] <- rv$datatestin[1,i+1]
            points(approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,1], xout= predictions[i, 1])$y,
                   approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,2], xout= predictions[i, 1])$y,pch=1,cex = 1.5)  
            
            points(approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,1], xout= rv$datatestin[1,i+1])$y,
                   approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,2], xout= rv$datatestin[1,i+1])$y, pch = 16)  
            
            predictions[i,2:4] <- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = predictions[i,1])$y,
                                    approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = predictions[i,1])$y,
                                    approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = predictions[i,1])$y)
            
            actualtest[i,2:4] <- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = actualtest[i,1])$y,
                                   approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = actualtest[i,1])$y,
                                   approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = actualtest[i,1])$y)
            
        }
        graphics::box(which = "plot", col = "white")
    })
    
    output$fr <- renderPlot({
        updatedvars <- c(input$innerpoly/100,  input$knnin,input$outermult, input$outerpoly/100, input$nbp ) 
        replayPlot(x)
        outplay <<- playground(rv$bipltest,20, axin = accounttype, grid = input$gridsize,varnamesin = varnamesall,unifbar = (input$score == "Ratio"))
        outplay <- outplay[[1]]
        
        Zstar <- scale(rv$datatestin[,-1], rv$bipltest$bipl$X.means, scale = FALSE) %*% rv$bipltest$bipl$Br %*% rv$bipltest$bipl$rotate.mat %*% rv$bipltest$bipl$reflect.mat
        Zstar <- as.data.frame(Zstar)
        colnames(Zstar) <- c("X1","X2")
        points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
        
        predictions<- matrix(rep(NA,4*18),nrow=18)
        actualtest <- matrix(rep(NA,4*18),nrow=18)
        
        for(i in accounttype)
        {
            points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
            predictions[i, 1] <- round(DrawOrthogline(x1 = rv$bipltest$Zaxes[[i]][1,1], 
                                                      y1 = rv$bipltest$Zaxes[[i]][1,2], x2 =  rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,1]),1], 
                                                      y2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,2]),2], val1 = rv$bipltest$Zaxes[[i]][1,3], 
                                                      val2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,3]),3], 
                                                      px = Zstar[1,1], py = Zstar[1, 2], ort.lty = 2), 
                                       digits = 4)
            actualtest[i,1] <- rv$datatestin[1,i+1]
            points(approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,1], xout= predictions[i, 1])$y,
                   approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,2], xout= predictions[i, 1])$y,pch=1,cex = 1.5)  
            
            points(approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,1], xout= rv$datatestin[1,i+1])$y,
                   approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,2], xout= rv$datatestin[1,i+1])$y, pch = 16)  
            
            predictions[i,2:4] <- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = predictions[i,1])$y,
                                    approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = predictions[i,1])$y,
                                    approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = predictions[i,1])$y)
            
            actualtest[i,2:4] <- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = actualtest[i,1])$y,
                                   approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = actualtest[i,1])$y,
                                   approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = actualtest[i,1])$y)
            
        }
        graphics::box(which = "plot", col = "white")
    })
    
    output$eco <- renderPlot({
      updatedvars <- c(input$innerpoly/100,  input$knnin,input$outermult, input$outerpoly/100, input$nbp ) 
        replayPlot(x)
        outplay <<- playground(rv$bipltest,20, axin = economytype, grid = input$gridsize,varnamesin = varnamesall,unifbar = (input$score == "Ratio"))
        outplay <- outplay[[1]]
        
        Zstar <- scale(rv$datatestin[,-1], rv$bipltest$bipl$X.means, scale = FALSE) %*% rv$bipltest$bipl$Br %*% rv$bipltest$bipl$rotate.mat %*% rv$bipltest$bipl$reflect.mat
        Zstar <- as.data.frame(Zstar)
        colnames(Zstar) <- c("X1","X2")
        points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
        
        predictions<- matrix(rep(NA,4*18),nrow=18)
        actualtest <- matrix(rep(NA,4*18),nrow=18)

        for(i in economytype)
        {
            points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
            predictions[i, 1] <- round(DrawOrthogline(x1 = rv$bipltest$Zaxes[[i]][1,1], 
                                                      y1 = rv$bipltest$Zaxes[[i]][1,2], x2 =  rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,1]),1], 
                                                      y2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,2]),2], val1 = rv$bipltest$Zaxes[[i]][1,3], 
                                                      val2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,3]),3], 
                                                      px = Zstar[1,1], py = Zstar[1, 2], ort.lty = 2), 
                                       digits = 4)
            actualtest[i,1] <- rv$datatestin[1,i+1]
            points(approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,1], xout= predictions[i, 1])$y,
                   approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,2], xout= predictions[i, 1])$y,pch=1,cex = 1.5)  
            
            points(approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,1], xout= rv$datatestin[1,i+1])$y,
                   approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,2], xout= rv$datatestin[1,i+1])$y, pch = 16)  
            
            predictions[i,2:4] <- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = predictions[i,1])$y,
                                    approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = predictions[i,1])$y,
                                    approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = predictions[i,1])$y)
            
            actualtest[i,2:4] <- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = actualtest[i,1])$y,
                                   approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = actualtest[i,1])$y,
                                   approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = actualtest[i,1])$y)
            
        }
        graphics::box(which = "plot",col = "white")
    })
    
    output$ir <- renderPlot({
      updatedvars <- c(input$innerpoly/100,  input$knnin,input$outermult, input$outerpoly/100, input$nbp ) 
        replayPlot(x)
        outplay <<- playground(rv$bipltest,20, axin = interesttypes, grid = input$gridsize,varnamesin = varnamesall,unifbar = (input$score == "Ratio"))
        outplay <- outplay[[1]]
        
        Zstar <- scale(rv$datatestin[,-1], rv$bipltest$bipl$X.means, scale = FALSE) %*% rv$bipltest$bipl$Br %*% rv$bipltest$bipl$rotate.mat %*% rv$bipltest$bipl$reflect.mat
        Zstar <- as.data.frame(Zstar)
        colnames(Zstar) <- c("X1","X2")
        points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
        
        predictions<- matrix(rep(NA,4*18),nrow=18)
        actualtest <- matrix(rep(NA,4*18),nrow=18)

        for(i in interesttypes)
        {
            points(x = Zstar[1,1], y = Zstar[1,2],pch=15) 
            predictions[i, 1] <- round(DrawOrthogline(x1 = rv$bipltest$Zaxes[[i]][1,1], 
                                                      y1 = rv$bipltest$Zaxes[[i]][1,2], x2 =  rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,1]),1], 
                                                      y2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,2]),2], val1 = rv$bipltest$Zaxes[[i]][1,3], 
                                                      val2 = rv$bipltest$Zaxes[[i]][length(rv$bipltest$Zaxes[[i]][,3]),3], 
                                                      px = Zstar[1,1], py = Zstar[1, 2], ort.lty = 2), 
                                       digits = 4)
            actualtest[i,1] <- rv$datatestin[1,i+1]
            points(approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,1], xout= predictions[i, 1])$y,
                   approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,2], xout= predictions[i, 1])$y,pch=1,cex = 1.5)  
            
            points(approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,1], xout= rv$datatestin[1,i+1])$y,
                   approx(x = rv$bipltest$Zaxes[[i]][,3], y= rv$bipltest$Zaxes[[i]][,2], xout= rv$datatestin[1,i+1])$y, pch = 16)  
            
            predictions[i,2:4] <- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = predictions[i,1])$y,
                                    approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = predictions[i,1])$y,
                                    approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = predictions[i,1])$y)
            
            actualtest[i,2:4] <- c(approx(x = outplay[[i]][,1],y = outplay[[i]][,4],xout = actualtest[i,1])$y,
                                   approx(x = outplay[[i]][,1],y = outplay[[i]][,5],xout = actualtest[i,1])$y,
                                   approx(x = outplay[[i]][,1],y = outplay[[i]][,6],xout = actualtest[i,1])$y)
            
        }
        graphics::box(which = "plot", col = "white")
    })

}

shinyApp(ui=ui, server = server)

