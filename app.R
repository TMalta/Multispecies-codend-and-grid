#########################################################################################
#########################################################################################
#########################################################################################
##############          SHINY script for Trawl Selectivity lecture         ##############
##############                      By Tiago Veiga Malta                   ##############       
##############                     version 1.0 10/08/2020                  ##############
#########################################################################################
#########################################################################################
#########################################################################################

library(shiny)
library(ggplot2)
library(data.table)
library(grid)
library(gridExtra)
library(shinythemes)


# list.of.packages <- c("shiny", "ggplot2", "data.table", "grid", "gridExtra")
# new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)) install.packages(new.packages)
# 
# 
# lapply(list.of.packages, library, character.only = TRUE)





ui <- shinyUI(
 
  navbarPage( theme = shinytheme("superhero"),
              "Effect of combined selectivity of a sorting grid and codend",
                         tabPanel("Selectivity parameters",
                                  sidebarPanel(
                                    h2("Grid Selectivity"),
                                    h4(id="red-heading", "Red curve"),
                                    tags$style("#red-heading{color: red;}"),
                                    checkboxInput("GridInGear", "No grid in the trawl", value = TRUE),
                                    sliderInput("BarSpacing",
                                                "Grid's bar spacing in mm:",
                                                min = 10,
                                                max = 100,
                                                value = 50,
                                                step = 2),
                                    
                                    h2("Codend Selectivity"),
                                    h4(id="blue-heading", "Blue curve"),
                                    tags$style("#blue-heading{color: blue;}"),
                                    

                                    sliderInput("MeshSize",
                                                "Mesh size in codend (mm):",
                                                min = 70,
                                                max = 200,
                                                value = 80,
                                                step = 10),
                                    uiOutput("OAImages"),
                                    sliderInput("OpeningAngle",
                                                "Opening angle of meshes (\u00B0):",
                                                min = 10,
                                                max = 90,
                                                value = 25,
                                                step = 5),
                                    
                                    br(),
                                    br(),
                                    h5("For further information please contact Tiago Veiga Malta - timat@aqua.dtu.dk"),
                                    img(src='DTU_logo.jpg', align = "center", style="width: 100px"),
                                    width = 4
                                  ),
                                  mainPanel(
                                    h2("Grid and Codend selectivity curves"),
                                    plotOutput("SelectivityPlot", height = "auto"),
                                    h2("Overall trawl selectivity"),
                                    plotOutput("OverallSelectivityPlot", height = "auto")
                                  )
                         ),
                         tabPanel("Effect on discard ratio",

                                  
                                  wellPanel(
                                    img(src='DTU_logo.jpg', align = "center", style="width: 100px"),
                                    fluidRow(
                                      column(3,
                                             style = "margin-top: 63px;",
                                             h4("Average length:")
                                             ),
                                      column(3,
                                             h4("Roundfish"),
                                             sliderInput("PopAverageLength_Roundfish","",
                                                         min = 20,
                                                         max = 80,
                                                         value = 30)
                                             ),
                                      column(3,
                                             h4("Flatfish"),
                                             sliderInput("PopAverageLength_Flatfish","",
                                                         min = 20,
                                                         max = 70,
                                                         value = 27)
                                             ),
                                      column(3,
                                             h4("Crustacean"),
                                             sliderInput("PopAverageLength_Crustacean","",
                                                         min = 2,
                                                         max = 6,
                                                         value = 3.5,
                                                         step = 0.1)
                                             ),
                                    ),
                                    fluidRow(
                                      column(3,
                                             h4("Standard deviation:"),
                                             style = "margin-top: 25px;"
                                      ),
                                      column(3,
                                               sliderInput("PopSD_Roundfish", "",
                                                           min = 1,
                                                           max = 20,
                                                           value = 20)
                                      ),
                                      column(3,
                                             sliderInput("PopSD_Flatfish", "",
                                                         min = 1,
                                                         max = 10,
                                                         value = 5)
                                      ),
                                      column(3,
                                             sliderInput("PopSD_Crustacean", "",
                                                         min = 0.1,
                                                         max = 2,
                                                         value = 1)
                                      ),
                                    ),
                                    fluidRow(
                                      column(3,
                                             h4("Minimum conservation reference size"),
                                             style = "margin-top: 25px;"
                                      ),
                                      column(3,
                                               sliderInput("MCRS_Roundfish","",
                                                           min = 1,
                                                           max = 100,
                                                           value = 35)
                                      ),
                                      column(3,
                                             sliderInput("MCRS_Flatfish","",
                                                         min = 1,
                                                         max = 100,
                                                         value = 27)
                                      ),
                                      column(3,
                                             sliderInput("MCRS_Crustacean","",
                                                         min = 0.1,
                                                         max = 10,
                                                         value = 3.2)
                                      ),
                                    )
                                  ),
                                  fluidRow(
                                    column(4,
                                           plotOutput("PopulationPlot_Roundfish", height = "auto"),
                                           style= "height:400px"
                                           ),
                                    column(4,
                                           plotOutput("PopulationPlot_Flatfish", height = "auto"),
                                           style= "height:400px"
                                    ),
                                    column(4,
                                           plotOutput("PopulationPlot_Crustacean", height = "auto"),
                                           style= "height:400px"
                                    )
                                  )
                                  
                         )
)
)
  
server <-shinyServer(function(input,output, session) {
  output$OAImages <- renderUI({

    img(src=paste0('Mesh', input$OpeningAngle, '.jpg'), style=paste0("width: ", input$MeshSize*1.5,"px; display: block; margin-left: auto; margin-right: auto;"))

  })
  
  
  DF.l50.sr.Roundfish_codend <- data<-read.table("./Data/Roundfish_diamonds_r_input.txt", header = T, sep = "\t", dec = ".")
  DF.l50.sr.Flatfish_codend <- data<-read.table("./Data/Flatfish_diamonds_r_input.txt", header = T, sep = "\t", dec = ".")
  DF.l50.sr.Crustacean_codend <- data<-read.table("./Data/Crustacean_diamonds_r_input_fixed.txt", header = T, sep = "\t", dec = ".")
  DF.l50.sr.Roundfish_grid <- data<-read.table("./Data/Roundfish_grid_r_input.txt", header = T, sep = "\t", dec = ".")
  DF.l50.sr.Flatfish_grid <- data<-read.table("./Data/Flatfish_grid_r_input.txt", header = T, sep = "\t", dec = ".")
  DF.l50.sr.Crustacean_grid <- data<-read.table("./Data/Crustacean_grid_r_input.txt", header = T, sep = "\t", dec = ".")
  
  DF <- reactive({
    DF.Roundfish<- data.frame(Species = "Roundfish", Length = seq(0.5,100.5,0.1))
    DF.Flatfish<- data.frame(Species = "Flatfish", Length = seq(0.5,90.5,0.1))
    DF.Crustacean<- data.frame(Species = "Crustacean", Length = seq(0.05,8.05,0.01))
    
    L50.codend.Roundfish<-DF.l50.sr.Roundfish_codend[DF.l50.sr.Roundfish_codend$m==input$MeshSize & DF.l50.sr.Roundfish_codend$oa==input$OpeningAngle,]$l50
    SR.codend.Roundfish<-DF.l50.sr.Roundfish_codend[DF.l50.sr.Roundfish_codend$m==input$MeshSize & DF.l50.sr.Roundfish_codend$oa==input$OpeningAngle,]$sr
    L50.codend.Flatfish<-DF.l50.sr.Flatfish_codend[DF.l50.sr.Flatfish_codend$m==input$MeshSize & DF.l50.sr.Flatfish_codend$oa==input$OpeningAngle,]$l50
    SR.codend.Flatfish<-DF.l50.sr.Flatfish_codend[DF.l50.sr.Flatfish_codend$m==input$MeshSize & DF.l50.sr.Flatfish_codend$oa==input$OpeningAngle,]$sr
    L50.codend.Crustacean<-DF.l50.sr.Crustacean_codend[DF.l50.sr.Crustacean_codend$m==input$MeshSize & DF.l50.sr.Crustacean_codend$oa==input$OpeningAngle,]$l50/10
    SR.codend.Crustacean<-DF.l50.sr.Crustacean_codend[DF.l50.sr.Crustacean_codend$m==input$MeshSize & DF.l50.sr.Crustacean_codend$oa==input$OpeningAngle,]$sr/10
    
    L50.grid.Roundfish<-DF.l50.sr.Roundfish_grid[DF.l50.sr.Roundfish_grid$bar_dist==input$BarSpacing,]$l50
    SR.grid.Roundfish<-DF.l50.sr.Roundfish_grid[DF.l50.sr.Roundfish_grid$bar_dist==input$BarSpacing,]$sr
    L50.grid.Flatfish<-DF.l50.sr.Flatfish_grid[DF.l50.sr.Flatfish_grid$bar_dist==input$BarSpacing,]$l50
    SR.grid.Flatfish<-DF.l50.sr.Flatfish_grid[DF.l50.sr.Flatfish_grid$bar_dist==input$BarSpacing,]$sr
    L50.grid.Crustacean<-DF.l50.sr.Crustacean_grid[DF.l50.sr.Crustacean_grid$bar_dist==input$BarSpacing,]$l50/10
    SR.grid.Crustacean<-DF.l50.sr.Crustacean_grid[DF.l50.sr.Crustacean_grid$bar_dist==input$BarSpacing,]$sr/10
    
    DF.Roundfish$Prop_Codend <- (exp((log(9)/SR.codend.Roundfish)*(DF.Roundfish$Length-L50.codend.Roundfish)))/(1+(exp((log(9)/SR.codend.Roundfish)*(DF.Roundfish$Length-L50.codend.Roundfish))))
    DF.Flatfish$Prop_Codend <- (exp((log(9)/SR.codend.Flatfish)*(DF.Flatfish$Length-L50.codend.Flatfish)))/(1+(exp((log(9)/SR.codend.Flatfish)*(DF.Flatfish$Length-L50.codend.Flatfish))))
    DF.Crustacean$Prop_Codend <- (exp((log(9)/SR.codend.Crustacean)*(DF.Crustacean$Length-L50.codend.Crustacean)))/(1+(exp((log(9)/SR.codend.Crustacean)*(DF.Crustacean$Length-L50.codend.Crustacean))))
    
    
    if(input$GridInGear==0){
      DF.Roundfish$Prop_Grid<- 1-(exp((log(9)/SR.grid.Roundfish)*(DF.Roundfish$Length-L50.grid.Roundfish)))/(1+(exp((log(9)/SR.grid.Roundfish)*(DF.Roundfish$Length-L50.grid.Roundfish))))
      DF.Flatfish$Prop_Grid <- 1-(exp((log(9)/SR.grid.Flatfish)*(DF.Flatfish$Length-L50.grid.Flatfish)))/(1+(exp((log(9)/SR.grid.Flatfish)*(DF.Flatfish$Length-L50.grid.Flatfish))))
      DF.Crustacean$Prop_Grid <- 1-(exp((log(9)/SR.grid.Crustacean)*(DF.Crustacean$Length-L50.grid.Crustacean)))/(1+(exp((log(9)/SR.grid.Crustacean)*(DF.Crustacean$Length-L50.grid.Crustacean))))
      
    } else{
      DF.Roundfish$Prop_Grid<- 1
      DF.Flatfish$Prop_Grid <- 1
      DF.Crustacean$Prop_Grid <- 1
    }
    
    DF.Roundfish$L50 <- DF.l50.sr.Roundfish_codend[DF.l50.sr.Roundfish_codend$m==input$MeshSize & DF.l50.sr.Roundfish_codend$oa==input$OpeningAngle,]$l50
    DF.Flatfish$L50 <- DF.l50.sr.Flatfish_codend[DF.l50.sr.Flatfish_codend$m==input$MeshSize & DF.l50.sr.Flatfish_codend$oa==input$OpeningAngle,]$l50
    DF.Crustacean$L50 <- DF.l50.sr.Crustacean_codend[DF.l50.sr.Crustacean_codend$m==input$MeshSize & DF.l50.sr.Crustacean_codend$oa==input$OpeningAngle,]$l50/10
    
    DF.Roundfish$SR <- DF.l50.sr.Roundfish_codend[DF.l50.sr.Roundfish_codend$m==input$MeshSize & DF.l50.sr.Roundfish_codend$oa==input$OpeningAngle,]$sr
    DF.Flatfish$SR <- DF.l50.sr.Flatfish_codend[DF.l50.sr.Flatfish_codend$m==input$MeshSize & DF.l50.sr.Flatfish_codend$oa==input$OpeningAngle,]$sr
    DF.Crustacean$SR <- DF.l50.sr.Crustacean_codend[DF.l50.sr.Crustacean_codend$m==input$MeshSize & DF.l50.sr.Crustacean_codend$oa==input$OpeningAngle,]$sr/10
    
    DF<-rbind(DF.Roundfish, DF.Flatfish, DF.Crustacean)
    
    return(DF)
  })
  
  output$SelectivityPlot <- renderPlot({

    
    Plot.title.lab<-paste0("Roundfish (L50 ",DF.l50.sr.Roundfish_codend[DF.l50.sr.Roundfish_codend$m==input$MeshSize & DF.l50.sr.Roundfish_codend$oa==input$OpeningAngle,]$l50, " SR ",  DF.l50.sr.Roundfish_codend[DF.l50.sr.Roundfish_codend$m==input$MeshSize & DF.l50.sr.Roundfish_codend$oa==input$OpeningAngle,]$sr,")   ",
                           " Flatfish (L50 ",DF.l50.sr.Flatfish_codend[DF.l50.sr.Flatfish_codend$m==input$MeshSize & DF.l50.sr.Flatfish_codend$oa==input$OpeningAngle,]$l50, " SR ",  DF.l50.sr.Flatfish_codend[DF.l50.sr.Flatfish_codend$m==input$MeshSize & DF.l50.sr.Flatfish_codend$oa==input$OpeningAngle,]$sr,")   ",
                           "\n", " Crustacean (L50 ",DF.l50.sr.Crustacean_codend[DF.l50.sr.Crustacean_codend$m==input$MeshSize & DF.l50.sr.Crustacean_codend$oa==input$OpeningAngle,]$l50, " SR ",  DF.l50.sr.Crustacean_codend[DF.l50.sr.Crustacean_codend$m==input$MeshSize & DF.l50.sr.Crustacean_codend$oa==input$OpeningAngle,]$sr,")")
    
    if(input$GridInGear==0){
      ggplot(DF(), aes(x=Length, y=Prop_Grid))+
        geom_line(colour="red", size=1.5)+
        geom_line(aes(Length, Prop_Codend), colour="blue", size=1.5)+
        labs(x="Length", y="Retention propability") +
        ylim(0, 1)+
        facet_wrap(~Species, scales = "free", dir = "v")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = 'white', colour = "black"),
              axis.title = element_text(face="bold", size=20),
              axis.text = element_text(size=16),
              plot.title = element_text(hjust = 0.5),
              strip.text.x = element_text(size=20))
        
    } else{
      ggplot(DF(), aes(x=Length, y=Prop_Grid))+
        geom_line(aes(Length, Prop_Codend), colour="blue", size=1.5)+
        labs(x="Length", y="Retention propability", title=Plot.title.lab) +
        ylim(0, 1)+
        facet_wrap(~Species, scales = "free", dir = "v")+ 
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = 'white', colour = "black"),
              axis.title = element_text(face="bold", size=20),
              axis.text = element_text(size=16),
              plot.title = element_text(hjust = 0.5),
              strip.text.x = element_text(size=20))
    }
    
    
    
  }, height=function() { session$clientData$output_SelectivityPlot_width * 0.6 })
  
  output$OverallSelectivityPlot <- renderPlot({
    
    if(input$GridInGear==0){
      ggplot(DF(), aes(x=Length, y=Prop_Codend*Prop_Grid))+
        geom_line(colour="black", size=1.5)+
        labs(x="Length", y="Retention propability") +
        ylim(0, 1)+
        facet_wrap(~Species, scales = "free", dir = "v")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = 'white', colour = "black"),
              axis.title = element_text(face="bold", size=20),
              axis.text = element_text(size=16),
              plot.title = element_text(hjust = 0.5),
              strip.text.x = element_text(size=20))
    } else{
      ggplot(DF(), aes(x=Length, y=Prop_Codend))+
        geom_line(colour="black", size=1.5)+
        labs(x="Length", y="Retention propability") +
        ylim(0, 1)+
        facet_wrap(~Species, scales = "free", dir = "v")+
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(fill = 'white', colour = "black"),
              axis.title = element_text(face="bold", size=20),
              axis.text = element_text(size=16),
              plot.title = element_text(hjust = 0.5),
              strip.text.x = element_text(size=20))
    }
    
    
    
  }, height=function() { session$clientData$output_OverallSelectivityPlot_width * 0.6 })
  
  Population <- reactive({
    PopulationStructure.Roundfish<- data.frame(Species = "Roundfish", Length = seq(0.5,100.5,0.1), PopProp= dnorm(seq(0.5,100.5,0.1), mean=input$PopAverageLength_Roundfish, sd=input$PopSD_Roundfish))
    PopulationStructure.Flatfish<- data.frame(Species = "Flatfish", Length = seq(0.5,90.5,0.1), PopProp= dnorm(seq(0.5,90.5,0.1), mean=input$PopAverageLength_Flatfish, sd=input$PopSD_Flatfish))
    PopulationStructure.Crustacean<- data.frame(Species = "Crustacean", Length = seq(0.05,8.05,0.01), PopProp= dnorm(seq(0.05,8.05,0.01), mean=input$PopAverageLength_Crustacean, sd=input$PopSD_Crustacean))
    
    PopulationStructure<- rbind(PopulationStructure.Roundfish, PopulationStructure.Flatfish, PopulationStructure.Crustacean)
    PopulationStructure$PopNum <- PopulationStructure$PopProp*100000
    return(PopulationStructure)
  })
  
  ####Roundfish
  PopulationCaught_Roundfish <- reactive({
    req(Population())
    req(DF())
    DF<-DF()[DF()$Species == "Roundfish",]
    PopulationCaughtStructure <- Population()[Population()$Species == "Roundfish",]

    PopulationCaughtStructure$Catch <- DF$Prop_Codend*DF$Prop_Grid*PopulationCaughtStructure$PopNum

    return(PopulationCaughtStructure)
  })

  DiscardShade_Roundfish <- reactive({
    req(PopulationCaught_Roundfish())
    temp<-PopulationCaught_Roundfish()
    temp[temp$Length >= input$MCRS_Roundfish,]<-NA

    return(temp)
  })

  CatchShade_Roundfish <- reactive({
    req(PopulationCaught_Roundfish())
    temp<-PopulationCaught_Roundfish()
    temp[temp$Length < input$MCRS_Roundfish,]<-NA

    return(temp)
  })

  CatchLossShade_Roundfish <- reactive({
    req(Population())
    req(PopulationCaught_Roundfish())
    temp<-PopulationCaught_Roundfish()
    temp<-cbind(temp,Population()[Population()$Species == "Roundfish","PopNum"])
    temp[temp$Length < input$MCRS_Roundfish,]<-NA

    return(temp)
  })

  CommercialCatch_Roundfish <- reactive({
    req(PopulationCaught_Roundfish())
    CC<-sum(PopulationCaught_Roundfish()[PopulationCaught_Roundfish()$Length >= input$MCRS_Roundfish, ]$Catch)/sum(PopulationCaught_Roundfish()$Catch)
    return(CC)
  })

  UnwantedCatch_Roundfish <- reactive({
    req(PopulationCaught_Roundfish())
    UC<-sum(PopulationCaught_Roundfish()[PopulationCaught_Roundfish()$Length < input$MCRS_Roundfish, ]$Catch)/sum(PopulationCaught_Roundfish()$Catch)
    return(UC)
  })

  LossOfCommercialCatch_Roundfish <- reactive({
    req(Population())
    req(PopulationCaught_Roundfish())
    LCC<-1-sum(PopulationCaught_Roundfish()[PopulationCaught_Roundfish()$Length >= input$MCRS_Roundfish, ]$Catch)/sum(Population()[Population()$Species == "Roundfish" & Population()$Length >= input$MCRS_Roundfish, ]$PopNum)
    return(LCC)
  })

####Flatfish  
  PopulationCaught_Flatfish <- reactive({
    req(Population())
    req(DF())
    DF<-DF()[DF()$Species == "Flatfish",]
    PopulationCaughtStructure <- Population()[Population()$Species == "Flatfish",]

    PopulationCaughtStructure$Catch <- DF$Prop_Codend*DF$Prop_Grid*PopulationCaughtStructure$PopNum

    return(PopulationCaughtStructure)
  })

  DiscardShade_Flatfish <- reactive({
    req(PopulationCaught_Flatfish())
    temp<-PopulationCaught_Flatfish()
    temp[temp$Length >= input$MCRS_Flatfish,]<-NA

    return(temp)
  })

  CatchShade_Flatfish <- reactive({
    req(PopulationCaught_Flatfish())
    temp<-PopulationCaught_Flatfish()
    temp[temp$Length < input$MCRS_Flatfish,]<-NA

    return(temp)
  })

  CatchLossShade_Flatfish <- reactive({
    req(Population())
    req(PopulationCaught_Flatfish())
    temp<-PopulationCaught_Flatfish()
    temp<-cbind(temp,Population()[Population()$Species == "Flatfish","PopNum"])
    temp[temp$Length < input$MCRS_Flatfish,]<-NA

    return(temp)
  })

  CommercialCatch_Flatfish <- reactive({
    req(PopulationCaught_Flatfish())
    CC<-sum(PopulationCaught_Flatfish()[PopulationCaught_Flatfish()$Length >= input$MCRS_Flatfish, ]$Catch)/sum(PopulationCaught_Flatfish()$Catch)
    return(CC)
  })

  UnwantedCatch_Flatfish <- reactive({
    req(PopulationCaught_Flatfish())
    UC<-sum(PopulationCaught_Flatfish()[PopulationCaught_Flatfish()$Length < input$MCRS_Flatfish, ]$Catch)/sum(PopulationCaught_Flatfish()$Catch)
    return(UC)
  })

  LossOfCommercialCatch_Flatfish <- reactive({
    req(Population())
    req(PopulationCaught_Flatfish())
    LCC<-1-sum(PopulationCaught_Flatfish()[PopulationCaught_Flatfish()$Length >= input$MCRS_Flatfish, ]$Catch)/sum(Population()[Population()$Species == "Flatfish" & Population()$Length >= input$MCRS_Flatfish, ]$PopNum)
    return(LCC)
  })
  
  ####Crustacean  
  PopulationCaught_Crustacean <- reactive({
    req(Population())
    req(DF())
    DF<-DF()[DF()$Species == "Crustacean",]
    PopulationCaughtStructure <- Population()[Population()$Species == "Crustacean",]

    PopulationCaughtStructure$Catch <- DF$Prop_Codend*DF$Prop_Grid*PopulationCaughtStructure$PopNum

    return(PopulationCaughtStructure)
  })

  DiscardShade_Crustacean <- reactive({
    req(PopulationCaught_Crustacean())
    temp<-PopulationCaught_Crustacean()
    temp[temp$Length >= input$MCRS_Crustacean,]<-NA

    return(temp)
  })

  CatchShade_Crustacean <- reactive({
    req(PopulationCaught_Crustacean())
    temp<-PopulationCaught_Crustacean()
    temp[temp$Length < input$MCRS_Crustacean,]<-NA

    return(temp)
  })

  CatchLossShade_Crustacean <- reactive({
    req(Population())
    req(PopulationCaught_Crustacean())
    temp<-PopulationCaught_Crustacean()
    temp<-cbind(temp,Population()[Population()$Species == "Crustacean","PopNum"])
    temp[temp$Length < input$MCRS_Crustacean,]<-NA

    return(temp)
  })

  CommercialCatch_Crustacean <- reactive({
    req(PopulationCaught_Crustacean())
    CC<-sum(PopulationCaught_Crustacean()[PopulationCaught_Crustacean()$Length >= input$MCRS_Crustacean, ]$Catch)/sum(PopulationCaught_Crustacean()$Catch)
    return(CC)
  })

  UnwantedCatch_Crustacean <- reactive({
    req(PopulationCaught_Crustacean())
    UC<-sum(PopulationCaught_Crustacean()[PopulationCaught_Crustacean()$Length < input$MCRS_Crustacean, ]$Catch)/sum(PopulationCaught_Crustacean()$Catch)
    return(UC)
  })

  LossOfCommercialCatch_Crustacean <- reactive({
    req(Population())
    req(PopulationCaught_Crustacean())
    LCC<-1-sum(PopulationCaught_Crustacean()[PopulationCaught_Crustacean()$Length >= input$MCRS_Crustacean, ]$Catch)/sum(Population()[Population()$Species == "Crustacean" & Population()$Length >= input$MCRS_Crustacean, ]$PopNum)
    return(LCC)
  })
  
  output$PopulationPlot_Roundfish <- renderPlot({

    plot<-ggplot(Population()[Population()$Species == "Roundfish",], aes(x=Length, y=PopNum))+
      geom_line(data=PopulationCaught_Roundfish()[PopulationCaught_Roundfish()$Species == "Roundfish",], aes(x=Length, y=Catch), colour="#669933", size=1.5)+
      geom_ribbon(data= DiscardShade_Roundfish()[DiscardShade_Roundfish()$Species == "Roundfish",], aes(ymin=0 , ymax = Catch), fill="#FF0000", alpha=0.8)+
      geom_ribbon(data= CatchShade_Roundfish()[CatchShade_Roundfish()$Species == "Roundfish",], aes(ymin=0 , ymax = Catch), fill="#669933", alpha=0.8)+
      geom_ribbon(data= CatchLossShade_Roundfish()[CatchLossShade_Roundfish()$Species == "Roundfish",], aes(ymin=Catch , ymax = PopNum), fill="#0066FF", alpha=0.8)+
      geom_vline(xintercept =input$MCRS_Roundfish, linetype="dotted", size=1, colour="red") +
      labs(x="Length", y="Population/Catch structure") +
      geom_text(aes(x=input$MCRS_Roundfish+3, label="MCRS", y=1.05*max(Population()[Population()$Species == "Roundfish",]$PopNum)), colour="red") +
      geom_line(colour="black", size=1.5)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = 'white', colour = "black"),
            axis.title = element_text(face="bold", size=20),
            axis.text = element_text(size=16),
            plot.title = element_text(hjust = 0.5))


    print(plot, vp=viewport(height=0.8))


    grid.text(0.08, unit(1,"npc") - unit(1,"line"),
              label=paste0("Commercial catch: ", round(CommercialCatch_Roundfish()*100, digits = 1),"%\n"),
              gp=gpar(col="#669933",fontsize=session$clientData$output_PopulationPlot_Roundfish_width*0.025),just = "left")
    grid.text(0.08, unit(1,"npc") - unit(1.3,"line"),
              label=paste0("\nUnwanted catch: ", round(UnwantedCatch_Roundfish()*100, digits = 1),"%\n"),
              gp=gpar(col="#FF0000",fontsize=session$clientData$output_PopulationPlot_Roundfish_width*0.025),just = "left")
    grid.text(0.08, unit(1,"npc") - unit(1.6,"line"),
              label=paste0("\nLoss of commercial catch: ", round(LossOfCommercialCatch_Roundfish()*100, digits = 1),"%"),
              gp=gpar(col="#0066FF",fontsize=session$clientData$output_PopulationPlot_Roundfish_width*0.025),just = "left")

  }, height=function() { session$clientData$output_PopulationPlot_Roundfish_width * 0.75 })

  
  output$PopulationPlot_Flatfish <- renderPlot({
    
    plot<-ggplot(Population()[Population()$Species == "Flatfish",], aes(x=Length, y=PopNum))+
      geom_line(data=PopulationCaught_Flatfish()[PopulationCaught_Flatfish()$Species == "Flatfish",], aes(x=Length, y=Catch), colour="#669933", size=1.5)+
      geom_ribbon(data= DiscardShade_Flatfish()[DiscardShade_Flatfish()$Species == "Flatfish",], aes(ymin=0 , ymax = Catch), fill="#FF0000", alpha=0.8)+
      geom_ribbon(data= CatchShade_Flatfish()[CatchShade_Flatfish()$Species == "Flatfish",], aes(ymin=0 , ymax = Catch), fill="#669933", alpha=0.8)+
      geom_ribbon(data= CatchLossShade_Flatfish()[CatchLossShade_Flatfish()$Species == "Flatfish",], aes(ymin=Catch , ymax = PopNum), fill="#0066FF", alpha=0.8)+
      geom_vline(xintercept =input$MCRS_Flatfish, linetype="dotted", size=1, colour="red") +
      labs(x="Length", y="Population/Catch structure") +
      geom_text(aes(x=input$MCRS_Flatfish+3, label="MCRS", y=1.05*max(Population()[Population()$Species == "Flatfish",]$PopNum)), colour="red") +
      geom_line(colour="black", size=1.5)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = 'white', colour = "black"),
            axis.title = element_text(face="bold", size=20),
            axis.text = element_text(size=16),
            plot.title = element_text(hjust = 0.5))
    
    
    print(plot, vp=viewport(height=0.8))
    
    
    grid.text(0.08, unit(1,"npc") - unit(1,"line"),
              label=paste0("Commercial catch: ", round(CommercialCatch_Flatfish()*100, digits = 1),"%\n"),
              gp=gpar(col="#669933",fontsize=session$clientData$output_PopulationPlot_Flatfish_width*0.025),just = "left")
    grid.text(0.08, unit(1,"npc") - unit(1.3,"line"),
              label=paste0("\nUnwanted catch: ", round(UnwantedCatch_Flatfish()*100, digits = 1),"%\n"),
              gp=gpar(col="#FF0000",fontsize=session$clientData$output_PopulationPlot_Flatfish_width*0.025),just = "left")
    grid.text(0.08, unit(1,"npc") - unit(1.6,"line"),
              label=paste0("\nLoss of commercial catch: ", round(LossOfCommercialCatch_Flatfish()*100, digits = 1),"%"),
              gp=gpar(col="#0066FF",fontsize=session$clientData$output_PopulationPlot_Flatfish_width*0.025),just = "left")
    
  }, height=function() { session$clientData$output_PopulationPlot_Flatfish_width * 0.75 })
  
  
  output$PopulationPlot_Crustacean <- renderPlot({
    
    plot<-ggplot(Population()[Population()$Species == "Crustacean",], aes(x=Length, y=PopNum))+
      geom_line(data=PopulationCaught_Crustacean()[PopulationCaught_Crustacean()$Species == "Crustacean",], aes(x=Length, y=Catch), colour="#669933", size=1.5)+
      geom_ribbon(data= DiscardShade_Crustacean()[DiscardShade_Crustacean()$Species == "Crustacean",], aes(ymin=0 , ymax = Catch), fill="#FF0000", alpha=0.8)+
      geom_ribbon(data= CatchShade_Crustacean()[CatchShade_Crustacean()$Species == "Crustacean",], aes(ymin=0 , ymax = Catch), fill="#669933", alpha=0.8)+
      geom_ribbon(data= CatchLossShade_Crustacean()[CatchLossShade_Crustacean()$Species == "Crustacean",], aes(ymin=Catch , ymax = PopNum), fill="#0066FF", alpha=0.8)+
      geom_vline(xintercept =input$MCRS_Crustacean, linetype="dotted", size=1, colour="red") +
      labs(x="Length", y="Population/Catch structure") +
      geom_text(aes(x=input$MCRS_Crustacean+0.3, label="MCRS", y=1.05*max(Population()[Population()$Species == "Crustacean",]$PopNum)), colour="red") +
      geom_line(colour="black", size=1.5)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = 'white', colour = "black"),
            axis.title = element_text(face="bold", size=20),
            axis.text = element_text(size=16),
            plot.title = element_text(hjust = 0.5))
    
    
    print(plot, vp=viewport(height=0.8))
    
    
    grid.text(0.08, unit(1,"npc") - unit(1,"line"),
              label=paste0("Commercial catch: ", round(CommercialCatch_Crustacean()*100, digits = 1),"%\n"),
              gp=gpar(col="#669933",fontsize=session$clientData$output_PopulationPlot_Crustacean_width*0.025),just = "left")
    grid.text(0.08, unit(1,"npc") - unit(1.3,"line"),
              label=paste0("\nUnwanted catch: ", round(UnwantedCatch_Crustacean()*100, digits = 1),"%\n"),
              gp=gpar(col="#FF0000",fontsize=session$clientData$output_PopulationPlot_Crustacean_width*0.025),just = "left")
    grid.text(0.08, unit(1,"npc") - unit(1.6,"line"),
              label=paste0("\nLoss of commercial catch: ", round(LossOfCommercialCatch_Crustacean()*100, digits = 1),"%"),
              gp=gpar(col="#0066FF",fontsize=session$clientData$output_PopulationPlot_Crustacean_width*0.025),just = "left")
    
  }, height=function() { session$clientData$output_PopulationPlot_Crustacean_width * 0.75 })
  
  
})

shinyApp(ui = ui, server = server)
