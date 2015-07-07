library(shiny)
library(DT)
library(devtools)
if (!require(rCharts)) {
  install_github('rCharts', 'ramnathv')
  library(rCharts)
}
library(reshape2)
library(plyr)
library(dplyr)
library(ggplot2)
library(stringr)

pd <- idata$total_mmet
pdl <- NULL

shinyServer(function(input, output, session){
  
  plotTables <- reactive({
    (input$scenario != 'none')
  })
  
  plotDataTable<- reactive({
    if (input$scenario == 't')
      data <- tdata
    else
      data <- idata
    if (input$ag != 'All'){
      data <- subset(data, age_group == input$ag)
    }
    if (input$gender != 3)
      data <- subset(data, Sex_B01ID %in% input$gender)
    
    if (input$ses != "All"){
      data <- subset(data, NSSec_B03ID %in% input$ses)
    }
    
    if (input$ethnicity != "All"){
      data <- subset(data, EthGroupTS_B02ID %in% input$ethnicity)
    }
    data[is.na(data)] <- 0
    
    pd <<- data
  })
  
  output$plotMode <- renderPlot({
    plotDataTable()
    if (!is.null(pd)){
      if (input$scenario == 't'){
        filtered_title <- getFilteredTitle(tdata)
        bcounts <- count(tdata, "MainMode_reduced_val")
        
        bcounts[["Total Population"]] <- bcounts$freq / sum(bcounts$freq) * 100
        bcounts$freq <- NULL
        
        scounts <- count(pd, "MainMode_reduced_val")
        scounts$freq1 <- scounts$freq / sum(scounts$freq) * 100
        
        ecounts <- data.frame(v1=bcounts[["Total Population"]], Filtered_Frequency=scounts[match(bcounts$MainMode_reduced_val, scounts$MainMode_reduced_val), 3])

        bcounts[["Selected Population"]] <- ecounts$Filtered_Frequency
        
        df.long<-melt(bcounts)

        print(ggplot(df.long,aes(MainMode_reduced_val,value,fill=variable)) + ggtitle(paste("Main Mode: Total population versus population selected for scenario \n(selected population currently defined as ", filtered_title, ")", sep = "")) 
              + geom_bar(stat="identity",position="dodge") 
              + labs(x = "", y = "Frequency (%) ") + theme(text = element_text(size = 14)))
        
      }

      else{
        filtered_title <- getFilteredTitle(idata)
        max_val <- max(pd$total_mmet)
        if (max_val < 60){
          if (max_val > 5){
            hist(pd$total_mmet, xlab = "Total Marginal MET", main = paste("Total Marginal MET of population selected for scenario \n(selected population currently defined as: ",filtered_title, ")", sep = ""),
                 breaks = c(seq(min(pd$total_mmet), ceiling(max(pd$total_mmet)), by = 5), max(pd$total_mmet)))
          }else{
            hist(pd$total_mmet, xlab = "Total Marginal MET", main = paste("Total Marginal MET of population selected for scenario \n(selected population currently defined as: ",filtered_title, ")", sep = ""))
          }
        }
        else{
          hist(pd$total_mmet, xlab = "Total Marginal MET", main = paste("Total Marginal MET of population selected for scenario \n(selected population currently defined as: ",filtered_title, ")", sep = ""),
               breaks = c(seq(min(pd$total_mmet), 60, by = 5),max(pd$total_mmet)), xlim = c(min(pd$total_mmet), 60), right=FALSE)
        }
      }
    }
    
  })

  output$plotBaseline <- renderPlot({
    if (!is.null(tdata)){
      if (input$scenario == 'i'){
        hist(idata$total_mmet, xlab = "Total Marginal MET", main = "Total Marginal MET of Total Population",
             breaks = c(seq(min(idata$total_mmet), 60, by = 5),max(idata$total_mmet)), xlim = c(min(idata$total_mmet), 60), right=FALSE)
      }
    }
    
  })

  getFilteredTitle <- function(data){
    filtered_title <- "total population (baseline)"
    if (nrow(data) != nrow (pd)){
      
      displayGender <- "All"
      if (input$gender == 1){
        displayGender <- "Male"
      }else if (input$gender == 2){
        displayGender <- "Female"
      }
      
      displayEthnicity <- "All"
      if (input$ethnicity == 1){
        displayEthnicity <- "White"
      }else if (input$ethnicity == 2){
        displayEthnicity <- "Non-White"
      }
      
      ses <- c("All" = "All",
               "Managerial and professional occupations" = 1,
               "Intermediate occupations and small employers" = 2,
               "Routine and manual occupations" = 3,
               "Never worked and long-term unemployed" = 4,
               "Not classified (including students)" = 5)      
      
      displaySES <- "All"
      if (input$ses == 1){
        displaySES <- "Managerial and professional occupations"
      }else if (input$ses == 2){
        displaySES <- "Intermediate occupations and small employers"
      }else if (input$ses == 3){
        displaySES <- "Routine and manual occupations"
      }else if (input$ses == 4){
        displaySES <- "Never worked and long-term unemployed"
      }else if (input$ses == 5){
        displaySES <- "Not classified (including students)"
      }
      
      filtered_title <- paste("Age Group: ", str_trim(input$ag), ", Gender: ", displayGender, " \n Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity, sep = "" )
      filtered_title
    }else
      filtered_title
  }

  
  generateScenarioTable<- reactive({
    
#     lMS <- input$inMS
    lTDR <- input$inTDR
    lEB <- input$inEB
    lEQ <- input$inEQ
    
    data <- sdata
#     if (lMS != "All")
#       data <- subset(data, MS == lMS)# & TDR == lTDR & equity == lEQ & ebike == lEB)
    if (lTDR != "All")
      data <- subset(data, TDR == lTDR)
    if (lEQ != "All")
      data <- subset(data, equity == lEQ)
    if (lEB != "All")
      data <- subset(data, ebike == lEB)
    
    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    # data[order(Age),]
    scdata <<- data
  })

  reactiveFunction <- reactive({
  
    lTDR <- input$inTDR
    lEB <- input$inEB
    lEQ <- input$inEQ
    
  })
  
  #   Solid
  #   ShortDash
  #   ShortDot
  #   ShortDashDot
  #   ShortDashDotDot
  #   Dot
  #   Dash
  #   LongDash
  #   DashDot
  #   LongDashDot
  #   LongDashDotDot
  
  genericPlot <- function(var){
#     reactiveFunction()
#     cat("start", nrow(scdata), "\n")
#     if (!is.null(sdata)){}
#     cat(input$inTDR,  " : ", input$inEB, " : ", input$inEQ, "\n")
    #if (input$inTDR == "All" & input$inMS == "All"){
#     input$inTDR
#     input$inEB
#     input$inEQ
    
    h1 <- Highcharts$new()
    h1$chart(type = "spline")
    # types of charts: http://api.highcharts.com/highcharts#plotOptions
    h1$yAxis(title = list(text = var))
    #h1$xAxis(categories = sort(unique(sdata$MS), decreasing = F)[-1], title = list(text = 'Cycling Multiplier'))
    h1$xAxis(categories = sort(unique(sdata$MS), decreasing = F), title = list(text = 'Cycling Multiplier'))
    #h1$legend(title = list(text = "(Click to disable/enable splines)"))
    #h1$legend(symbolWidth = 80)
    
    if (input$inTDR == "All"){                 # & input$inEB == "All" & input$inEQ == "All"){
      if (input$inEB != "All" & input$inEQ != "All"){
        sub1 <- subset(scdata, TDR == 0.7 & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
      }
      
      if (input$inEB == "All" & input$inEQ != "All"){
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
      }
      
      
      if (input$inEB != "All" & input$inEQ == "All"){
        sub1 <- subset(scdata, TDR == 0.7 & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB ", input$inEB, " and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.7 & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.7 (EB ", input$inEB, " and EQ 1)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB", input$inEB, " and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.8 & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.8 (EB ", input$inEB, " and EQ 1)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB ", input$inEB, " and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == 0.9 & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 0.9 (EB ", input$inEB, " and EQ 1)" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB ", input$inEB, "and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == 1.0 & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR 1.0 (EB ", input$inEB, "and EQ 1)" , sep = ""))
      }
      
      if (input$inEB == "All" & input$inEQ == "All"){
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.7 (EB 0 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.7 (EB 0 and EQ 1)")
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.7 (EB 1 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.7 & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.7 (EB 1 and EQ 1)")
        
        
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.8 (EB 0 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.8 (EB 0 and EQ 1)")
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.8 (EB 1 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.8 & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.8 (EB 1 and EQ 1)")
        
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.9 (EB 0 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.9 (EB 0 and EQ 1)")
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 0.9 (EB 1 and EQ 0)")
        sub1 <- subset(scdata, TDR == 0.9 & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 0.9 (EB 1 and EQ 1)")
        
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 1.0 (EB 0 and EQ 0)")
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 1.0 (EB 0 and EQ 1)")
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = "TDR 1.0 (EB 1 and EQ 0)")
        sub1 <- subset(scdata, TDR == 1.0 & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = "TDR 1.0 (EB 1 and EQ 1)")
        
        
      }
      
      
      
    }else{
      
      
      if (input$inEB != "All" & input$inEQ != "All"){
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == as.numeric(input$inEB) & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB ", input$inEB, " and EQ ", input$inEQ, ")" , sep = ""))
        
      }
      
      if (input$inEB == "All" & input$inEQ != "All"){
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 0 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 0 and EQ ", input$inEQ, ")" , sep = ""))
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 1 & equity == as.numeric(input$inEQ))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 1 and EQ ", input$inEQ, ")" , sep = ""))
        
      }
      
      
      if (input$inEB != "All" & input$inEQ == "All"){
        sub1 <- subset(scdata, TDR == input$inTDR & equity == 0 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB ", input$inEB, " and EQ 0)" , sep = ""))
        sub1 <- subset(scdata, TDR == input$inTDR & equity == 1 & ebike == as.numeric(input$inEB))
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB ", input$inEB, " and EQ 1)" , sep = ""))
        
      }
      
      if (input$inEB == "All" & input$inEQ == "All"){
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 0 & equity == 0)
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 0 and EQ 0)"))
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 0 & equity == 1)
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 0 and EQ 1)"))
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 1 & equity == 0)
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 1 and EQ 0)"))
        sub1 <- subset(scdata, TDR == input$inTDR & ebike == 1 & equity == 1)
        h1$series(data = sub1[[var]], name = paste("TDR ", input$inTDR, " (EB 1 and EQ 1)"))

      }
      
      
      
      
    }
    
    h1$exporting(enabled = T)
    return(h1)
  }
  
  output$plotCycPercent <- renderChart({
    generateScenarioTable()
    h <- genericPlot("cyclists.perc")
    h$set(dom = 'plotCycPercent')
    return (h)
  })
  
  output$plotCO2R <- renderChart({
    generateScenarioTable()
    h <- genericPlot("CO2R.perc")
    h$set(dom = 'plotCO2R')
    return (h)
    
  })
  
  output$plotCarAccess <- renderChart({
    generateScenarioTable()
    h <- genericPlot("nocar.caraccess")
    h$set(dom = 'plotCarAccess')
    return (h)
  })
  
  output$plotGenericVariable <- renderChart({
    generateScenarioTable()
    #retrieveVariableName()
    h <- genericPlot(input$varname)
    h$set(dom = 'plotGenericVariable')
    return (h)
  })

})
