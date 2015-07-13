library(shiny)
library(DT)
library(devtools)
if (!require(rCharts)) {
  install_github('rCharts', 'ramnathv')
  library(rCharts)
}
library(reshape2)
library(dplyr)
library(plyr)
library(ggplot2)
library(stringr)

pd <- idata$total_mmet
bd <- NULL
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
  
  output$plotMode <- renderChart({
    plotDataTable()
    if (!is.null(pd)){
      if (input$scenario == 't'){
        h1 <- Highcharts$new()
        h1$chart(type = "column")
        h1$plotOptions(column=list(animation=FALSE))
        
        filtered_title <- getFilteredTitle(tdata)
        extended_title <- paste("Main Mode: Total population versus population selected for scenario (selected population currently defined as ", filtered_title, ")", sep = "")
        h1$title(text = extended_title)
        bcounts <- count(tdata, "MainMode_reduced_val")
        h1$xAxis(categories = bcounts[["MainMode_reduced_val"]], title = list(text = 'Main Mode'))#, style = list(font = 'bold 14px')))
        
        #categories = seq(from = 0, to = 100 , by =20)
        h1$tooltip(valueSuffix= '%')
        
        bcounts$tp <- bcounts$freq / sum(bcounts$freq) * 100
        bcounts$tp <- round(bcounts$tp, digits = 1)
        bcounts$freq <- NULL
        
        scounts <- count(pd, "MainMode_reduced_val")
        filter <- FALSE
        if (sum(scounts$freq, na.rm = T) >= 10)
          filter <- TRUE
        
        scounts$freq1 <- scounts$freq / sum(scounts$freq) * 100
        scounts$freq1 <- round(scounts$freq1, digits = 1)
        
        ecounts <- data.frame(v1=bcounts$tp, Filtered_Frequency=scounts[match(bcounts$MainMode_reduced_val, scounts$MainMode_reduced_val), 3])
        
        bcounts[["Total Population"]] <- bcounts$tp
        bcounts$tp <- NULL
        
        bcounts[["Selected Population"]] <- ecounts$Filtered_Frequency
        h1$series(data = bcounts[["Total Population"]], name = "Total Population")
        if(filter){
          h1$series(data = bcounts[["Selected Population"]], name = "Selected Population")
          h1$yAxis(min = 0, max = max(80, max(bcounts[["Total Population"]], na.rm=TRUE), max(bcounts[["Selected Population"]], na.rm=TRUE)), tickInterval = 20, title = list(text = 'Percentage %'))
          
        }else{
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
          h1$yAxis(min = 0, max = max(80, max(bcounts[["Total Population"]], na.rm=TRUE)), tickInterval = 20, title = list(text = 'Percentage %'))
        }
        
        h1$set(dom = 'plotMode')
        h1$exporting(enabled = T)
        return (h1)
      }else{
        filtered_title <- getFilteredTitle(idata)
        
        h1 <- Highcharts$new()
        h1$chart(type = "column")
        h1$plotOptions(column=list(animation=FALSE))
        
        #, >0 >= 4.4, >4.4 >=8.75, >8.75>= 13.2
        #bc <- table (cut (idata$total_mmet, breaks = c(seq(min(idata$total_mmet), 60, by = 5),max(idata$total_mmet)), xlim = c(min(idata$total_mmet), 60)))
        #bc <- table (cut (idata$total_mmet, breaks = c(seq(min(idata$total_mmet), 60, by = 5),max(idata$total_mmet)), xlim = c(min(idata$total_mmet), 60)))
        
        bc <- as.data.frame(table (cut (idata$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(idata$total_mmet)))))
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        #cat("total: ", nrow(as.data.frame(bc)), "\n")
        bc1max <- max(bc$Freq, na.rm = T)
        
#         h1$xAxis(categories = as.list(append(as.numeric( sub("\\D*(\\d+).*", "\\1", bc$Var1[-1])), "60+")), title = list(text = 'Total Marginal MET'))
#         h1$xAxis(categories = as.list(append(str_extract(bc$Var1, "\\d+\\.*\\d*"), "52+")), title = list(text = 'Total Marginal MET'))
        h1$xAxis(categories = as.list(append(c(seq(-4.4,52.8, 4.4))[-1], "52.7+")), title = list(text = 'Total Marginal MET'))

        
        h1$series(data = bc$Freq, name = "Total Population")
        #cat(nrow(pd), "\n")
        max_val <- 0
        if (nrow(pd) > 1)
          max_val <- max(pd$total_mmet, na.rm = T)
        h <- NULL
        if (max_val <= 52.8){
          if (max_val > 5){
            #cat(seq(min(pd$total_mmet), ceiling(max(pd$total_mmet)), by = 5), max(pd$total_mmet), "\n")
            bc <- table (cut (pd$total_mmet, breaks = c(seq(min(pd$total_mmet) - 4.4, ceiling(max(pd$total_mmet) - 4.4), by = 4.4), max(pd$total_mmet))))
            
          }else{
            #cat(c(0, max(pd$total_mmet) + 1), "\n")
            
            bc <- table (cut (pd$total_mmet, breaks = c(-4.4, max(pd$total_mmet) - 4.4 )))
          }
        }
        else{
          #cat(c(seq(min(pd$total_mmet), 55, by = 5),max(pd$total_mmet)), "\n")
          bc <- table (cut (pd$total_mmet, breaks = c(seq(-4.4,52.8, 4.4), max(pd$total_mmet))))
          #bc <- table (cut (pd$total_mmet, breaks = c(seq(min(pd$total_mmet) - 4.4, 52.8, by = 4.4),max(pd$total_mmet) - 4.4)))
        }
        extended_title <- paste("Total Marginal MET of population selected for scenario (selected population currently defined as: ",filtered_title, ")", sep = "")
        bc <- as.data.frame(bc)
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        filter <- FALSE
        if (sum(bc$Freq, na.rm = T) > 10)
          filter <- TRUE
        bc2max <- 0
        #cat(bc[1:1], " : ", bc[1:2], "\n")
        #cat("filtered: ", nrow(as.data.frame(bc)), "\n")
        #print(bc, quote = TRUE, row.names = FALSE)
        if (nrow(as.data.frame(bc)) > 0)
          bc2max <- max(bc$Freq, na.rm = T)
        max_y <- max(bc1max, bc2max)
        h1$yAxis(min = 0, max = max(30, max_y), tickInterval = 10, title = list(text = 'Percentage %'))
        
        if(filter){
          h1$series(data = bc$Freq, name = "Selected Population")
          mmet_per <- round(nrow(subset(pd, total_mmet >= 8.75 & total_mmet <= 17.5)) / nrow(pd) * 100, digits = 1)
          h1$subtitle(text = paste("Selected population with marginal MET between 8.75 and 17.5: ", mmet_per, "%", sep=""), style = list(font = 'bold 12px "Trebuchet MS", Verdana, sans-serif'))
        }else{
          h1$subtitle(text = HTML("Sorry: Not Enough Data to Display Selected Population (Population Size &lt; 10)"), style = list(font = 'bold 14px "Trebuchet MS", Verdana, sans-serif', color = "#f00"))
        }
      
        h1$title(text = extended_title)
        h1$tooltip(valueSuffix= '%')
        h1$set(dom = 'plotMode')
        h1$exporting(enabled = T)
        return(h1)
      }
    }
  })
  
  output$plotBaseline <- renderChart({
    if (!is.null(tdata)){
      if (input$scenario == 'i'){
        
        filtered_title <- getFilteredTitle(idata)
        max_val <- max(idata$total_mmet)
        h <- NULL
        bc <- table (cut (idata$total_mmet, breaks = c(seq(min(idata$total_mmet), 60, by = 5),max(idata$total_mmet)), xlim = c(min(idata$total_mmet), 60)))
        extended_title <- paste("Total Marginal MET of total population")
        bc <- as.data.frame(bc)
        bc$Freq <- round(bc$Freq  / sum(bc$Freq) * 100, digits = 1)
        
        h1 <- Highcharts$new()
        h1$title(text = extended_title)
        h1$tooltip(valueSuffix= '%')
        h1$xAxis(categories = bc$Var1, title = list(text = 'Total Marginal MET'))
        
        h1$chart(type = "column")
        h1$plotOptions(column=list(animation=FALSE))
        h1$series(data = bc$Freq, name = "Total Population")
        h1$set(dom = 'plotBaseline')
        h1$exporting(enabled = T)
        return(h1)
      }else{
        h1 <- Highcharts$new()
        h1$set(dom = 'plotBaseline')
        h1$exporting(enabled = T)
        return(h1)
      }
    }
    
  })
  
  getFilteredBDTitle <- function (){
    filtered_title <- paste("Cycling Multiplier: ", input$inBDMS, ", TDR: ", input$inBDTDR, ", Ebike: ", input$inBDEB, " and Equity: ", input$inBDEQ, sep = "" )
    filtered_title
    
  }
  
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
      
      filtered_title <- paste("Age Group: ", str_trim(input$ag), ", Gender: ", displayGender, ", Socio Economic Classification: ", displaySES, " and Ethnicity: ", displayEthnicity, sep = "" )
      filtered_title
    }else
      filtered_title
  }
  
  
  generateBDScenarioTable<- reactive({
    
    lMS <- input$inBDMS
    lTDR <- input$inBDTDR
    lEB <- input$inBDEB
    lEQ <- input$inBDEQ
    
    data <- msharedtata
    data <- subset(data, MS == (as.numeric(lMS) + 1) & TDR == lTDR & equity == lEQ & ebike == lEB)
    
    data[is.na(data)] <- 0
    data <- arrange(data, MS)
    bd <<- data
  })
  
  
  generateScenarioTable<- reactive({
    
    #     lMS <- input$inMS
    lTDR <- input$inTDR
    lEB <- input$inEB
    lEQ <- input$inEQ
    
    data <- sdata
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
  
  genericPlot <- function(var){
    
    h1 <- Highcharts$new()
    h1$chart(type = "spline")
    # types of charts: http://api.highcharts.com/highcharts#plotOptions
    h1$yAxis(title = list(text = var))
    h1$xAxis(categories = sort(unique(sdata$MS), decreasing = F), title = list(text = 'Cycling Multiplier'))
    
    if (input$inTDR == "All"){
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
  
  
  output$plotBDMode <- renderChart({
    generateBDScenarioTable()
    if (!is.null(bd)){
      h1 <- Highcharts$new()
      h1$chart(type = "column")
      h1$plotOptions(column=list(animation=FALSE))
      
      filtered_title <- getFilteredBDTitle()
      extended_title <- paste("Mode Share: Total population versus selected scenario (scenario defined as ", filtered_title, ")", sep = "")
      h1$title(text = extended_title)
      baseline <- subset(msharedtata, MS == 1)
      h1$series(data = baseline$case, name = "Baseline")
      h1$series(data = bd$case, name = "Scenario")
      
      h1$xAxis(categories = c("Walk", "Car Driver", "Car Passenger", "Bus", "Train", "Other", "Bicycle", "Ebike"))
      
      h1$tooltip(valueSuffix= '%')
      
      h1$set(dom = 'plotBDMode')
      h1$exporting(enabled = T)
      return (h1)
    }
  })
  
})
