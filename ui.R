library(DT)
library(devtools)
if (!require(rCharts)) {
  install_github('rCharts', 'ramnathv')
  library(rCharts)
}
source("data-processing.R")
uMS <- append("All", sort(unique(sdata$MS))[-1]) # c("0" = "0","1" = "1", "2" = "2", "4" = "4", "8" = "8","16" = "16", "32" = "32", "64" = "64") #as.list(as.character(sort(unique(sdata$MS))))
uTDR <- append("All", sort(unique(sdata$TDR), decreasing = F)[-1])
uEQ <- append("All",sort(unique(sdata$equity)))
#uEQ <- sort(unique(sdata$equity))
uEB <- append("All", sort(unique(sdata$ebike)))
#uEB <- sort(unique(sdata$ebike))
variableList <- t(as.matrix(colnames(sdata)))
variableList <- variableList[,6:length(colnames(sdata))]

scenarios <- c("Trips" = "t",
               "Individuals" =    "i")

ag <- "All"
ag <- append(ag, sort(unique(as.character(tdata$age_group))))

gender <- c("All" = 3,
            "Male" = 1,
            "Female" = 2)


shinyUI(pageWithSidebar(
  headerPanel("Co-Benefit Model"),
  sidebarPanel(
    conditionalPanel(condition="input.conditionedPanels==1",
                     #selectInput(inputId = "inMS", label = h4("Select Cycling Multiplier:"), choices =  uMS),
                     selectInput(inputId = "inTDR", label = h4("Select Travel Distance Reduction (TDR):"), choices =  uTDR),
                     selectInput(inputId = "inEB", label = h4("Select Ebike:"), choices =  uEB),
                     selectInput(inputId = "inEQ", label = h4("Select Equity:"), choices =  uEQ),
                     selectInput('varname', label = h4('Plot Variable:'), variableList)


    ),
    conditionalPanel(condition="input.conditionedPanels==2",
                     radioButtons("scenario", "Scenario:", scenarios, inline = TRUE),
                     selectizeInput("ag", "Age Group:", ag, selected = ag[1], multiple = F), #options = list(maxOptions = 2)),
                     radioButtons("gender", "Gender: ", gender, inline = TRUE)
    )
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Scenarios", value=1,
                showOutput('plotCycPercent', "highcharts"),
                showOutput('plotGenericVariable', "highcharts")
                #showOutput('plotCO2R', "HighCharts"),
                #showOutput('plotCarAccess', "HighCharts")
               ),

      tabPanel("Baseline", value=2,
               plotOutput("plotMode"),
               plotOutput("plotBaseline"))
      , id = "conditionedPanels"
    )
  )
))