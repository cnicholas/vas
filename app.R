#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(dplyr)
library(qcc)

options.old<-qcc.options()
qcc.options(bg.margin="white")


ui <- fluidPage(
  titlePanel("Variation Analysis System"),
  sidebarLayout(
    sidebarPanel(width=2,
                 fileInput("file1", "Choose Excel File", accept = ".xlsx")
                 #checkboxInput("header", "Header", TRUE)
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data",  
                           fluidRow(
                             column( width=10,
                                     dataTableOutput("contents"))
                           )
                  ),
                  tabPanel("Histogram", 
                           fluidRow(
                             column(width=10, 
                                    plotOutput("plot")
                             )
                           )
                  ),
                  tabPanel("Summary", 
                           fluidRow(
                             column(width=10, 
                                    dataTableOutput("summary"))
                           )
                  ),
                  tabPanel("Xbar Charts", 
                           fluidRow(
                             column(width=10,
                                    plotOutput("rsg_lane")
                             )
                           ),
                           fluidRow(
                             column(width=10,
                                    br()
                             )
                           ),
                           fluidRow(
                             column(width=10, 
                                    plotOutput("rsg_phase")
                             )
                           ),
                           fluidRow(
                             column(width=10,
                                    br()
                             )
                           ),
                           fluidRow(
                             column(width=10, 
                                    plotOutput("rsg_pt")
                             )
                           )
                  ),
                  tabPanel("SD Charts",
                           fluidRow(
                             column(width=10,
                                    plotOutput("rsg_pt_sd")
                             )
                           )
                  ),
                  tabPanel("IMR Charts", 
                           fluidRow(
                             column(width=10,
                                    plotOutput("rsgl1p1")
                             )
                           ),
                           fluidRow(
                             column(width=10,
                                    br()
                             )
                           ),
                           fluidRow(
                             column(width=10, 
                                    plotOutput("rsgl4p2")
                             )
                           )
                  )
      )
      
    )
  )
)

server <- function(input, output) {
  
  d <- reactive({
    qcc.options(bg.margin="white")
    
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "xlsx", "Please upload an Excel file"))
    
    data <- read_xlsx(file$datapath, sheet="data")
    names(data) <- tolower(names(data))
    names(data) <- c("pull","lane","phase","lp","fill_weight")
    #Make data available
    data
    
  })
  output$plot <- renderPlot({
    data=d()
    hist(data$fill_weight, 
         main="Histogram of Fill Weights",
         xlab="Fill Weight (grams)")
  })
  output$contents <- renderDataTable({
    data <- d()
  })
  
  output$summary <- renderDataTable({
    data <- d()
    mean_lp <- data %>% group_by(lane, phase) %>% summarize( mean_fw = round(mean (fill_weight, na.rm = TRUE), 2))
  })
  
  output$rsgl1p1<-renderPlot({
    data <- d()
    data %>% 
      filter(lane == 1 & phase == 1 & is.na(fill_weight) == FALSE) %>%
      with(qcc(data=fill_weight, type="xbar.one", title="Lane 1 - Phase 1", ylab="Fill Weight (grams)"))
  })
  
  output$rsgl4p2<-renderPlot({
    data <- d()
    data %>% 
      filter(lane == 4 & phase == 2 & is.na(fill_weight) == FALSE) %>%
      with(qcc(data=fill_weight, type="xbar.one", title="Lane 4 - Phase 2", ylab="Fill Weight (grams)"))
  })
  
  output$rsg_lane <- renderPlot({
    data<-d()
    rsg_lane <- qcc.groups(data$fill_weight, data$lane)
    qcc(rsg_lane, type="xbar", title="Analysis of PDC Sample Averages", xlab="PD Factor-Lane", ylab="Fill Weight (grams)")
  })
  
  output$rsg_phase <- renderPlot({
    data<-d()
    rsg_phase <- qcc.groups(data$fill_weight, data$phase)
    qcc(rsg_phase, type="xbar", title="Analysis of PDC Sample Averages", xlab="PD Factor-Phase", ylab="Fill Weight (grams)")
  })
  
  output$rsg_pt <- renderPlot({
    data<-d()
    rsg_pull <- qcc.groups(data$fill_weight, data$pull)
    qcc(rsg_pull, type="xbar", title="Analysis of PDC Sample Averages", xlab="Production Time", ylab="Fill Weight (grams)")
  })
  
  output$rsg_pt_sd <- renderPlot({
    data<-d()
    rsg_pull <- qcc.groups(data$fill_weight, data$pull)
    qcc(rsg_pull, type="S", title="Analysis of PDC Sample Standard Deviations", xlab="Production Time", ylab="Fill Weight (grams)")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)










