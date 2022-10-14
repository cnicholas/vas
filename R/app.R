library(shiny)
library(readxl)
library(purrr)

vasApp <- function(...) {
  ui <- fluidPage(
    titlePanel("Variation Analysis System"),
    sidebarLayout(
      sidebarPanel(
        fileInput("datafile",label = "Select a file to analyze", accept = c(".xls",".xlsx"), multiple = FALSE, buttonLabel = "Choose File"),
        tableOutput("dataSummary"),
     ),
      mainPanel(textOutput("greeting"),
                textOutput("chars"))
    )
  )

  server <- function(input, output, session) {

    loadDataSet<- function(path){
      readxl::read_xlsx(path,sheet="data")
    }
    dataset<-reactive({

      if(is.null(input$datafile))
         return(NULL)

      inputFile<-input$datafile

      loadDataSet(inputFile$datapath)

    })

    output$dataSummary<-renderTable(input$datafile)

    greeting <- reactive({
      paste0("Hello ", input$name, "!")
    })
    num_chars <- reactive({
      nchar(input$name)
    })

    output$greeting <- renderText({
      greeting()
    })
    output$chars <- renderText({
      paste("There are:",
            num_chars() ,
            "characters in",
            input$name,
            collapse = " ")
    })
  }
  shinyApp(ui, server, ...)
}
