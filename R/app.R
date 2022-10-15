library(shiny)
library(readxl)
library(purrr)
library(stringr)

vasApp <- function(...) {
  ui <- fluidPage(
    titlePanel("Variation Analysis System"),
    sidebarLayout(
      sidebarPanel(

        fileInput("datafile",label = "Select a file to analyze", accept = c(".xls",".xlsx"), multiple = FALSE, buttonLabel = "Choose File"),
        tableOutput("datafilestats"),
        selectInput("variable_response",label = "Response Variable (y):", choices=NULL),
        selectInput("variable_time",label = "Time Dimension (t):", choices=NULL),
        selectInput("variable_rsg",label = "RSG Variables (x):", choices=NULL, multiple = TRUE),

      ),
      mainPanel(
        tabsetPanel(id="tabset",
                    tabPanel("data_tab",title = "Data",
                             tableOutput("dataSummary")),
                    tabPanel("Summary Statistics"),
                    tabPanel("Charts",
                             textOutput("greeting"),
                             textOutput("chars")
                    )
        )
      )
    )
  )

  server <- function(input, output, session) {

    loadDataSet<- function(path){
      readxl::read_xlsx(path,sheet="data")
    }

    dataset<-reactive({

      req(input$datafile)

      inputFile<-input$datafile

      loadDataSet(inputFile$datapath)

    })
    output$datafilestats<-renderTable(dataset()%>%summarize(Observations=n(), Variables=ncol(.), "Missing Values"=sum(is.na(.))))
    observeEvent(input$datafile, {
      message("in input_file ObserveEvent")
      req(dataset())
      message(paste("Column names are: ",names(dataset())))
      response_choices <- names(dplyr::select_if(dataset(), is.numeric))
      updateSelectInput(session, "variable_response", choices = response_choices, selected='') #set selected to '' to avoid triggering observe
    })
#GET Analysis structure defined
    observeEvent(input$variable_response, {
      if(input$variable_response !='')
        message(paste("Observing variable_response changes: ", input$variable_response))
        unselected_variables<- !stringr::str_detect(names(dataset()),input$variable_response)
        message(unselected_variables)
        time_variable_choices<-names(dataset()[unselected_variables])
        message(time_variable_choices)
        updateSelectInput(session, "variable_time", choices = time_variable_choices, selected='')
        updateSelectInput(session, "variable_rsg", choices = time_variable_choices, selected='')
    })
    observeEvent(input$variable_time, {
      if(input$variable_time !='')
        message(paste("Observing variable_time changes: ", input$variable_time))
      #filter remaining variable by y and t to populate options for rsg variables
      unselected_variables<- !names(dataset())%in%c(input$variable_response, input$variable_time)
      message(unselected_variables)

      rsg_variable_choices<-names(dataset()[unselected_variables])
      message(rsg_variable_choices)
      updateSelectInput(session, "variable_rsg", choices = rsg_variable_choices, selected='')
    })

    output$dataSummary<-renderTable(dataset())

  }
  shinyApp(ui, server, ...)
}
