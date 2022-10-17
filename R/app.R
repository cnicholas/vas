library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(shinyFeedback)

vasApp <- function(...) {
  ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    titlePanel("Variation Analysis System"),
    sidebarLayout(
      sidebarPanel(

        fileInput("datafile",label = "Select a file to analyze", accept = c(".xls",".xlsx"), multiple = FALSE, buttonLabel = "Choose File"),
        tableOutput("datafilestats"),
        selectInput("variable_response",label = "Response Variable (y):", choices=NULL),
        selectInput("variable_time",label = "Time Dimension (t):", choices=NULL),
        selectInput("variable_rsg",label = "RSG Variables (x):", choices=NULL, multiple = TRUE),
        actionButton("analyze",label = "Analyze"),
        tableOutput("rsg_stats")

      ),
      mainPanel(
        tabsetPanel(id="tabset",
                    tabPanel(value="tab_data",title = "Data",
                             tableOutput("dataSummary")),
                    tabPanel(value="tab_data_stats", title="Summary Statistics"),
                    tabPanel(value="tab_charts",title="Charts",
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
      response_choices <- get_choices(names(dplyr::select_if(dataset(), is.numeric)))
      updateSelectInput(session, "variable_response", choices = response_choices, selected='') #set selected to '' to avoid triggering observe
    })
    #GET Analysis structure defined
    get_choices<-function(choices, selected=c("")){
      choices[!choices%in%selected]
    }

    observeEvent(input$variable_response, {
      if(input$variable_response !='')
        message(paste("Observing variable_response changes: ", input$variable_response))

      time_variable_choices<-get_choices(names(dataset()),input$variable_response)
      message(time_variable_choices)
      updateSelectInput(session, "variable_time", choices = time_variable_choices, selected='')

    })
    #set choices parameters (control, selected_variables<character vector) returns vector of choices
    observeEvent(input$variable_time, {
      if(input$variable_time !='')
        message(paste("Observing variable_time changes: ", input$variable_time))
      #filter remaining variable by y and t to populate options for rsg variables

      rsg_variable_choices<-get_choices(names(dataset()),c(input$variable_response,input$variable_time))
      message(rsg_variable_choices)
      updateSelectInput(session, "variable_rsg", choices = rsg_variable_choices, selected='')
    })
    observeEvent(input$variable_rsg,{
      message(paste("RSG defined: ", input$variable_rsg))
    })
    observeEvent(input$analyze, {
      y<- input$variable_response
      t<- input$variable_time
      rsg<- input$variable_rsg
      req(y,t,rsg)

      message(rsg)
    })

    rsgs<- reactive({
      if(input$analyze==0)
        return(NULL)
      req(dataset())
      message(paste("in actionbutton: ",input$analyze))
      #need to create list of symbols to do dynamic calculations
      rsg_def <- lapply(input$variable_rsg, as.symbol)

      rsg_col_name <-paste0(input$variable_rsg,collapse="_")
      message(paste0("rsg col name: ",rsg_col_name))
      #fill_weights%>%mutate(!!var:=paste0(!!!cols))%>%head()
      dataset() %>% mutate(!!rsg_col_name := paste(!!!rsg_def, sep="_")) %>%
                             #group_by_at(.vars=input$variable_rsg) %>%
                             group_by(!!sym(rsg_col_name)) %>%
        summarize("Mean (y)" = mean(!!sym(input$variable_response), na.rm=TRUE),
                  "sd (y)" = sd(!!sym(input$variable_response), na.rm=TRUE),
                  "Missing Values"=sum(is.na(!!sym(input$variable_response))))
    })
    output$rsg_stats<-renderTable(rsgs())
    output$dataSummary<-renderTable(dataset())

  }
  shinyApp(ui, server, ...)
}
