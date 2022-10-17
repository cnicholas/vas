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
        hr(),
        tableOutput("rsg_stats"),
        textOutput("disclaimer")

      ),
      mainPanel(
        tabsetPanel(id="tabset",
                    tabPanel(value="tab_data",title = "Original Data",
                             dataTableOutput("dataSummary")
                    ),
                    tabPanel(value="tab_rsg_data", title="Analysis Dataset",
                            "The Analysis dataset excludes missing values and is sorted by rational subgroup and time",
                            hr(),
                            dataTableOutput("rsg_full")
                    ),
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


    rsgs_info<- reactive({
      if(input$analyze==0)
        return(NULL)

      req(dataset(),input$variable_rsg,input$variable_response,input$variable_time)
      message(paste("in actionbutton: ",input$analyze))

      #need to create list of symbols to do dynamic calculations
      rsg_def <- lapply(input$variable_rsg, as.symbol)

      rsg_col_name <-paste0(input$variable_rsg,collapse="_")
      message(paste0("rsg col name: ",rsg_col_name))
      #fill_weights%>%mutate(!!var:=paste0(!!!cols))%>%head()

      #Return a list of data frames 1 for summary and 1 with added columns
      #Build Rational Subgroup Fullset with RSG column
      full <-dataset() %>%  mutate(!!rsg_col_name := paste(!!!rsg_def, sep="_")) %>%
        filter(is.na(!!sym(input$variable_response))==FALSE) %>%
        arrange(!!sym(rsg_col_name), !!sym(input$variable_time)) %>%
        select(!!sym(rsg_col_name),!!sym(input$variable_time),!!sym(input$variable_response), everything())

      message(nrow(full))
      #Build Rational Subgroup Summary
      summary<-dataset() %>%
                mutate(!!rsg_col_name := paste(!!!rsg_def, sep="_")) %>%
                group_by(!!sym(rsg_col_name)) %>%
                summarize(n=n(),
                  "Mean (y)" = mean(!!sym(input$variable_response), na.rm=TRUE),
                  "sd (y)" = sd(!!sym(input$variable_response), na.rm=TRUE),
                  "Missing Values"=sum(is.na(!!sym(input$variable_response))))

      list(summary=summary, full=full)
    })
    disclaimer_msg<-reactive({
      req(rsgs_info())
      if(sum(rsgs_info()$summary$"Missing Values")>0)
        "Note: Missing values removed for calculation of the mean and standard deviation!"
    })
    output$rsg_stats<-renderTable(rsgs_info()$summary)
    output$dataSummary<-renderDataTable(dataset())
    output$disclaimer<-renderText(disclaimer_msg())
    output$rsg_full<- renderDataTable(rsgs_info()$full)
  }
  shinyApp(ui, server, ...)
}
