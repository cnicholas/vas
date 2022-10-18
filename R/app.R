library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(shinyFeedback)
library(TTR)


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
                             selectInput("histogram_filter",label= "Filter",choices=NULL, multiple=FALSE),
                             plotOutput("greeting"),
                             textOutput("stuff"),

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

      #need to create symbols to do dynamic calculations
      #rsg is a list
      rsg_def_symbol <- lapply(input$variable_rsg, as.symbol)
      rsg_col_name <-paste0(input$variable_rsg,collapse="_") #for mutate label
      rsg_col_name_symbol<-sym(rsg_col_name)
      input_response_symbol<-sym(input$variable_response)
      input_time_symbol<- sym(input$variable_time)

      message(paste0("rsg col name: ",rsg_col_name))
      #fill_weights%>%mutate(!!var:=paste0(!!!cols))%>%head()

      #Return a list of data frames 1 for summary and 1 with added columns
      #Build Rational Subgroup Fullset with RSG column
      full <-dataset() %>%  mutate(!!rsg_col_name := paste(!!!rsg_def_symbol, sep="_")) %>%
        filter(is.na(!!input_response_symbol)==FALSE) %>%
        arrange(!!rsg_col_name_symbol, !!input_time_symbol) %>%
        select(!!rsg_col_name_symbol,!!input_time_symbol,!!input_response_symbol, everything())

      message(nrow(full))
      #Build Rational Subgroup Summary
      summary<-dataset() %>%
                mutate(!!rsg_col_name := paste(!!!rsg_def_symbol, sep="_")) %>%
                group_by(!!rsg_col_name_symbol) %>%
                summarize(n=n(),
                  "Mean (y)" = mean(!!input_response_symbol, na.rm=TRUE),
                  "sd (y)" = sd(!!input_response_symbol, na.rm=TRUE),
                  "Missing Values"=sum(is.na(!!input_response_symbol)))

      list(summary=summary, full=full)
    })

    observeEvent(input$histogram_filter,{
      if(input$histogram_filter!='')
        message(paste("filter value: ", input$histogram_filter))


    })

    histogramServer<- reactive({

      req(rsgs_info()$full)

      data<-rsgs_info()$full
      rsg_col_name <-paste0(input$variable_rsg,collapse="_") #for mutate label
      choices<-data %>% distinct(!!sym(rsg_col_name))
      updateSelectInput(session, "histogram_filter", choices = choices, selected='')
      "processed"

    })

    disclaimer_msg<-reactive({
      req(rsgs_info())
      if(sum(rsgs_info()$summary$"Missing Values")>0)
        "Note: Missing values removed for calculation of the mean and standard deviation!"
    })
    output$rsg_stats <- renderTable(rsgs_info()$summary)
    output$dataSummary <- renderDataTable(dataset())
    output$disclaimer <- renderText(disclaimer_msg())
    output$rsg_full <- renderDataTable(rsgs_info()$full)
    output$stuff <- renderText(histogramServer())
  }
  shinyApp(ui, server, ...)
}
