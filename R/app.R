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
                             selectInput("histogram_filter",label= "Rational Subgroup:",choices=NULL, multiple=FALSE),
                             numericInput("bins", "Bins",25, min=1),
                             plotOutput("hist"),
                             textOutput("stuff"),

                    )
        )
      )
    )
  )

  server <- function(input, output, session) {

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


    rsg_data<- reactive({
      if(input$analyze==0)
        return(NULL)

      req(dataset(),input$variable_rsg,input$variable_response,input$variable_time)
      message(paste("in actionbutton: ",input$analyze))

      #need to create symbols to do dynamic calculations
      #rsg is a list
      rsg_col_symbols <- lapply(input$variable_rsg, as.symbol) #need for creating rsg column
      rsg_name <-paste0(input$variable_rsg,collapse="_") #for mutate label
      rsg_name_symbol<-sym(rsg_name)
      response_symbol<-sym(input$variable_response)
      time_symbol<- sym(input$variable_time)
      #create a list of meta data about analysis structure to use throughout the app
      rsg_meta<-mget(c("rsg_col_symbols","rsg_name","rsg_name_symbol","response_symbol","time_symbol"))

      #Return a list of data frames 1 for summary and 1 with added columns
      #Build Rational Subgroup Fullset with RSG column
      full <-dataset() %>%  mutate(!!rsg_name := paste(!!!rsg_col_symbols, sep="_")) %>%
        filter(is.na(!!response_symbol)==FALSE) %>%
        arrange(!!rsg_name_symbol, !!time_symbol) %>%
        select(!!rsg_name_symbol,!!time_symbol,!!response_symbol, everything())

      #Build Rational Subgroup Summary
      summary<-dataset() %>%
                mutate(!!rsg_name := paste(!!!rsg_col_symbols, sep="_")) %>%
                group_by(!!rsg_name_symbol) %>%
                summarize(n=n(),
                  "Mean (y)" = mean(!!response_symbol, na.rm=TRUE),
                  "sd (y)" = sd(!!response_symbol, na.rm=TRUE),
                  "Missing Values"=sum(is.na(!!response_symbol)))

      list(summary=summary, full=full, meta=rsg_meta)
    })

    observeEvent(input$histogram_filter,{
      if(input$histogram_filter!='')
        message(paste("filter value: ", input$histogram_filter))


    })

    histogramServer<- reactive({
      message("histogramServer")
      req(rsg_data()$full)

      data<-rsg_data()$full
      choices<-data %>% distinct(!!rsg_data()$meta$rsg_name_symbol)
      choices<-c("All",choices)
      updateSelectInput(session, "histogram_filter", choices = choices, selected='All')

      ""
    })
    hist_data <- reactive({
      req(rsg_data()$full)
      data <- rsg_data()$full
      message(typeof(data))
      response_sym <- sym(input$variable_response)
      if (input$histogram_filter == 'All') {
        result <- data %>% select(!!rsg_data()$meta$response_symbol)
        return(result[[1]])
      } else{
        result <-
          data %>% filter(!!rsg_data()$meta$rsg_name_symbol == input$histogram_filter) %>%
          select(!!rsg_data()$meta$response_symbol)
        return(result[[1]])
      }
    })

    disclaimer_msg<-reactive({
      req(rsg_data())
      if(sum(rsg_data()$summary$"Missing Values")>0)
        "Note: Missing values removed for calculation of the mean and standard deviation!"
    })
    output$rsg_stats <- renderTable(rsg_data()$summary)
    output$dataSummary <- renderDataTable(dataset())
    output$disclaimer <- renderText(disclaimer_msg())
    output$rsg_full <- renderDataTable(rsg_data()$full)
    output$stuff <- renderText(histogramServer())
    output$hist <- renderPlot({
      req(input$histogram_filter)
      message("in render plot")
      title <- paste("Histogram for RSG: ", input$histogram_filter)
      hist(hist_data(), breaks = input$bins, main = title, xlab=input$variable_response)
    }, res = 96)
  }
  shinyApp(ui, server, ...)
}
