library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(shinyFeedback)
library(qcc)




vasApp <- function(...) {

  qcc.options(bg.margin="white") #produce control charts with white background

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
        tabsetPanel(
          id = "tabset",
          tabPanel(
            value = "tab_data",
            title = "Original Data",
            dataTableOutput("dataSummary")
          ),
          tabPanel(
            value = "tab_rsg_data",
            title = "Analysis Dataset",
            "The Analysis dataset excludes missing values and is sorted by rational subgroup and time",
            hr(),
            dataTableOutput("rsg_full")
          ),
          tabPanel(
            value = "tab_charts",
            title = "Histograms",
            selectInput(
              "rsg_selected_hist1",
              label = "Rational Subgroup:",
              choices = NULL,
              multiple = FALSE
            ),
            numericInput("bins1", "Bins", 25, min = 1),
            plotOutput("hist1"),
            hr(),
            selectInput(
              "rsg_selected_hist2",
              label = "Rational Subgroup:",
              choices = NULL,
              multiple = FALSE
            ),
            numericInput("bins2", "Bins", 25, min = 1),
            plotOutput("hist2")
             ),
          tabPanel(
            value = "tab_xbar",
            title = "X-bar Charts",
            #Chart 1
            plotOutput("xbar_chart"),
          ),
          tabPanel(
            value = "tab_IMR",
            title = "Individuals Charts",
            #Chart 1
            selectInput(
              "rsg_selected_imr1",
              label = "Rational Subgroup:",
              choices = NULL,
              multiple = FALSE
            ),
            plotOutput("imr_chart1"),
            hr(),
            #Chart 2
            selectInput(
              "rsg_selected_imr2",
              label = "Rational Subgroup:",
              choices = NULL,
              multiple = FALSE
            ),
            plotOutput("imr_chart2")

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
      #should the choices be set when the RSG variable is changed
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
      response<-input$variable_response
      response_symbol<-sym(input$variable_response)
      time<-input$variable_time
      time_symbol<- sym(input$variable_time)
      #create a list of meta data about analysis structure to use throughout the app
      rsg_meta<-mget(c("rsg_col_symbols","rsg_name","rsg_name_symbol","response_symbol","time_symbol"))

      #Return a list of data frames 1 for summary and 1 with added columns
      #Build Rational Subgroup Fullset with RSG column
      full <-dataset() %>%

        mutate(!!rsg_name := paste(!!!rsg_col_symbols, sep="_")) %>%
        filter(is.na(!!response_symbol)==FALSE) %>%
        arrange(!!rsg_name_symbol, !!time_symbol) %>%
        mutate(ma=zoo::rollmean(!!response_symbol,2,fill=NA),
               residual_ma=!!response_symbol-ma) %>%
        select(!!rsg_name_symbol,!!time_symbol,!!response_symbol, everything())

      #Build Rational Subgroup Summary
      summary<-dataset() %>%
                mutate(!!rsg_name := paste(!!!rsg_col_symbols, sep="_")) %>%
                group_by(!!rsg_name_symbol) %>%
                summarize(n=n(),
                  "Mean (y)" = mean(!!response_symbol, na.rm=TRUE),
                  "sd (y)" = sd(!!response_symbol, na.rm=TRUE),
                  "Missing Values"=sum(is.na(!!response_symbol)))
      #Refactor this out
      choices<-full %>% distinct(!!rsg_meta$rsg_name_symbol)
      choices<-c("All",choices)
      updateSelectInput(session, "rsg_selected_hist1", choices = choices, selected='All')
      updateSelectInput(session, "rsg_selected_hist2", choices = choices, selected='All')
      updateSelectInput(session, "rsg_selected_imr1", choices = choices, selected='All')
      updateSelectInput(session, "rsg_selected_imr2", choices = choices, selected='All')

      list(summary=summary, full=full, meta=rsg_meta)
    })

    observeEvent(input$rsg_selected_hist,{
      if(input$rsg_selected_hist!='')
        message(paste("filter value: ", input$rsg_selected_hist))


    })

    hist_data <- reactive({
      req(rsg_data()$full)
      data <- rsg_data()$full
      message(typeof(data))
      response_sym <- sym(input$variable_response)
      if (input$rsg_selected_hist == 'All') {
        result <- data %>% select(!!rsg_data()$meta$response_symbol)
        return(result[[1]])
      } else{
        result <-
          data %>% filter(!!rsg_data()$meta$rsg_name_symbol == input$rsg_selected_hist) %>%
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

    output$hist1 <- renderPlot({
      message("output plot hist1")
      req(input$rsg_selected_hist1,rsg_data())
      message("at create hist function call")
      create_hist(rsg_data(), input$rsg_selected_hist1, bins=input$bins1)

    }, res = 96)
    output$hist2 <- renderPlot({
      message("output plot hist2")
      req(input$rsg_selected_hist2,rsg_data())
      message("at create hist function call")
      create_hist(rsg_data(), input$rsg_selected_hist2, bins=input$bins2)

    }, res = 96)
    output$imr_chart1<-renderPlot({

      req(rsg_data(), input$rsg_selected_imr1)
      create_imr(rsg_data(),input$rsg_selected_imr1)

    }, res = 96)

    output$imr_chart2<-renderPlot({

      req(rsg_data(), input$rsg_selected_imr2)
      chart<-create_imr(rsg_data(),input$rsg_selected_imr2)


    }, res = 96)
    output$xbar_chart<-renderPlot({

      req(rsg_data())
      create_xbar(rsg_data())

    }, res = 96)
  }
  shinyApp(ui, server, ...)
}
