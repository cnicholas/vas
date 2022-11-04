library(shiny)
library(readxl)
library(dplyr)
library(purrr)
library(stringr)
library(qcc)

wd<-getwd()

# source(paste(wd,"loadData.R",sep="/"), local = TRUE)
# source(paste(wd,"controlChart.R",sep="/"), local = TRUE)
# source(paste(wd,"histogram.R",sep="/"), local = TRUE)

vasApp <- function(...) {
  options(shiny.maxRequestSize = 15 * 1024^2)
  qcc.options(bg.margin="white") #produce control charts with white background

  ui <- fluidPage(

    titlePanel("Variation Analysis System"),
    sidebarLayout(
      sidebarPanel(width = 3,

                   fileInput( "file", label="Choose File:"),
                   checkboxInput( "has_headers", label="Has Column Names", value=TRUE),
                   selectInput( "file_input", label=NULL, choices=c("Choose..."), selected="Choose..."),
                   actionButton( "btn_load_data", label="Load Data"),
                   tags$hr(),

                   tableOutput("datafilestats"),
                   selectInput("variable_response",label = "Response Variable (y):", choices=NULL, selected=NULL),
                   selectInput("variable_time",label = "Time Dimension (t):", choices=NULL),
                   selectInput("variable_rsg",label = "RSG Variables (x):", choices=NULL, multiple = TRUE),
                   actionButton("analyze",label = "Analyze"),
                   hr(),
                   tableOutput("rsg_stats")


      ),
      mainPanel(width = 9,
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
                    plotOutput("xbar_chart")
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
    rv<-reactiveValues(dataset=data.frame(), is_excel=NULL, meta=list(), summary=data.frame())

    get_choices<-function(choices, selected=c("")){

      choices[!choices%in%selected]
    }
    is_default_selection<-function(selection){

      str_detect(selection, "Choose")
    }

    observeEvent(input$file, {

      updateSelectInput(session, "variable_response", choices ='', selected='')
      updateSelectInput(session, "variable_time", choices ='', selected='')
      updateSelectInput(session, "variable_rsg", choices ='', selected='')



      file<-input$file

      ext <- tools::file_ext(file$datapath)

      excel<- if(ext%in%c("xls","xlsx")) TRUE else FALSE

      if (excel){
        sheets<-file$datapath %>% excel_sheets()%>%str_sort()
        rv$is_excel<-TRUE
        choices <- get_choices(sheets)
        choices<-cbind(c("Choose a worksheet!"), choices)
        updateSelectInput(session, "file_input", choices = choices) #set selected to '' to avoid triggering observe
      }else{
        rv$is_excel<-FALSE
        choices<-c("Comma","Tab","Pipe")
        choices<-cbind(c("Choose a delimiter!"), choices)
        updateSelectInput(session, "file_input", choices = choices)
      }

    })
     observeEvent(input$btn_load_data, {

      #req(input$file, input$btn_load_data, input$file_input, input$has_headers)


      file <- input$file
      header<-input$has_headers
      datapath <- file$datapath
      file_param <-input$file_input

      if(rv$is_excel) {
        validate(need(!is_default_selection(input$file_input), 'Select a worksheet to load!'))
        out <-
          loadDataExcel(
            datapath = datapath,
            sheet = file_param,
            headers = input$has_headers
          )
        rv$dataset<-out
      } else{
        validate(need(!is_default_selection(input$file_input), 'Select a delimiter for the file to load!'))
        #write logic for delimiter
        rv$dataset <- loadDataText(datapath = datapath, headers = input$has_headers)

      }

      data<-rv$dataset
      col_names<- names(dplyr::select_if(data, is.numeric))

      response_choices <- get_choices(col_names)
      updateSelectInput(session, "variable_response", choices = response_choices, selected='')
    })

    observeEvent(input$variable_response, {

      data<-rv$dataset

      if(input$variable_response!= ''){
        col_names<-names(data)
        time_variable_choices<-get_choices(col_names,input$variable_response)
        updateSelectInput(session, "variable_time", choices = time_variable_choices, selected='')
        updateSelectInput(session, "variable_rsg", choices = character(0), selected='')
      }
    })
    #set choices parameters (control, selected_variables<character vector) returns vector of choices
    observeEvent(input$variable_time, {

      data<-rv$dataset
      if(input$variable_time !=''){
      col_names<- names(data)
      rsg_variable_choices<-get_choices(col_names,c(input$variable_response,input$variable_time))
      updateSelectInput(session, "variable_rsg", choices = rsg_variable_choices, selected='')
      }
    })

   #Refactor this code out.
    observeEvent(input$analyze,{

      req(rv$dataset,input$variable_rsg,input$variable_response,input$variable_time)


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
      rv$meta<-mget(c("rsg_col_symbols","rsg_name","rsg_name_symbol","response_symbol","time_symbol"))

      #Return a list of data frames 1 for summary and 1 with added columns
      #Build Rational Subgroup Fullset with RSG column
      rv$dataset <-rv$dataset %>%

        mutate(!!rsg_name := paste(!!!rsg_col_symbols, sep="_")) %>%
        filter(is.na(!!response_symbol)==FALSE) %>%
        arrange(!!rsg_name_symbol, !!time_symbol) %>%
        mutate(ma=zoo::rollmean(!!response_symbol,2,fill=NA),
               residual_ma=!!response_symbol-ma) %>%
        select(!!rsg_name_symbol,!!time_symbol,!!response_symbol, everything())

      #Build Rational Subgroup Summary
      rv$summary<-summary<-rv$dataset %>%
        mutate(!!rsg_name := paste(!!!rsg_col_symbols, sep="_")) %>%
        group_by(!!rsg_name_symbol) %>%
        summarize(n=n(),
                  "Mean (y)" = mean(!!response_symbol, na.rm=TRUE),
                  "sd (y)" = sd(!!response_symbol, na.rm=TRUE),
                  "Missing Values"=sum(is.na(!!response_symbol)))
      #Refactor this out
      choices<-rv$dataset %>% distinct(!!rv$meta$rsg_name_symbol)
      choices<-c("All",choices)
      updateSelectInput(session, "rsg_selected_hist1", choices = choices, selected='All')
      updateSelectInput(session, "rsg_selected_hist2", choices = choices, selected='All')
      updateSelectInput(session, "rsg_selected_imr1", choices = choices, selected='All')
      updateSelectInput(session, "rsg_selected_imr2", choices = choices, selected='All')


    })

    observeEvent(input$rsg_selected_hist,{
      if(input$rsg_selected_hist!='')
        message(paste("filter value: ", input$rsg_selected_hist))
    })

    disclaimer_msg<-reactive({

      if(sum(rv$summary$"Missing Values")>0)
        "Note: Missing values removed for calculation of the mean and standard deviation!"
    })

    output$rsg_stats <- renderTable({
      req(rv$dataset)
      rv$summary})

     output$dataSummary <- renderDataTable({
                                            rv$dataset
                                           })
    output$disclaimer <- renderText(disclaimer_msg())
    output$rsg_full <- renderDataTable({

      rv$dataset

    })

    output$hist1 <- renderPlot({

      req(input$rsg_selected_hist1)

      create_hist(rv$dataset,rv$meta,input$rsg_selected_hist1, bins=input$bins1)

    }, res = 96)

    output$hist2 <- renderPlot({
      message("output plot hist2")
      req(input$rsg_selected_hist2)
      message("at create hist function call")
      create_hist(rv$dataset,rv$meta, input$rsg_selected_hist2, bins=input$bins2)

    }, res = 96)
  #   output$imr_chart1<-renderPlot({
  #
  #     req(rsg_data(), input$rsg_selected_imr1)
  #     create_imr(rsg_data(),input$rsg_selected_imr1)
  #
  #   }, res = 96)
  #
  #   output$imr_chart2<-renderPlot({
  #
  #     req(rsg_data(), input$rsg_selected_imr2)
  #     chart<-create_imr(rsg_data(),input$rsg_selected_imr2)
  #
  #
  #   }, res = 96)
  #   output$xbar_chart<-renderPlot({
  #
  #     req(rsg_data())
  #     create_xbar(rsg_data())
  #
  #   }, res = 96)
   }
  shinyApp(ui, server, ...)
}
vasApp()
