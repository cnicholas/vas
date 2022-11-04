library(shiny)
library(stringr)
library(dplyr)


fileLoadUI <- function(id) {
  tagList(
    fileInput(NS(id, "file"), label="Choose File:"),
    checkboxInput(NS(id, "has_headers"), label="Has Column Names", value=TRUE),
    selectInput(NS(id, "file_input"), label=NULL, choices=c("Choose..."), selected="Choose..."),
    actionButton(NS(id, "btn_load_data"), label="Load Data"),
    tags$hr(),
    tableOutput(NS(id,"datafilestats"))
  )
}

fileLoadServer <- function(id) {

  moduleServer(id, function(input, output, session) {

    is_excel<-reactiveVal(NULL)
    is_default_selection<-function(selection){

      str_detect(selection, "Choose")
  }

    dataset <- eventReactive(input$btn_load_data, {

      req(input$file, input$btn_load_data, input$file_input, input$has_headers)

      message("in dataset")
      message(paste("in dataset fi: ", input$file_input))
      file <- isolate(input$file)
      header<-isolate(input$has_headers)
      datapath <- file$datapath
      file_param <-input$file_input

      if(is_excel()) {
        validate(need(!is_default_selection(input$file_input), 'Select a worksheet to load!'))
        out <-
          loadDataExcel(
            datapath = datapath,
            sheet = file_param,
            headers = input$has_headers
          )
          return(out)
      } else{
        validate(need(!is_default_selection(input$file_input), 'Select a delimiter for the file to load!'))
          #write logic for delimiter
          out <- loadDataText(datapath = datapath, headers = input$has_headers)
          return(out)
        }
    })

    observeEvent(input$file, {

      req(input$file)

      file<-input$file

      ext <- tools::file_ext(file$datapath)
      excel<- if(ext%in%c("xls","xlsx")) TRUE else FALSE

      if (excel){
        sheets<-file$datapath %>% excel_sheets()%>%str_sort()
        is_excel(TRUE)
        choices <- get_choices(sheets)
        choices<-cbind(c("Choose a worksheet!"), choices)
        updateSelectInput(session, "file_input", choices = choices) #set selected to '' to avoid triggering observe
      }else{
        is_excel(FALSE)
        choices<-c("Comma","Tab","Pipe")
        choices<-cbind(c("Choose a delimiter!"), choices)
        updateSelectInput(session, "file_input", choices = choices)
      }

    })

    output$datafilestats<-renderTable({



      data<-dataset()

      data%>%summarize("Obs."=n(), Variables=ncol(.), "Missing Values"=sum(is.na(.)))

    })

    return(dataset)
  })
}
## Only run examples in interactive R sessions
if (interactive()) {

  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileLoadUI("inputfile")
      ),
      mainPanel(
        tableOutput("contents")
      )
    )
  )

  server <- function(input, output) {
    fileLoadServer("inputfile")
  }

  shinyApp(ui, server)
}




