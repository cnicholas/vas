library(shiny)


fileLoadUI <- function(id) {
  tagList(
    fileInput(NS(id, "file"), label="Choose File:"),
    checkboxInput(NS(id, "has_headers"), label="Has Column Names", value=TRUE),
    textOutput(NS(id,"choose")),
    selectInput(NS(id, "file_input"), label=NULL, choices=NULL),
    actionButton(NS(id, "load_data"), "Load Data"),
    tags$hr(),
    tableOutput(NS(id,"datafilestats"))
  )
}

fileLoadServer <- function(id) {

  moduleServer(id, function(input, output, session) {
    observeEvent(input$load_data,{
      message("click event")
      get_data()})
    identify_file<-reactive({
      message("identify")
      req(input$file)

      inputFile<-input$file

      ext <- tools::file_ext(inputFile$datapath)

      if(ext%in%c("xls","xlsx")){

        sheets<-inputFile$datapath %>% excel_sheets()%>%str_sort()

        choices <- get_choices(sheets)
        updateSelectInput(session, "file_input", choices = choices, selected='') #set selected to '' to avoid triggering observe

      }else{
        choices<-c("Comma","Tab","Pipe")
        updateSelectInput(session, "file_input", choices = choices, selected='')
      }
      out<-ext
    })

    get_data <- reactive({
      req(input$load_data)
      message("IN LOAD DATA")

      file <- input$file
      datapath <- file$datapath
      message(datapath)
      ext <- identify_file()
      if (ext %in% c("xls", "xlsx")) {
        validate(need(input$file_input, 'Select a worksheet to load!'))
        out <-
          loadDataExcel(
            datapath = datapath,
            sheet = input$file_input,
            headers = input$has_headers
          )
        return(out)
      } else{
        validate(need(
          input$file_input,
          'Select a delimiter for the file to load!'
        ))
        #write logic for delimiter
        out <-
          loadDataText(datapath = datapath,
                       headers = input$has_headers
          )
      }

      out
    })

    output$choose<-renderText({
      req(input$file)
      if(identify_file()%in%c("xls","xlsx")) "Choose a worksheet:" else paste("Choose a delimiter:")
      })

    output$datafilestats<-renderTable({
      data<-get_data()

      data%>%summarize(Obs=n(), Variables=ncol(.), "Missing Values"=sum(is.na(.)))
    })

    reactive(load_data())
  })
}
# ## Only run examples in interactive R sessions
# if (interactive()) {
#
#   ui <- fluidPage(
#     sidebarLayout(
#       sidebarPanel(
#         fileLoadUI("inputfile")
#       ),
#       mainPanel(
#         tableOutput("contents")
#       )
#     )
#   )
#
#   server <- function(input, output) {
#     fileLoadServer("inputfile")
#   }
#
#   shinyApp(ui, server)
# }




