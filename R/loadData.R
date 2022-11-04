library(readxl)
library(readr)



loadDataSet<- function(datapath){

  ext <- tools::file_ext(datapath)
  ext <- tolower(ext)
  message(paste("other file type: ", ext))
  if (ext %in% c("xls","xlsx")){
    readxl::read_xlsx(datapath,sheet="data")
  }

  else{
    switch(ext,
           csv = vroom::vroom(datapath, delim = ",",col_names = TRUE, show_col_types = FALSE),
           tsv = vroom::vroom(datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv, .tsv, .xls, or .xlsx file")
    )
  }
}

loadDataExcel<- function(datapath, sheet="data", headers=TRUE){
  out<-readxl::read_xlsx(datapath,sheet=sheet,col_names = headers)
}

loadDataText<- function(datapath, delim="",headers=TRUE){
  out<-vroom::vroom(datapath, delim = ",", col_names = headers, show_col_types = FALSE)
}

handling_excel<-function(datapath){

  #wkb <- readxl::read_xlsx(datapath)
  sheets<-datapath %>%
    excel_sheets()%>%n_distinct()

  print(sheets)
  sheets

}


