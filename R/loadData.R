library(readxl)
library(readr)



loadDataSet<- function(datapath){

  ext <- tools::file_ext(datapath)

  if (ext %in% c("xls","xlsx")){
    readxl::read_xlsx(datapath,sheet="data")
  }

  else{message(paste("other file type: ", ext))}

}


