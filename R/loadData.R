library(readxl)
library(readr)



loadDataSet<- function(datapath){

  ext <- tools::file_ext(datapath)

  message(paste("other file type: ", ext))
  if (ext %in% c("xls","xlsx")){
    readxl::read_xlsx(datapath,sheet="data")
  }

  else{
    switch(ext,
           csv = vroom::vroom(datapath, delim = ",",col_names = TRUE, show_col_types = FALSE),
           tsv = vroom::vroom(datapath, delim = "\t")
    )
  }

}


