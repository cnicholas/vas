create_xbar<- function(rsg_data){

  data<-rsg_data$full
  meta<-rsg_data$meta

  title<-paste("X-bar Chart for RSG", meta$rsg_name, sep=": ")
  xlab<-paste("Rational Subgroups",meta$rsg_name, sep=": ")
  ylab<-meta$response

  var<-data %>% select(meta$response)%>%pull()
  rsg<-data%>%select(meta$rsg_name)%>%pull()

  #build chart
  groups<-qcc.groups(var ,rsg)
  qcc.options(bg.margin="white") #produce control charts with white background
  qcc(data = groups, type="xbar")

}

create_imr<-function(rsg_data, rsg_selected="All"){

  data<-rsg_data$full
  meta<-rsg_data$meta

  title<-paste("RSG", rsg_selected, sep=": ")
  xlab<-paste("Time (t)", meta$time, sep=": ")
  ylab<-meta$response


  if (rsg_selected == 'All') {
    result <- data %>% select(meta$response)

  } else{
    result <-data %>% filter(!!meta$rsg_name_symbol == rsg_selected) %>%
      select(meta$response)
  }
  qcc.options(bg.margin="white") #produce control charts with white background
  qcc(result,type="xbar.one",xlab=xlab,ylab=ylab, title=title)
}



