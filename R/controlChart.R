create_xbar<- function(data,meta){

  meta<-meta
  #Remove Groups with 1 observation

  data<-data%>%group_by(!!meta$rsg_name_symbol)%>%filter(n()>1)%>%ungroup() #added ungroup() to eliminate the "add variable" warning

  title<-paste("X-bar Chart for RSG", meta$rsg_name, sep=": ")
  xlab<-paste("Rational Subgroups",meta$rsg_name, sep=": ")
  ylab<-meta$response

  var<-data %>% select(meta$response)%>%pull()
  rsg<-data%>%select(meta$rsg_name)%>%pull()

  #build chart
  groups<-qcc.groups(var ,rsg)
  qcc.options(bg.margin="white") #produce control charts with white background
  qcc(data = groups, type="xbar", title=title, ylab=ylab, xlab=xlab,)

}

create_imr<-function(data,meta, rsg_selected="All"){

  data<-data
  meta<-meta

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
  cc_chart<-qcc(result,type="xbar.one",xlab=xlab,ylab=ylab, title=title)
}



