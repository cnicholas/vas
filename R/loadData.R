
loadDataSet<- function(path){
  readxl::read_xlsx(path,sheet="data")
}


filter="1_1"
response="fill.weight"
colname="lane_phase"
sym_colname<-sym(colname)
sym_response<-sym(response)
meta<-mget(c("colname","sym_colname","sym_response"))
rsg<-fill_weights%>%mutate(lane_phase=paste(lane,phase,sep="_"))
result<-rsg%>%filter(is.na(!!meta$sym_response)==FALSE &!!meta$sym_colname==filter)%>%select(!!meta$sym_response)
hist(x=result[[1]], main="All",xlab = response)

