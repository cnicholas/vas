library(qcc)
qcc.options(bg.margin="white")
loadDataSet<- function(path){
  readxl::read_xlsx(path,sheet="data")
}

data("fill_weights")
filter="1_2"
response="fill.weight"
colname="lane_phase"
sym_colname<-sym(colname)
sym_response<-sym(response)
meta<-mget(c("colname","sym_colname","sym_response"))
rsg<-fill_weights%>%mutate(lane_phase=paste(lane,phase,sep="_"))
result<-rsg%>%filter(is.na(!!meta$sym_response)==FALSE &!!meta$sym_colname==filter)%>%select(!!meta$sym_response)
hist(x=result[[1]], main="All",xlab = response)

qcc(result,type="xbar.one",xlab=response,title=(paste("RSG",colname,filter,sep=": ")))

fill_weights%>%filter(is.na(fill.weight)==FALSE)%>%
  group_by(lane,phase)%>%
  mutate(
    n=n(),
    mean=mean(fill.weight,na.rm=TRUE),
    sd=sd(fill.weight,na.rm=TRUE),
    mr=mean(abs(diff(fill.weight,1))),
    lpl=mean-(3.27*mr),
    upl=mean+(3.27*mr))
diff(c(1:20),1)

