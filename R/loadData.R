library(qcc)
library(TTR)
library(dplyr)

qcc.options(bg.margin="white")
loadDataSet<- function(path){
  readxl::read_xlsx(path,sheet="data")
}

# data("fill_weights")
# filter="1_2"
# response="fill.weight"
# colname="lane_phase"
# sym_colname<-sym(colname)
# sym_response<-sym(response)
# meta<-mget(c("colname","sym_colname","sym_response"))
# rsg<-fill_weights%>%mutate(lane_phase=paste(lane,phase,sep="_"))%>%filter(is.na(fill.weight)==FALSE)
#
# result<-rsg%>%filter(is.na(!!meta$sym_response)==FALSE &!!meta$sym_colname==filter)%>%select(!!meta$sym_response)
# hist(x=result[[1]], main="All",xlab = response)
# rsg_data <- list(full=rsg, meta=meta)
#
# groups<-qcc.groups(rsg_data$full$fill.weight,rsg_data$full$lane_phase)
# qcc(groups, type="xbar")
# ma<-fill_weights%>%filter(is.na(fill.weight)==FALSE)%>%
#   group_by(lane,phase)%>%
#   #arrange(lxp,pull)%>%
#   mutate(
#     ma=zoo::rollmean(fill.weight,2,fill=NA),
#     residual_ma=fill.weight-ma)%>%arrange(lxp, pull)
# ma%>%filter((is.na(ma)))
# #
# # fill_weights%>%filter(is.na(fill.weight)==FALSE)%>%
# #   group_by(lane,phase)%>%
# #   with(SMA(fill.weight,2))
# input$variable_response<-"lxp"
# var<-rsg_data()$full[,input$variable_response]
# #message(var)
# rsg<-rsg_data()$full[,rsg_data()$meta$rsg_name]
# # message(rsg)
# #build chart
# groups<-qcc.groups(var,rsg)
# message(groups)
# qcc(groups, type="xbar")
