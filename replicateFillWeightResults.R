library(openxlsx)
library(qcc)
library(dplyr)
library(TTR)
library(tidyr)
library(purrr)
library(broom)
options.old<-qcc.options()
qcc.options(bg.margin="white")

#load a file
file_name <- "/Users/cnicholas/Documents/projects/fill_weight/FILLWEIGHTDATAN=800.xlsx"
data <- read.xlsx(file_name, sheet='data', colNames = T)
#Make headings lower case to ease typing

names(data) <- tolower(names(data))

#Figure 4
head(data,16)

#Figure 5
hist(data$fill.weight, breaks=50,xlim=c(230,244))

#Figure 7 
#obs 40 is NA
data %>% filter(lane==4 & phase==1) %>% with(plot(fill.weight,type="b", col="blue"))

#remove NA and calculate moving average
lines(data %>% filter(lane==4 & phase==1 & is.na(fill.weight)==FALSE) %>% with(SMA(fill.weight,2)), type="b", col="red")

l4p1<- data %>% filter(lane==4 & phase==1 & is.na(fill.weight)==FALSE) %>% 
  mutate(SMA.fill.weight = SMA(fill.weight,2),
         residual=fill.weight-SMA.fill.weight) 
#Figure 9 #need to remove NAs
plot(l4p1&residual, type="b", col="blue")

print(paste0("Fill Weight: ", sd(l4p1$fill.weight)))
print(paste0("Moving Average Residuals: ", sd(l4p1$residual[-1]))) # After removing NAs result does not match .5536 in write-up

residuals.full <- data %>% arrange(lxp,pull) %>% filter(is.na(fill.weight)==FALSE) %>%
  mutate(
    SMA.fill.weight = SMA(fill.weight,2),
    residual=fill.weight-SMA.fill.weight)

#Figure 10
plot(residuals.full$fill.weight, type="b", col="blue")
lines(residuals.full$SMA.fill.weight, type="b", col="red")

#Figure 11
plot(residuals.full$residual, type="b", col="blue")

#Figure 12
hist(residuals.full$residual, breaks=30)
residuals.full %>% filter(is.na(fill.weight)==FALSE) %>% summarize(sd.fill.weight=sd(fill.weight))
residuals.full %>% filter(is.na(residual)==FALSE) %>% summarize(sd.residual=sd(residual))
# sd does not match for residuals

residuals.full %>% group_by(lane,phase)%>%mutate()
res=glm(fill.weight~lane+phase, data=residuals.full)
glm(fill.weight~lane + phase, data=residuals.full)

nested <- residuals.full %>% 
  nest(data = -lxp)
#Comparison of SD on data original, residuals GLM and residuals SMA
nested %>% rowwise() %>% 
  mutate(
    sd.orginal = sd(data$fill.weight),
    sd.glm = sd(glm(data$fill.weight~data$pull+data$lane)$residuals),
    sd.sma = sd(data$residual))





