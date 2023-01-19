library(mgcv)
library(MASS)
library(glmnet)
library(tidyverse)
library(mpath)
library(patchwork)
# load("E:/Downloads/Data/Data/Accidents.RData")
# load("E:/Downloads/Data/Data/Breakdowns.RData")
# load("E:/Downloads/Data/Data/Fires.RData")
appa1 = Accidents
appa2 = Fires
for(row in 1:nrow(appa2)){
  for (rownum in 1:nrow(appa1)) {
    if(appa1$Tunnel[rownum] == appa2$Tunnel[row]){
      appa2[row,13] = appa1$Width[rownum]
      appa2[row,14] = appa1$Lanes[rownum]
    }}
}
Fires = appa2
library(patchwork)


appa1 = Accidents
appa2 = Breakdowns
for(row in 1:nrow(appa2)){
  for (rownum in 1:nrow(appa1)) {
    if(appa1$Tunnel[rownum] == appa2$Tunnel[row]){
      appa2[row,14] = appa1$Width[rownum]
      appa2[row,15] = appa1$Lanes[rownum]
    }}
}
Breakdowns = appa2


plot1/plot2/plot3#for traffic
plot1_2/plot2_2/plot3_2#for speed limit









# 
# ggplot()+
#   geom_point(data = breakd , mapping = aes(y = Breakdowns,  x = Traffic), colour = 'blue')+
#   geom_point(data = firr , mapping = aes(y = Fires,  x = log(Traffic)), colour = 'green')
