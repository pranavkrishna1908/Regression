library(mgcv);library(MASS);library(glmnet);library(tidyverse);library(mpath);library(patchwork)
# load("E:/Downloads/Data/Data/Accidents.RData")
# load("E:/Downloads/Data/Data/Breakdowns.RData")
# load("E:/Downloads/Data/Data/Fires.RData")
#as mentioned, the data for breakdowns and fires did not have a few covariates which were in the accident data
#we fill in them here
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
colnames(Fires) = c(colnames(Fires)[1:12],'Width', 'Lanes')
#we create another variable to house the data. So, we can make changes on the data while not losing the original copy
firr = Fires
#we remove the column for tunnels
firr = firr[,-c(11)]
firr$Length  = log(firr$Length)
firr$Traffic = log(firr$Traffic)

#plot the fires vs traffic
plot3 = ggplot()+
  geom_point(data = firr , mapping = aes(y = Fires,  x = log(Traffic)))+
  labs( x = 'log(Traffic)', y = 'Fires')+
  theme(plot.title = element_text(hjust = 0.5))


#plot against log-length
plot3_2 = ggplot()+
  geom_point(data = firr , mapping = aes(y = Fires,  x = Limit))+
  labs(x = 'Speed Limit', y = 'Fires')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c(seq(30,110,10)))

#try variance stabilising transform with the linear model
firr$Fires = 2 *sqrt(firr$Fires)
temp = glm(Fires~., data = firr)
temp2 = stepAIC(temp)
(temp2)
print(temp2$aic)


#ignore the changes made to the data so far
firr = Fires
firr = firr[,-c(11)]


#remove outliers for too larege fires and too large traffic
firr = firr[-which(firr$Fires > 12),]
firr = firr[-which(firr$Traffic > 1.5*10^9),]

firr$Length  = log(firr$Length)#take log of length
firr$Traffic = log(firr$Traffic)#take log of traffic
rownames(firr) = 1:nrow(firr)



firesthing1_prop = glm(Fires ~ ., data = firr, family = poisson)
#plot(firesthing1_prop)
#
# Warning message:
# not plotting observations with leverage one:
#  147 #remove observation 147
firr2 = firr#[-c(147,38,26,120),]#we dont' remove a few observations whrer the fit was very bad,
rownames(firr2) = 1:nrow(firr2)
firr2_prop = firr2


firesthing2_prop = glm(Fires ~ ., data = firr2, family = poisson)

final_prime_prop = stepAIC(firesthing2_prop)
# final2 = stepAIC(firesthing1)
#check for extra complexity
final_prime_prop$aic
glm(formula = Fires ~ Traffic + HGV + Slope + Length + SlopeType, 
    family = poisson, data = firr2)$aic#baseline
glm(formula = Fires ~ Traffic + HGV + Slope + Length , 
    family = poisson, data = firr2)$aic#try to remove slope type first, because of lack of information, succeeded
glm(formula = Fires ~ Traffic  + Slope + Length , #coudnt remove length
    family = poisson, data = firr2)$aic
glm(formula = Fires ~ Traffic   + Length , #successfully remove slope
    family = poisson, data = firr2)$aic
final_prop = glm(formula = Fires ~ Traffic   + Length , #coudnt remove length
                 family = poisson, data = firr2)
#diagnostics
ggplot()+
  geom_point(aes(x = final_prop$fitted.values, y = final_prop$residuals))
firr3_prop = firr2[-which(final_prop$residuals>6   ),]
final_prop = glm(formula = Fires ~ Traffic   + Length , #coudnt remove length
                 family = poisson, data = firr3_prop)
firr3 = firr3_prop

final = final_prop

anova(final_prop, test="Chisq")

temp_res = rstandard(final_prop,type='deviance')
devplot = ggplot()+
  geom_point(aes(x = final_prop$fitted.values, y = temp_res))+
  labs( x = 'Fitted Values', y = 'Deviance')+
  theme(plot.title = element_text(hjust = 0.5))
  
temp_cook = cooks.distance(final_prop)
cookplot = ggplot()+
  geom_point(aes(x = final_prop$fitted.values, y = temp_cook))+
  labs( x = 'Fitted Values', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(firr3) - 2*(final$df.null - final$df.residual)))
cookvslev = ggplot()+
  geom_point(aes(x =hatvalues(final) , y = temp_cook))+
  labs( x = 'Leverage', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(firr3) -2*(final$df.null - final$df.residual)), colour = 'red')

resplot = ggplot()+
  geom_point(aes(x = predict(final), y = temp_res))+
  labs( x = 'Fitted Values', y = 'Residuals')+
  theme(plot.title = element_text(hjust = 0.5))
qqdev = ggplot()+
  geom_qq(aes(sample = temp_res))+
  geom_qq_line(aes(sample = temp_res))+
  labs( x = 'Theoretical Quantiles', y = 'Sample Quantiles')

temp_res[which(temp_cook>8/(nrow(firr3) - 2*2))]




resplot/cookvslev | qqdev

#using hgv as number
#ignore the changes made to the data so far
firr = Fires
firr = firr[,-c(11)]


#remove outliers for too larege fires and too large traffic
firr = firr[-which(firr$Fires > 12),]
firr = firr[-which(firr$Traffic > 1.5*10^9),]

#firr$Length  = log(firr$Length)take log of length


firr$HGV = firr$HGV * firr$Traffic
#firr$HGV = log(firr$HGV)
firr$Traffic = log(firr$Traffic)#take log of traffic
rownames(firr) = 1:nrow(firr)



firesthing1 = glm(Fires ~ ., data = firr, family = poisson)
#plot(firesthing1)
# Warning message:
# not plotting observations with leverage one:
#  147 #remove observation 147
firr2 = firr[-c(147,38,26,120),]#we remove a few observations whrer the fit was very bad,
firesthing2 = glm(Fires ~ ., data = firr2, family = poisson)
final_prime = stepAIC(firesthing2)
# final2 = stepAIC(firesthing1)
glm(formula = Fires ~ HGV + Slope + Length + SlopeType + Company + 
      Width + Lanes, family = poisson, data = firr2)$aic
glm(formula = Fires ~ HGV + Slope + Length + SlopeType + Company + #successfully remove lanes
      Width , family = poisson, data = firr2)$aic
glm(formula = Fires ~ HGV + Slope + Length + SlopeType + Company  #successfully remove width
       , family = poisson, data = firr2)$aic
glm(formula = Fires ~ HGV + Slope + Length  + Company  #coudnt remove compnay, successfully remove slope type
    , family = poisson, data = firr2)$aic
glm(formula = Fires ~ HGV  + Length  + Company  #successfully remove slope
    , family = poisson, data = firr2)$aic
final = glm(formula = Fires ~ HGV  + Length  + Company  #couldnt remove hgv
            , family = poisson, data = firr2)
plot(final)
summary(final)
anova(final)

ggplot()+
  geom_point(aes(x = final$fitted.values, y = final$residuals))
firr3 = firr2[-which(final$residuals>6   ),]
final = glm(formula = Fires ~ HGV  + Length  + Company  #couldnt remove hgv
            , family = poisson, data = firr3)
ggplot()+
  geom_point(aes(x = final$fitted.values, y = final$residuals))+
  labs( x = 'Fitted Values', y = 'Residuals')+
  theme(plot.title = element_text(hjust = 0.5))

temp = rstandard(final,type='deviance')
ggplot()+
  geom_point(aes(x = final$fitted.values, y = temp))+
  labs( x = 'Fitted Values', y = 'Deviance')+
  theme(plot.title = element_text(hjust = 0.5))


final = final_prop



