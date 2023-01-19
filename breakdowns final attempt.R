library(mgcv);library(MASS);library(glmnet);library(tidyverse);library(mpath);library(patchwork)
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
colnames(Breakdowns) = c(colnames(Breakdowns)[1:13], 'Width', 'Lanes')
#added the missing covariates to the data
breakd = Breakdowns
breakd = breakd[,-c(12)]
#remove the column corresponding to the tunnels
breakd$Year = as.factor(breakd$Year)
#treat year as a factor variable

#plot vs traffic
plot2 = ggplot()+
  geom_point(data = breakd , mapping = aes(y = Breakdowns,  x = Traffic, col = Year))+
  labs(x = 'log(Traffic)', y = 'Breakdowns')+
  theme(plot.title = element_text(hjust = 0.5))

breakd = breakd[-which(breakd$Breakdowns> 179),] # remove outliers for breakdowns
breakd = breakd[-which(breakd$Traffic > 1.2*10^8),] #remove 5 outliers of traffic

#plot vs speed limit
plot2_2 = ggplot()+
  geom_point(data = breakd , mapping = aes(y = Breakdowns,  x = Limit, col = Year))+
  labs(x = 'Speed Limit', y = 'Breakdowns')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c(seq(30,110,10)))

#investigatory plot for effect of speed limit in 2002
temp = breakd[which(breakd$Year == '2002'),]
temp2 = ggplot()+
  geom_point(data = temp , mapping = aes(y = Breakdowns,  x = Limit))+
               labs(x = 'Speed Limit', y = 'Breakdowns')+
               theme(plot.title = element_text(hjust = 0.5))

#linear model
breakdLinear = breakd
breakdLinear$Breakdowns = 2 * sqrt(breakdLinear$Breakdowns)
linear_model = glm(Breakdowns~., data= breakdLinear)
breakd_prime = breakd[which(breakd$HGV == 0),]
#work with hgv non-zero
#poisson model
breakd = breakd[-which(breakd$HGV == 0),]
breakd_prop = breakd
breakd$HGV = breakd$HGV * breakd$Traffic
breakd$Traffic = log(breakd$Traffic)
breakd$HGV = log(breakd$HGV)
breakd$Length = log(breakd$Length)
thing21 = glm(Breakdowns ~ ., data = breakd, family = poisson) #bith were good to aic and deviance down 200
final21 = stepAIC(thing21)



#negative binomial model #hgv as number
thing22 = glm.nb(Breakdowns ~ ., data = breakd) 
thing222 = glm.nb(formula = Breakdowns ~ .+Slope:SlopeType, data = breakd, 
                  link = log)
final222 = stepAIC(thing222)
final222 = step(final222, k = log(964))
final222$aic
glm.nb(formula = Breakdowns ~ Direction + Traffic + Slope + Length + 
         SlopeType + Company + Lanes, data = breakd, init.theta = 2.032280963, 
       link = log)$aic#baseline

anova(final222)
#          Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                        963     3110.3              
# Direction  1    21.95       962     3088.4 2.793e-06 ***
# Traffic    1   154.86       961     2933.5 < 2.2e-16 ***
# Slope      1   210.08       960     2723.4 < 2.2e-16 ***
# Length     1   898.23       959     1825.2 < 2.2e-16 ***
# SlopeType  4   215.19       955     1610.0 < 2.2e-16 ***
# Company   22   475.33       933     1134.7 < 2.2e-16 ***
# Lanes      1    27.90       932     1106.8 1.280e-07 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

final222 = glm.nb(formula = Breakdowns ~  Traffic + Slope + Length + 
                    SlopeType + Company , data = breakd, init.theta = 2.032280963, 
                  link = log)


#with hgv as proportion
#negative binomial model
thing22_prop = glm.nb(Breakdowns ~ ., data = breakd_prop) 
thing222_prop = glm.nb(formula = Breakdowns ~ .+Slope:SlopeType, data = breakd_prop, 
                  link = log)
final222_prop = stepAIC(thing222)
final222_prop = step(final222, k = log(964))








#diagnostics for hgv non zero
final = final222
anova(final, test = 'Chisq')


ggplot()+
  geom_point(aes(x = final222$fitted.values, y = final222$residuals))
temp_res = rstandard(final222,type='deviance')

resplot = ggplot()+
  geom_point(aes(x = log(final222$fitted.values), y = temp_res))+
  labs( x = 'Fitted Values', y = 'Residuals')+
  theme(plot.title = element_text(hjust = 0.5))

devplot = ggplot()+
  geom_point(aes(x = final222$fitted.values, y = temp_res))+
  labs( x = 'Fitted Values', y = 'Deviance')+
  theme(plot.title = element_text(hjust = 0.5))

temp_cook = cooks.distance(final222)
cookplot = ggplot()+
  geom_point(aes(x = final222$fitted.values, y = temp_cook))+
  labs( x = 'Fitted Values', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(breakd) - 2*(final$df.null - final$df.residual)))

cookvslev = ggplot()+
  geom_point(aes(x =hatvalues(final222) , y = temp_cook))+
  labs( x = 'Leverage', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(breakd) -2*(final$df.null - final$df.residual)), colour = 'red')

qqdev = ggplot()+
  geom_qq(aes(sample = temp_res))+
  geom_qq_line(aes(sample = temp_res))+
  labs( x = 'Theoretical Quantiles', y = 'Sample Quantiles')

temp_res[which(temp_cook>8/(nrow(breakd) - 2*2))]
resplot/cookvslev|qqdev



























































#for hgv zero
breakd_prime = breakd_prime[,-c(5,7)]
breakd_prime = droplevels(breakd_prime)

#poisson model
thing11= glm(Breakdowns ~ ., data = breakd_prime, family = poisson)
final11 = stepAIC(thing11)

#nb model
thing12 = glm.nb(Breakdowns ~ .+Slope:SlopeType, data = breakd_prime) #aic down 3280, defnitely
final12 = stepAIC(thing12)
glm.nb(formula = Breakdowns ~ Slope + Length + Company + Width + 
         Lanes, data = breakd_prime, init.theta = 7.703032585, link = log)$aic#baseline
glm.nb(formula = Breakdowns ~ Slope + Length + Company + Width  #removed lanes
         , data = breakd_prime, init.theta = 7.703032585, link = log)
glm.nb(formula = Breakdowns ~ Slope + Length + Company #removed width
       , data = breakd_prime, init.theta = 7.703032585, link = log)$aic
glm.nb(formula = Breakdowns ~ Slope  + Company #removed length
       , data = breakd_prime, init.theta = 7.703032585, link = log)$aic
glm.nb(formula = Breakdowns ~    Company #removed slope
       , data = breakd_prime, init.theta = 7.703032585, link = log)$aic

final12 = glm.nb(formula = Breakdowns ~   Company , data = breakd_prime, link = log)
#plot(final12)













#diagnostics for hgv zero
anova(final12, test="Chisq")

temp_res = rstandard(final12,type='deviance')

ggplot()+
  geom_point(aes(x = log(final12$fitted.values), y = final12$residuals))

resplot = ggplot()+
  geom_point(aes(x = log(final12$fitted.values), y = final12$residuals))+
  labs( x = 'Fitted Values', y = 'Residuals')+
  theme(plot.title = element_text(hjust = 0.5))

devplot = ggplot()+
  geom_point(aes(x = log(final12$fitted.values), y = temp_res))+
  labs( x = 'Fitted Values', y = 'Deviance')+
  theme(plot.title = element_text(hjust = 0.5))

temp_cook = cooks.distance(final12)
cookplot = ggplot()+
  geom_point(aes(x = log(final12$fitted.values), y = temp_cook))+
  labs( x = 'Fitted Values', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(breakd_prime) - 2*(final$df.null - final$df.residual)))

cookvslev = ggplot()+
  geom_point(aes(x =hatvalues(final12) , y = temp_cook))+
  labs( x = 'Leverage', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(breakd_prime) -2*(final$df.null - final$df.residual)), colour = 'red')


qqdev = ggplot()+
  geom_qq(aes(sample = temp_res))+
  geom_qq_line(aes(sample = temp_res))+
  labs( x = 'Theoretical Quantiles', y = 'Sample Quantiles')

temp_res[which(temp_cook>8/(nrow(breakd_prime) - 2*2))]
resplot/cookvslev|qqdev




#another model we tried 
# acc2 = breakd
# acc2$Slope =  acc2$Slope * I(acc2$Slope<0)       #slope*I(slope>0)
# acc2$new = acc2$Slope*I(acc2$Slope >= 0)
# thing3 = glm.nb(Breakdowns ~ ., data = acc2)
# finally = stepAIC(thing3)
# finally = step(finally, k = log(964))
# final222
