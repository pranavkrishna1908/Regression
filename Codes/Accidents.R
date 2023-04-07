# load("E:/Downloads/Data/Data/Accidents.RData")
# load("E:/Downloads/Data/Data/Breakdowns.RData")
# load("E:/Downloads/Data/Data/Fires.RData")
library(mgcv);library(MASS);library(glmnet);library(tidyverse);library(mpath);library(patchwork)
accdata = Accidents[,-c(14)]#remove the tunnels column
Accidents_initial = Accidents#store a copy in memory

#plot vs traffic
plot1 = ggplot()+
  geom_point(data = accdata , mapping = aes(y = Acc,  x = Traffic, col = Year))+
  labs(x = 'log(Traffic)', y = 'Accidents')+
  theme(plot.title = element_text(hjust = 0.5))


accdata = accdata[-which(accdata$Acc > 80), ] #80 accidents outlier
accdata = accdata[-which(accdata$Traffic > 1.2*10^8),] #8 outliers for traffic large

#plot vs speed limit
# plot1 = ggplot()+
#   geom_point(data = accdata , mapping = aes(y = Acc,  x = Limit, col = Year))+
#   labs(x = 'Speed Limit', y = 'Accidents')+
#   theme(plot.title = element_text(hjust = 0.5))



plot1_2 = ggplot()+
  geom_point(data = accdata , mapping = aes(y = Acc,  x = Limit,  colour = Year))+
  labs(x = 'Speed Limit', y = 'Accidents')+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(limits=c(seq(30,110,10)))+
  theme(legend.position = 'none')


#plot to see the accidents in 2002 vs limit
temp_2002 = accdata[which(accdata$Year == '2002'),]
temp2002 = ggplot()+
  geom_point(data = temp_2002 , mapping = aes(y = Acc,  x = Limit))+
  labs(x = 'Speed Limit', y = 'accdata')+
  theme(plot.title = element_text(hjust = 0.5))


#try linlear model
accdata$Acc =  2 * sqrt(accdata$Acc)
accdata$Year = as.factor(accdata$Year)
accdata$Traffic = log(accdata$Traffic)
accdata$Length = log(accdata$Length)
linear_mode = glm(Acc~., data = accdata)#meh
#plot(linear_mode)


#foreget changes
accdata = Accidents[,-c(2,14)]
accdata$Year = as.factor(accdata$Year)

# accdata = accdata[-c(224,225),] # two outliers for traffic small
# separate the hgv zero data
hgvzero = accdata[which(accdata$HGV == 0),]
hgvzero = hgvzero[,-c(4,6)]
hgvzero = droplevels(hgvzero)


#non-zero hgv #hgv as number
accdata = accdata[-which(accdata$HGV == 0),]
accdata$HGV = accdata$HGV*accdata$Traffic
accdata$HGV = log(accdata$HGV)
accdata$Traffic = log(accdata$Traffic)
accdata$Length = log(accdata$Length)

thing = glm(Acc ~ .+Slope:SlopeType, data = accdata, family = poisson)
thing2 = glm.nb(Acc ~ .+Slope:SlopeType, data = accdata) # +HGV:Urban:Limit+UrbanSlopeType
final = stepAIC(thing2)
anova(final)
final2 = step(final, k = log(nrow(accdata)))
glm.nb(formula = Acc ~ Traffic + HGV + Urban + Type + Length + 
         Width + Company, data = accdata, init.theta = 2.152239646, 
       link = log)$aic#baseline
anova(final2, test = 'Chisq')
# Analysis of Deviance Table
# 
# Model: Negative Binomial(2.2566), link: log
# 
# Response: Acc
# 
# Terms added sequentially (first to last)
# 
# 
# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                       1081     3011.1              
# HGV        1   705.85      1080     2305.2 < 2.2e-16 ***
#   Slope      1    10.50      1079     2294.7  0.001192 ** 
#   Urban      1   557.99      1078     1736.7 < 2.2e-16 ***
#   Length     1    70.53      1077     1666.2 < 2.2e-16 ***
#   SlopeType  4   212.03      1073     1454.2 < 2.2e-16 ***
#   Width      1     0.01      1072     1454.1  0.933025    
# Lanes      1    22.05      1071     1432.1 2.661e-06 ***
#   Company   22   260.69      1049     1171.4 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

final3 = glm.nb(formula = Acc ~ HGV + Slope + Urban + Length + SlopeType 
          + Lanes + Company, data = accdata, init.theta = 2.256621109, 
       link = log)#baseline


final = final3


#diagnostics for hgv non zero
anova(final3, test="Chisq")



ggplot()+
  geom_point(aes(x = log(final3$fitted.values), y = final3$residuals))
temp_res = rstandard(final3,type='deviance')

resplot = ggplot()+
  geom_point(aes(x = log(final3$fitted.values), y = temp_res))+
  labs( x = 'Fitted Values', y = 'Residuals')+
  theme(plot.title = element_text(hjust = 0.5))

devplot = ggplot()+
  geom_point(aes(x = log(final3$fitted.values), y = temp_res))+
  labs( x = 'Fitted Values', y = 'Deviance')+
  theme(plot.title = element_text(hjust = 0.5))

temp_cook = cooks.distance(final3)
cookplot = ggplot()+
  geom_point(aes(x = log(final3$fitted.values), y = temp_cook))+
  labs( x = 'Fitted Values', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(accdata) - 2*(final$df.null - final$df.residual)))

cookvslev = ggplot()+
  geom_point(aes(x =hatvalues(final3) , y = temp_cook))+
  labs( x = 'Leverage', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(accdata) -2*(final$df.null - final$df.residual)), colour = 'red')

qqdev = ggplot()+
  geom_qq(aes(sample = temp_res))+
  geom_qq_line(aes(sample = temp_res))+
  labs( x = 'Theoretical Quantiles', y = 'Sample Quantiles')

temp_res[which(temp_cook>8/(nrow(accdata) - 2*2))]
(resplot/cookvslev)|qqdev

ggplot()+
   geom_qq(aes(sample = temp_res))+
  geom_qq_line(aes(sample = temp_res))

































#non-zero hgv, we use proportions now
#foreget changes
accdata = Accidents[,-c(2,14)]
accdata$Year = as.factor(accdata$Year)
accdata$Traffic = log(accdata$Traffic)
accdata = accdata[-which(accdata$HGV == 0),]
# accdata$HGV = accdata$HGV*accdata$Traffic
# accdata$HGV = log(accdata$HGV)
accdata$Length = log(accdata$Length)

thing_prop = glm(Acc ~ .+Slope:SlopeType, data = accdata, family = poisson)
thing2_prop = glm.nb(Acc ~ .+Slope:SlopeType, data = accdata) # +HGV:Urban:Limit+UrbanSlopeType
final_prop = stepAIC(thing2_prop)
anova(final_prop)
final2_prop = step(final, k = log(nrow(accdata)))
glm.nb(formula = Acc ~ HGV + Slope + Urban + Length + SlopeType + 
         Lanes + Company, data = accdata, init.theta = 2.185271244, 
       link = log)$aic#baseline
anova(final2_prop, test = 'Chisq')
# Analysis of Deviance Table
# 
# Model: Negative Binomial(2.1853), link: log
# 
# Response: Acc
# 
# Terms added sequentially (first to last)
# 
# 
# Df Deviance Resid. Df Resid. Dev  Pr(>Chi)    
# NULL                       1081     2961.4              
# HGV        1   690.31      1080     2271.1 < 2.2e-16 ***
#   Slope      1    10.31      1079     2260.7   0.00132 ** 
#   Urban      1   549.79      1078     1711.0 < 2.2e-16 ***
#   Length     1    68.66      1077     1642.3 < 2.2e-16 ***
#   SlopeType  4   207.37      1073     1434.9 < 2.2e-16 ***
#   Lanes      1    21.82      1072     1413.1 2.993e-06 ***
#   Company   22   246.21      1050     1166.9 < 2.2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


glm.nb(formula = Acc ~ HGV + Slope + Urban + Length + SlopeType + 
         Lanes + Company, data = accdata, init.theta = 2.185271244, 
       link = log)
final2prop = glm.nb(formula = Acc ~ HGV + Slope + Urban + Length + SlopeType + 
                              Lanes + Company, data = accdata, init.theta = 2.185271244, 
                          link = log)





#zero hgv
thing11prime = glm.nb(Acc ~ .+Slope:SlopeType, data = hgvzero)
thing12prime = glm(Acc ~ .+Slope:SlopeType, data = hgvzero, family = poisson)

thing11prime
finalprime = stepAIC(thing11prime)

# finalprime$aic#baseline
glm.nb(formula = Acc ~ Year  + Traffic + Slope + Type + 
         SlopeType + Company, data = hgvzero, init.theta = 8.081369483, 
       link = log)$aic


glm.nb(formula = Acc ~ Year  +Traffic  + Type + #successfully remove slope
         SlopeType + Company, data = hgvzero, init.theta = 8.081369483, 
       link = log)$aic
glm.nb(formula = Acc ~ Year    + Type + #successfully remove traffic
         SlopeType + Company, data = hgvzero, init.theta = 8.081369483, 
       link = log)$aic
glm.nb(formula = Acc ~      Type + #successfully remove year
         SlopeType + Company, data = hgvzero, init.theta = 8.081369483, 
       link = log)$aic
glm.nb(formula = Acc ~       
         SlopeType + Company, data = hgvzero, init.theta = 8.081369483, 
       link = log)$aic




finalprime2 = glm.nb(formula = Acc ~       
                       SlopeType + Company, data = hgvzero, init.theta = 8.081369483, 
                     link = log)

final = finalprime2

#diagnostics for hgv non zero
anova(final, test="Chisq")



ggplot()+
  geom_point(aes(x = log(final$fitted.values), y = final$residuals))
temp_res = rstandard(final,type='deviance')

resplot = ggplot()+
  geom_point(aes(x = log(final$fitted.values), y = temp_res))+
  labs( x = 'Fitted Values', y = 'Residuals')+
  theme(plot.title = element_text(hjust = 0.5))

devplot = ggplot()+
  geom_point(aes(x = log(final$fitted.values), y = temp_res))+
  labs( x = 'Fitted Values', y = 'Deviance')+
  theme(plot.title = element_text(hjust = 0.5))

temp_cook = cooks.distance(final)
cookplot = ggplot()+
  geom_point(aes(x = log(final$fitted.values), y = temp_cook))+
  labs( x = 'Fitted Values', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(hgvzero) - 2*(final$df.null - final$df.residual)))

cookvslev = ggplot()+
  geom_point(aes(x =hatvalues(final) , y = temp_cook))+
  labs( x = 'Leverage', y = 'Cooks Distance')+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_hline( yintercept = 8/(nrow(hgvzero) -2*(final$df.null - final$df.residual)), colour = 'red')

qqdev = ggplot()+
  geom_qq(aes(sample = temp_res))+
  geom_qq_line(aes(sample = temp_res))+
  labs( x = 'Theoretical Quantiles', y = 'Sample Quantiles')

temp_res[which(temp_cook>8/(nrow(hgvzero) - 2*2))]
(resplot/cookvslev)|qqdev

ggplot()+
  geom_qq(aes(sample = temp_res))+
  geom_qq_line(aes(sample = temp_res))

# 
# 
# 
# #discuss direction
# #predictions less thn 0
# #since data is skewed right, taking log makes sense but there are zeros
# 
# 
# 
# # colnames(bbotstrap) = names(thing2$coefficients)
# # 'random effects'
# 
# 
# 
# acc2 = accdata
# acc2$Slope =  acc2$Slope * I(acc2$Slope<0)       #slope*I(slope>0)
# acc2$new = acc2$Slope*I(acc2$Slope >= 0)
# thing3 = glm.nb(Acc ~ ., data = acc2)
# final2 = stepAIC(thing3)
# (final2)
# 
# #something = glmregNB(Acc ~ ., data = accdata, alpha = 1)
# #accidents_breakdowns =left_join(Accidents,Breakdowns, by = c('Year', "Direction", "Traffic"  , "HGV"     ,  "Slope"   ,  "Urban"  ,
# #                                                             "Type"  ,    "Length"  ,  "Limit"  ,   "SlopeType" ,"Tunnel"  ,  "Company"  ))
