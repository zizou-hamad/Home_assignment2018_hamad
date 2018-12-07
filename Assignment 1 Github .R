
#########################
#Load packages and data 	
#########################

#Load packages 
library(PerformanceAnalytics)

# Load data 
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_1.csv")

mydata=data_sample_1


#########################
#Look for outliers	  	
#########################

summary(mydata)
chart.Boxplot(mydata)
chart.Boxplot(mydata.noutliers)

#eliminate outliers 
mydata.noutliers <- mydata[-c(28,112,146),]

summary(mydata.noutliers)
mydata.new=mydata.noutliers
summary(mydata.new)

#########################
#Hierarchical regression		
#########################

#We first build a model to predict the pain by using only sex and age as predictors.  
mod_pain1 <- lm(pain ~ sex + age, data = mydata.new)

# Next, we want to see whether we can improve the effeffctiveness of our prediction by taking 
# into account psychological factors in our model, in addition to sex and age we add 
# STAI, pain catastrophizing, mindfulness, and cortisol measures (model 2). 
mod_pain2 = lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = mydata.new)   

# We can look at the adj. R squared statistic to see how much variance is explained by the new and the old model.
summary(mod_pain1)$adj.r.squared
summary(mod_pain2)$adj.r.squared

#########################
#Anova and AIC 	
#########################

anova(mod_pain1, mod_pain2)

AIC(mod_pain1)
AIC(mod_pain2)

#test the models 
mod_pain2

lm(pain ~ sex + age + STAI_trait + pain_cat + mindfulness + cortisol_saliva + cortisol_serum, data = mydata.new)

summary(mod_pain1)
summary(mod_pain2)
confint(mod_pain1)
confint(mod_pain2)


#standardised Coefficients 
standardCoefs(mod_pain1)
standardCoefs(mod_pain2)

##Check the assumptions

#Checking for outliers and the normality of the residuals
residuals(object = mod_pain1)
residuals(object = mod_pain2) 

rstandard( model = mod_pain1) 
rstandard( model = mod_pain2) 

cooks.distance( model = mod_pain1)
cooks.distance( model = mod_pain2)

plot(x = mod_pain1, which = 4) 
plot(x = mod_pain2, which = 5)

plot( x = mod_pain1, which = 2 )
plot( x = mod_pain2, which = 2 )

#Checking the linearity of the relationship
plot(x = mod_pain1, which = 1)
plot(x = mod_pain2, which = 1)

#Checking the homogeneity of variance

plot(x = mod_pain1, which = 3)
plot(x = mod_pain2, which = 3)

# Check for collinearity
library(car)
vif(mod = mod_pain1)
vif(mod = mod_pain2)



