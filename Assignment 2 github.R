

full.model <- lm(pain ~ sex + age + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = mydata.new)


step(object = full.model, # start at the full model
direction = "backward" # allow it remove predictors but not add them
)

backward.model <- lm(pain ~ sex + age + pain_cat + mindfulness + cortisol_serum, data = mydata.new)


theorybased.model <- mod_pain2
theorybased.model = mod_pain2

anova(theorybased.model, backward.model)


data_home_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class-2018/master/home_sample_2.csv")

summary(data_home_2)
mydata_2 = data_home_2


theorybased.model

theory = lm(formula = pain ~ sex + age + STAI_trait + pain_cat + mindfulness + 
cortisol_saliva + cortisol_serum, data = data_home_2)

backward = lm(formula = pain ~ sex + age + pain_cat + mindfulness + cortisol_serum, 
data = data_home_2)

backward
summary(backward)
summary(theory)

backward.sample1=lm(formula = pain ~ sex + age + pain_cat + mindfulness + cortisol_serum, 
data = mydata.new)

backward.sample1

summary(backward.sample1)

standardCoefs(backward.sample1)

confint(backward.sample1)

###############################################
mydata_2_predict <- mydata_2

mydata_2_predict$predict_theory = predict(theory, data=mydata_2_predict)

mydata_2_predict$predict_backward = predict(backward, data=mydata_2_predict)

mydata_2_predict$predict_theory


#Sum of squared differences between prediction of pain and pain raiting 
# Smaller SSD = better model 

ssd_theory = sum((mydata_2_predict$predict_theory - mydata_2_predict$pain)^2)
ssd_theory

ssd_backward = sum((mydata_2_predict$predict_backward - mydata_2_predict$pain)^2)

ssd_backward

anova(mod_pain1, mod_pain2)

AIC(theory)
AIC(backward)

AIC(theorybased.model)


summary(theory)

summary(theorybased.model)
summary(backward.model)
AIC(theorybased.model)
AIC(backward.model)

anova(theorybased.model, backward.model)

backward
summary(backward)
