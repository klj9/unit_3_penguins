#2023-02-28
#Multiple Linear Regression

#When modelling to understand impacts of multiple variables:
#   x1 and x2 should not be correlated - can't separate explanatory power
#When modelling for prediction:
#   can throw in as many variables as possible - correlation doesn't matter

#Continuous (numerical) vs categorical (aka factor/nominal/discrete) variables
#   can treat numerical variables like year as a factor using as.factor(<<data$column>>)

#We want to create a regression to predict bill depth using bill length and species

library(palmerpenguins)
library(tidyverse)
library(ggiraph)
library(ggiraphExtra)

head(penguins)
summary(penguins)

penguins_lm_3 = penguins %>%
  filter(!is.na(bill_depth_mm),
         !is.na(bill_length_mm),
         !is.na(species))
head(penguins_lm_3)
summary(penguins_lm_3)

#build model
lm_3 = lm(bill_depth_mm ~ bill_length_mm + species, data=penguins_lm_3)
class(lm_3)
summary(lm_3)#same slope but different y-intercept for all three species
coef(lm_3)
anova(lm_3)#not the best way to display this info - better for categorical
my_results = broom::tidy(lm_3, conf.int=TRUE, conf.level=0.95) %>% #returns as tibble of the values
  #display upper and lower bounds of confidence interval (default 95%)
  mutate_if(is.numeric, round, 2)#rounds to 2 decimal places
  
#visualize model
#method 1: from ggiraph package
ggPredict(lm_3, se=TRUE, interactive=TRUE)#se parameter shows standard error area

#method 2: base r to generate predictions
lm_3_predictions = predict(lm_3, interval="confidence", level=0.95)#returns vector of predictions for bill depth associated w each row
  #adding confidence interval adds additional columns for lower and upper
head(lm_3_predictions)

penguins_lm_3_predict = cbind(penguins_lm_3, lm_3_predictions)
  #append these three new columns to the original dataset

ggplot(data=penguins_lm_3_predict, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill=species, color=NULL), alpha=0.3)+
  geom_point()+
  geom_line(aes(y=fit))

#if we want to predict outside the range of our data - generate new data
newdata_bill_length_mm = seq(min(penguins_lm_3$bill_length_mm), 
                             max(penguins_lm_3$bill_length_mm),
                             by=0.1)
head(newdata_bill_length_mm)  
newdata = expand.grid(bill_length_mm=newdata_bill_length_mm, 
                      species=unique(penguins_lm_3$species))
head(newdata)  
summary(newdata)  

newdata_predict_lm_3 = cbind(newdata, predict(lm_3, newdata=newdata, interval="confidence"))
head(newdata_predict_lm_3)  

ggplot()+
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(data=newdata_predict_lm_3, 
              aes(ymin=lwr, ymax=upr, x=bill_length_mm, fill=species), 
              alpha=0.5)+
  geom_line(data=newdata_predict_lm_3, aes(y=fit, x=bill_length_mm, color=species))
#same figure as before, but model extrapolates outside range of observations for each species

#method 3: tidyverse to generate predictions
lm_3_predict = lm_3 %>%
  broom::augment(data=penguins_lm_3, se_fit=TRUE, interval="confidence")
glimpse(lm_3_predict)
  #gives fitted, lower, upper, se, residuals, etc

ggplot()+
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(data=lm_3_predict, 
              aes(ymin=.lower, ymax=.upper, x=bill_length_mm, fill=species), 
              alpha=0.5)+
  geom_line(data=lm_3_predict, aes(y=.fitted, x=bill_length_mm, color=species))

