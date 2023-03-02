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
#from ggiraph package
ggPredict(lm_3, se=TRUE, interactive=TRUE)#se parameter shows standard error area

#base r to generate predictions
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

#generate new data - expand grid with tidyverse instead of base r
newdata = penguins_lm_3 %>%
  tidyr::expand(bill_length_mm, species)#fills out rest of data frame with every combo of these variables
head(newdata)#dropped every column that wasn't passed

lm_3_predict = lm_3 %>%
  broom::augment(newdata=newdata, se_fit=TRUE, interval="confidence")
head(lm_3_predict)#appends .fitted, .lower, .upper, and .se.fit

#visualize with expanded dataframe
ggplot()+
  geom_point(data=penguins_lm_3, aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_ribbon(data=lm_3_predict, 
              aes(ymin=.lower, ymax=.upper, x=bill_length_mm, fill=species), 
              alpha=0.5)+
  geom_line(data=lm_3_predict, aes(y=.fitted, x=bill_length_mm, color=species))

#adding an interaction term - depth explained by each var individually AND interaction between the vars
#lm_4 = lm(bill_depth_mm ~ bill_length_mm + species + bill_length_mm:species,
          #data=penguins_lm_3)#formal/explicit way to pass it in r
lm_4 = lm(bill_depth_mm ~ bill_length_mm * species,
          data=penguins_lm_3)
summary(lm_4)#interaction terms are actually not significant - added model complexity is not worthwhile

#tools for model comparison/evaluation
AIC(lm_3, lm_4)#evaluate model fitness - smaller is better (2 units=significantly better)
    #lm_3 is a better fit than lm_4

best_model = step(lm_4)#goes thru variables one by one to find best fit with lowest complexity
summary(best_model)#exactly the same as lm_3

#plot with interation
lm_4_predict = lm_4 %>%
  broom::augment(interval="confidence")
head(lm_4_predict)

ggplot(data=lm_4_predict)+
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm, color=species))+
  geom_line(aes(x=bill_length_mm, y=.fitted, color=species))+
  geom_ribbon(aes(x=bill_length_mm, ymin=.lower, ymax=.upper, fill=species), alpha=0.5)
#slight differences in slope is not worth the added model complexity in this case

library(car)

#depth ~ bill length + flipper length + body mass for gentoo only
gentoo = penguins %>%
  filter(species=="Gentoo")

lm_gentoo_1 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
lm_gentoo_2 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm, data=gentoo)
lm_gentoo_3 = lm(bill_depth_mm ~ bill_length_mm + flipper_length_mm + body_mass_g, data=gentoo)

AIC(lm_gentoo_1, lm_gentoo_2, lm_gentoo_3)#3 is the best fitting model
step(lm_gentoo_3)#keeps all three variables
vif(lm_gentoo_3)#variance inflation factor

head(penguins_lm_3)

#collapse visualization into 3 2D plots by setting two variables constant while we plot each ind var
newdata = gentoo %>%
  select(bill_length_mm) %>%
  mutate(flipper_length_mm = median(gentoo$flipper_length_mm, na.rm=TRUE),
         body_mass_g = median(gentoo$body_mass_g, na.rm=TRUE))

head(newdata)
lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata=newdata, interval="confidence")
head(lm_gentoo_3_predict)

ggplot(data=lm_gentoo_3_predict)+
  geom_point(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_line(aes(x=bill_length_mm, y=.fitted))+
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=bill_length_mm), alpha=0.5)+
  annotate("text", x=57, y=13.5, label=paste0("flipper length = ", 
                                              median(gentoo$flipper_length_mm, na.rm=TRUE),
                                              " mm"))+
  annotate("text", x=57, y=13.8, label=paste0("body mass = ", 
                                              median(gentoo$body_mass_g, na.rm=TRUE),
                                              " g"))

#do the same process to visualize flipper length now
newdata = gentoo %>%
  select(flipper_length_mm) %>%
  mutate(body_mass_g = median(gentoo$body_mass_g, na.rm=TRUE),
         bill_length_mm = median(gentoo$bill_length_mm, na.rm=TRUE))

head(newdata)
lm_gentoo_3_predict = lm_gentoo_3 %>%
  broom::augment(newdata=newdata, interval="confidence")
head(lm_gentoo_3_predict)

ggplot(data=lm_gentoo_3_predict)+
  geom_point(data=gentoo, aes(x=flipper_length_mm, y=bill_depth_mm))+
  geom_line(aes(x=flipper_length_mm, y=.fitted))+
  geom_ribbon(aes(ymin=.lower, ymax=.upper, x=flipper_length_mm), alpha=0.5)+
  annotate("text", x=208, y=17.2, label=paste0("bill length = ", 
                                              median(gentoo$bill_length_mm, na.rm=TRUE),
                                              " mm"))+
  annotate("text", x=208, y=16.8, label=paste0("body mass = ", 
                                              median(gentoo$body_mass_g, na.rm=TRUE),
                                              " g"))

#ANOVA
penguin_lm = lm(body_mass_g ~ species + sex, data=penguins)
anova(penguin_lm)

penguin_anova = aov(body_mass_g ~ species + sex, data=penguins)
summary(penguin_anova)
TukeyHSD(penguin_anova)
