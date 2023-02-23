#2023-02-23
#Linear Models

#Assumptions to use a linear regression:
#   -linear relationship
#   -normally distributed residuals
#   -no/little colinearity
#   -no autocorrelation (samples independent)
#   -homoscedasticity (homogeneous residuals across independent variable)

library(palmerpenguins)
library(tidyverse)
library(GGally)
library(broom)

head(penguins)

penguins %>%
  select(species, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g) %>%
  GGally::ggpairs(aes(color=species))

penguins %>%
  select(bill_depth_mm, bill_length_mm, species) %>%
  ggpairs()
  
#running our linear model (not separated by species)
lm_1 = lm(bill_depth_mm ~ bill_length_mm, data=penguins)
class(lm_1)#r has a specific lm class for this
summary(lm_1)#gives table of st error, p-value, t-value, as well as df, r^2, f-stat

ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method="lm")#plots linear model instead of loess smoother

plot(lm_1)#generates mutiple plots: 
#   -residuals vs. fitted y values - homoscedasticity if smoothing line is flat
#   -QQ plot of residuals vs. theoretical normal distribution - normal if on 1 to 1 line
#   -sqrt of residuals vs. fitted y values
#   -residuals vs. leverage (how much weight each point has on the model parameters) - want wide to narrow centered on zero
#   -for each plot, highlights the three most potentially problematic points

gentoo = penguins %>%
  filter(species=="Gentoo")

gentoo %>%
  select(bill_depth_mm, bill_length_mm) %>%
  ggpairs()#see a better distribution, more linear relationship

lm_2 = lm(bill_depth_mm ~ bill_length_mm, data=gentoo)
summary(lm_2)

plot(lm_2)#looks much better than the first

#now we'll plot the gentoo data with the linear regression
ggplot(data=gentoo, aes(x=bill_length_mm, y=bill_depth_mm))+#can call data up here to reduce repetition
  geom_point()+
  geom_smooth(method="lm")
  #geom_point(data=penguins %>% filter(species=="Adelie"), 
             #aes(x=bill_length_mm, y=bill_depth_mm, color="red"))can override initial setup later

#plot each species lm separately

ggplot(data=penguins, aes(x=bill_length_mm, y=bill_depth_mm))+
  geom_point(aes(color=species))+
  geom_smooth(aes(color=species), method="lm")+
  geom_smooth(aes(color="black"),method="lm")+
  theme_bw()

#exercise 5.1:
lm_3 = lm(bill_depth_mm ~ flipper_length_mm, data=gentoo)
summary(lm_3)
ggplot(data=gentoo, aes(x=flipper_length_mm, y=bill_depth_mm))+
  geom_point()+
  geom_smooth(method="lm")
plot(lm_3)
#flipper length better explains bill depth 
#   - adj. r^2 value is 0.50 vs 0.41 for bill length, with p<0.001
