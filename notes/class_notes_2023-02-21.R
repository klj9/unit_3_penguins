#2023-02-21
#T-Tests and Correlations

library(tidyverse)
library(palmerpenguins)
library(rstatix)

head(penguins)

################################################################################
#T-Tests
################################################################################

ggplot(data=penguins)+
  geom_histogram(aes(x=body_mass_g, fill=species))

#one-sample t-test - compare data-derived mean to literature-derived mean
gentoo = penguins %>%
  filter(species=="Gentoo")
head(gentoo)

ggplot(data=gentoo)+
  geom_histogram(aes(x=body_mass_g))#t-test assumes normal distribution
  #our disribution is normal looking enough given the sample size

ggplot(data=gentoo)+
  stat_qq(aes(sample=body_mass_g))
  #plots sample vs. theoretical - looking for a a 1:1 relationship if distibution is normal
  #quantile-quantile plot

gentoo %>%
  summarize(mean_body_mass_g = mean(body_mass_g, na.rm=TRUE),
            sd_body_mass_g = sd(body_mass_g, na.rm=TRUE))

#run the t-test
t.test(gentoo$body_mass_g, mu=5500)
  #p-value of 6e-16 supports alternative hypothesis that true mean != 5500g

t_test_results = gentoo %>%
  t_test(body_mass_g ~ 1, mu=5500)#1 for one-vector t-test
  #returns data frame with n, degrees of freedom, and p-value
  #our mean is significantly smaller that the literature - maybe life-stage, prey availability, etc

#two-sample t-test - are the gentoo and adelies significantly different?
data_for_t_test = penguins %>%
  filter(species %in% c("Gentoo", "Adelie"),
         !is.na(body_mass_g)) %>%
  select(species, body_mass_g) %>% #can drop the other columns if needed
  droplevels() #gets rid of variables that are no longer needed (eg chinstrap category)

summary(data_for_t_test)

data_for_t_test %>%
  group_by(species) %>% #separates mean by species
  summarize(mean=mean(body_mass_g),
            sd = sd(body_mass_g))

ggplot(data=data_for_t_test)+
  stat_qq(aes(sample=body_mass_g))+
  facet_wrap(~species, scales="free") #plotting together and on fixed scales gives the impression of bad distribution

#check equality of variance assumption
data_for_t_test %>%
  levene_test(body_mass_g ~ species)
  #p-value > 0.05, so the assumption holds
  #automatically uses welch, which doesn't require this assumption unlike students t-test

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species)
  #p-value of 2.2e-16 rejects null hypothesis - means are different

t.test(data_for_t_test$body_mass_g ~ data_for_t_test$species, var.equal=TRUE)
  #runs the basic 2-sample t-test instead of the welch
  #if data is paired, can add parameter paired=TRUE

################################################################################
#Correlations
################################################################################

ggplot(data=gentoo) +
  geom_point(aes(x=bill_length_mm, y=bill_depth_mm))#promising relationship

#check normal distribution assumption
ggplot(data=gentoo)+
  stat_qq(aes(sample=bill_length_mm))

ggplot(data=gentoo)+
  stat_qq(aes(sample=bill_depth_mm))

#run the correlation test
cor(x=gentoo$bill_length_mm, y=gentoo$bill_depth_mm, use="complete.obs")#filters out rows w nas

gentoo %>%
  cor_test(bill_length_mm, bill_depth_mm)#runs same test, but keeps it in a nice table
  #can use other tests if requirements/assumptions aren't met, using "method" parameter

head(gentoo)

#get a correlation matrix - compare more than one pair of columns at once

cor(gentoo[ ,c(3:6)], use="complete.obs")
  #all these anatomical variables strongly correlated, strongest between body mass and bill depth

library(GGally)

penguins %>%
  select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, species) %>%
  GGally::ggpairs(aes(color=species))#visualizes smoothed histogram, scatter plot, and correlation w significance for each variable

