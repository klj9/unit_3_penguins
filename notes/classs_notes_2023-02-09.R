#2023-02-09
#tidyverse and dplyr


library(tidyverse)
library("palmerpenguins")

head(penguins)
summary(penguins)
glimpse(penguins)#like head but transposed in a different format
class(penguins)#tbl is essentially Hadley's version of a data frame

mean(as.data.frame(penguins$bill_depth_mm, na.rm=TRUE))#if tbl causes problems, can force into data frame

#dplyr package comes with lots of useful functions for processing data

#filter by species
gentoo = filter(.data=penguins, species=="Gentoo")#returns all columns, but only certain rows
head(gentoo)
summary(gentoo)

gentoo_ladies = filter(penguins, species=="Gentoo", sex=="female")#can stack multiple filters
summary(gentoo_ladies)
#dplyr functions written so that data is the first parameter, uses pipe style %>% to push thru 
#each step instead of nesting functions

gentoo_ladies = penguins %>%
  filter(species=="Gentoo") %>%
  filter(sex=="female")
summary(gentoo_ladies)

penguins %>%
  filter(sex=="female") %>%
  summarize(maean_mass_g = mean(body_mass_g))

#much tighter and easier to read than the base r format for the same thing:
#mean_ladies_mass = mean(penguins$body_mass_g[penguins$sex == "female"], na.rm=TRUE)

mass_by_species_sex = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%#stack multiple group-by parameters
  summarize(mean_mass_g = mean(body_mass_g))
#gives us 6 means, grouped by species first then by sex

write_csv(x=mass_by_species_sex, file="data/mass_by_species_sex.csv")

count_by_species_sex = penguins %>%
  filter(!is.na(sex)) %>%
  group_by(species, sex) %>%
  summarize(count = n())

penguins_for_usa = penguins %>%
  mutate(body_mass_lb = body_mass_g*0.0022)
glimpse(penguins_for_usa)

penguins %>%
  distinct(island)

for_my_advisor = penguins %>%
  select(-bill_length_mm, -bill_depth_mm)#use - to exclude columns
glimpse(for_my_advisor)

penguins %>%
  arrange(desc(body_mass_g))#default arranges smallest to largest; desc for descending


#exercise 1.3

#penguins_ad_dream_biscoe = penguins %>%
#  mutate(bill_lenth_in = bill_length_mm*0.039) %>%
#  filter(species=="Adelie", island!="Torgersen") %>%
#  summarize(mean_ad_length_in = mean(bill_lenth_in), stdev_ad_lenth_in = sd(bill_lenth_in))

#penguins_ad_torgersen = penguins %>%
#  filter(!is.na(bill_length_mm)) %>%
#  mutate(bill_lenth_in = bill_length_mm*0.039) %>%
#  filter(species=="Adelie", island=="Torgersen") %>%
#  summarize(mean_length_in = mean(bill_lenth_in), stdev_lenth_in = sd(bill_lenth_in))

penguins %>%
  filter(species=="Adelie",
         island %in% c("Biscoe", "Dream"),
         !is.na(bill_length_mm)) %>%
  mutate(bill_length_in = bill_length_mm*0.039) %>%
  summarize(mean_bill_length_in = mean(bill_length_in), sd_bill_length_in = sd(bill_length_in))
