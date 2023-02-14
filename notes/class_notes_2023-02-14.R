library(tidyverse)
library(palmerpenguins)

head(penguins)
find("filter")#there are two: one in the stats package and one in the dplyr
#stats::filer() to specify which package to call from

penguins_without_nas = penguins %>%
  filter(!is.na(flipper_length_mm), !is.na(body_mass_g), !is.na(sex))
flipper_v_mass = ggplot(data=penguins_without_nas)+
  geom_point(aes(x=flipper_length_mm, #aesthetics - map variables to plot features
                 y=body_mass_g, 
                 color=sex, #creates a legend automatically
                 shape=species))+
  geom_smooth(aes(x=flipper_length_mm, #uses lowess smoother by default, can customize
                  y=body_mass_g))+
  xlab("FLipper Length (mm)")+
  ylab("Body Mass (g)")+
  ggtitle("Penguins")+
  theme_bw()
ggsave(filename="figures/flipper_v_mass.png", 
       plot=flipper_v_mass,
       width=7, height=5, units="in", dpi=300) #dots per inch
#defaults to last plot created, can also explicitly name plot
#pngs don't lose info in compression, unlike pdfs; tifs also good

#look at number of penguins caught each year by species

penguins_ts = penguins %>%
  group_by(year, species) %>%
  summarize(num_penguins = n())

ggplot(data=penguins_ts)+
  geom_line(aes(x=year, y=num_penguins, color=species))+
  xlab("Year")+
  ylab("Penguin Count")

#histograms to show the distribution of one dimension

ggplot(data=penguins)+
  geom_histogram(aes(x=flipper_length_mm, fill=species), #"color" param for filled object means outline
                 position="identity", 
                 alpha=0.5)+
  scale_fill_manual(values=c("darkorange", "darkorchid", "cyan4"))

ggplot(data=penguins)+
  geom_boxplot(aes(y=flipper_length_mm, x=species, color=species))+
  geom_jitter(aes(y=flipper_length_mm, x=species, color=species),
              width=0.2)+
  scale_color_manual(values=c("darkblue", "darkorange", "darkgreen"))+
  ylab("Flipper Length (mm)")+
  xlab("Species")+
  theme_bw()

#bar charts

ggplot(data=penguins)+
  geom_bar(aes(x=sex, fill=species))+
  facet_wrap(~species)+ #can add nrow or ncolumn(?) parameter to stack the plots instead
  theme_bw()

ggplot(data=penguins)+
  geom_bar(aes(x=island, fill=species))+
  facet_wrap(~species, nrow=3)+
  coord_flip()#bars go sideways instead
