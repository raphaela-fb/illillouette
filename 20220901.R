# Illillouette 2021 data exploration

#libraries
library(readr)
library(dplyr)
library(ggplot2)
library(lattice)
library(tidyverse)

#loading .csv files for botany (timed search, and transects) and fuels (Browns transects and surface fuel = litter and duff) and plot metadata

setwd("/Users/raphaela/Desktop/Illillouette")
search <- read.csv("2021/search.csv", header = TRUE)
fuels <- read.csv("2021/fuels.csv")
metadata <- read.csv("2021/metadata.csv")
transect <- read.csv("2021/ground_cover.csv")
surface <- read.csv("2021/surf_fuel.csv", header = TRUE)

# subset? of 2021 botany plots
plots_2021 <- c("ynp_41", "ynp_44", "ynp_83", "ynp_84", "ynp_30", "ynp_34", "ynp_1", "ynp_4", "ynp_19", "ynp_53", "ynp_100", "ynp_15", "ynp_7", "ynp_24", "ynp_49", "ynp_51", "ynp_60", "aaw_121", "aaw_100")

#Averaging measurements by plot using dplyr


mean_surface <- surface %>% group_by(Plot) %>%
  summarise(mean(Litter..cm.), mean(Duff..cm.), mean(Surf.fuel.ht..cm.), .groups= 'drop')

#this also works but not as nice
#surface_plot <- aggregate(x = surface, by = list(surface$Plot), FUN = "mean")


plot(x = mean_surface$`mean(Surf.fuel.ht..cm.)`, y = mean_surface$`mean(Litter..cm.)`)


#plotting in a point cloud 
p <- cloud( mean_surface$`mean(Surf.fuel.ht..cm.)`~ mean_surface$`mean(Litter..cm.)` * `mean(Duff..cm.)`, pch = ".", data = mean_surface)
s <- xyplot(mean_surface$`mean(Litter..cm.)` ~ `mean(Duff..cm.)`, pch = ".", aspect = 2.44, data = mean_surface)
print(s, split = c(1, 1, 2, 1), more = TRUE)
print(p, split = c(2, 1, 2, 1))


#Looking at plant lists 

head(search)

unique(search$Plot)

search %>% count(Plot)
# 19 plots

search %>% count(Species)

unique(search$Species)
#205 unique species values but not in good shape at this point with IDs 

#"alpha diversity" by plot
alpha <- search %>% count(Plot)

plots <- c(alpha$Plot)

barplot(n ~ Plot, data = alpha, names.arg = c(plots))

# Looking at alpha diversity by site, A = Ansel Adams, B = Badge Pass, I = Illillouette Creek Basin

# Assigning site labels 
sites <- c("A", "A", "B", "I", "B", "B", "B", "I", "I", "B", "I", "I", "I", "I", "I", "I", "B", "I", "I")
length(sites)

# Alpha diversity by site 
alpha_site <- cbind(alpha, sites)

#Average alpha per site
mean_alpha <- alpha_site %>% group_by(sites) %>%
  summarise(mean(n), .groups= 'drop')

barplot(`mean(n)` ~ sites, data = mean_alpha)

boxplot(n ~ sites, data = alpha_site)
#Box plot is good 

# Exploring range of values for Illilouette alpha diversity 

alpha_ill <- alpha_site[sites == "I", ]

barplot(n ~ Plot, data = alpha_ill)

# Looking for relationship between surface fuels and alpha diversity

plot(x= alpha$n, y = mean_surface$`mean(Litter..cm.)`)

length(mean_surface$`mean(Litter..cm.)`)

botany



rename(mean_surface, "litter" = "mean(Litter..cm.)")
rename(mean_surface, "duff" = "mean(Duff..cm.)")
rename(mean_surface, "surf" = "mean(Surf.fuel..cm.)")

mean_surface


sums <- rowSums(mean_surface[2:4])
barplot(sums)

alpha_plots <- alpha$Plot

means <- cbind(mean_surface, sums)
means2 <- means[c(1,2,6,7,8,9,10,12,13,14,15,17, 20, 21, 22, 27, 29,33,34), ]

means3 <- means2[ , c(1, 5)]
means4 <- cbind(means3, alpha$n)

colnames(means4) <- c('plot', 'surfs', 'div')
means4


plot(div ~ surfs, data = means4)
abline(lm(div ~ surfs, data =means4), col = 'red')


lm(formula = div ~ surfs, data = means4)

plot(surfs ~ div, data = means4)
abline(lm(surfs ~ div, data =means4), col = 'red')
lm(formula = surfs ~ div, data = means4)

# looks like surface depth and species are not correlated... hmm.. still can look at the browns transects data... ???

fire_hist <- read.csv("/Users/raphaela/Desktop/Illillouette/forestry_fhist.csv")

hist(fire_hist$nfires1970)

plot(x = fire_hist$nfires1970, y = alpha_site$n)
