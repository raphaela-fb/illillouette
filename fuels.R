# install the 'devtools' package
install.packages("devtools")

# load the 'devtools' package
library(devtools)

# install Rfuels
devtools::install_github('danfosterfire/Rfuels')

#Then you can load the Rfuels package with:
  
library(Rfuels)

# load the example fuels data from the .csv file
example_fuels_data = read.csv(file = '/Users/raphaela/Desktop/Illillouette/fuels_data/fuels_data.csv', stringsAsFactors = TRUE)

# load the example trees data from the .csv file
example_trees_data = read.csv(file = '/Users/raphaela/Desktop/Illillouette/fuels_data/trees_data.csv',stringsAsFactors = TRUE)

transect_fuel_loads =  estimate_fuel_loads(fuels_data = example_fuels_data,
                      trees_data = example_trees_data,
                      results_type = 'results_only')

total_fuels <- transect_fuel_loads[ , c(1, 16)]
total_fuels



sum_fuels <- total_fuels %>%
  group_by(plot_id) %>%
  summarise(Freq = sum(fuelload_fwd_Mgha))

plot(x = sum_fuels, y = alpha)

plot(c(3.15))
