# My goal is to create a line graph measuring how many babies are given certain names over time
# Plan
# Load ggplot2, dcData, tidy verse
# Look at the baby names data
# Filter the data to only include the names Adam, Laura, Theo, Spencer, Donovan, and Riley
# ggplot with the filtered data
## Map the species on the x, count on the y, and the name as color and dash pattern
## Make it a line graph
## Label the axis and the graph as a whole, as well the legend
## Color code based on name
## Dash based on name
## Set the legend to the bottom
# Improve and polish

library(ggplot2)
library(dcData)
library(tidyverse)
data(BabyNames)
head(BabyNames)

#Create a dataset with only the names of the Carmichael Kids
myBabyNames <- BabyNames %>%
  filter(
    name == c("Donovan","Spencer", "Theo", "Riley")
      )%>%
  view()

#Create a line graph of the counts of Carmichael Kids names over time
ggplot(
  data = myBabyNames,
  mapping = aes( 
    x = year, #over time
    y = count, # number of babies with the name
    color = name,
    linetype = name
  )
) + 
  geom_line() + 
  labs( #Label the graph, axis, and the legend
    x = "Year",
    y = "Total Number of People with Name",
    color = "Name",
    linetype = "Name",
    title = "Popularity of Carmichael Kids Names Over Time"
  ) +
  scale_color_manual( 
    values = c("orange2", "purple","forestgreen", "red") #bold colors
  ) +
  theme_minimal() +
  theme( 
    legend.position = "right" #move legend right of graph
  )

