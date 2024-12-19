### Goal - Create simple displays to understand the spread of variables
###        and the potential relationships between them
# Plan - 
# 1) Install necessary packages (ggplot,kable)
# 2) Create histograms of each variable to see general spread
## a) use ggplot package and Final_Table previously made
## b) Win Percentage, Population, and Attendace should be graphed
# 3) Create basic plots to show potential correlation between the data
## a) Win Pct vs. Population ()
## b) Attendance vs. Win Pct ()
## c) Population vs. Attendance () Will not statistically prove, but maybe useful

# Step 1) Load Packages
library(ggplot2)
library(kableExtra)
library(tidyverse)

# Step 2) Create Histograms 
# Create histogram for WinPercentage
ggplot(
  data = Final_Table,
         mapping = aes(
           x = WinPercentage)) + # x-axis is win percentage
  geom_histogram(binwidth = 0.05, #new bar every 5% difference in win pct
                 fill = "blue", 
                 color = "black", 
                 alpha = 0.7)+
  labs(title = "NFL Win Percentage",
       x = "Win Percentage",
       y = "Frequency") +
  theme_bw()


# Create histogram for Attendance with x as attendance and y as frequency
ggplot( 
  data = Final_Table,
         mapping = aes(
           x = Attendance)) + #Graphing average season attendance 
  geom_histogram(binwidth = 2500, #Bars measure segments of 2500
                 fill = "green", 
                 color = "black", 
                 alpha = 0.7)+
  labs(title = "NFL Stadium Attendance",
       x = "Attendance",
       y = "Frequency") +
  theme_bw()


# Create histogram for Population with x as population and y as frequency
ggplot(
  data = Final_Table,
         mapping = aes(
           x = Population)) + #Population is the x axis
  geom_histogram(binwidth = 250000, #For every 250 thousand split bars
                 fill = "red", 
                 color = "black", 
                 alpha = 0.7)+
  labs(title = "NFL City Populations",
       x = "City Population",
       y = "Frequency") +
  theme_bw()

# Step 3) Discovering potential correlation



