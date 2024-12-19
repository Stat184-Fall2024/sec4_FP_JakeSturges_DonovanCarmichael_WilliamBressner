### Goal - Create simple displays to understand the spread of variables
###        and the potential relationships between them
# Plan - 
# 1) Install necessary packages (ggplot,kable)
# 2) Create histograms of each variable to see general spread
## a) use ggplot package and Final_Table previously made
## b) Win Percentage, Population, and Attendace should be graphed
# 3) Create basic plots to show potential correlation between the data
## a) Win Pct vs. Population (Scatter)
## b) Attendance vs. Win Pct (Scatter)
## c) Population vs. Attendance (Scatter) Will not statistically prove, but maybe useful
## d) Win Pct vs. Team (Boxplot)
## e) Population vs. Team (Boxplot))
## f) Attendance vs. Team (Boxplot)

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
# Population vs. Attendance Scatterplot to show likely relationship there
ggplot(
  data = Final_Table, 
       mapping = aes(x = Population / 1000000,# sets x values to Population 
                     y = Attendance/1000, #y values to Attendance
                     color = Team)) + # sets color to match team
  geom_point(shape = 19) + # sets shape of the points to a closed circle
  labs(
    x = "Population (Millions)", # sets labels for the graph
    y = "Attendance (Thousands)",
    title = "Population vs Attendance"
  ) + 
  theme_bw()

# Population vs. Win Pct Scatterplot to show likely relationship there
ggplot(
  data = Final_Table, 
  mapping = aes(x = Population / 1000000,# sets x values to Population
                y = WinPercentage, #y values to winPercentage
                color = Team)) + # sets color to match team
  geom_point(shape = 19) + # sets shape of the points to a closed circle
  labs(
    x = "Population (Millions)", # sets labels for the graph
    y = "Win Percentage",
    title = "Population vs Win Percentage"
  ) + 
  theme_bw()

# Win Percentage vs. Attendance Scatterplot to show likely relationship there
ggplot(
  data = Final_Table, 
  mapping = aes(x = WinPercentage,# sets x values to attendance 
                y = Attendance/1000, #y values to winPercentage
                color = Team)) + # sets color to match team
  geom_point(shape = 19) + # sets shape of the points to a closed circle
  labs(
    x = "Win Percentage", # sets labels for the graph
    y = "Attendance (Thousands)",
    title = "Win Percentage vs Attendance"
  ) + 
  theme_bw()

# Graph the teams and win percentages with boxplot since it may be confounding
ggplot( 
  data = Final_Table,
   aes(x=as.factor(Team), # x-axis is based on teams
       y=WinPercentage, # y-axis is win percentage
       fill = as.factor(Team))) + # Fill based on which team
    geom_boxplot( 
      alpha=0.7) + 
  labs(title = "NFL Team vs. Win Percentage",
       x = "Team",
       y = "Win Percentage") +
  theme_bw() +
  theme(
    axis.text.x = element_blank() # remove names from x-axis for readability
  )+
  scale_fill_manual(
    values = rainbow(length(unique(Final_Table$Team))) # Assign teams colors
  )

# Graph the teams and Attendance with boxplot since it may be confounding
ggplot( 
  data = Final_Table,
  aes(x=as.factor(Team), # x-axis is based on teams
      y= Attendance, # y-axis is Attendnace
      fill = as.factor(Team))) + # Fill based on which team
  geom_boxplot( 
    alpha=0.7) + 
  labs(title = "NFL Team vs. Attendance",
       x = "Team",
       y = "Attendance") +
  theme_bw() +
  theme(
    axis.text.x = element_blank() # remove names from x-axis for readability
  )+
  scale_fill_manual(
    values = rainbow(length(unique(Final_Table$Team))) # Assign teams colors
  )

# Graph the teams and Population with boxplot since it may be confounding
ggplot( 
  data = Final_Table,
  aes(x=as.factor(Team), # x-axis is based on teams
      y= Population, # y-axis is Population
      fill = as.factor(Team))) + # Fill based on which team
  geom_boxplot( 
    alpha=0.7) + 
  labs(title = "NFL Team vs. Population",
       x = "Team",
       y = "Population") +
  theme_bw() +
  theme(
    axis.text.x = element_blank() # remove names from x-axis for readability
  )+
  scale_fill_manual(
    values = rainbow(length(unique(Final_Table$Team))) # Assign teams colors
  )
