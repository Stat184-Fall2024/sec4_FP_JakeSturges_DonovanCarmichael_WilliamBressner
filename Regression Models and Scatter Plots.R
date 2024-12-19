## plan
## create 2 regression models, city population vs win percentage and avg stadium attendance vs win percentage
## next test to see if the regression coefficients are significant

library(ggplot2)

ggplot(data = Final_Table, 
       mapping = aes(x = Population / 1000000,# sets x values to attendance, y values to winPercentage
                     y = WinPercentage * 100, 
                     color = Team)) + # sets color to match team
         geom_point(shape = 19) + # sets shape of the points to a closed circle
  geom_smooth(method = "lm", se = TRUE, color = "red") + # creates a linear line of best fit for the scatterplot
  labs(
    x = "Population(Millions)", # sets labels for the graph
    y = "Win Percentage",
    title = "Population vs Win Percentage"
  )


PopulationRegModel <- lm(WinPercentage~Population, data = Final_Table) # sets regression model for population vs winpercentage
summary(PopulationRegModel) # creates summary statistics for the above regression model


ggplot(data = Final_Table, 
       mapping = aes(x = Attendance, # sets x values to attendance, y values to winPercentage
                     y = WinPercentage * 100,
                     color = Year)) + # codes each point color by year
  geom_point(shape = 19) + # sets shape of the points to a closed circle
  geom_smooth(method = "lm", se = TRUE, color = "red") +# creates a linear line of best fit for the scatterplot
  labs(
    x = "Attendance",   # sets labels for the graph
    y = "Win Percentage",
    title = "Attendance vs Win Percentage"
  )


StadiumAttendanceRegModel <- lm(WinPercentage~Attendance, data = Final_Table) # sets regression model for population vs winpercentage
summary(StadiumAttendanceRegModel) # creates summary statistics for the above regression model
