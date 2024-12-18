## plan
## create 2 regression models, city population vs win percentage and avg stadium attendance vs win percentage
## next test to see if the regression coefficients are significant

library(ggplot2)


plot(Population~WinPercentage,data = Final_Table, pch = 16, color = "red")

PopulationRegModel <- lm(Population~WinPercentage, data = Final_Table)
summary(PopulationRegModel)




plot(Attendance~WinPercentage,data = Final_Table, pch = 16, color = "blue")

StadiumAttendanceRegModel <- lm(Attendance~WinPercentage, data = Final_Table)
summary(StadiumAttendanceRegModel)