### Making 3 Histograms
# Create histogram for WinPercentage with x as win percentage and y as frequency
Final_Table %>%
  ggplot(Final_Table,
         mapping = aes(
           x = WinPercentage)) +
  geom_histogram(binwidth = 0.1, 
                 fill = "blue", 
                 color = "black", 
                 alpha = 0.7)+
  labs(title = "Histogram of Win Percentage",
       x = "Win Percentage",
       y = "Frequency") +
  theme_bw()

# Create histogram for Attendance with x as attendance and y as frequency
Final_Table %>%
  ggplot(Final_Table,
         mapping = aes(
           x = Attendance)) +
  geom_histogram(binwidth = 5000, 
                 fill = "green", 
                 color = "black", 
                 alpha = 0.7)+
  labs(title = "Histogram of Attendance",
       x = "Attendance",
       y = "Frequency") +
  theme_bw()


# Create histogram for Population with x as population and y as frequency
Final_Table %>%
  ggplot(Final_Table,
         mapping = aes(
           x = Population)) +
  geom_histogram(binwidth = 500000, 
                 fill = "red", 
                 color = "black", 
                 alpha = 0.7)+
  labs(title = "Histogram of Population",
       x = "Population",
       y = "Frequency") +
  theme_bw()
  

