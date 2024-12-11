# Goal: Find, Tidy, Clean, Wrangle Data into usable tables
# PLAN
# 1) Load necessary data packages: {rvest}, {tidyverse}
#
# 2) Find the needed website urls for the data
# a] US Census for city population
# b] NFl Website for Season Records 
# c] ESPN for stadium attendance over the last 
#
# 3) Create population table
# a] Read Census data into a table 
# b] Tidy Data so city is case with attributes of:
#.      population and year
# c] Clean the data to remove all n/a or 0 values
# d] Remove all cities without an NFL team
#
# 4) Create NFL Win Percentage by Year Table
# a] Read the data into a table using read_html function
# b] Tidy the data so that each case is a Team with attributes: 
#.      year and win percentage 
# c] Clean any data that does not follow table guidelines
#
# 5) Create Average NFL Stadium Attendance by year table
# a] Read the yearly tables into R using read_html
# b] Join the data into one table 
# c] Clean and tidy the table, making each case a team 
#.      with attributes: year and average stadium attendance
# d] Clean any n/a or 0 values out of the data set

library(tidyverse)
library(rvest)
library(readxl)


# Step 3)
#Create original data set for NFL city populations
#Need list of NFL locations according to the data set
#Use Census Data to create US population data set from 2010-2019
US_Populations <- read_excel("US_City_Populations.xlsx") %>%
  select(c(2,5:14)) %>% #Isolate city and yearly population data
  slice(-(n=1)) %>% #Remove First Row of n/a values
  set_names("City", "2010", "2011","2012","2013","2014","2015",
            "2016","2017","2018","2019")%>% #Rname cols 
  #Select only the cities with NFL teams using filter 
  filter(City %in% c("Phoenix city, Arizona", "Atlanta city, Georgia", 
         "Baltimore city, Maryland", "Buffalo city, New York", 
         "Charlotte city, North Carolina", "Chicago city, Illinois", 
         "Cincinnati city, Ohio", "Cleveland city, Ohio", 
         "Dallas city, Texas", "Denver city, Colorado", 
         "Detroit city, Michigan", "Green Bay city, Wisconsin", 
         "Houston city, Texas", "Indianapolis city (balance), Indiana", 
         "Jacksonville city, Florida", "Kansas City city, Missouri", 
         "Las Vegas city, Nevada", "Los Angeles city, California", 
         "Miami city, Florida", "Minneapolis city, Minnesota", 
         "Nashville-Davidson metropolitan government (balance), Tennessee", 
         "New Orleans city, Louisiana", "New York city, New York", 
         "Philadelphia city, Pennsylvania", 
         "Pittsburgh city, Pennsylvania", "San Francisco city, California",
         "Seattle city, Washington", "Tampa city, Florida", 
         "Washington city, District of Columbia", 
         "Boston city, Massachusetts"))

#Cast all the 2010 values to numbers since for some reason they weren't
US_Populations$"2010" <- as.numeric(as.character(US_Populations$"2010"))

#Reshape and rename the table with case an NFL city in a certain year
#Attribute would be population
NFL_Yearly_Populations <- US_Populations %>%
  pivot_longer(
    cols = starts_with("20"), #Make every yearly column into one column of years
    names_to = "Year", #All years in the year column
    values_to = "Population" #All yearly populations in this column
  ) %>%
  arrange(City) #Rearrange the data so it's alphabetical


#Step 4) JAKE's got it


#Step 5) 
#Read in URLs: https://www.espn.com/nfl/attendance/_/year/2010
#              https://www.espn.com/nfl/attendance/_/year/2011
#              https://www.espn.com/nfl/attendance/_/year/2012
#              https://www.espn.com/nfl/attendance/_/year/2013
#              https://www.espn.com/nfl/attendance/_/year/2014
#              https://www.espn.com/nfl/attendance/_/year/2015
#              https://www.espn.com/nfl/attendance/_/year/2016
#              https://www.espn.com/nfl/attendance/_/year/2017
#              https://www.espn.com/nfl/attendance/_/year/2018
#              https://www.espn.com/nfl/attendance/_/year/2019
#Creating a data frame for the stadium attendance data for each year
#Gather 2010 Stadium data from espn
stadiumRaw_2010 <- read_html("https://www.espn.com/nfl/attendance/_/year/2010") %>%
  html_elements(css = "table") %>%
  html_table() #List of data frames read in 
#Make one data frame for 2010
stadium_2010 <- stadiumRaw_2010[[1]] %>%
  mutate(
    Year = c(2010) #Add year column for 2010
  )
#Gather 2011 Stadium data from espn
stadiumRaw_2011 <- read_html("https://www.espn.com/nfl/attendance/_/year/2011") %>%
  html_elements(css = "table") %>%
  html_table() #List of data frames read in 
#Make one data frame 2011
stadium_2011 <- stadiumRaw_2011[[1]] %>%
  mutate(
    Year = c(2011) #Add year column for 2011
  )
#Gather 2012 Stadium data from espn
stadiumRaw_2012 <- read_html("https://www.espn.com/nfl/attendance/_/year/2012") %>%
  html_elements(css = "table") %>%
  html_table() #List of data frames read in 
#Make one data frame for 2012
stadium_2012 <- stadiumRaw_2012[[1]]%>%
  mutate(
    Year = c(2012) #Add year column for 2012
  ) 
#Gather 2013 Stadium data from espn
stadiumRaw_2013 <- read_html("https://www.espn.com/nfl/attendance/_/year/2013") %>%
  html_elements(css = "table") %>%
  html_table() #List of data frames read in 
#Make one data frame for 2013
stadium_2013 <- stadiumRaw_2013[[1]] %>%
  mutate(
    Year = c(2013) #Add year column for 2013
  )
#Gather 2014 Stadium data from espn
stadiumRaw_2014 <- read_html("https://www.espn.com/nfl/attendance/_/year/2014") %>%
  html_elements(css = "table") %>%
  html_table() #List of data frames read in 
#Make one data frame 2014
stadium_2014 <- stadiumRaw_2014[[1]]%>%
  mutate(
    Year = c(2014) #Add year column for 2014
  ) 
#Gather 2015 Stadium data from espn
stadiumRaw_2015 <- read_html("https://www.espn.com/nfl/attendance/_/year/2015") %>%
  html_elements(css = "table") %>%
  html_table() #List of data frames read in 
#Make one data frame for 2015
stadium_2015 <- stadiumRaw_2015[[1]]%>%
  mutate(
    Year = c(2015) #Add year column for 2015
  ) 
#Gather 2016 Stadium data from espn
stadiumRaw_2016 <- read_html("https://www.espn.com/nfl/attendance/_/year/2016") %>%
  html_elements(css = "table") %>%
  html_table()#List of data frames read in 
#Make one data frame for 2016
stadium_2016 <- stadiumRaw_2016[[1]]%>%
  mutate(
    Year = c(2016) #Add year column for 2016
  )
#Gather 2017 Stadium data from espn
stadiumRaw_2017 <- read_html("https://www.espn.com/nfl/attendance/_/year/2017") %>%
  html_elements(css = "table") %>%
  html_table()#List of data frames read in 
#Make one data frame for 2017 
stadium_2017 <- stadiumRaw_2017[[1]]%>%
  mutate(
    Year = c(2017) #Add year column for 2017
  ) 
#Gather 2018 Stadium data from espn
stadiumRaw_2018 <- read_html("https://www.espn.com/nfl/attendance/_/year/2018") %>%
  html_elements(css = "table") %>%
  html_table() #List of data frames read in 
#Make one data frame for 2018
stadium_2018 <- stadiumRaw_2018[[1]] %>%
  mutate(
    Year = c(2018) #Add year column for 2018
  )
#Gather 2019 Stadium data from espn
stadiumRaw_2019 <- read_html("https://www.espn.com/nfl/attendance/_/year/2019") %>%
  html_elements(css = "table") %>%
  html_table() #List of data frames read in 
#Make one data frame for stadium 2019
stadium_2019 <- stadiumRaw_2019[[1]] %>%
  mutate(
    Year = c(2019) #Add year column for 2019
  )
  
#Join the yearly data into one data frame then tidy 
yearly_Stadium_Data <- bind_rows(
  stadium_2010,stadium_2011,stadium_2012,stadium_2013,
  stadium_2014,stadium_2015,stadium_2016,stadium_2017,
  stadium_2018,stadium_2019) %>%
  select(c(2,5,12)) %>%
  set_names("Team","Attendance","Year")

  

  
  




