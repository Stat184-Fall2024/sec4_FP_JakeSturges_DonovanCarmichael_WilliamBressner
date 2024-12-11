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


#Step 4) 
library(rvest)
library(tidyr)
library(tidyverse)

## the goal of this piece of code is to harvest data from espn and put together a table with each case being
## a team in a given year from the timespan of 2010-2019 and the variable being win percentage.

## this code reads the data in from the ESPN website

ESPNWINPCT <- read_html(x = "https://www.espn.com/nfl/standings") %>%
  html_elements(css = "table") %>%
  html_table()

##since the data is read in in 4 tibbles, the 1st and 3rd being the team names for their respective conferences(AFC,NFC)
## and the 2nd and 4th being the statistics associated with each team, I combined all the AFC teams with their data and
## the NFC teams with their data, then combined all this data with the bind_rows method.

AFCWINPCT <- bind_cols(ESPNWINPCT[[1]],ESPNWINPCT[[2]])

NFCWINPCT <- bind_cols(ESPNWINPCT[[3]],ESPNWINPCT[[4]])                    

##after we have all the necessary data we remove all data other than team name and win percentage with the select method
## and we get rid of filler rows by slicing only the rows with teams
NFLWINPCT <- bind_rows(NFCWINPCT,AFCWINPCT) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  ## I separated by spaces to get all the team names in one column then i got rid of the rest of the columns so we 
  ## are left with just team name and win pct
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  ## to add the year I mutated a column onto the end with 2024 for every team
  mutate(
    Year = c(2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024,2024)
  )

## Now that we can do this for one year, we just need to do this for the years 2010-2019 then use bind_rows for
## all 10 data frames to get our finished product



#2010
ESPNWINPCT2010 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2010") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2010 <- bind_cols(ESPNWINPCT2010[[1]],ESPNWINPCT2010[[2]])

NFCWINPCT2010 <- bind_cols(ESPNWINPCT2010[[3]],ESPNWINPCT2010[[4]])

NFLWINPCT2010 <- bind_rows(NFCWINPCT2010,AFCWINPCT2010) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010,2010)
  )

#2011
ESPNWINPCT2011 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2011") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2011 <- bind_cols(ESPNWINPCT2011[[1]],ESPNWINPCT2011[[2]])

NFCWINPCT2011 <- bind_cols(ESPNWINPCT2011[[3]],ESPNWINPCT2011[[4]])

NFLWINPCT2011 <- bind_rows(NFCWINPCT2011,AFCWINPCT2011) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011,2011)
  )

#2012
ESPNWINPCT2012 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2012") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2012 <- bind_cols(ESPNWINPCT2012[[1]],ESPNWINPCT2012[[2]])

NFCWINPCT2012 <- bind_cols(ESPNWINPCT2012[[3]],ESPNWINPCT2012[[4]])

NFLWINPCT2012 <- bind_rows(NFCWINPCT2012,AFCWINPCT2012) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012,2012)
  )

#2013
ESPNWINPCT2013 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2013") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2013 <- bind_cols(ESPNWINPCT2013[[1]],ESPNWINPCT2013[[2]])

NFCWINPCT2013 <- bind_cols(ESPNWINPCT2013[[3]],ESPNWINPCT2013[[4]])

NFLWINPCT2013 <- bind_rows(NFCWINPCT2013,AFCWINPCT2013) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013,2013)
  )

#2014
ESPNWINPCT2014 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2014") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2014 <- bind_cols(ESPNWINPCT2014[[1]],ESPNWINPCT2014[[2]])

NFCWINPCT2014 <- bind_cols(ESPNWINPCT2014[[3]],ESPNWINPCT2014[[4]])

NFLWINPCT2014 <- bind_rows(NFCWINPCT2014,AFCWINPCT2014) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014,2014)
  )

#2015
ESPNWINPCT2015 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2015") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2015 <- bind_cols(ESPNWINPCT2015[[1]],ESPNWINPCT2015[[2]])

NFCWINPCT2015 <- bind_cols(ESPNWINPCT2015[[3]],ESPNWINPCT2015[[4]])

NFLWINPCT2015 <- bind_rows(NFCWINPCT2015,AFCWINPCT2015) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015,2015)
  )

#2016
ESPNWINPCT2016 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2016") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2016 <- bind_cols(ESPNWINPCT2016[[1]],ESPNWINPCT2016[[2]])

NFCWINPCT2016 <- bind_cols(ESPNWINPCT2016[[3]],ESPNWINPCT2016[[4]])

NFLWINPCT2016 <- bind_rows(NFCWINPCT2016,AFCWINPCT2016) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016,2016)
  )

#2017
ESPNWINPCT2017 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2017") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2017 <- bind_cols(ESPNWINPCT2017[[1]],ESPNWINPCT2017[[2]])

NFCWINPCT2017 <- bind_cols(ESPNWINPCT2017[[3]],ESPNWINPCT2017[[4]])

NFLWINPCT2017 <- bind_rows(NFCWINPCT2017,AFCWINPCT2017) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017,2017)
  )

#2018
ESPNWINPCT2018 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2018") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2018 <- bind_cols(ESPNWINPCT2018[[1]],ESPNWINPCT2018[[2]])

NFCWINPCT2018 <- bind_cols(ESPNWINPCT2018[[3]],ESPNWINPCT2018[[4]])

NFLWINPCT2018 <- bind_rows(NFCWINPCT2018,AFCWINPCT2018) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018,2018)
  )

#2019
ESPNWINPCT2019 <- read_html(x = "https://www.espn.com/nfl/standings/_/season/2019") %>%
  html_elements(css = "table") %>%
  html_table()

AFCWINPCT2019 <- bind_cols(ESPNWINPCT2019[[1]],ESPNWINPCT2019[[2]])

NFCWINPCT2019 <- bind_cols(ESPNWINPCT2019[[3]],ESPNWINPCT2019[[4]])

NFLWINPCT2019 <- bind_rows(NFCWINPCT2019,AFCWINPCT2019) %>%
  select(1,5)%>%
  set_names("TeamName", "WinPercentage") %>%
  slice(2,3,4,5,7,8,9,10,12,13,14,15,17,18,19,20,22,23,24,25,27,28,29,30,32,33,34,35,37,38,39,40) %>%
  separate_wider_delim(
    cols = "TeamName",
    delim = " ",
    names = c("a","b","c","Team"),
    too_few = "align_end"
  ) %>%
  select(4,5) %>%
  mutate(
    Year = c(2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019,2019)
  )

## adds all the different years data together to make a final dataset
NFLWINPCTFINAL <- bind_rows(NFLWINPCT2010,NFLWINPCT2011,NFLWINPCT2012,NFLWINPCT2013,NFLWINPCT2014,NFLWINPCT2015,NFLWINPCT2016,NFLWINPCT2017,NFLWINPCT2018,NFLWINPCT2019)


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

unneeded_rows_1 <- seq(from = 1, to = 340, by = 34) #Sequence of unneeded rows
unneeded_rows_2 <- seq(from = 1, to = 330, by = 33) #Rest of unneeded rows
#Join the yearly data into one data frame then tidy 
yearly_Stadium_Data <- bind_rows(
  stadium_2010,stadium_2011,stadium_2012,stadium_2013,
  stadium_2014,stadium_2015,stadium_2016,stadium_2017,
  stadium_2018,stadium_2019) %>%
  select(c(2,5,12)) %>%
  set_names("Team","Attendance","Year")%>%
  slice(-c(unneeded_rows_1)) %>% #Removes rows that aren't teams
  slice(-c(unneeded_rows_2)) #Removes rows that aren't teams


  

  
  




