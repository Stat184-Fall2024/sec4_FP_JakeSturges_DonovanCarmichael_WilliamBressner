# Goal: Combine the three tidied datasets into one
# Case: team and year 
# Attributes: win percentage, city, population
#Plan:
# Step 1) collect all 3 data frames into this file
#Must have the run the collection code in the same Environment
# Step 2) Wrangle tables to make case matching easy
# a] Duplicate certain city results for the cities with multiple teams
# b] Add/edit Team column so that it corresponds correctly with each city:
#.       NY - Giants/Jets, LA - Rams/Chargers
# c] Rename teams and cities of all tables so all have team name
# Step 3) Create Attendance/Win PCT table
# a] join_by year and team
# Step 4) make the final table by adding the population in
# a] join_by year and team
# Step 4) Clean and Tidy any unforeseen loose ends
# a] Make sure all datatypes match and are usable
# b] Check for correct number of rows after joining

#Arrange Win% data by alphabetically by team 
Win_PCT <- NFLWINPCTFINAL %>%
  arrange(Team)
#Make the percetage a numerical variable to make data usable
Win_PCT$WinPercentage <- as.numeric((Win_PCT$WinPercentage))

#used for duplicating city data with two teams associated to it
NY_duplicates <- c(1,1,1,1,1,1,1,1,1,1) 
LA_duplicates <- c(1,1,1)
#Copy alphabetized city data
City_Populations <- NFL_Yearly_Populations 
#Duplicate cities with 2 teams (LA & NY)
#Id the rows you want to dup, and how many dups
LA_duplications <- rep(172:174, LA_duplicates) #Los angeles rows x2
LA_Population_dup <- City_Populations[LA_duplications,]%>%#Dup rows in new set
  mutate(#Rename this duplicate data to make it unique to same city data
    City = case_match(
      .x = City,
      "Los Angeles city, California" ~ "Chargers",
      .default = "Chargers"
    )
  )
NY_duplications <- rep(221:230, NY_duplicates) #New york rows x2
NY_Population_dup <- City_Populations[NY_duplications,] %>%
  mutate(#Rename this duplicate data to make it unique to same city data
    City = case_match(
      .x = City,
      "New York city, New York" ~ "Jets",
      .default = "Jets"
  )
)
#Add the duplicate city data to the City Populations, renamed, and rename cities
Team_Populations <- bind_rows(#Combine data
  City_Populations, LA_Population_dup, NY_Population_dup 
) %>% mutate( #Set all the names to the team name, not city
  City = case_match(
    .x = City,
    "Phoenix city, Arizona" ~ "Cardinals",
    "Atlanta city, Georgia" ~ "Falcons",
    "Baltimore city, Maryland" ~ "Ravens",
    "Buffalo city, New York" ~ "Bills",
    "Charlotte city, North Carolina" ~ "Panthers",
    "Chargers" ~ "Chargers",
    "Chicago city, Illinois" ~ "Bears",
    "Cincinnati city, Ohio" ~ "Bengals",
    "Cleveland city, Ohio" ~ "Browns",
    "Dallas city, Texas" ~ "Cowboys",
    "Denver city, Colorado" ~ "Broncos",
    "Detroit city, Michigan" ~ "Lions",
    "Green Bay city, Wisconsin" ~ "Packers",
    "Houston city, Texas" ~ "Texans",
    "Indianapolis city (balance), Indiana" ~ "Colts",
    "Jacksonville city, Florida" ~ "Jaguars",
    "Kansas City city, Missouri" ~ "Chiefs",
    "Las Vegas city, Nevada" ~ "Raiders",
    "Los Angeles city, California" ~ "Rams",
    "Miami city, Florida" ~ "Dolphins",
    "Minneapolis city, Minnesota" ~ "Vikings",
    "Boston city, Massachusetts" ~ "Patriots",
    "New Orleans city, Louisiana" ~ "Saints",
    "New York city, New York" ~ "Giants",
    "Jets" ~ "Jets",
    "Philadelphia city, Pennsylvania" ~ "Eagles",
    "Pittsburgh city, Pennsylvania" ~ "Steelers",
    "San Francisco city, California" ~ "49ers",
    "Seattle city, Washington" ~ "Seahawks",
    "Tampa city, Florida" ~ "Buccaneers",
    "Nashville-Davidson metropolitan government (balance), Tennessee" ~ "Titans",
    "Washington city, District of Columbia" ~ "Commanders",
    "St. Louis city, Missouri" ~ "Rams", 
    "San Diego city, California" ~ "Chargers",
    .default = "missing"
  )
)%>% set_names(
  "Team", #Make it to team to make things easier when joining
  "Year",
  "Population"
)%>% arrange(Team)
#Change year type to make it compatable for joining, set it to number
 

#Create the stadium data to have the names of each team the way the Win% does
Stadium_Data <- yearly_Stadium_Data%>%
  pivot_wider(
    id_cols = Team,
    names_from = Year,
    values_from = Attendance
  ) %>% arrange (#Alphabetize the data to make renaming easier
    Team
    )%>% mutate( #Set all the names to the team name, not city
    Team = case_match(
      .x = Team,
      "Arizona" ~ "Cardinals",
      "Atlanta" ~ "Falcons",
      "Baltimore" ~ "Ravens",
      "Buffalo" ~ "Bills",
      "Carolina" ~ "Panthers",
      "Chargers" ~ "Chargers",
      "Chicago" ~ "Bears",
      "Cincinnati" ~ "Bengals",
      "Cleveland" ~ "Browns",
      "Dallas" ~ "Cowboys",
      "Denver" ~ "Broncos",
      "Detroit" ~ "Lions",
      "Green Bay" ~ "Packers",
      "Houston" ~ "Texans",
      "Indianapolis" ~ "Colts",
      "Jacksonville" ~ "Jaguars",
      "Kansas City" ~ "Chiefs",
      "Las Vegas" ~ "Raiders",
      "Los Angeles" ~ "Rams",
      "Miami" ~ "Dolphins",
      "Minnesota" ~ "Vikings",
      "New England" ~ "Patriots",
      "New Orleans" ~ "Saints",
      "NY Giants" ~ "Giants",
      "NY Jets" ~ "Jets",
      "Philadelphia" ~ "Eagles",
      "Pittsburgh" ~ "Steelers",
      "San Francisco" ~ "49ers",
      "Seattle" ~ "Seahawks",
      "Tampa Bay" ~ "Buccaneers",
      "Tennessee" ~ "Titans",
      "Washington" ~ "Commanders",
      .default = "missing"
    )
  )%>% pivot_longer(#make case team and year and attribute attendance
    cols = starts_with("20"), #Make every yearly column into one column of years
    names_to = "Year", #All years in the year column
    values_to = "Attendance" #All yearly attendance avgs in this column
  ) %>% arrange(Team) #Alphabetize them to make it easier to combine data sets
#Set the years to numbers not strings, since other datasets have num datatype
Stadium_Data$Year <- as.numeric(as.character(Stadium_Data$Year)) 
#Make the attendance numbers to make data usable, must remove commas
Stadium_Data$Attendance <- as.numeric(gsub(",","",Stadium_Data$Attendance))

#Combine the Stadium and win% now that they have the same format
Attendance_Win_PCT_Table <- full_join(
  x = Win_PCT,
  y = Stadium_Data,
  by = join_by(Year == Year, Team == Team) #Match cases of year and team
)%>% relocate(WinPercentage, .after = Year) #Rearrange cols: team|year|Win|Atten

#Add in the Population data to the table for the final table
Final_Table <- full_join(
  x = Attendance_Win_PCT_Table,
  y = Team_Populations,
  by = join_by(Year == Year, Team == Team) #Match cases of year and team
) %>% arrange(Year)%>%
  arrange(Team)

head(Final_Table)
