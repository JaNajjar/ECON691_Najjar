#ECON 691
#Homework#2
#Jawad Najjar
#November 22 2023



#-----------Import Library----------
library(tidyverse)
library(tidycensus)

#------------Import Data---------------
Vote_data <- read.csv("./Data/Vote Data.csv")

#####-----Part 1----------------

#Creating a function 

State <- c("IN", "KY", "OH")
Vote_analysis <- Vote_data %>%
  filter(year == 2020) %>%
  filter(state_po %in% State) %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>%
  mutate(cand = case_when(party == "DEMOCRAT" ~ "Biden",
                          party == "REPUBLICAN" ~ "Trump"),
         Vote_percentage = candidatevotes / totalvotes) %>%
  pivot_wider(id_cols = c(county_fips, state, county_name),
              names_from = cand,
              values_from = c(Vote_percentage, candidatevotes)) %>%
  
 
  
  
  
   
  #------Renaming the variables
  rename(State = state,
         Biden = candidatevotes_Biden,
         Trump = candidatevotes_Trump,
         pTrump = Vote_percentage_Trump,
         pBiden = Vote_percentage_Biden,
         County = county_name,
         GEOID = county_fips)




#####------------Part 2------------------
#Provide census key and variables discussed in class sessions

#census_api_key("d525a15c915cddd1c7cc8ac0fd02c2c42999b407",install = TRUE)
census_variables <- c("B01001_001","B01001_002","B02001_002","B02001_003","B01002_002","B01002_003")

#-------Data Analysis
#class code
acs <- get_acs(geography = "county",
               variables = census_variables,
               year = 2020,
               state = c(18, 21, 39),
               geometry = TRUE)

main <- acs %>%
  select(!moe) %>% #You needed to remove the moe first because this causes the large matrix of NAs that messed
                    #up your last map because between the ID and the moe, it creates more "unique" observations.
  pivot_wider(names_from = "variable", values_from = "estimate") %>%
  rename("TotPop"="B01001_001",
         "Male"="B01001_002",
         "White"="B02001_002",
         "Black"="B02001_003",
         "Age_M"="B01002_002",
         "Age_F"="B01002_003") %>%
  mutate(pMale = Male/TotPop,
         pWhite = White/TotPop,
         pBlack = Black/TotPop,
         GEOID = as.numeric(GEOID))

main_inp <- main %>%
  select(-c("NAME", "White", "Black","Male")) #"moe", removed


#####--------Part 3--------------------



plotData <- full_join(Vote_analysis, main_inp, by = "GEOID")

#Biden Plot
ggplot(plotData) + geom_sf(aes(geometry = geometry, fill = pBiden))
# Trump Plot
ggplot(plotData) + geom_sf(aes(geometry = geometry, fill = pTrump))

# Male Age Plot
ggplot(plotData) + geom_sf(aes(geometry = geometry, fill = Age_M)) + scale_fill_continuous(limits = c(0, 100), type = "viridis", name = "Age_M")+
  theme_bw()+
  theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())
