# Econ 691 Homework#3
# Jawad Najjar Z1885445
# Date Dec,6th 2022

rm(list=ls()) #Clears the memory from previous variables 

#---------------------------Import Libraries--------------------------

library(tidyverse)
library(tidycensus)
library(stargazer)

#----------------------------------------------------------------------
##########Import Data############
vote <- read.csv("./Data/Vote Data.csv") #Import voting data

covid <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv") #import covid data

census_api_key("d525a15c915cddd1c7cc8ac0fd02c2c42999b407",install = TRUE) # use census key

#####---------------------Part 1-----------------------------


vote.cast <- vote %>%   
  filter(year == 2020) %>% #initial filtering stage
  group_by(county_fips) %>%
  summarise(cast = sum(totalvotes))

State <- c("IN", "KY", "OH") #using states from Hw#2


Vote_analysis <- vote %>%
  filter(year == 2020) %>%
  full_join(., vote.cast, by = "county_fips") %>%
  filter(state_po %in% State) %>%
  filter(party == "DEMOCRAT" | party == "REPUBLICAN") %>%
  mutate(cand = case_when(party == "DEMOCRAT" ~ "Biden",
                          party == "REPUBLICAN" ~ "Trump"),
         percent = candidatevotes / cast) %>%
  select(state, county_name, county_fips, cand, candidatevotes, percent) %>%
  pivot_wider(names_from = cand,
              values_from = c(percent, candidatevotes)) %>%
  left_join(.,vote.cast, by = "county_fips") %>%
  rename(Biden = candidatevotes_Biden,
         Trump = candidatevotes_Trump,
         pctBiden = percent_Biden,
         pctTrump = percent_Trump) %>%
  mutate(GEOID = as.character(county_fips)) %>%
  select(-county_fips)

#--------------------------------------------------------------
######------------Data filtering and merging--------------

census_variables  <- c("B01001_001","B01001_002","B02001_002","B02001_003","B01002_002","B01002_003")
fips <- c(18, 21, 39)

acs <- get_acs(geography = "county",
               variables = census_variables ,
               year = 2020,
               state = fips,
               geometry = FALSE)


census <- acs %>%
  select(GEOID, variable, estimate) %>%
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
         geometry = NULL) %>%
  replace(is.na(.), 0) %>%
  group_by(GEOID) %>%
  summarise(total_pop = sum(TotPop),
            med_male = mean(Age_M),
            med_fem = mean(Age_F),
            pMale = mean(pMale),
            pWhite = mean(pWhite),
            pBlack = mean(pBlack)) %>%
  full_join(., Vote_analysis, by = "GEOID")


Covid_state <- covid %>%
  filter(state %in% c("Indiana", "Kentucky", "Ohio") & date >= '2021-01-01') %>%
  group_by(fips) %>%
  summarise(total_cases = sum(cases), total_deaths = sum(deaths)) %>%
  mutate(GEOID = as.character(fips))




#####----Part (1) "Per Capita Cases and Per Capita Deaths"-------------------



Vote_Covid <- full_join(census, Covid_state, by = "GEOID") %>%
  mutate(per_cap_cases = total_cases/total_pop, per_cap_deaths = total_deaths/total_pop) %>%
  arrange(desc(per_cap_deaths))%>%
  arrange(desc(per_cap_cases))


#####-----Part (2) "Summary Statistics and Regression Analysis" -----------



summary(Vote_Covid)


core2 <- core %>%
  mutate(s.fips = substr(GEOID, 1, 2),
         state = case_when(s.fips == "18" ~ "Indiana",
                           s.fips == "39" ~ "Ohio",
                           s.fips == "21" ~ "Kentucky"))
core2$state <- factor(core2$state, levels = c("Indiana","Kentucky", "Ohio"))


#----------------------Part 3 "Final data Presentation"-----------------------

######------------Trump data variable------
(model1 <- lm(pctTrump ~ pMale + state, data = Vote_Covid))
(model2 <- lm(pctTrump ~ pMale + pWhite + state, data = Vote_Covid))
(model3 <- lm(pctTrump ~ pMale + pWhite + med_male + med_fem + state, data = Vote_Covid))



######------------cases percentages per Capital ------
(model4 <- lm(per_cap_cases ~ pMale + state, data = Vote_Covid))
(model5 <- lm(per_cap_cases ~ pMale + pWhite + state, data = Vote_Covid))
(model6 <- lm(per_cap_cases ~ pMale + pWhite + med_male + med_fem + pctTrump + state, data = Vote_Covid))



######------------Deaths percentages per Capital ------
(model7 <- lm(per_cap_deaths ~ pMale + state, data = Vote_Covid))
(model8 <- lm(per_cap_deaths ~ pMale + pWhite + state, data = Vote_Covid))
(model9 <- lm(per_cap_deaths ~ pMale + pWhite + med_male + med_fem + pctTrump + state, data = Vote_Covid))



#------------------------Reviewing Results ------------------------

stargazer(as.data.frame(Vote_Covid), type = "html", title = "Descriptive Statistics", out = "./ResultStats.html")
stargazer(model1, model2, model3, type = "html", out = "./ResultTrump.html")
stargazer(model4, model5, model6, type = "html", out = "./ResultCases.html")
stargazer(model7, model8, model9, type = "html", out = "./ResultDeaths.html")