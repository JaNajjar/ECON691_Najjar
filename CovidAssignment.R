#First Assignment Econ691 
# November,3, 2022 
# Student: Jawad Najjar
#Z1885445 



library(tidyverse) #import the library tidy verse

#---------------------------Import Data--------------------------

Covid19 <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")


#-----------------Data function and analysis--------------------

#  calculate the change in the data values from day to day


DIF <- function(i){
  change_d <- i-lag(i)
   return(change_d)
}


DIF_per <- function(i){
  change_per <- ((i-lag(i))/lag(i))
  return(round(change_per,3))
}

Covid19.IL <- Covid19 %>% #Redefine the data
  filter(state == "Illinois" & county == "Cook" & date >= '2021-01-01') %>%
  mutate(daily_cases = DIF(cases),
         daily_deaths = DIF(deaths))%>%
  mutate(percentage_daily_cases = DIF_per(daily_cases),
         percentage_daily_deaths = DIF_per(daily_deaths)) %>%
mutate(percentage_daily_cases = ifelse(is.infinite(percentage_daily_cases), NA, percentage_daily_cases),
       percentage_daily_deaths = ifelse(is.infinite(percentage_daily_deaths), NA, percentage_daily_deaths),
       Date=as.Date(date,"%Y-%m-%d"))



#------------------Part 3 Plotting-------------------------

plot(Covid19.IL$Date, Covid19.IL$percentage_daily_cases,
     type = 'l',
     xlab = "Year",
     ylab = "Daily Changes in New Cases, %")
# plot percentage change of new deaths
plot(Covid19.IL$Date, Covid19.IL$percentage_daily_deaths,
     type = 'l',
     xlab = "Year",
     ylab = "Daily Changes in New Deaths, %")