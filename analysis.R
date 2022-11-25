

#clear your environment variables
rm(list = ls())

#Load tidyverse package
library(tidyverse)
library(dplyr)
library(ggplot2)

source("a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ---- 
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#
#Load the data into a variable. (Variable: `incarceration_df`)
url <- "https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv"
destination <- "c:\\jiejie\\homework4\\data\\incarceration_trends.csv"
download.file(url,destination)

incarceration_df2<- read.csv("c:\\jiejie\\homework4\\data\\incarceration_trends.csv",stringsAsFactors = FALSE) %>% 
  filter(year==1990)%>%
  select(year,state,county_name,total_pop,total_pop_15to64,aapi_pop_15to64,black_pop_15to64,latinx_pop_15to64,native_pop_15to64,white_pop_15to64,region,division,total_jail_pop,aapi_jail_pop,black_jail_pop,latinx_jail_pop,native_jail_pop,white_jail_pop,other_race_jail_pop,total_prison_pop,aapi_prison_pop,black_prison_pop,latinx_prison_pop,native_prison_pop,other_race_prison_pop,white_prison_pop,aapi_female_prison_pop,aapi_male_prison_pop,black_female_prison_pop,black_male_prison_pop,latinx_female_prison_pop,latinx_male_prison_pop,native_female_prison_pop,native_male_prison_pop,other_race_female_prison_pop,other_race_male_prison_pop,white_female_prison_pop,white_male_prison_pop,total_jail_pop_rate,female_jail_pop_rate,male_jail_pop_rate,aapi_jail_pop_rate,black_jail_pop_rate,latinx_jail_pop_rate,native_jail_pop_rate,white_jail_pop_rate,total_prison_pop_rate,aapi_prison_pop_rate,black_prison_pop_rate,latinx_prison_pop_rate,native_prison_pop_rate,white_prison_pop_rate)
View(incarceration_df2)

races_jail_total_state <- incarceration_df2 %>% group_by(state) %>% 
  summarise(allraces_jail_statetotal=sum(total_jail_pop,na.rm = TRUE),allraces_pop_15to64_statetotal=sum(total_pop_15to64,na.rm = TRUE),aapi_jail_statetotal=sum(aapi_jail_pop,na.rm = TRUE),aapi_pop_15to64_statetotal=sum(aapi_pop_15to64,na.rm = TRUE),black_jail_statetotal=sum(black_jail_pop,na.rm = TRUE),black_pop_15to64_statetotal=sum(black_pop_15to64,na.rm = TRUE),latinx_jail_statetotal=sum(latinx_jail_pop,na.rm = TRUE),latinx_pop_15to64_statetotal=sum(latinx_pop_15to64,na.rm = TRUE),native_jail_statetotal=sum(native_jail_pop,na.rm = TRUE),native_pop_15to64_statetotal=sum(native_pop_15to64,na.rm = TRUE),white_jail_statetotal=sum(white_prison_pop,na.rm = TRUE),white_pop_15to64_statetotal=sum(white_pop_15to64,na.rm = TRUE))
View(races_jail_total_state)

races_jail_ratio_state <- races_jail_total_state %>% mutate(allraces_jail_ratio_state=allraces_jail_statetotal/allraces_pop_15to64_statetotal,aapi_jail_ratio_state=aapi_jail_statetotal/aapi_pop_15to64_statetotal,black_jail_ratio_state=black_jail_statetotal/black_pop_15to64_statetotal,latinx_jail_ratio_state=latinx_jail_statetotal/latinx_pop_15to64_statetotal,native_jail_ratio_state=native_jail_statetotal/native_pop_15to64_statetotal,white_jail_ratio_state=white_jail_statetotal/white_pop_15to64_statetotal,.keep="unused")
View(races_jail_ratio_state) 

#%>% group_by(year)
#Year 1990 
races_jail_ratio_average <- races_jail_ratio_state  %>% summarise(allraces_jail_ratio_average=mean(allraces_jail_ratio_state),black_jail_ratio_average=mean(black_jail_ratio_state),latinx_jail_ratio_average=mean(latinx_jail_ratio_state),native_jail_ratio_average=mean(native_jail_ratio_state),white_jail_ratio_average=mean(white_jail_ratio_state))
View(races_jail_ratio_average)
#black_jail_ratio_average_allstates year
black_jail_ratio_average_allstates <- races_jail_ratio_average %>% select(black_jail_ratio_average) 
black_jail_ratio_average_allstates
#black_jail_ratio_highest_state year
black_jail_ratio_highest_state <- races_jail_ratio_state %>% filter(black_jail_ratio_state==max(black_jail_ratio_state)) %>% pull(state)
black_jail_ratio_highest_state
#black_jail_ratio_lowest_state year
black_jail_ratio_lowest_state <- races_jail_ratio_state %>% filter(black_jail_ratio_state==min(black_jail_ratio_state)) %>% pull(state)
black_jail_ratio_lowest_state

#plot to show the black jail ratio is higher than other races
races_jail_ratio_state_long <- races_jail_ratio_state  %>%
  select(state, aapi_jail_ratio_state, black_jail_ratio_state, latinx_jail_ratio_state, native_jail_ratio_state, white_jail_ratio_state) %>%
  gather(key = race, value = ratio, -state) # all columns except `state`
#View(races_jail_ratio_state_long) View(races_jail_ratio_state)
ggplot(races_jail_ratio_state_long) +
  geom_col(mapping = aes(x = state, y = ratio, fill = race))

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
#This function ... <todo:  update comment>
#This data wrangling function should return a data frame that is suitable for visualization. This function takes no parameters. 
incarceration_df3<- read.csv("c:\\jiejie\\homework4\\data\\incarceration_trends.csv",stringsAsFactors = FALSE) %>%select(year,state,county_name,total_pop,total_pop_15to64,aapi_pop_15to64,black_pop_15to64,latinx_pop_15to64,native_pop_15to64,white_pop_15to64,region,division,total_jail_pop,aapi_jail_pop,black_jail_pop,latinx_jail_pop,native_jail_pop,white_jail_pop,other_race_jail_pop,total_prison_pop,aapi_prison_pop,black_prison_pop,latinx_prison_pop,native_prison_pop,other_race_prison_pop,white_prison_pop,aapi_female_prison_pop,aapi_male_prison_pop,black_female_prison_pop,black_male_prison_pop,latinx_female_prison_pop,latinx_male_prison_pop,native_female_prison_pop,native_male_prison_pop,other_race_female_prison_pop,other_race_male_prison_pop,white_female_prison_pop,white_male_prison_pop,total_jail_pop_rate,female_jail_pop_rate,male_jail_pop_rate,aapi_jail_pop_rate,black_jail_pop_rate,latinx_jail_pop_rate,native_jail_pop_rate,white_jail_pop_rate,total_prison_pop_rate,aapi_prison_pop_rate,black_prison_pop_rate,latinx_prison_pop_rate,native_prison_pop_rate,white_prison_pop_rate)
View(incarceration_df3)

get_year_jail_pop <- function() {
  # TODO: Implement this function 
  year_jail_pop <-incarceration_df3 %>% group_by(year) %>% summarise(total_jail_pop_allcounties=sum(total_jail_pop,na.rm = TRUE))
  return(year_jail_pop)
}

#This plotting function should return the chart. This function: (1) Takes no parameters; and (2) Should call the data wrangling function.
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function 
  year_jail_pop <- get_year_jail_pop()
  myplot<-ggplot(data=year_jail_pop)+
    geom_col(mapping=aes(x=year,y=total_jail_pop_allcounties))
  return(myplot)   
} 
plot_jail_pop_for_us()

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
#This data wrangling function should return a data frame that is suitable for visualization. The parameter states should be a vector of states.
incarceration_df4<- read.csv("c:\\jiejie\\homework4\\data\\incarceration_trends.csv",stringsAsFactors = FALSE) %>%select(year,state,county_name,total_pop,total_pop_15to64,aapi_pop_15to64,black_pop_15to64,latinx_pop_15to64,native_pop_15to64,white_pop_15to64,region,division,total_jail_pop,aapi_jail_pop,black_jail_pop,latinx_jail_pop,native_jail_pop,white_jail_pop,other_race_jail_pop,total_prison_pop,aapi_prison_pop,black_prison_pop,latinx_prison_pop,native_prison_pop,other_race_prison_pop,white_prison_pop,aapi_female_prison_pop,aapi_male_prison_pop,black_female_prison_pop,black_male_prison_pop,latinx_female_prison_pop,latinx_male_prison_pop,native_female_prison_pop,native_male_prison_pop,other_race_female_prison_pop,other_race_male_prison_pop,white_female_prison_pop,white_male_prison_pop,total_jail_pop_rate,female_jail_pop_rate,male_jail_pop_rate,aapi_jail_pop_rate,black_jail_pop_rate,latinx_jail_pop_rate,native_jail_pop_rate,white_jail_pop_rate,total_prison_pop_rate,aapi_prison_pop_rate,black_prison_pop_rate,latinx_prison_pop_rate,native_prison_pop_rate,white_prison_pop_rate)
View(incarceration_df4)
get_jail_pop_by_states <-function(states) {
    year_jail_pop_allstates <-incarceration_df4 %>% group_by(year,state) %>% summarise(jail_pop_total_state=sum(total_jail_pop,na.rm = TRUE)) %>% spread(key=state,value=jail_pop_total_state)
    View(year_jail_pop_allstates)
    year_jail_pop_states <- year_jail_pop_allstates %>% select(states)
    return(year_jail_pop_states)
} 
#plot_jail_pop_by_states(states): This plotting function should return the chart. 
#The parameter states should be a vector of states. This function should call the data wrangling function.
plot_jail_pop_by_states <-function(states){
  year_jail_pop_states <- get_jail_pop_by_states(states)

  lineplot <-ggplot(data=year_jail_pop_states,aes(x=year))+
    geom_line(aes(y=year_jail_pop_states[[states[1]]]),color="red")+
    geom_line(aes(y=year_jail_pop_states[[states[2]]]),color="green")+
    geom_line(aes(y=year_jail_pop_states[[states[3]]]),color="blue")+
    geom_line(aes(y=year_jail_pop_states[[states[4]]]),color="yellow")+
    geom_line(aes(y=year_jail_pop_states[[states[5]]]),color="purple")
  
  return(lineplot)
}
tstates = c("KS","KY","LA","WA","CA")
plot_jail_pop_by_states(tstates)

#Draw multiple lines: https://www.geeksforgeeks.org/how-to-create-a-plot-using-ggplot2-with-multiple-lines-in-r/

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
#This chart should show how two different continuous variables are related to one another. 
#Think carefully about what such a comparison means, and what you want to communicate to your user. 
#Your first step should be to find potential trends in the dataset.
#----------------------------------------------------------------------------#
incarceration_df5<- read.csv("c:\\jiejie\\homework4\\data\\incarceration_trends.csv",stringsAsFactors = FALSE) %>% 
  select(year,state,county_name,total_pop_15to64,female_pop_15to64,male_pop_15to64,total_jail_pop,female_jail_pop,male_jail_pop)
View(incarceration_df5)

#get male percentage in states for specific year
get_jail_gend_pops_states <-function(thisyear){
  year_jail_gend_pop_states <- incarceration_df5 %>% filter(year==thisyear) %>% group_by(state) %>%
    summarise(male_jail_pop_state=sum(male_jail_pop,na.rm = TRUE),female_jail_pop_state=sum(female_jail_pop,na.rm = TRUE))
  View(year_jail_gend_pop_states)
 return(year_jail_gend_pop_states)
}
#plot male in jail is higher than female in jail
plot_jail_gend_ratio_states <-function(year){
  year_jail_gend_pop_states<-get_jail_gend_pops_states(year)
  
  year_jail_gend_pop_states_long <- year_jail_gend_pop_states %>%
    gather(key=gendar,value=ratio,-state)
  
  myplot <-ggplot(year_jail_gend_pop_states_long) +
    geom_col(mapping = aes(x = state, y = ratio, fill = gendar))
  
  return(myplot)
}
plot_jail_gend_ratio_states(2018)


## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
library("ggmap")
## Load data frames ----
incarceration_df6<- read.csv("c:\\jiejie\\homework4\\data\\incarceration_trends.csv",stringsAsFactors = FALSE) %>% 
  select(year,state,county_name,total_pop_15to64,female_pop_15to64,male_pop_15to64,total_jail_pop,female_jail_pop,male_jail_pop)
View(incarceration_df6)

state_names_and_codes<-read.csv("c:\\jiejie\\homework4\\state_names_and_codes.csv",stringsAsFactors = FALSE) %>% 
  rename(state=State,code=Code) %>% select(state,code) %>% mutate(state=tolower(state))
View(state_names_and_codes)

get_jail_man_percent_states <-function(thisyear){
  year_jail_male_percentage_states <- incarceration_df6 %>% filter(year==thisyear) %>% group_by(state) %>%
    summarise(male_jail_pop_state=sum(male_jail_pop,na.rm = TRUE),total_jail_pop_state=sum(total_jail_pop,na.rm = TRUE)) %>%
    mutate(male_jail_percent=male_jail_pop_state/total_jail_pop_state,.keep="unused") %>%
    rename(code=state)
  View(year_jail_male_percentage_states)
  return(year_jail_male_percentage_states)
}

jail_man_percent_states <- get_jail_man_percent_states(2018) 
View(jail_man_percent_states)
jail_man_percent_states<-left_join(jail_man_percent_states,state_names_and_codes) %>% select(state,male_jail_percent)
View(jail_man_percent_states)

  state_shape <- map_data("state") %>% # load state shapefile
  rename(state = region) %>% # rename for joining
  left_join(jail_man_percent_states, by="state") # join eviction data
  View(state_shape)
  
  # Define a minimalist theme for maps
  blank_theme <- theme_bw() +
    theme(
      axis.line = element_blank(),        # remove axis lines
      axis.text = element_blank(),        # remove axis labels
      axis.ticks = element_blank(),       # remove axis ticks
      axis.title = element_blank(),       # remove axis titles
      plot.background = element_blank(),  # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank()      # remove border around plot
    )
  
  ggplot(state_shape) +
    geom_polygon(
      mapping = aes(x = long, y = lat, group = group, fill = male_jail_percent),
      color = "white", # show state outlines
      size = .1        # thinly stroked
    ) +
    coord_map() + # use a map-based coordinate system
    scale_fill_continuous(low = "Red", high = "#132B43") +
    labs(fill = "Male percentage in jail")+
    blank_theme
 