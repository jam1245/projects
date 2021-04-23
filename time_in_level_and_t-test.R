library(tidyverse)
library(lubridate)
library(ggthemes)
library(RColorBrewer)
library(gridExtra)
library(readr)


rms_time_in_level <- read_csv("C:/Users/e394102/Box/SWPA Work/Data/time_in_level.csv")
head(rms_time_in_level)


library(readxl)
cohorts <- read_excel("C:/Users/e394102/Box/SWPA Work/Data/DEEP_ProgramID_byCohort_Year.xlsx")
glimpse(cohorts)

cohort2019 <- cohorts %>% filter(Cohort == 2019)
cohort2020 <- cohorts %>% filter(Cohort == 2020)


rms_time_in_level %>%
  filter(EmplID %in% cohorts$LMPID)

Cohort2019_TiL <- rms_time_in_level %>%
  filter(EmplID %in% cohort2019$LMPID)

Cohort2020_TiL <- rms_time_in_level %>%
  filter(EmplID %in% cohort2020$LMPID)


Cohort2019_TiL %>% count(Level_Name_Time, sort = TRUE)
Cohort2020_TiL %>% count(Level_Name_Time, sort = TRUE)
length(unique(cohort2019$LMPID))
length(unique(cohort2020$LMPID))


Cohort2019_TiL %>% filter(EffectiveDate >= 2019)



cohort_sub2019_twos <- Cohort2019_TiL %>% filter(Level_Name_Time == "Level Two")
###  
all_twos <- rms_time_in_level %>% filter(Level_Name_Time == "Level Two")


# First, ensure that your data pass a test of homoscedasticity--are the variances homogenous? 
# We do this in R with a Fisher's F-test, var.test(x, y).

var.test(all_twos$days_in_level, rms_time_in_level$days_in_level)

#If your p > 0.05, then you can assume that the variances of both samples are homogenous. In this case, we run a classic 
# Student's two-sample t-test by setting the parameter var.equal = TRUE.


# Since the F-test returns a p < 0.05, then you can assume that the variances of the two groups are different (heteroscedasticity). 
# In this case, you can run a Welch t-statistic. Simply set var.equal = FALSE.

t.test(all_twos$days_in_level, rms_time_in_level$days_in_level, var.equal = FALSE)




rms_time_in_levels %>%
  group_by(Level_Name_Time) %>%
  summarize(mean_ = mean(days_in_level, na.rm = TRUE))

























library(readr)
job <- read_csv("C:/Users/e394102/OneDrive/Data/job_file.csv")
head(job)


#job <- job %>% filter(EffectiveDate >= '2019-01-01')


TimeBeforeLevel2Promotion <- function(dataframe) {
  data <- dataframe %>% 
    #  filter(BA == "Rotary and Mission Systems") %>%  
    # STEP ONE filter out the Exempt folks from our data
    filter(LevelName != "Level NEX") %>% 
    # STEP TWO filter only level ones OR twos
    filter(LevelNumber == 1 | LevelNumber == 2) 
  
  # STEP THREE   
  ##  grab the level Ones and filter by min effective date
  lvl_ones <- data %>% group_by(EmplID, Level) %>% 
    filter(LevelNumber == 1)  %>% 
    filter(EffectiveDate >= '2020-01-01') %>%
    filter(EffectiveDate == min(EffectiveDate))
  
  ## grab the level Twos and filter by min effective date
  lvl_twos <- data %>% group_by(EmplID, Level) %>% 
    filter(LevelNumber == 2) %>% 
    #filter(EffectiveDate == '2019-01-01')
    filter(EffectiveDate == min(EffectiveDate))
  
  # STEP FOUR 
  ## time to bind rows -- like an append where we stack the dataset on top of each other
  df <- bind_rows(lvl_ones, lvl_twos) 
  
  # STEP FIVE 
  ## order the dataframe by ID so they are grouped together 
  ## we should have rows where they are a level one and a level two now grouped together 
  df <- df[with(df,order(EmplID)),]
  
  # STEP SIX 
  ## create a logical variable to see if a person was promoted or not // TRUE = Level Promotion 
  df$logical <- (df$LevelNumber > lag(df$LevelNumber)) %>% 
    replace_na("FALSE") # this replace_na fixes the first row in the dataset which comes up as an NA
  
  # STEP SEVEN 
  ## calculate days in the level 
  final <- df %>%
    # group by emp id and level --- note not sure this is even necessary 
    group_by(EmplID) %>%
    # calculate days in level 
    mutate(days_in_level = difftime(max(EffectiveDate), min(EffectiveDate), units = c("days"))) %>%
    ## only keep those that were promoted with logical == TRUE 
    ## remove the zero days in level 
    filter(logical == 'TRUE' & days_in_level > 0) %>%
    mutate(Level_Name_Time = 'Level One')
  
  # STEP EIGHT 
  ## change the days in level datetime column value to numeric 
  final$days_in_level <- as.numeric(final$days_in_level)
  
  # STEP NINE 
  return(final)
  
}


### ONLY LEVEL CHANGE -- NOT NEC A PROMO ---
### Counts people that might have left and come back to LM (not couting time off in between )
### not counting people staying in level 
### transfers in are not nec counted 


promos <- TimeBeforeLevel2Promotion(job) #%>% 
  #filter(EffectiveDate > '2019-01-01') #%>% 
#filter(days_in_level >= 60)

# if we wat to write as a csv
# write.csv(promos, file = "promos.csv")






Two2ThreePromotion <- function(dataframe) {
  data <- dataframe %>% 
    #  filter(BA == "Rotary and Mission Systems") %>%  
    # STEP ONE filter out the Exempt folks from our data
    filter(LevelName != "Level NEX") %>% 
    # STEP TWO filter only level ones OR twos
    filter(LevelNumber == 2 | LevelNumber == 3) 
  
  # STEP THREE   
  ## just grab the level Ones and filter by min effective date
  lvl_twos <- data %>% group_by(EmplID, Level) %>% 
    filter(LevelNumber == 2)  %>% 
    filter(EffectiveDate >= '2020-01-01') %>%
    filter(EffectiveDate == min(EffectiveDate))
  
  ## just grab the level Twos and filter by min effective date
  lvl_threes <- data %>% group_by(EmplID, Level) %>% 
    filter(LevelNumber == 3) %>% 
    filter(EffectiveDate == min(EffectiveDate))
  
  # STEP FOUR 
  ## time to bind rows -- like an append where we stack the dataset on top of each other
  df <- bind_rows(lvl_twos, lvl_threes) 
  
  # STEP FIVE 
  ## order the dataframe by ID so they are grouped together 
  ## we should have rows where they are a level one and a level two now grouped together 
  df <- df[with(df,order(EmplID)),]
  
  # STEP SIX 
  ## create a logical variable to see if a person was promoted or not // TRUE = Level Promotion 
  df$logical <- (df$LevelNumber > lag(df$LevelNumber)) %>% 
    replace_na("FALSE") # this replace_na fixes the first row in the dataset which comes up as an NA
  
  # STEP SEVEN 
  ## calculate days in the level 
  final <- df %>%
    # group by emp id and level --- note not sure this is even necessary 
    group_by(EmplID) %>%
    # calculate days in level 
    mutate(days_in_level = difftime(max(EffectiveDate), min(EffectiveDate), units = c("days"))) %>%
    ## only keep those that were promoted with logical == TRUE 
    ## remove the zero days in level 
    filter(logical == 'TRUE' & days_in_level > 0) %>%
    mutate(Level_Name_Time = 'Level Two')
  
  
  # STEP EIGHT 
  ## change the days in level datetime column value to numeric 
  final$days_in_level <- as.numeric(final$days_in_level)
  
  # STEP NINE 
  return(final)
  
}


two_threes <- Two2ThreePromotion(job) #%>% filter(EffectiveDate >= '2019-01-01')


test <- two_threes %>%
  filter(EmplID %in% cohorts$LMPID) 

View(test)

Three2FourPromotion <- function(dataframe) {
  data <- dataframe %>% 
    #  filter(BA == "Rotary and Mission Systems") %>%  
    # STEP ONE filter out the Exempt folks from our data
    filter(LevelName != "Level NEX") %>% 
    # STEP TWO filter only level ones OR twos
    filter(LevelNumber == 3 | LevelNumber == 4) 
  
  # STEP THREE   
  ## just grab the level Ones and filter by min effective date
  lvl_threes <- data %>% group_by(EmplID, Level) %>% 
    filter(LevelNumber == 3)  %>% 
    filter(EffectiveDate >= '2020-01-01') %>%
    filter(EffectiveDate == min(EffectiveDate))
  
  ## just grab the level Twos and filter by min effective date
  lvl_fours <- data %>% group_by(EmplID, Level) %>% 
    filter(LevelNumber == 4) %>% 
    filter(EffectiveDate == min(EffectiveDate))
  
  # STEP FOUR 
  ## time to bind rows -- like an append where we stack the dataset on top of each other
  df <- bind_rows(lvl_threes, lvl_fours) 
  
  # STEP FIVE 
  ## order the dataframe by ID so they are grouped together 
  ## we should have rows where they are a level one and a level two now grouped together 
  df <- df[with(df,order(EmplID)),]
  
  # STEP SIX 
  ## create a logical variable to see if a person was promoted or not // TRUE = Level Promotion 
  df$logical <- (df$LevelNumber > lag(df$LevelNumber)) %>% 
    replace_na("FALSE") # this replace_na fixes the first row in the dataset which comes up as an NA
  
  # STEP SEVEN 
  ## calculate days in the level 
  final <- df %>%
    # group by emp id and level --- note not sure this is even necessary 
    group_by(EmplID) %>%
    # calculate days in level 
    mutate(days_in_level = difftime(max(EffectiveDate), min(EffectiveDate), units = c("days"))) %>%
    ## only keep those that were promoted with logical == TRUE 
    ## remove the zero days in level 
    filter(logical == 'TRUE' & days_in_level > 0)%>%
    mutate(Level_Name_Time = 'Level Three')
  
  # STEP EIGHT 
  ## change the days in level datetime column value to numeric 
  final$days_in_level <- as.numeric(final$days_in_level)
  
  # STEP NINE 
  return(final)
  
}


threes_fours <- Three2FourPromotion(job) 


Four2FivePromotion <- function(dataframe) {
  data <- dataframe %>% 
    #  filter(BA == "Rotary and Mission Systems") %>%  
    # STEP ONE filter out the Exempt folks from our data
    filter(LevelName != "Level NEX") %>% 
    # STEP TWO filter only level ones OR twos
    filter(LevelNumber == 4 | LevelNumber == 5) 
  
  # STEP THREE   
  ## just grab the level Ones and filter by min effective date
  lvl_fours <- data %>% group_by(EmplID, Level) %>% 
    filter(LevelNumber == 4)  %>% 
    filter(EffectiveDate >= '2020-01-01') %>%
    filter(EffectiveDate == min(EffectiveDate))
  
  ## just grab the level Twos and filter by min effective date
  lvl_fives <- data %>% group_by(EmplID, Level) %>% 
    filter(LevelNumber == 5) %>% 
    filter(EffectiveDate == min(EffectiveDate))
  
  # STEP FOUR 
  ## time to bind rows -- like an append where we stack the dataset on top of each other
  df <- bind_rows(lvl_fours, lvl_fives) 
  
  # STEP FIVE 
  ## order the dataframe by ID so they are grouped together 
  ## we should have rows where they are a level one and a level two now grouped together 
  df <- df[with(df,order(EmplID)),]
  
  # STEP SIX 
  ## create a logical variable to see if a person was promoted or not // TRUE = Level Promotion 
  df$logical <- (df$LevelNumber > lag(df$LevelNumber)) %>% 
    replace_na("FALSE") # this replace_na fixes the first row in the dataset which comes up as an NA
  
  # STEP SEVEN 
  ## calculate days in the level 
  final <- df %>%
    # group by emp id and level --- note not sure this is even necessary 
    group_by(EmplID) %>%
    # calculate days in level 
    mutate(days_in_level = difftime(max(EffectiveDate), min(EffectiveDate), units = c("days"))) %>%
    ## only keep those that were promoted with logical == TRUE 
    ## remove the zero days in level 
    filter(logical == 'TRUE' & days_in_level > 0) %>%
    mutate(Level_Name_Time = 'Level Four')
  
  # STEP EIGHT 
  ## change the days in level datetime column value to numeric 
  final$days_in_level <- as.numeric(final$days_in_level)
  
  # STEP NINE 
  return(final)
  
}


four_to_fives <- Four2FivePromotion(job) 




data2019 <- bind_rows(promos, two_threes, threes_fours, 
                                       four_to_fives) #five_to_six, six_to_seven, seven_to_eight)




data2020 <- bind_rows(promos, two_threes, threes_fours, 
                      four_to_fives) #five_to_six, six_to_seven, seven_to_eight)

data_incohorts2020 <- data2020 %>%
  filter(EmplID %in% cohorts$LMPID) 

rms_time_in_level %>%
  group_by(Level_Name_Time) %>%
  summarize(mean_ = mean(days_in_level, na.rm = TRUE))


data_incohorts <- data2019 %>%
     filter(EmplID %in% cohorts$LMPID) 

write.csv(data_incohorts, file = "C:/Users/e394102/Box/SWPA Work/Data/time_in_level_deep_cohorts.csv")





flat <- read_csv("C:/Users/e394102/OneDrive/Data/Metrics_Flat2020.csv")
head(flat)

data2019 %>%
  filter(EmplID %in% cohorts$LMPID) %>% View()


data <- flat %>% filter(EmplID %in% cohort2019$LMPID)  %>% select(EmplID, Name, 
                                                               LevelName, EffectiveDate)

View(data)



data_incohorts <- read_csv("C:/Users/e394102/Box/SWPA Work/Data/time_in_level_deep_cohorts.csv")


cohort2019
cohort2020 

Cohort2019_TiL <- data_incohorts %>%
  filter(EmplID %in% cohort2019$LMPID)


Cohort2020_TiL <- data_incohorts2020 %>%
  filter(EmplID %in% cohort2020$LMPID)


Cohort2020_TiL %>% count(LevelName, sort = TRUE)


rms_time_in_level %>%
  group_by(Level_Name_Time) %>%
  summarize(mean_ = mean(days_in_level, na.rm = TRUE))


Cohort2019_TiL %>%
  group_by(Level_Name_Time) %>%
  summarize(mean_ = mean(days_in_level, na.rm = TRUE))



Cohort2020_TiL %>%
  group_by(Level_Name_Time) %>%
  filter(EmplID != "209025") %>% 
  summarize(mean_ = mean(days_in_level, na.rm = TRUE))


length(unique(Cohort2019_TiL$EmplID))
length(unique(cohort2020$LMPID))




data_incohorts2020 <- data2020 %>%
  filter(EmplID %in% cohorts$LMPID) 

rms_time_in_level %>%
  group_by(Level_Name_Time) %>%
  summarize(mean_ = mean(days_in_level, na.rm = TRUE))
