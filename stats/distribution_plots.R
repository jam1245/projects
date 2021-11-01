library(tidyverse)
library(RJDBC)
library(RPresto)
library(feather)
library(modeltime)
library(modeltime.ensemble)
library(timetk)
library(janitor)
library(dbplyr)
library(tidymodels)
library(lubridate)
library(devtools)



# connect to presto tables 
ew_snaps <- hive_connection()

ew_snaps_tbl <- ew_snaps %>%
  select(`Employee ID`, `Business Area Descr`, `Service Years - Whole`, `Month End Date`, 
         `Headcount`, `Hire Count`, `Term Count`, `Job Level`, `Alt Dept`, `Alt Dept Descr`, `Alt Dept Descr Short`, 
         `Direct Indirect`, `Job Function Descr`, `Full Name`, 
         `Job Type Level`, `Age - Decimal`, `Termination Type`) %>% as_tibble()



dbDisconnect(conn)
rm(conn)

ew_snaps_tbl <- clean_names(ew_snaps_tbl) %>%  mutate(age = as.numeric(age_decimal)) 
glimpse(ew_snaps_tbl)

#x <- df %>%   filter(job_type_level == "L7" | job_type_level == "L8") 
#glimpse(x)

#x$age_decimal <- as.numeric(x$age_decimal)

ret_rms_counts <- ew_snaps_tbl %>% 
  select(month_end_date, term_count, termination_type, job_type_level, business_area_descr) %>%
  filter(termination_type == "R") %>% 
  filter(business_area_descr == "Rotary and Mission Systems") %>% 
  #mutate(job_level_num = as.numeric(job_level)) %>% 
  #filter(job_level_num >= 5) %>% glimpse()
  filter(job_type_level == "L5" | job_type_level == "L6" | 
           job_type_level == "L7" | job_type_level == "L8") %>%
  group_by(month_end_date) %>% 
  dplyr::summarise(value = sum(term_count, na.rm = TRUE)) %>% 
  add_column(statistic = "actuals", parameter = "Retirement") %>% 
  dplyr::rename(per_dim = month_end_date) %>% 
  select(parameter, per_dim, statistic, value)

glimpse(ret_rms_counts)


### 

# headcount for level five and sixs

rms_five_six_tbl <- ew_snaps_tbl %>% 
  filter(business_area_descr == "Rotary and Mission Systems") %>%
  filter(job_type_level == "L5" | job_type_level == "L6" & headcount==1) %>% 
  filter(month_end_date >= "2017-01-01") %>% 
  group_by(month_end_date) %>% 
  summarise(median = sum(headcount, na.rm = TRUE)) %>% ungroup() %>% 
  rename(per_dim = month_end_date) %>% 
  mutate(parameter = "TotalActive", year = "NA") %>% 
  select(parameter, per_dim, median)

ggplot(data=rms_five_six_tbl, aes(x=per_dim, y=heads, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point() +
  ylim(0, 3000) +
  theme_fivethirtyeight() + 
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +    
  # facet_wrap(~business_area_descr)+
  ylab('L5/L6 Headcount') + 
  xlab('') +
  # ylim(0,1.5)+
  #scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(title = "L5&6 Headcounts are slightly increasing",
       subtitle = "LM-wide L7 & 8 Executives",
       caption = "Source: RMS HR and Data Science Team") 

### 
# L7 and L8 headcount 
exec_heads = ew_snaps_tbl %>% 
  filter(business_area_descr == "Rotary and Mission Systems") %>%
  filter(job_type_level == "L7" | job_type_level == "L8") %>%
  filter(headcount==1) %>% 
  group_by(month_end_date) %>% 
  summarise(heads = n_distinct(employee_id))%>% ungroup() %>% View()


### 

eligible = ew_snaps_tbl %>% 
  filter(business_area_descr == "Rotary and Mission Systems") %>%
  filter(service_years_whole>=5 & age>=55 & headcount==1) %>% 
  group_by(month_end_date) %>% summarise(eligible = n_distinct(employee_id))%>% ungroup() 

retire = ew_snaps_tbl %>% 
  filter(business_area_descr == "Rotary and Mission Systems") %>%
  filter(termination_type=='R') %>% 
  group_by(month_end_date) %>% 
  summarise(retire  = n_distinct(employee_id)) %>% ungroup() 

RRE = left_join(eligible, retire, by='month_end_date') %>% mutate(rre = retire/eligible)

# left join on date - headcoun
## anyone over 55 and 5 should be considered a retirement (missing out on empl leaving for other reasons)
ggplot(data=RRE, aes(x=month_end_date, y=rre, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()


### Prep data for density plots ---- 

glimpse(ew_snaps_tbl)

ew_retire_tbl <- ew_snaps_tbl %>% filter(termination_type == "R") %>%
  filter(job_type_level == "L7" | job_type_level == "L8") %>%
  mutate(age = as.numeric(age_decimal)) %>% 
  mutate(sd_one_less = mean(age) - sd(age)) %>%
  mutate(sd_one_up = sd(age) + mean(age))


rms_retire_tbl <- ew_snaps_tbl %>% filter(termination_type == "R") %>%
    filter(business_area_descr == "Rotary and Mission Systems") %>%
  filter(job_type_level == "L7" | job_type_level == "L8" |
           job_type_level == "L6" | job_type_level == "L5") %>%
    #filter(business_area_descr == "Rotary and Mission Systems") %>%
  #filter(job_type_level == "L7" | job_type_level == "L8") %>%
  mutate(age = as.numeric(age_decimal)) %>% 
  mutate(sd_one_less = mean(age) - sd(age)) %>%
  mutate(sd_one_up = sd(age) + mean(age))

c6AltDept_retire_tbl <- ew_snaps_tbl %>% filter(termination_type == "R") %>%
  filter(alt_dept_descr_short == "C6ISR" | alt_dept_descr_short == "C4USS") %>%
  filter(business_area_descr == "Rotary and Mission Systems") %>%
  filter(job_type_level == "L7" | job_type_level == "L8" |
           job_type_level == "L6" | job_type_level == "L5") %>%
  mutate(age = as.numeric(age_decimal)) %>% 
  mutate(sd_one_less = mean(age) - sd(age)) %>%
  mutate(sd_one_up = sd(age) + mean(age))

glimpse(ew_retire_tbl)  

mean(c6AltDept_retire_tbl$age)
sd(c6AltDept_retire_tbl$age)
mean(rms_retire_tbl$age)
sd(rms_retire_tbl$age)


sd(ew_retire_tbl$age)
mean(ew_retire_tbl$age)


density_plot(d$age)

library(ggthemes)

lm_plot <- ggplot(ew_retire_tbl, aes(x = age))

lm <- lm_plot + geom_density(color = "black", fill = "gray") + 
  geom_vline(aes(xintercept = mean(age)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = sd_one_less), color = "blue", linetype = 4, size = 0.5) +
  geom_vline(aes(xintercept = sd_one_up), color = "blue", linetype = 4, size = 0.5) +
  xlim(50, 75) +
  theme_fivethirtyeight() + 
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +    
  # facet_wrap(~business_area_descr)+
  ylab('') + 
  xlab('') +
  # ylim(0,1.5)+
  #scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(title = "LM-Wide Executive Retirement Age",
       subtitle = "LM-wide L7 & 8 Executives",
       caption = "Source: RMS HR and Data Science Team") 

lm

rms_plot <- ggplot(rms_retire_tbl, aes(x = age))

rms <- rms_plot + geom_density(color = "black", fill = "gray") + 
  geom_vline(aes(xintercept = mean(age)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = sd_one_less), color = "blue", linetype = 4, size = 0.5) +
  geom_vline(aes(xintercept = sd_one_up), color = "blue", linetype = 4, size = 0.5) +
  xlim(50, 75) +
  theme_fivethirtyeight() + 
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +    
  #    facet_wrap(~business_area_descr)+
  ylab('') + 
  xlab('') +
  # ylim(0,1.5)+
  #scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(title = "RMS Retirement Age",
       subtitle = "RMS L5-L8 Employees",
       caption = "Source: RMS HR and Data Science Team") 

c6_plot <- ggplot(c6AltDept_retire_tbl, aes(x = age))

c6isr_ggpt <- c6_plot + geom_density(color = "black", fill = "gray") + 
  geom_vline(aes(xintercept = mean(age)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = sd_one_less), color = "blue", linetype = 4, size = 0.5) +
  geom_vline(aes(xintercept = sd_one_up), color = "blue", linetype = 4, size = 0.5) +
  xlim(50, 75) +
  theme_fivethirtyeight() + 
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +    
  #    facet_wrap(~business_area_descr)+
  ylab('') + 
  xlab('') +
  # ylim(0,1.5)+
  #scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(title = "C6ISR Alt Dept Retirement Age",
       subtitle = "RMS L5-L8 Employees",
       caption = "Source: RMS HR and Data Science Team") 


rms
lm
c6isr_ggpt

library(ggarrange)
figure <- ggarrange(rms, lm,
                    labels = c("A", "B"),
                    ncol = 2, nrow = 1)
figure




sims5_6 <- read_csv('/mnt/sandbox/retirements/20210712191005_2021JulRMS-adhoc-l5l6_fullOutputDF.csv.gz')


rms_retire_tbl <- ew_snaps_tbl %>% filter(termination_type == "R") %>%
  filter(business_area_descr == "Rotary and Mission Systems") %>%
  filter(job_type_level == "L7" | job_type_level == "L8") %>%
  mutate(age = as.numeric(age_decimal)) %>% 
  mutate(sd_one_less = mean(age) - sd(age)) %>%
  mutate(sd_one_up = sd(age) + mean(age))



glimpse(ew_retire_tbl)  


p <- sims5_6 %>% 
  filter(parameter == "Retirement") %>%
  select(parameter, per_dim, statistic, value) %>%
  #  mutate(per_dim = as.Date(per_dim, format =  "%m/%d/%Y")) %>%
  filter(statistic %in% c('median','lower','upper')) %>% # no proxy here 
  #   filter(parameter=='Hiring') %>% 
  bind_rows(ret_rms_counts) %>% 
  arrange(per_dim) %>%
  mutate(year = lubridate::year(per_dim)) %>%
  spread(statistic, value) %>% 
  mutate(mid = ifelse(is.na(actuals), median, actuals)) %>%
  mutate(statistic = ifelse(is.na(actuals), "forecast", "actuals")) %>% 
  ggplot(., aes(x=per_dim, y = mid, group = statistic)) +
  geom_line(aes(linetype=statistic, color=statistic))+
  scale_color_manual(values=c('#4880e8','#f75b00'))+
  # geom_line(aes(y = mid), color = statistic) + 
  #geom_line(aes(y = mid), color = "steelblue4") + 
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.3)+
  theme_fivethirtyeight() + #xt(aes(label = mid), vjust = 1.5, colour = "white") +
  theme(legend.position='none',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =9)) +    
  ylab('Retirement Counts') + 
  xlab('') +
  ylim(0,15)+
  labs(title = "L5/6 Retirements are Expected to Remain at Current Pace",
       subtitle = "L5 & L6 Retirements in Five Year Simulation",
       caption = "Source: RMS HR and LM Data Science Team")  

p

summary_tbl %>% 
  group_by(as.factor(year)) %>% 
  summarise(retires = sum(mid))

summary_tbl %>% filter(year == 2021) %>%
  group_by(as.factor(per_dim)) %>% 
  summarise(retires = sum(mid)) %>% View()



sims %>% 
  # headcount by month / ret by month 
  filter(parameter %in% c('Retirement','Eligible55n5')) %>%
  filter(statistic %in% c('actuals','median')) %>%
  select(per_dim, parameter, value) %>%
  spread(parameter, value) %>%
  mutate(rate = Retirement/Eligible55n5)  






p1 <- sims5_6 %>% 
  filter(parameter == "TotalActive") %>%
  select(parameter, per_dim, statistic, value) %>%
  #  mutate(per_dim = as.Date(per_dim, format =  "%m/%d/%Y")) %>%
  filter(statistic %in% c('median','lower','upper')) %>% # no proxy here 
  #   filter(parameter=='Hiring') %>% 
  #  mutate(year = lubridate::year(per_dim)) %>%
  spread(statistic, value) %>% 
  bind_rows(rms_five_six_tbl) %>% 
  arrange(per_dim) %>%
  filter(per_dim <= "2026-01-01") %>%
  #  mutate(mid = ifelse(is.na(actuals), median, actuals)) %>%
  #  mutate(statistic = ifelse(is.na(actuals), "forecast", "actuals")) %>% 
  ggplot(., aes(x=per_dim, y = median)) +
  #  geom_line(aes(linetype=statistic, color=statistic))+
  #  scale_color_manual(values=c('#4880e8','#f75b00'))+
  # geom_line(aes(y = mid), color = statistic) + 
  geom_line(aes(y = median), color = "steelblue4") + 
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.3)+
  theme_fivethirtyeight() + #xt(aes(label = mid), vjust = 1.5, colour = "white") +
  theme(legend.position='none',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =9)) +    
  ylab('Total Active Headcount') + 
  xlab("Date") +
  ylim(0,3500)+
  labs(title = "L5/6 Headcounts are Expected to Increase",
       subtitle = "L5 & L6 Headcount in Five Year Simulation",
       caption = "Source: RMS HR and LM Data Science Team")  

p1



rms_five_six_tbl %>% 
  mutate(year = lubridate::year(per_dim)) %>%
  group_by(as.factor(year)) %>% 
  summarise(heads = sum(median))

sim_summary_active <- sims5_6 %>% 
  filter(parameter == "TotalActive") %>%
  select(parameter, per_dim, statistic, value) %>%
  filter(statistic %in% c('median')) %>% # no proxy here 
  spread(statistic, value) 








p2 <- sims5_6 %>% 
  filter(parameter == "RetEligible_55n5") %>%
  select(parameter, per_dim, statistic, value) %>%
  #  mutate(per_dim = as.Date(per_dim, format =  "%m/%d/%Y")) %>%
  filter(statistic %in% c('median','lower','upper')) %>% # no proxy here 
  #   filter(parameter=='Hiring') %>% 
  #  mutate(year = lubridate::year(per_dim)) %>%
  spread(statistic, value) %>% 
  #  bind_rows(rms_five_six_tbl) %>% 
  #  arrange(per_dim) %>%
  filter(per_dim <= "2026-01-01") %>%
  #  mutate(mid = ifelse(is.na(actuals), median, actuals)) %>%
  #  mutate(statistic = ifelse(is.na(actuals), "forecast", "actuals")) %>% 
  ggplot(., aes(x=per_dim, y = median)) +
  #  geom_line(aes(linetype=statistic, color=statistic))+
  #  scale_color_manual(values=c('#4880e8','#f75b00'))+
  # geom_line(aes(y = mid), color = statistic) + 
  geom_line(aes(y = median), color = "steelblue4") + 
  geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.3)+
  theme_fivethirtyeight() + #xt(aes(label = mid), vjust = 1.5, colour = "white") +
  theme(legend.position='none',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =9)) +    
  ylab('Total Active Headcount') + 
  xlab("Date") +
  ylim(0,3500)+
  labs(title = "L5/6 Headcounts are Expected to Increase",
       subtitle = "L5 & L6 Headcount in Five Year Simulation",
       caption = "Source: RMS HR and LM Data Science Team")  

p2


### l5 and l6 age of retirement 



lm_plot_56 <- ew_snaps_tbl %>% filter(termination_type == "R") %>%
  filter(job_type_level == "L5" | job_type_level == "L6") %>%
  mutate(age = as.numeric(age_decimal)) %>% 
  mutate(sd_one_less = mean(age) - sd(age)) %>%
  mutate(sd_one_up = sd(age) + mean(age)) %>% 
  ggplot(aes(x = age))

lm <- lm_plot_56 + geom_density(color = "black", fill = "gray") + 
  geom_vline(aes(xintercept = mean(age)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = sd_one_less), color = "blue", linetype = 4, size = 0.5) +
  geom_vline(aes(xintercept = sd_one_up), color = "blue", linetype = 4, size = 0.5) +
  xlim(50, 75) +
  theme_fivethirtyeight() + 
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +    
  # facet_wrap(~business_area_descr)+
  ylab('') + 
  xlab('') +
  # ylim(0,1.5)+
  #scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(title = "LM-Wide Executive Retirement Age",
       subtitle = "LM-wide L5 & 6 Executives",
       caption = "Source: RMS HR and Data Science Team") 


rms_plot56 <- ew_snaps_tbl %>% filter(termination_type == "R") %>%
  filter(business_area_descr == "Rotary and Mission Systems") %>%
  filter(job_type_level == "L5" | job_type_level == "L6") %>%
  mutate(age = as.numeric(age_decimal)) %>% 
  mutate(sd_one_less = mean(age) - sd(age)) %>%
  mutate(sd_one_up = sd(age) + mean(age)) 

%>% 
  ggplot(aes(x = age))

mean(rms_plot56$age)

rms <- rms_plot56 + geom_density(color = "black", fill = "gray") + 
  geom_vline(aes(xintercept = mean(age)), color = "#FC4E08", linetype = "dashed", size = 1) + 
  geom_vline(aes(xintercept = sd_one_less), color = "blue", linetype = 4, size = 0.5) +
  geom_vline(aes(xintercept = sd_one_up), color = "blue", linetype = 4, size = 0.5) +
  xlim(50, 75) +
  theme_fivethirtyeight() + 
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8), 
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank()) +    
  #    facet_wrap(~business_area_descr)+
  ylab('') + 
  xlab('') +
  # ylim(0,1.5)+
  #scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(title = "RMS Executive Retirement Age",
       subtitle = "RMS L5 & 6 Executives",
       caption = "Source: RMS HR and Data Science Team") 



rms
lm
