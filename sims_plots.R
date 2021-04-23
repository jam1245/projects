
library(tidyverse)
library(forecast)
library(RColorBrewer)
library(ggthemes)

library(readr)
women_hires <- read_csv("C:/Users/e394102/OneDrive/Data/women_hires_sim.csv")
glimpse(women_hires)

colnames(women_hires) <- c("Date", "women_hires")

glimpse(women_hires)

monthly <- ts(women_hires[,2], start = c(2017, 01), frequency = 12)
autoplot(monthly)
monthly


head_sub <- window(monthly, start = 2017)

head_sub %>% ggseasonplot(women_hires, labels = "both") +
  theme_fivethirtyeight() + 
  #    scale_colour_brewer(palette = "Dark2")+
  scale_color_manual(values=c("#636363", "#636363", "#31a354", "#2b8cbe","#e6550d")) +
 # scale_fill_brewer(palette = "RdBu") + 
  # scale_fill_manual(values = RdBu) +
  #scale_color_brewer(palette = "Blues") +
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8)) +    
  ylab('Hires') + 
  xlab('Date') +
  ylim(0,160)+
  labs(title = "Simulation compared to past years",
       subtitle = "Seasonal Women Hires",
       caption = "Source: RMS HR") 



library(readr)
sims <- read_csv("C:/Users/e394102/Box/SWPA Work/OngoingProjects/D&I_Sims/rms-special-sims_2020-09-30_from_QP.csv", 
                 col_types = cols(per_dim = col_date(format = "%m/%d/%Y")))
glimpse(sims)


sims %>% 
  filter(run_name == "RMS_Total_Female") %>%
  filter(statistic %in% c('actuals','median','lower','upper')) %>% 
  filter(parameter=='Hiring') %>% 
  mutate(year = lubridate::year(per_dim)) %>%
  spread(statistic, value) %>% 
  filter(year <= 2021 & year >= 2018) %>% 
  mutate(mid = ifelse(is.na(actuals), median, actuals)) %>%
  ggplot(., aes(x=per_dim, y = mid)) +
 # geom_line(aes(y = actuals), color = "steelblue4") + 
  geom_point( aes(y = actuals))+
  geom_point(aes(y = median), colour = "red") +
  geom_smooth(method = 'lm', se=F) +
   # geom_line(aes(y = median), color="red4", linetype="longdash") +
 # scale_color_manual(values = c("#fdae6", "steelblue")) +
  theme_fivethirtyeight() + 
             geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.3)+
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8)) +    
  ylab('Women Hires') + 
  xlab('Date') +
  ylim(0,200)+
  scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(title = "2021 Simulation Captures Historic Upward Women Hiring Trend",
       subtitle = "Women Hiring Trend in Simulation",
       caption = "Source: RMS HR") 





sims2 <- sims %>% filter(per_dim <= '2021-12-31' & per_dim >= '2016-01-01')




sims2 = df %>% as_tibble() %>% filter(run_name %in% c('RMS_Total_Male','RMS_Total_Female')) %>%
  filter(statistic %in% c('actuals','median','upper','lower')) %>%
  filter(parameter %in% c('TotalActive','VolAttrition','Retirement','Layoffs','OtherAttrition','Hiring','XferIn','XferOut')) %>%
  select(run_name, parameter, statistic, per_dim, value) %>% unique()

flows = sims2 %>% 
  filter(parameter!='TotalActive') %>% 
  filter(!parameter %in% c('XferIn','XferOut')) %>% 
  mutate(flow = ifelse(parameter %in% c('Hiring','XferIn'),'Inflow',
                       ifelse(parameter %in% c('VolAttrition','Retirement','Layoffs','OtherAttrition','XferOut'),'Outflow',
                              NA))) %>% 
  filter(statistic %in% c('actuals','median','lower','upper')) %>% 
  group_by(run_name,per_dim,statistic,flow) %>%
  summarise(value = sum(value)) %>%
  ungroup() 

headcount = sims2 %>%
  filter(parameter=='TotalActive') %>%
  filter(statistic %in% c('actuals','median')) %>%
  select(run_name, per_dim, headcount=value) 

flow = left_join(flows, headcount, by=c('run_name','per_dim')) %>%
  mutate(rate = value/headcount) %>%
  mutate(rate = ifelse(flow=='Inflow',rate,-1*rate)) %>%
  spread(statistic,rate)

flow1 = left_join(flows, headcount, by=c('run_name','per_dim')) %>%
  mutate(rate = value/headcount) %>%
  mutate(rate = ifelse(flow=='Inflow',rate,-1*rate)) %>%
  filter(statistic %in% c('actuals','median')) %>%
  mutate(alpha = ifelse(statistic=='actuals',1,0.8)) %>% 
  filter(run_name=='RMS_Total_Female') %>%
  filter(lubridate::year(per_dim)>=2016)

bounds = left_join(flows, headcount, by=c('run_name','per_dim')) %>%
  mutate(rate = value/headcount) %>%
  mutate(rate = ifelse(flow=='Inflow',rate,-1*rate)) %>%
  select(-headcount,-value) %>% unique() %>% 
  filter(statistic %in% c('median','lower','upper')) %>%
  spread(statistic,rate) %>% 
  filter(run_name=='RMS_Total_Female') %>%
  filter(lubridate::year(per_dim)>=2016)

ggplot()+
  geom_errorbar(inherit.aes = F, data = bounds, aes(x=per_dim, y=median, ymin=lower, ymax=upper, color=flow), size=0.5, show.legend = F)+
  geom_bar(inherit.aes = F, data=flow1, aes(x=per_dim, y=rate, color=flow, fill=flow), size=0.95, stat="identity") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_date(expand=c(0.01,0), date_breaks = "1 year", date_labels = "%Y", 
               guide=guide_axis(angle = 90))+
 # theme(axis.title = element_blank(),
#        legend.title = element_blank(),
#        axis.ticks = element_blank())  +
  theme_fivethirtyeight() + 
 # geom_ribbon(aes(ymin=lower,ymax=upper), alpha=0.3)+
  theme(legend.position='bottom',plot.title = element_text(size =14), 
        axis.text = element_text(size =8), axis.title = element_text(size =8), 
        axis.title.x = element_text(size =8), axis.title.y = element_text(size =8)) +    
 # ylab('Women Hires') + 
#  xlab('Date') +
#  ylim(0,500)+
 # scale_x_date(date_breaks = "1 year",date_labels = "%Y")+
  labs(title = "Historic Women Inflows (Hires) is Greater Than Attrition",
       subtitle = "Women Hiring Trends in Simulation",
       caption = "Source: RMS HR") 






rms_women <- sims %>% 
  filter(run_name == "RMS_Total_Female") %>%
  filter(statistic %in% c('actuals','median','lower','upper')) %>% 
  filter(parameter=='Hiring') %>% 
  mutate(year = lubridate::year(per_dim)) %>%
  spread(statistic, value) %>% 
  filter(year <= 2021 & year >= 2017) %>% 
  select(per_dim, actuals)

monthly <- ts(rms_women[,2], start = c(2017, 01), frequency = 12, end = c(2020,08))
autoplot(monthly)
monthly

fcast <- auto.arima(monthly, seasonal = T) 

fcast <- fcast %>% forecast(h=16) 

fcast %>% autoplot()


df <- as.data.frame(fcast)

df2021 <- tail(df, 12)

sum(df2021$`Point Forecast`)



fc <- hw(monthly, damped = TRUE, seasonal="additive", h=16)
autoplot(monthly) +
  autolayer(fc, series="HW add damped", PI=FALSE)+
  guides(colour=guide_legend(title="forecasts"))

df <- as.data.frame(fc)

df2021 <- tail(df, 12)

sum(df2021$`Point Forecast`)

