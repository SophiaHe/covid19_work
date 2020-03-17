library(tidyverse)
library(lubridate)
library(plotly)
library(gganimate)

# setwd("C:/Users/hyq92/Documents/R/Covid19")

df <- read.csv("covid_19_clean_complete.csv", header = T)

df2 <- df %>%
  mutate(date = as.Date(paste0(as.character(Date),"20"),"%m/%d/%Y")) %>%
  group_by(country = `Country.Region`, date) %>%
  summarise(cases = sum(Confirmed), 
            total_deaths= sum(Deaths),
            total_recovered = sum(Recovered)) %>%
  group_by(country) %>%
  mutate(days_since_50 = as.numeric(date-min(date[cases >= 100]))) %>%
  ungroup() %>%
  filter(is.finite(days_since_50) & days_since_50 >= 0) %>% 
  group_by(country) %>%
  mutate(new_cases = cases-cases[days_since_50 == 0],
         new_deaths = total_deaths - total_deaths[days_since_50 == 0],
         new_recovery = total_recovered - total_recovered[days_since_50 == 0]) %>%
  filter(sum(cases >= 100) >= 5) %>%
  filter(cases >= 100) %>% 
  dplyr::select(-total_deaths, -total_recovered) 


covid19_anim <- function(input){
  p <- ggplot(input, aes(days_since_50, cases, color=country)) +
    geom_line() + 
    geom_point(aes(x=days_since_50,y=cases))+
    geom_text(aes(x = max(days_since_50)+.1, label = country, hjust=0),size = 5) +
    transition_reveal(days_since_50) + 
    view_follow(fixed_y = TRUE)+
    coord_cartesian(clip = 'off') + 
    labs(title = 'COVID-19 Trend', subtitle = '-Updated on March 16, 2020',
         x= 'Number of Days Since 100 Cases', 
         y = 'Total Confirmed Cases') +
    scale_colour_manual(values = c("Italy" = "#ff0000", "Korea, South" = "#ffd700", "Iran" = "#60a885", "Spain" = "#843284", "Germany" = "#d4909c","US" = "#ff0000", "Canada" = "#ffd700"))+
    scale_linetype_manual(values = rep(c('solid'),5)) +
    scale_y_continuous(breaks = seq(0, max(input$cases), by = 5000)) +
    enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
    theme_minimal()+
    theme(
      plot.title = element_text(size=14, face="bold.italic"),
      axis.text=element_text(size=12),
      axis.line = element_line(colour = "black"),
      plot.margin = margin(3,15,3,3,"mm")
    )
    # theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
  
  animate(p, fps=5,renderer = gifski_renderer("gganim.gif"))
}

worldTop5 <- df2 %>% filter(country %in% c("Italy", "Korea, South", "Iran", "Spain", "Germany"))
USCAN <- df2 %>% filter(country %in% c("Canada", "US"))

covid19_anim(input = worldTop5)
covid19_anim(input = USCAN)

# Build Vis for China at Province Level
China <- df %>% filter(Country.Region %in% c("China")) %>% 
  mutate(date = as.Date(paste0(as.character(Date),"20"),"%m/%d/%Y")) %>%
  group_by(prov = `Province.State`, date) %>%
  summarise(cases = sum(Confirmed), 
            total_deaths= sum(Deaths),
            total_recovered = sum(Recovered)) %>%
  group_by(prov) %>%
  mutate(days_since_50 = as.numeric(date-min(date[cases >= 100]))) %>%
  ungroup() %>%
  filter(is.finite(days_since_50) & days_since_50 >= 0) %>% 
  group_by(prov) %>%
  mutate(new_cases = cases-cases[days_since_50 == 0],
         new_deaths = total_deaths - total_deaths[days_since_50 == 0],
         new_recovery = total_recovered - total_recovered[days_since_50 == 0]) %>%
  filter(sum(cases >= 100) >= 5) %>%
  filter(cases >= 100) %>% 
  dplyr::select(-total_deaths, -total_recovered) %>% 
  filter(prov %in% c('Guangdong','Henan','Zhejiang','Hunan','Anhui'))

covid19_anim <- function(input){
  p <- ggplot(input, aes(days_since_50, cases, color=prov)) +
    geom_line() + 
    geom_point(aes(x=days_since_50,y=cases))+
    geom_text(aes(x = max(days_since_50)+.1, label = prov, hjust=0),size = 5) +
    transition_reveal(days_since_50) + 
    view_follow(fixed_y = TRUE)+
    coord_cartesian(clip = 'off') + 
    labs(title = 'COVID-19 Trend', subtitle = '-Updated on March 16, 2020',
         x= 'Number of Days Since 100 Cases', 
         y = 'Total Confirmed Cases') +
    scale_colour_manual(values = c("Guangdong" = "#ff0000", "Henan" = "#ffd700", "Zhejiang" = "#60a885", "Hunan" = "#843284", "Anhui" = "#d4909c"))+
    scale_linetype_manual(values = rep(c('solid'),5)) +
    scale_y_continuous(breaks = seq(0, max(input$cases), by = 250)) +
    enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
    theme_minimal()+
    theme(
      plot.title = element_text(size=14, face="bold.italic"),
      axis.text=element_text(size=12),
      axis.line = element_line(colour = "black"),
      plot.margin = margin(3,15,3,3,"mm")
    )
  # theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
  
  animate(p, fps=5,renderer = gifski_renderer("gganim.gif"))
}

covid19_anim(input = China)






