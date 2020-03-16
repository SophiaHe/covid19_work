library(tidyverse)
library(lubridate)
library(plotly)
library(gganimate)

setwd("C:/Users/hyq92/Documents/R/Covid19")

df <- read.csv("covid_19_clean_complete.csv", header = T)

df3 <- df %>% mutate(Date = as.Date(paste0(as.character(Date),"20"),"%m/%d/%Y")) %>% 
  group_by(Date,Country.Region) %>% 
  summarize(total_confirmed = sum(Confirmed), 
            total_deaths = sum(Deaths), 
            total_recovered = sum(Recovered))

df2 <- df %>%
  mutate(date = as.Date(Date, "%m/%d/%y")) %>%
  group_by(country = `Country.Region`, date) %>%
  summarise(cases = sum(Confirmed), 
            total_deaths= sum(Deaths),
            total_recovered = sum(Recovered)) %>%
  group_by(country) %>%
  mutate(days_since_100 = as.numeric(date-min(date[cases >= 50]))) %>%
  ungroup() %>%
  filter(is.finite(days_since_100)) %>% 
  group_by(country) %>%
  mutate(new_cases = cases-cases[days_since_100 == 0],
         new_deaths = total_deaths - total_deaths[days_since_100 == 0],
         new_recovery = total_recovered - total_recovered[days_since_100 == 0]) %>%
  filter(sum(cases >= 50) >= 5) %>%
  filter(cases >= 50) %>% dplyr::select(-total_deaths, -total_recovered)

a <- df2 %>% filter(country %in% c("Italy", "Korea, South", "Iran", "Spain", "Germany", "US"))

# p <- ggplot(a, aes(Date, total_confirmed, color = Country.Region)) +
#   geom_point(aes(size = total_deaths,frame = Date, ids = Country.Region))

# fig <- ggplotly(p) %>%
#   animation_opts(
#     100, easing = "elastic", redraw = TRUE) %>%
#   # animation_button(
#   #   x = 1, xanchor = "right", y = 0, yanchor = "bottom"
#   # ) %>%
#   animation_slider(
#     currentvalue = list(prefix = "YEAR ", font = list(color="red"))
#   )
# fig

p <- ggplot(a, aes(days_since_100, cases, color=country)) +
  geom_line() + geom_text(aes(x = max(days_since_100)+.1, label = country, hjust=0)) +
  transition_reveal(days_since_100) + 
  # labs(title = "Date: {frame_time}") +
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') + 
  labs(title = 'COVID-19 Trend', y = 'Total Confirmed Cases') +
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )+
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

animate(p, fps=5)
