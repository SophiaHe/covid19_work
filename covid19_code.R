library(tidyverse)
library(lubridate)
library(plotly)

setwd("C:/Users/hyq92/Documents/R/Covid19")

df <- read.csv("covid_19_clean_complete.csv", header = T)

df2 <- df %>% mutate(Date = as.Date(paste0(as.character(Date),"20"),"%m/%d/%Y")) %>% 
  group_by(Date,Country.Region) %>% 
  summarize(total_confirmed = sum(Confirmed), 
            total_deaths = sum(Deaths), 
            total_recovered = sum(Recovered))

a <- df2 %>% filter(Country.Region %in% c("US", "Canada")) %>% as.data.frame()

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

p <- ggplot(a, aes(Date, total_confirmed, color=Country.Region)) +
  geom_line() + geom_text(aes(x = max(Date)+.1, label = Country.Region, hjust=0) +
  transition_reveal(Date) + 
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
