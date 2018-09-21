

library(tidyverse)
library(lubridate)

raw <- read_csv("~/Documents/Development/R/data/missionaryInterview/KI_17SEP2018.csv") %>%
  as.tibble()

raw %>% 
  mutate(Date = mdy(week_mon_sun_end_date),
         YoungMissionaries = elders + sisters,
         TotalMissionaries = elders + sisters + seniors) %>%
  group_by(mission_name, mission_president, Date) %>%
  select(contains("reported")) %>%
  rename(`New Investigators` = ni_reported,
         `Sacrament Attendance` = sa_reported,
          `Baptism Date Set` = bd_reported,
         `Baptism/Confirmation` = bc_reported) %>%
  gather("metric", "value", `New Investigators`, `Sacrament Attendance`, `Baptism Date Set`, `Baptism/Confirmation`) %>%
  ggplot(aes(x = Date, y = value, group = mission_name, colour = mission_president)) +
    geom_line() +
    theme_bw() +
    facet_grid(metric ~ ., scales = "free_y") +
    theme(legend.position = "none")



#