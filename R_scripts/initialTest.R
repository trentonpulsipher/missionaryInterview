


## Approach

# data discovery
# data vis
# documentation
# assessments
# executive reporting/suggestions


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

### Code 
library(tidyverse)
library(lubridate)
library(trelliscopejs)
library(rbokeh)

raw <- read_csv("~/Documents/Development/R/data/missionaryInterview/KI_17SEP2018.csv") %>%
  as.tibble()


# Mission President Effect
raw %>% 
  mutate(Date = mdy(week_mon_sun_end_date),
         YoungMissionaries = elders + sisters,
         TotalMissionaries = elders + sisters + seniors) %>%
  ggplot(aes(x = Date, y = ni_reported, colour = mission_president)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "lm", se = F) +
  theme_bw() +
  facet_wrap(~ mission_name, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = "", 
       y = "New Investigators Reported", 
       title = "Weekly Number of New Investigators by Mission")

raw %>% 
  mutate(Date = mdy(week_mon_sun_end_date),
         YoungMissionaries = elders + sisters,
         TotalMissionaries = elders + sisters + seniors, 
         ni_reported_numMiss = ni_reported / TotalMissionaries) %>%
  ggplot(aes(x = Date, y = ni_reported_numMiss, colour = mission_president)) +
    geom_point(alpha = .25) +
    geom_smooth(method = "lm", se = F) +
    theme_bw() +
    facet_wrap(~ mission_name, scales = "free_y") +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "New Investigators Reported per Missionary", 
         title = "Weekly Number of New Investigators per Missionary by Mission")


raw %>% 
  mutate(Date = mdy(week_mon_sun_end_date),
         YoungMissionaries = elders + sisters,
         TotalMissionaries = elders + sisters + seniors, 
         bc_reported_numMiss = bc_reported / TotalMissionaries) %>%
  ggplot(aes(x = Date, y = bc_reported_numMiss, colour = mission_president)) +
    geom_point(alpha = .25) +
    geom_smooth(method = "lm", se = F) +
    theme_bw() +
    facet_wrap(~ mission_name, scales = "free_y") +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "Baptisms/Confirmations Reported per Missionary", 
         title = "Weekly Number of Baptisms/Confirmations per Missionary by Mission")

raw %>% 
  mutate(Date = mdy(week_mon_sun_end_date),
         YoungMissionaries = elders + sisters,
         TotalMissionaries = elders + sisters + seniors, 
         sa_reported_numMiss = sa_reported / TotalMissionaries) %>%
  ggplot(aes(x = Date, y = sa_reported_numMiss, colour = mission_president)) +
    geom_point(alpha = .25) +
    geom_smooth(method = "lm", se = F) +
    theme_bw() +
    facet_wrap(~ mission_name, scales = "free_y") +
    theme(legend.position = "none") +
    labs(x = "", 
         y = "Sacrament Meeting Attendance Reported per Missionary", 
         title = "Weekly Sacrament Meeting Attendance per Missionary by Mission")

# Potential Outliers 
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
  ggplot(aes(x = Date, y = value, group = mission_name)) +
  geom_line() +
  geom_smooth(method = "loess") +
  theme_bw() +
  facet_grid(metric ~ mission_name, scales = "free_y") +
  theme(legend.position = "none") +
  labs(x = "", y = "") 

# calculate the area average 
areaAvg <- raw %>% 
  mutate(Date = mdy(week_mon_sun_end_date),
         `Young Missionaries` = elders + sisters,
         `Total Missionaries` = elders + sisters + seniors) %>%
  group_by(mission_name, mission_president, Date) %>%
  mutate(`Percent of Goal - New Investigators` = ni_reported / ni_goal,
         `Percent of Goal - Sacrament Attendance` = sa_reported / sa_goal,
         `Percent of Goal - Baptism Date Set` = bd_reported / bd_goal,
         `Percent of Goal - Baptism/Confirmation` = bc_reported / bc_goal,
         `New Investigators per Missionary` = ni_reported / `Total Missionaries`,
         `Sacrament Attendance per Missionary` = sa_reported / `Total Missionaries`,
         `Baptism Date Set per Missionary` = bd_reported / `Total Missionaries`,
         `Baptism/Confirmation per Missionary` = bc_reported / `Total Missionaries`) %>%
  rename(`New Investigators` = ni_reported,
         `Sacrament Attendance` = sa_reported,
         `Baptism Date Set` = bd_reported,
         `Baptism/Confirmation` = bc_reported,
         `New Investigators (Goal)` = ni_goal,
         `Sacrament Attendance (Goal)` = sa_goal,
         `Baptism Date Set (Goal)` = bd_goal,
         `Baptism/Confirmation (Goal)` = bc_goal,
         Elders = elders,
         Sisters = sisters,
         Seniors = seniors) %>%
  group_by(Date) %>%
  select(-mission_name, -mission_president, -week_mon_sun_end_date) %>%
  gather("metric", "value", 
         `Elders`, `Sisters`, `Seniors`, `Young Missionaries`, `Total Missionaries`,
         `New Investigators`, `Sacrament Attendance`, `Baptism Date Set`, `Baptism/Confirmation`,
         `New Investigators per Missionary`, `Sacrament Attendance per Missionary`, 
         `Baptism Date Set per Missionary`, `Baptism/Confirmation per Missionary`,
         `New Investigators (Goal)`, `Sacrament Attendance (Goal)`, `Baptism Date Set (Goal)`, `Baptism/Confirmation (Goal)`,
         `Percent of Goal - New Investigators`, `Percent of Goal - Sacrament Attendance`,
         `Percent of Goal - Baptism Date Set`, `Percent of Goal - Baptism/Confirmation`) %>%
  group_by(Date, metric) %>%
  summarise(AreaAvg = mean(value, na.rm = T))


## Visualizations
byMetric <- raw %>% 
  mutate(Date = mdy(week_mon_sun_end_date),
         `Young Missionaries` = elders + sisters,
         `Total Missionaries` = elders + sisters + seniors) %>%
  group_by(mission_name, mission_president, Date) %>%
  mutate(`Percent of Goal - New Investigators` = ni_reported / ni_goal,
         `Percent of Goal - Sacrament Attendance` = sa_reported / sa_goal,
         `Percent of Goal - Baptism Date Set` = bd_reported / bd_goal,
         `Percent of Goal - Baptism/Confirmation` = bc_reported / bc_goal,
         `New Investigators per Missionary` = ni_reported / `Total Missionaries`,
         `Sacrament Attendance per Missionary` = sa_reported / `Total Missionaries`,
         `Baptism Date Set per Missionary` = bd_reported / `Total Missionaries`,
         `Baptism/Confirmation per Missionary` = bc_reported / `Total Missionaries`) %>%
  rename(`New Investigators` = ni_reported,
         `Sacrament Attendance` = sa_reported,
         `Baptism Date Set` = bd_reported,
         `Baptism/Confirmation` = bc_reported,
         `New Investigators (Goal)` = ni_goal,
         `Sacrament Attendance (Goal)` = sa_goal,
         `Baptism Date Set (Goal)` = bd_goal,
         `Baptism/Confirmation (Goal)` = bc_goal,
         Elders = elders,
         Sisters = sisters,
         Seniors = seniors) %>%
  gather("metric", "value", 
         `Elders`, `Sisters`, `Seniors`, `Young Missionaries`, `Total Missionaries`,
         `New Investigators`, `Sacrament Attendance`, `Baptism Date Set`, `Baptism/Confirmation`,
         `New Investigators per Missionary`, `Sacrament Attendance per Missionary`, 
         `Baptism Date Set per Missionary`, `Baptism/Confirmation per Missionary`,
         `New Investigators (Goal)`, `Sacrament Attendance (Goal)`, `Baptism Date Set (Goal)`, `Baptism/Confirmation (Goal)`,
         `Percent of Goal - New Investigators`, `Percent of Goal - Sacrament Attendance`,
         `Percent of Goal - Baptism Date Set`, `Percent of Goal - Baptism/Confirmation`)

byMetric %>%
  left_join(areaAvg %>%
              filter(is.finite(AreaAvg)), 
            by = c("Date", "metric")) %>%
  group_by(mission_name, metric) %>%
  filter(mission_name %in% c("A", "B")) %>%
  rename(Pres = mission_president) %>%
  nest() %>%
  mutate(panel = map_plot(data,
                          ~ figure(xlab = "", ylab = "") %>%
                            ly_points(x = Date, y = value, color = Pres, #mission_president, 
                                      hover = list(`Mission President` = Pres, #mission_president,
                                                   `Date` = Date,
                                                   `Value` = value),
                                      data = .x,
                                      legend = F) %>% 
                            ly_lines(x = Date, y = AreaAvg,
                                     hover = list(`Date` = Date, 
                                                  `Area Avg` = AreaAvg),
                                     data = .x, 
                                     legend = F))) %>%
  trelliscope(name = "metric by mission", self_contained = T)


#