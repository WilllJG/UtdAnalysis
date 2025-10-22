library(dplyr)
library(readr)
library(lubridate)
library(fuzzyjoin)
library(ggplot2)

setwd("XXX")

# reaad file and bind to one dataset
files <- list.files(".", pattern = "\\.csv")

df_list <- files %>% lapply(read_csv)

all_results <- bind_rows(df_list) 

setwd("XXX")

# reformat dates
all_results$Date <- dmy(all_results$Date)

# sort by date and filter for Utd
utd_results <- all_results %>%
  rename("Played" = Date) %>%
  arrange(Played) %>%
  filter(HomeTeam == "Man United" | AwayTeam == "Man United") %>%
  mutate(`united_points` = case_when(
    FTR == "D" ~ 1,
    FTR == "H" & HomeTeam == "Man United" ~ 3,
    FTR == "A" & AwayTeam == "Man United" ~ 3,
    TRUE ~ 0
  )) %>%
  mutate(`united_goals` = case_when(
    HomeTeam == "Man United" ~ FTHG,
    AwayTeam == "Man United" ~ FTAG,
    TRUE ~ 0)
  ) %>%
  mutate(`goals_against` = case_when(
    AwayTeam == "Man United" ~ FTHG,
    HomeTeam == "Man United" ~ FTAG,
    TRUE ~ 0)
  )
# read manager dates & reformat
dates <- read.csv("./Dates.csv")

dates$date_started <- dmy(dates$date_started)
dates$date_ended <- dmy(dates$date_ended)

# try fuzzyjoin
result <- fuzzy_left_join(
  utd_results, dates,
  by = c("Played" = "date_started", "Played" = "date_ended"),
  match_fun = list(`>=`, `<=`)
)

result <- result %>%
  select(Played, HomeTeam, AwayTeam, united_points, name, united_goals, goals_against)

# add cumulative points & goals
df_cm <- result %>%
  filter(name != "Michael Carrick)" & name != "Ralf Rangnick " & name != "Ruud van Nistelrooy)" & name != "Ryan Giggs") %>%
  arrange(name, Played) %>%     # sort correctly
  group_by(name) %>%
  mutate(games_managed = n()) %>%
  mutate(game_number = row_number()) %>%
  mutate(cm_points = cumsum(united_points)) %>%
  mutate(cm_gs = cumsum(united_goals)) %>%
  mutate(cm_ga = cumsum(goals_against)) %>%
  filter(game_number < 36) %>%
  ungroup()

# build chart
ggplot(df_cm, aes(x = Played, y = cm_points, color = name)) +
  geom_line(size = 1.2) +
  labs(
    title = "Cumulative Points by Manager Over Time",
    x = "Date",
    y = "Cumulative Points"
  ) +
  theme_minimal(base_size = 14)

# grid
ggplot(df_cm, aes(x = game_number, y = cm_points)) +
  geom_line(size = 1.2, show.legend = FALSE,  color = '#E43F77') + # hide legend since facets label managers
  labs(
    x = "With 34/35 Premier League matches played",
    y = "Cumulative Points"
  ) +
  ylim(0, 65) +
  scale_y_continuous(limits = c(0, 70), expand = c(0, 0)) +
  facet_wrap(~ name, ncol = 3, scales = "free_y") +  # grid layout
  theme_minimal(base_size = 12) +
  theme(
    element_text(family = "Palatino"),
    strip.text = element_text(family = "Palatino", size = 12, face = "bold"),  # larger facet titles
    plot.title = element_text(family = "Palatino", size = 12, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),  # Remove minor gridlines
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "#f5f2eb", color = NA),
    axis.title.x = element_text(family = "Palatino"),
    axis.title.y = element_text(family = "Palatino")
  )

# data for flourish
totals <- df_cm %>%
  filter(game_number == 34 | game_number == 35)

write.csv(df_cm,"Utd managers.csv")

# filter for big six
big6games <- result %>%
  filter(game_number < 36) 

# add cumulative points & goals
big6games <- result %>%
  filter(name != "Michael Carrick)" & name != "Ralf Rangnick " & name != "Ruud van Nistelrooy)" & name != "Ryan Giggs") %>%
  arrange(name, Played) %>%     # sort correctly
  group_by(name) %>%
  mutate(game_number = row_number()) %>%
  filter(HomeTeam == "Chelsea" | HomeTeam == "Man City" | HomeTeam == "Arsenal" |
           HomeTeam == "Tottenham" | HomeTeam == "Liverpool" | AwayTeam == "Chelsea" | 
           AwayTeam == "Man City" | AwayTeam == "Arsenal" | AwayTeam == "Tottenham" | 
           AwayTeam == "Liverpool" ) %>%
  mutate(cm_points = cumsum(united_points)) %>%
  mutate(cm_gs = cumsum(united_goals)) %>%
  mutate(cm_ga = cumsum(goals_against)) %>%
#  filter(game_number < 36) %>%
  mutate(games_managed = n()) %>%
  ungroup()

write_csv(big6games, "big6data.csv")
