#Load packages
library(dplyr)
library(ggplot2)

#Dataset
df = read.csv("games.csv")

#Create variables
#Home_win equals 1 if the home team won and 0 if they lost
#Year pulls the year from the game date
df = df |>
  mutate(
    home_win = ifelse(PTS_home > PTS_away, 1, 0),
    year = as.numeric(substr(GAME_DATE_EST, 1, 4))
  )

#Graph 1: Show how home team 3PT% relates to win rate
df |>
  filter(FG3_PCT_home <= 0.8) |>
  mutate(bin = round(FG3_PCT_home, 2)) |>
  group_by(bin) |>
  summarize(win_rate = mean(home_win, na.rm = TRUE)) |>
  
  ggplot(aes(x = bin, y = win_rate)) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_point(color = "#D95F02", size = 2.5) +
  labs(
    title = "Win Rate vs 3PT%",
    x = "Home Team 3PT%",
    y = "Win Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#Graph 2: Win rate by 3PT% level
df |>
  filter(!is.na(FG3_PCT_home)) |>
  mutate(
    pct_group = case_when(
      FG3_PCT_home < 0.30 ~ "Below 30%",
      FG3_PCT_home >= 0.30 & FG3_PCT_home < 0.40 ~ "30% to 39%",
      FG3_PCT_home >= 0.40 ~ "40% and above"
    )
  ) |>
  group_by(pct_group) |>
  summarize(
    win_rate = mean(home_win, na.rm = TRUE)
  ) |>
  ggplot(aes(x = pct_group, y = win_rate, fill = pct_group)) +
  geom_col() +
  scale_fill_manual(values = c("#D95F02", "#2C7FB8", "#1B9E77")) +
  labs(
    title = "Win Rate by 3PT% Group",
    x = "3PT% Group",
    y = "Win Rate"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

#Graph 3: Distribution of 3PT%
df |>
  ggplot(aes(x = FG3_PCT_home)) +
  geom_histogram(fill = "#2C7FB8", bins = 30) +
  labs(
    title = "Distribution of 3PT%",
    x = "3PT%",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold")
  )

#Graph 4: Average 3PT% in wins vs losses
df |>
  mutate(result = ifelse(home_win == 1, "Win", "Loss")) |>
  filter(!is.na(result)) |> 
  group_by(result) |>
  summarize(avg_3pt = mean(FG3_PCT_home, na.rm = TRUE)) |>
  
  ggplot(aes(x = result, y = avg_3pt, fill = result)) +
  geom_col() +
  scale_fill_manual(values = c("#2C7FB8", "#D95F02")) +
  labs(
    title = "Average 3PT%: Wins vs Losses",
    x = "",
    y = "Average 3PT%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

#Graph 5: Average 3PT% over time
df |>
  group_by(year) |>
  summarize(
    avg_3pt = mean(FG3_PCT_home, na.rm = TRUE)
  ) |>
  ggplot(aes(x = year, y = avg_3pt)) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_point(color = "#D95F02", size = 2.5) +
  labs(
    title = "Average 3PT% Over Time",
    x = "Year",
    y = "Average 3PT%"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

#Graph 6: Compare FG% in wins vs losses
df |>
  mutate(result = ifelse(home_win == 1, "Win", "Loss")) |>
  group_by(result) |>
  summarize(avg_fg = mean(FG_PCT_home, na.rm = TRUE)) |>
  
  ggplot(aes(x = result, y = avg_fg, fill = result)) +
  geom_col() +
  scale_fill_manual(values = c("#2C7FB8", "#D95F02")) +
  labs(
    title = "Average FG%: Wins vs Losses",
    x = "",
    y = "FG%"
  ) +
  theme_minimal()

#Graph 7: More points over time?
df |>
  group_by(year) |>
  summarize(avg_pts = mean(PTS_home, na.rm = TRUE)) |>
  
  ggplot(aes(x = year, y = avg_pts)) +
  geom_line(color = "#2C7FB8", linewidth = 1.2) +
  geom_point(color = "#D95F02") +
  labs(
    title = "Average Points Over Time",
    x = "Year",
    y = "Points"
  ) +
  theme_minimal()