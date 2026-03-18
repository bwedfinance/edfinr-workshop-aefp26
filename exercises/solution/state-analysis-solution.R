# COVID Fund Spending Analysis
# SOLUTION SCRIPT
#
# This is the completed version of state-analysis.R with all blanks filled in.
# For the scaffolded exercise, see: state-analysis.R
# For step-by-step guidance, see: state-analysis.qmd

# Part 1: Setup ---------------------------------------------------------------

library(tidyverse)
library(edfinr)
library(scales)
library(viridis)

# TODO: Change these to your name and state
your_name <- "alex"
my_state <- "GA"

# Pull single year of full data for your state
state_data <- get_finance_data(
  yr = "2022",
  geo = my_state,
  dataset_type = "full"
)

glimpse(state_data)

# Part 2: Select and clean ----------------------------------------------------

# The full dataset has hundreds of columns. Pick the variables we need for
# COVID spending analysis.

covid_exp <- state_data |>
  select(
    # identifiers and enrollment
    ncesid, year, state, cbsa, cong_dist, dist_name, enroll,
    # revenue per-pupil
    rev_total_pp, rev_local_pp, rev_state_pp, rev_fed_pp,
    # socio-economic variables
    mhi, mpv, ba_plus_pct, stpov_pct, urbanicity,
    # expenditure data
    exp_cur_total, exp_covid_total, exp_covid_instr
  )

# Part 3: Per-pupil calculations ----------------------------------------------

# The expenditure columns are totals, not per-pupil. Use across() to convert
# all of them at once, then calculate the instructional share of COVID spending.

covid_exp <- covid_exp |>
  mutate(
    # convert all exp_ columns to per-pupil
    across(
      starts_with("exp_"),
      \(x) x / enroll,
      .names = "{.col}_pp"
    ),
    # what share of COVID funds went to instruction?
    exp_covid_instr_pct = exp_covid_instr / exp_covid_total
  )

# Part 4: Build the scatter plot -----------------------------------------------

# Did higher-poverty districts spend more COVID funds per pupil?

ggplot(data = covid_exp) +
  geom_point(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp,
      color = exp_covid_instr_pct,
      size = enroll
    ),
    alpha = .6
  ) +
  geom_smooth(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp
    ),
    method = "lm", color = "orange"
  ) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis(labels = label_percent()) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  theme_bw() +
  labs(
    x = "Student Poverty %",
    y = "Total COVID Fund Expenditures Per-Pupil",
    size = "Enrollment",
    color = "Instructional Exp. as % of\nTotal COVID Fund Exp.",
    caption = "Data Source: edfinr"
  )

# Part 5: Extend to multi-year (stretch goal) ---------------------------------

# Pull multi-year data using range syntax
state_multi <- get_finance_data(
  yr = "2020:2022",
  geo = my_state,
  dataset_type = "full"
)

# Apply the same select -> across -> mutate pipeline
covid_multi <- state_multi |>
  select(
    ncesid, year, state, cbsa, cong_dist, dist_name, enroll,
    rev_total_pp, rev_local_pp, rev_state_pp, rev_fed_pp,
    mhi, mpv, ba_plus_pct, stpov_pct, urbanicity,
    exp_cur_total, exp_covid_total, exp_covid_instr
  ) |>
  mutate(
    across(
      starts_with("exp_"),
      \(x) x / enroll,
      .names = "{.col}_pp"
    ),
    exp_covid_instr_pct = exp_covid_instr / exp_covid_total
  )

# Faceted scatter plot showing COVID spending over time
ggplot(data = covid_multi) +
  geom_point(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp,
      color = exp_covid_instr_pct,
      size = enroll
    ),
    alpha = .6
  ) +
  geom_smooth(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp
    ),
    method = "lm", color = "orange"
  ) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis(labels = label_percent()) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  facet_wrap(~year) +
  theme_bw() +
  labs(
    x = "Student Poverty %",
    y = "Total COVID Fund Expenditures Per-Pupil",
    size = "Enrollment",
    color = "Instructional Exp. as % of\nTotal COVID Fund Exp.",
    caption = "Data Source: edfinr"
  )

# Part 6: Extension challenge --------------------------------------------------

# Complete the scatter plot in Parts 4-5 first, then pick ONE extension below.

## Option A: Compare multiple states -------------------------------------------

my_states <- "GA,TX,NC"

combined_raw <- get_finance_data(
  yr = "2020:2022",
  geo = my_states,
  dataset_type = "full"
)

combined <- combined_raw |>
  select(
    ncesid, year, state, cbsa, cong_dist, dist_name, enroll,
    rev_total_pp, rev_local_pp, rev_state_pp, rev_fed_pp,
    mhi, mpv, ba_plus_pct, stpov_pct, urbanicity,
    exp_cur_total, exp_covid_total, exp_covid_instr
  ) |>
  mutate(
    across(
      starts_with("exp_"),
      \(x) x / enroll,
      .names = "{.col}_pp"
    ),
    exp_covid_instr_pct = exp_covid_instr / exp_covid_total
  )

ggplot(data = combined) +
  geom_point(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp,
      color = exp_covid_instr_pct,
      size = enroll
    ),
    alpha = .6
  ) +
  geom_smooth(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp
    ),
    method = "lm", color = "orange"
  ) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis(labels = label_percent()) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  facet_grid(state ~ year) +
  theme_bw() +
  labs(
    x = "Student Poverty %",
    y = "Total COVID Fund Expenditures Per-Pupil",
    size = "Enrollment",
    color = "Instructional Exp. as % of\nTotal COVID Fund Exp.",
    caption = "Data Source: edfinr"
  )

## Option B: Swap the variable -------------------------------------------------

ggplot(data = covid_multi) +
  geom_point(
    aes(
      x = mhi,
      y = exp_covid_total_pp,
      color = exp_covid_instr_pct,
      size = enroll
    ),
    alpha = .6
  ) +
  geom_smooth(
    aes(
      x = mhi,
      y = exp_covid_total_pp
    ),
    method = "lm", color = "orange"
  ) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis(labels = label_percent()) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  facet_wrap(~year) +
  theme_bw() +
  labs(
    x = "Median Household Income",
    y = "Total COVID Fund Expenditures Per-Pupil",
    size = "Enrollment",
    color = "Instructional Exp. as % of\nTotal COVID Fund Exp.",
    caption = "Data Source: edfinr"
  )

## Option C: Within-state geography --------------------------------------------

# Filter to 2022 and districts with meaningful enrollment
geo_data <- covid_multi |>
  filter(year == 2022, enroll > 500, !is.na(cong_dist))

# Keep only congressional districts with 5+ districts for readable facets
cds_with_data <- geo_data |>
  count(cong_dist) |>
  filter(n >= 5) |>
  pull(cong_dist)

geo_data |>
  filter(cong_dist %in% cds_with_data) |>
  ggplot() +
  geom_point(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp,
      color = exp_covid_instr_pct,
      size = enroll
    ),
    alpha = .6
  ) +
  geom_smooth(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp
    ),
    method = "lm", color = "orange"
  ) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis(labels = label_percent()) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  facet_wrap(~cong_dist) +
  theme_bw() +
  labs(
    x = "Student Poverty %",
    y = "Total COVID Fund Expenditures Per-Pupil",
    size = "Enrollment",
    color = "Instructional Exp. as % of\nTotal COVID Fund Exp.",
    caption = "Data Source: edfinr"
  )

# Part 7: Save and share ------------------------------------------------------

ggsave(
  filename = paste0(your_name, "_", my_state, "_covid.png"),
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

# Upload your saved plot to the shared Google Drive folder: LINK
#
# Quality checklist:
# - Does the title tell the story?
# - Are axes labeled clearly with labs()?
# - Are dollar values formatted with scales::label_dollar()?
# - Is the data filtered to remove outliers or tiny districts?
# - Would a subtitle or caption add useful context?
