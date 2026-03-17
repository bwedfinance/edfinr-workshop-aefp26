# COVID Fund Spending Analysis
# Application Exercise
#
# Fill in the blanks (___) and TODOs below. If you get stuck,
# check the solution boxes in the companion file: state-analysis.qmd

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
# COVID spending analysis. Fill in the revenue, socio-economic, and
# expenditure columns.

covid_exp <- state_data |>
  select(
    # identifiers and enrollment
    ncesid, year, state, cbsa, cong_dist, dist_name, enroll,
    # revenue per-pupil
    ___,
    # socio-economic variables
    ___,
    # expenditure data
    ___
  )

# Part 3: Per-pupil calculations ----------------------------------------------

# The expenditure columns are totals, not per-pupil. Use across() to convert
# all of them at once, then calculate the instructional share of COVID spending.

covid_exp <- covid_exp |>
  mutate(
    # convert all exp_ columns to per-pupil
    across(
      ___,
      ___,
      .names = "___"
    ),
    # what share of COVID funds went to instruction?
    exp_covid_instr_pct = ___
  )

# Part 4: Build the scatter plot -----------------------------------------------

# Did higher-poverty districts spend more COVID funds per pupil?
# Fill in the aesthetics for geom_point and geom_smooth.

ggplot(data = covid_exp) +
  geom_point(
    aes(
      x = ___,
      y = ___,
      color = ___,
      size = ___
    ),
    alpha = .6
  ) +
  geom_smooth(
    aes(
      x = ___,
      y = ___
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
    # TODO: same columns as Part 2
  ) |>
  mutate(
    across(
      # TODO: same per-pupil calculation as Part 3
    ),
    exp_covid_instr_pct = exp_covid_instr / exp_covid_total
  )

# TODO: Copy your scatter plot from Part 4 and add:
# - Change data to covid_multi
# - Add facet_wrap(~year) before theme_bw()


# Part 6: Extension challenge --------------------------------------------------

# Complete the scatter plot in Parts 4-5 first, then pick ONE extension below.

## Option A: Compare multiple states -------------------------------------------

# TODO: Update geo to pull multiple states using a comma list (no spaces)
# e.g., geo = "GA,TX,NC"
# Then facet by state (or facet_grid(state ~ year) for both dimensions)


## Option B: Swap the variable -------------------------------------------------

# TODO: Replace stpov_pct with mhi (median household income) or
# ba_plus_pct (% adults with BA+). Update the x-axis label.
# Does the relationship flip?


## Option C: Within-state geography --------------------------------------------

# TODO: Filter to districts with enough enrollment to be meaningful (e.g., > 500)
# Use facet_wrap(~cbsa) or facet_wrap(~cong_dist) to show geographic variation
# You may need to filter to a subset of geographies that have enough districts


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
