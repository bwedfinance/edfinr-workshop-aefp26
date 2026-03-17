# load -------------
library(tidyverse)
library(edfinr)
library(scales)
library(viridis)

# get edfinr data from pandemic era
covid_yrs <- get_finance_data(
  yr = "2020:2022",
  dataset_type = "full"
  )

# clean -----------
covid_exp <- covid_yrs |>
  # select columns for analysis 
  select(
    # cols to include in most exploratory analysis
    ncesid, year, state, cbsa, cong_dist, dist_name, enroll, 
    # revenue per-pupil
    rev_total_pp, rev_local_pp, rev_state_pp, rev_fed_pp,
    # socio-econ vars
    mhi, mpv, ba_plus_pct, stpov_pct, urbanicity, 
    # expenditure data
    exp_cur_total, exp_covid_total, exp_covid_instr
  ) |> 
  # calculate expenditures per-pupil
  mutate(
    # perform this across several columns
    across(
      # only mutate columns that start with "exp_"
      starts_with("exp_"),
      # use this function
      \(x) x / enroll,
      # use this format to create column names
      .names = "{.col}_pp"
    ),
    # calculate % of covid funds used for instrucitonal expenses
    exp_covid_instr_pct = exp_covid_instr / exp_covid_total 
  )

# plot ------------
# explore relationship between stpov_pct and covid expenditures per-pupil
ggplot(data = covid_exp) +
  # add layer for points that use stpov_pct and total covid exp pp
  # for location, instr % for color, and enroll for size
  geom_point(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp,
      color = exp_covid_instr_pct,
      size = enroll
    ),
    # make the points 60% transparent
    alpha = .6
  ) + 
  # add a regression line
  geom_smooth(
    aes(
      x = stpov_pct,
      y = exp_covid_total_pp
    ), 
    # use linear model
    method = "lm",
    # make the line orange
    color = "orange"
  ) +
  # made pleasant variable formatting
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis(labels = label_percent()) +
  scale_size_area(
    max_size = 10,
    labels = label_comma()
    ) +
  # make multiple plots
  facet_wrap(
    # create a plot for each year of data
    ~year, 
    # display plots in 2 rows
    nrow = 2) +
  # set theme
  theme_bw() + 
  theme(
      plot.caption = element_text(hjust = 0)
  ) +
  # add labels for plot
  labs(
    x = "Student Poverty %",
    y = "Total COVID Fund Expenditures Per-Pupil",
    size = "Enrollment",
    color = "Instructional Exp. as % of\nTotal COVID Fund Exp.",
    # title = "",
    # subtitle = "", 
    caption = "Data Source: edfinr"
  ) 

ggsave(
  "covid_analysis_nationwide.png",
  units = "in", 
  height = 6,
  width = 10,
)

# explore relationship between stpov_pct and covid expenditures per-pupil
ggplot(data = covid_exp |> filter(state == "KY")) +
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
    method = "lm",
    color = "orange"
  ) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_color_viridis(labels = label_percent()) +
  scale_size_area(
    max_size = 10,
    labels = label_comma()
    ) +
  facet_wrap(~year, nrow = 2) +
  theme_bw() + 
  theme(
      plot.caption = element_text(hjust = 0)
  ) +
  # add labels for plot
  labs(
    x = "Student Poverty %",
    y = "Total COVID Fund Expenditures Per-Pupil",
    size = "Enrollment",
    color = "Instructional Exp. as % of\nTotal COVID Fund Exp.",
    # title = "",
    # subtitle = "", 
    caption = "Data Source: edfinr"
  ) 

ggsave(
  "covid_analysis_ky.png",
  units = "in", 
  height = 6,
  width = 10,
)
