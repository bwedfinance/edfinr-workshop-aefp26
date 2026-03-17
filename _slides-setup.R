# Shared setup for all slide decks
library(tidyverse)
library(edfinr)
library(scales)
library(viridis)
library(gt)

# ggplot theme with Bellwether brand
bw_navy <- "#1B2A4A"
bw_blue <- "#2E5FA1"
bw_light_blue <- "#6B9AC4"
bw_gold <- "#C4972F"
bw_gray <- "#F5F5F5"
bw_dark_gray <- "#4A4A4A"

theme_set(
  theme_minimal(base_size = 14, base_family = "Source Sans 3") +
    theme(
      plot.title = element_text(
        family = "Libre Baskerville",
        color = bw_navy,
        face = "bold",
        size = 18
      ),
      plot.subtitle = element_text(color = bw_dark_gray, size = 13),
      plot.caption = element_text(color = bw_light_blue, size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E0E0E0"),
      axis.title = element_text(color = bw_dark_gray),
      axis.text = element_text(color = bw_dark_gray),
      legend.position = "bottom"
    )
)

# Bellwether color scales
scale_color_bw <- function(...) {
  scale_color_manual(
    values = c(bw_navy, bw_blue, bw_light_blue, bw_gold, bw_dark_gray),
    ...
  )
}

scale_fill_bw <- function(...) {
  scale_fill_manual(
    values = c(bw_navy, bw_blue, bw_light_blue, bw_gold, bw_dark_gray),
    ...
  )
}

# knitr options
knitr::opts_chunk$set(
  fig.retina = 3,
  fig.align = "center",
  dpi = 300
)
