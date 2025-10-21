# eXtensible Time Series
# https://joshuaulrich.github.io/xts/
library(xts)
# dygraphs open source javascript charts
# https://rstudio.github.io/dygraphs
library(dygraphs)
# dplyr
library(dplyr, warn.conflicts = FALSE)

# may need to set working directory to current directory before starting, to read CSV in same directory
# e.g. setwd("~/repos/r-solar-dygraph")

# CSV file name (expect in current directory) named "solar-analysis.csv"
# data put together from energy provider:
# * first column "month", character, formatted as YYYYMMDD (first day of each month)
# * peak (kWh), integer
# * off-peak (kWh), integer
# * solar feed-in (kWh), integer
file_name <- "solar-analysis.csv"

# read CSV file into data frame
file_contents = read.csv(file_name)

# convert month field (named "month") to date (from "YYYYMMDD" format)
file_contents$month <- as.Date(as.character(file_contents$month), "%Y%m%d")
# make solar feed-in negative
# this is to help with calculations
file_contents$solar_feed_in <- file_contents$solar_feed_in * -1
# create a total kWh field by adding peak, off-peak and (now negative) solar feed-in
# if solar feed-in is missing, treat as zero
file_contents <- file_contents %>%
  # only sum for each row
  rowwise() %>%
  # replace missing solar feed-in with zero
  dplyr::mutate(total_kwh = sum(peak, off_peak, solar_feed_in, na.rm = TRUE))

# now create XTS object from CSV file contents
# "month" will be the datetime column, but not included in the resulting XTS object
# (note, whatever is used for "order.by" must be in date format)
file_xts <- xts::xts(file_contents[, -1], order.by = file_contents$month)

# debug: can create a time series for one field e.g. peak
# peak_ts <- ts(file_contents$peak, start = c(2022, 1), frequency = 12)
# plot(peak_ts)

# bonus: create data frame of Daily solar exposure for month from BOM web site, for Jan 2022 - Sep 2025
# for Hobart, Tasmania
# this is an indication of sunlight; impacted by cloud & number of daylight hours
# see https://reg.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=193&p_display_type=dailyDataFile&p_startYear=2022&p_c=-1774629015&p_stn_num=094193
daily_solar_exposure_hobart <- data.frame(
  # month in date type, as YYYYMMDD starting from 20220101 (same start date as solar data)
  # could probably generate this as a sequence rather than typing...anyway
  month = as.Date(c(
    "20220101", "20220201", "20220301", "20220401", "20220501", "20220601", "20220701", "20220801", "20220901", "20221001", "20221101", "20221201",
    "20230101", "20230201", "20230301", "20230401", "20230501", "20230601", "20230701", "20230801", "20230901", "20231001", "20231101", "20231201",
    "20240101", "20240201", "20240301", "20240401", "20240501", "20240601", "20240701", "20240801", "20240901", "20241001", "20241101", "20241201",
    "20250101", "20250201", "20250301", "20250401", "20250501", "20250601", "20250701", "20250801", "20250901"
  ), format = "%Y%m%d"),
  # monthly mean megajoule per square metre
  megajoule_per_sq_metre = c(
    # 2022
    17.8, 18, 11.8, 8.6, 6.4, 5.5, 6.5, 8.5, 11.2, 14.2, 20.4, 22.1,
    # 2023
    23.4, 17.4, 13.6, 9, 6.5, 5.7, 6.4, 8.8, 14.2, 17.7, 20.7, 23.2,
    # 2024
    21.6, 20.6, 14.8, 9.1, 6.3, 5.5, 6.7, 8.5, 12.8, 19, 20, 23,
    # 2025 (up to September)
    22.5, 18.5, 14.1, 9, 7, 5.8, 6.5, 9.4, 13.2
  )
)

# convert daily solar exposure to ribbon data expected by dygraph; use measure between 0 and 1,
# so 1 = sunny, 0.66 = medium, 0.33 = low, 0 = very low
# based on data, I guessed cutoffs at: 0-6.99 = 0, 7-13.99 = 0.33, 14-18.99 = 0.66, 19 and up = 1
# adapted from https://rstudio.github.io/dygraphs/gallery-ribbon.html
# create a vector of initially all zeros, as many elements as daily solar exposure data frame
ribbonData <- rep(0, nrow(daily_solar_exposure_hobart))
# set vector values at indexes matching daily solar exposures of greater than 7, to 0.33
# the "which" function returns the indexes matching a condition
ribbonData[which(daily_solar_exposure_hobart$megajoule_per_sq_metre >= 7)] <- 0.33
# if daily solar exposures was more than 14, assign 0.66
ribbonData[which(daily_solar_exposure_hobart$megajoule_per_sq_metre >= 14)] <- 0.66
# finally, set values at indexes matching daily solar exposures of greater than 19, to 1
ribbonData[which(daily_solar_exposure_hobart$megajoule_per_sq_metre >= 19)] <- 1

# plot from XTS object with dygraph (ignore solar feed in, 3rd column)
dygraphs::dygraph(file_xts[, -3], main = "Williams Home energy usage by month") %>%
  # add a ribbon for solar exposure
  # note this will be drawn on top of events/annotations
  dygraphs::dyRibbon(
    # data (0 to 1) from "ribbonData"
    data = ribbonData,
    # custom palette
    palette = c("#4010d4bb", "#72428499", "#cfbf8999", "#fff98a66"),
    # position at bottom of chart
    # (if "bottom" is not provided, anchors at bottom of chart)
    top = 0.001
  ) %>%
  # y-axis (kWh)
  dygraphs::dyAxis(
    # label
    "y", label = "kWh",
    # format with thousand separator
    axisLabelFormatter = "function(d, gran) { return d.toLocaleString() }"
  ) %>%
  # x-axis (date), make grid lines transparent
  dygraphs::dyAxis(
    "x", gridLineColor = "#ffffff00"
  ) %>%
  # specify series - label, color, style
  # note order matters here - first listed will be first in legend, and first if using stacked graphs
  dygraphs::dySeries("total_kwh", label = "Total (Peak + Off-peak - Solar feed in)", color = "#46A473", strokeWidth = 0, stepPlot = TRUE, fillGraph = TRUE) %>%
  dygraphs::dySeries("off_peak", label = "Off-peak", color = "#6E8DD5", strokeWidth = 2) %>%
  dygraphs::dySeries("peak", label = "Peak", color = "#CA7081", strokeWidth = 2, strokePattern = "dashed") %>%
  # lightly highlight the hovered series
  dygraphs::dyHighlight(
    highlightCircleSize = 3,
    highlightSeriesBackgroundAlpha = 0.8,
    hideOnMouseOut = TRUE,
    highlightSeriesOpts = list(strokeWidth = 3)
  ) %>%
  # annotations, with dates in format YYYY-MM-DD
  # leading spaces to not overlap ribbon
  dygraphs::dyEvent("2023-01-01", "      Solar installed", labelLoc = "bottom", strokePattern = "dotted", color = "#a1a1c2cc") %>%
  dygraphs::dyEvent("2025-02-01", "      +2 adults", labelLoc = "bottom", strokePattern = "dotted", color = "#a1a1c2cc") %>%
  dygraphs::dyEvent("2025-08-01", "      Home battery", labelLoc = "bottom", strokePattern = "dotted", color = "#a1a1c2cc") %>%
  # subtle shading for shorter days over winter, 45 days either side of solstice, with dates in format YYYY-MM-DD
  dygraphs::dyShading(from = "2022-05-07", to = "2022-08-05", color = "#fafafbcc") %>%
  dygraphs::dyShading(from = "2023-05-07", to = "2023-08-05", color = "#fafafbcc") %>%
  dygraphs::dyShading(from = "2024-05-07", to = "2024-08-05", color = "#fafafbcc") %>%
  dygraphs::dyShading(from = "2025-05-07", to = "2025-08-05", color = "#fafafbcc")
