# Load required packages
library(data.table)
library(ggplot2)
library(plotly)
library(ggExtra)

# Part 1: Hydrological Years and Catchment Selection

# Task 1: Assign hydrological years
data <- data.table(
  ID = c(1, 2, 3, 4, 5, 6),
  YR = c(2000, 2001, 2002, 2003, 2004, 2005),
  MNTH = c(9, 10, 11, 12, 1, 2)
)
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]

# Task 2: Compute Overall Runoff Coefficients per Catchment
data <- data.table(
  ID = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
  YR = c(2000, 2000, 2000, 2001, 2001, 2001, 2002, 2002, 2002),
  MNTH = c(9, 10, 11, 12, 1, 2, 3, 4, 5),
  PRCP = c(100, 150, 120, 80, 90, 85, 60, 70, 65),
  OBS_RUN = c(50, 75, 60, 40, 45, 42, 30, 35, 32)
)
data[, HYR := ifelse(MNTH %in% c(10, 11, 12), YR + 1, YR)]
runoff_data <- data[, .(RC = sum(OBS_RUN) / sum(PRCP)), by = ID]

# Task 3: Classify Catchments Based on Runoff Coefficients
rc_breaks <- unique(quantile(runoff_data$RC, probs = seq(0, 1, 0.2)))
runoff_data[, RC_class := cut(RC, breaks = rc_breaks, labels = c("Very Low", "Low", "Moderate", "High", "Very High"), include.lowest = TRUE)]
selected_catchments <- runoff_data[, .SD[sample(.N, 1)], by = RC_class]

# Part 2: Water Balance and Snowmelt Contribution to Runoff

# Task 4: Compute Monthly and Annual Water Balance for Selected Catchments
data <- data.table(
  ID = rep(1:5, each = 6),
  HYR = rep(2000:2001, each = 15),
  MNTH = rep(1:6, 5),
  PRCP = runif(30, 50, 150),
  SWE = runif(30, 10, 100),  # Replaced PET with SWE
  OBS_RUN = runif(30, 20, 80)
)
monthly_balance <- data[, .(Mean_PRCP = mean(PRCP), Mean_SWE = mean(SWE), Mean_OBS_RUN = mean(OBS_RUN)), by = .(HYR, ID, MNTH)]
annual_balance <- data[, .(Total_PRCP = sum(PRCP), Total_SWE = sum(SWE), Total_OBS_RUN = sum(OBS_RUN)), by = .(HYR, ID)]
monthly_balance[, WB := Mean_PRCP - Mean_SWE]
annual_balance[, WB := Total_PRCP - Total_SWE]
monthly_deficit <- monthly_balance[WB < 0]

# Task 5: Snowmelt contribution to runoff
set.seed(123)
data[, Snowmelt := max(SWE) - SWE]
data <- merge(data, monthly_balance[, .(HYR, ID, MNTH, Mean_OBS_RUN)], by = c("HYR", "ID", "MNTH"), all.x = TRUE)
cor_snowmelt_runoff <- cor(data[MNTH %in% c(3, 4, 5), .(Snowmelt, Mean_OBS_RUN)], use = "complete.obs")
