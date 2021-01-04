# BUSINESS SCIENCE UNIVERSITY
# DS4B 103-R: TIME SERIES FORECASTING FOR BUSINESS
# MODULE: FEATURE ENGINEERING

# GOAL ----
# - FIND ENGINEERED FEATURES BEFORE MODELING

# OBJECTIVES:
# - Time-Based Features - Trend-Based & Seasonal Features
# - Interactions
# - Fourier Series
# - Autocorrelated Lags
# - Special Events
# - External Regressor Lags

# LIBRARIES & DATA ----

# Core
library(tidyverse)
library(lubridate)
library(timetk)
library(plotly)

# Data
google_analytics_summary_tbl <- read_rds("00_data/google_analytics_summary_hourly.rds")

learning_labs_tbl <- read_rds("00_data/learning_labs.rds") 

subscribers_tbl   <- read_rds("00_data/mailchimp_users.rds")


# DATA PREPARATION ----
# - Apply Preprocessing to Target


# FEATURE INVESTIGATION ----

# 1.0 TIME-BASED FEATURES ----
# - tk_augment_timeseries_signature()

# * Time Series Signature ----


# * Trend-Based Features ----

# ** Linear Trend


# ** Nonlinear Trend - Basis Splines


# * Seasonal Features ----

# Weekly Seasonality


# ** Monthly Seasonality


# ** Together with Trend


# 2.0 INTERACTIONS ----



# 3.0 FOURIER SERIES ----
# - tk_augment_fourier

# Data Prep


# Model


# Visualize


# 4.0 LAGS ----
# - tk_augment_lags()

# Data Prep


# Model


# Visualize


# 5.0 SPECIAL EVENTS ----

# Data Prep


# Model


# Visualize

# 6.0 EXTERNAL LAGGED REGRESSORS ----
# - xregs

# Data Prep


# Model


# Visualize

# 7.0 RECOMMENDATION ----
# - Best model: 
# - Best Model Formula:

