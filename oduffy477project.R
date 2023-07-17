# ODUFFY 477 project

#### Prelims ####
# clear consol
cat("\014")

# load packages
library(rio)
library(tidyverse)
library(tseries)
library(dplyr)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
# FRED API key: fac2e3262a353e069c8471d0114ca9d7
library(tidyverse) # organize data
library(fredr) # use the FRED API
library(reshape2) # transform data into long (melt function)

setwd("/Users/sineadoduffy/Library/CloudStorage/OneDrive-UNC-Wilmington/class master/ecn477")

#### Set Recession Objects for Plots ####
# Define the shaded US recession periods
recessions <- data.frame(
  start = as.Date(c("1973-11-01", "1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01")),
  end = as.Date(c("1975-03-01", "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01"))
)
# line of code for shaded USA recessions
# geom_rect(data=recessions, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill="gray", alpha=0.2)


# Add shaded bars for global recessions since 1970
# plots not liking this????
global_recessions <- data.frame(start = as.Date(c("1973-01-01", "1980-01-01", "1990-07-01", "2008-12-01", "2020-02-20")),
                                     end = as.Date(c("1975-12-31", "1982-11-30", "1991-03-31", "2009-06-30", "2020-04-01")))
# geom_line for global recessions:
# geom_rect(data = global_recessions, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf), fill = "gray80", alpha = 0.5) +


#### Set FRED API Key & Objects ####
fred_key <- 'fac2e3262a353e069c8471d0114ca9d7' # save as character
# set FRED key to personal FRED API key
fredr_set_key(fred_key)

# Set start date and end date for sample
start_date <- as.Date("1973-01-01")
end_date <- as.Date("2023-04-01")

#### Pull CPI Data from FRED API ####

# Pull US CPI (%Change y/y) from FRED
# see documentation
data_cpi_us <- fredr(
  series_id='CPIAUCSL',
  observation_start= start_date,
  observation_end=end_date,
  units='pc1'
)

data_cpi_us <- rename(data_cpi_us, cpi_us=value) # rename 'value' to 'cpi_us'
data_cpi_us <- data_cpi_us %>% select(date, cpi_us)

# Plot US CPI Inflation
ggplot(data=data_cpi_us) +
  geom_line(aes(x=date,y=cpi_us), color="mediumblue") +
  labs(title="U.S. Consumer Price Index Inflation Rate",
       x = "Date",
       y = "% Change From One Year Ago",
       caption="Source: U.S. Bureau of Labor Statistics") +
  geom_rect(data=recessions, aes(xmin=start, xmax=end, ymin=-Inf, ymax=Inf), fill="gray", alpha=0.2) +
  theme_minimal()

# Pull Japan CPI Inflation (%Change y/y) from FRED
data_cpi_j <- fredr(
  series_id='JPNCPIALLMINMEI',
  observation_start= start_date,
  observation_end=end_date,
  units='pc1'
)

data_cpi_j <- rename(data_cpi_j, cpi_j=value) # rename 'value' to 'cpi_j'
data_cpi_j <- data_cpi_j %>% select(date, cpi_j)

# Merge CPI from US and Japan into one (wide) data frame
cpi_wide <- merge(data_cpi_us, data_cpi_j, by="date")
cpi_long <- melt(cpi_wide, id = "date") # ggplot likes long


# Plot Series: US & Japan CPI together
# didn't like my recession line
ggplot(data=cpi_long, aes(x=date,y=value, color=variable)) +
  geom_line() + 
  labs(title="U.S. and Japan CPI Inflation", x="Date", y="% Change Y/Y",
       color="",
       caption="Source: U.S. Bureau of Labor Statistics & OECD Main Economic Indicators") +
  scale_color_manual(labels=c("U.S.", "Japan"), values=c("mediumblue", "red1")) +
  theme_minimal()

#### Pull Policy Rates from FRED API ####
# Pull US FFR
data_r_us <- fredr(
  series_id='FEDFUNDS',
  observation_start= start_date,
  observation_end=end_date
)

data_r_us <- rename(data_r_us, ffr=value) # rename 'value' to 'ffr'
data_r_us <- data_r_us %>% select(date, ffr)
view(data_r_us)

# Pull Japan Central Bank Rates ("Intermediate: Less than 24 hrs")
data_r_j <- fredr(
  series_id='IRSTCB01JPM156N',
  observation_start= start_date,
  observation_end=end_date
)

data_r_j <- rename(data_r_j, jrate=value) # rename 'value' to 'j_rate'
data_r_j <- data_r_j %>% select(date, jrate)
view(data_r_j)

# Merge US and Japan Rate data.frames into one (wide) data frame
r_wide <- merge(data_r_us, data_r_j, by="date")
r_long <- melt(r_wide, id = "date") # ggplot likes long
view(r_wide)
view(r_long)

# Plot Series: J & US Rates Together
ggplot(data=r_long, aes(x=date,y=value, color=variable)) +
  geom_line() + 
  labs(title="Policy Rates, U.S. & Japan", x="Date", y="Percent",
       color="",
       caption="Source: Board of Governors & OECD Main Economic Indicators") +
  scale_color_manual(labels=c("U.S.", "Japan"), values=c("mediumblue", "red1")) +
  theme_minimal()



#### Pull Balance Sheet data from FRED API ####
# Pull US Balance Sheet RAW
data_bs_us <- fredr(
  series_id='WALCL',
  observation_start= start_date,
  observation_end=end_date
)

data_bs_us <- rename(data_bs_us, bs_us=value) # rename 'value' to 'cpi_us'
data_bs_us <- data_bs_us %>% select(date, bs_us)

# Pull Japan Assets RAW
data_bs_j <- fredr(
  series_id='JPNASSETS',
  observation_start= start_date,
  observation_end=end_date
)

data_bs_j <- rename(data_bs_j, bs_j=value) # rename 'value' to 'cpi_us'
data_bs_j <- data_bs_j %>% select(date, bs_j)

## DID NOT WORK...! ##
# Calculate relative changes compared to the base year (2008), add 'index' variable
# data_bs_us$index_us <- data_bs_us$bs_us / data_bs_us$bs_us[data_bs_us$date == "2008-01-02"] * 100
# data_bs_j$index_j <- data_bs_j$bs_j / data_bs_j$bs_j[data_bs_j$date == "2008-01-01"] * 100

# Merge US and Japan Index! data.frames into one (wide) data frame
bs_wide <- merge(data_bs_us, data_bs_j, by="date")
bs_long <- melt(bs_wide, id = "date") # ggplot likes long
view(bs_wide)
view(bs_long)


# Plot Series: US & Japan Total Assets together, check...
ggplot(data=bs_long, aes(x=date,y=value, color=variable)) +
  geom_line() + 
  labs(title="Total Assets, U.S. & Japan", x="Date", y="Level",
       color="",
       caption="Source: Board of Governors & Bank of Japan") +
  scale_color_manual(labels=c("Million USD", "100 Million JPNY"), values=c("mediumblue", "red1")) +
  theme_minimal()

#### Pull 10Y Bond Yields from FRED API ####
# Pull US 10 Year Treasury Yeild...
data_ty_us <- fredr(
  series_id='DGS10',
  observation_start= start_date,
  observation_end=end_date
)

data_ty_us <- rename(data_ty_us, ty_us=value) # rename 'value' to 'cpi_us'
data_ty_us <- data_ty_us %>% select(date, ty_us)
view(data_ty_us)


# Pull Japan 10 Year Long term ... percent
data_ty_j <- fredr(
  series_id='IRLTLT01JPM156N',
  observation_start= start_date,
  observation_end=end_date
)

data_ty_j <- rename(data_ty_j, ty_j=value) # rename 'value' to 'cpi_us'
data_ty_j <- data_ty_j %>% select(date, ty_j)
view(data_ty_j)

# Merge Series: US & Japan Yield on 10Yr Govt. Bond (%)
ty_wide <- merge(data_ty_us, data_ty_j, by="date")
ty_long <- melt(ty_wide, id = "date") # ggplot likes long
view(ty_wide)
view(ty_long)

# Inner Join & omit NA values
# US freq=daily; JPN freq=Monthly
library(dplyr)
ty_join_wide <- inner_join(na.omit(data_ty_us), na.omit(data_ty_j), by = "date")
ty_join_long <- melt(ty_join_wide, id = "date")

# Plot Series: US & Japan 10YR Yield together
ggplot(data=ty_join_long, aes(x=date,y=value, color=variable)) +
  geom_line() + 
  labs(title="Yield on 10Yr Govt. Bond, U.S. & Japan", x="Date", y="Percent",
       color="",
       caption="Source: Board of Governors & OECD Main Economic Indicators") +
  scale_color_manual(labels=c("10YR US Treasury", "10YR JGB"), values=c("mediumblue", "red1")) +
  theme_minimal()



#### Run ACF + ADF Tests ####
# Auto-Correlation Function for US data
acf_cpi_us <- acf(cpi_wide$cpi_us)
acf_r_us <- acf(r_wide$ffr)
acf_bs_us <- acf(bs_wide$bs_us) # might be off bc of units
acf_ty_us <- acf(ty_join_wide$ty_us)

# Auto-Correlation Function for Japan data
acf_cpi_j <- acf(cpi_wide$cpi_j)
acf_r_j <- acf(r_wide$jrate)
acf_bs_j <- acf(bs_wide$bs_j) # might be off bc of units
acf_ty_j <- acf(ty_join_wide$ty_j)

### Augmented Dickey-Fuller Tests ###
library(tseries)

## US DATA ##
# ADF for US CPI
adf.test(cpi_wide$cpi_us) # 0.09935 stationary (barely lol); lag order = 8
# ADF for US FFR
adf.test(r_wide$ffr) # 0.1679, non-stationary; lag order = 8
# ADF for US Balance Sheet
adf.test(bs_wide$bs_us) # 0.4978, non-stationary; lag order = 3
# ADF for US 10YR Yield
adf.test(ty_join_wide$ty_us) # 0.2311, non-stationary; lag order = 6

## JPN DATA ##
# ADF for JPN CPI
adf.test(cpi_wide$cpi_j) # 0.01, stationary; lag order = 8
# ADF for JPN FFR
adf.test(r_wide$jrate) # 0.01766, stationary; lag order = 8
# ADF for JPN Balance Sheet
adf.test(bs_wide$bs_j) # 0.3305, non-stationary; lag order = 3
# ADF for JPN 10YR Yield
adf.test(ty_join_wide$ty_j) # 0.5992, non-stationary; lag order = 6

# We will use ARIMA modeling as it can handle non-stationary data.



# Perform ADF tests and store the results for a table
library(knitr)
adf_results <- data.frame(
  Series = c("US CPI", "US FFR", "US Balance Sheet", "US 10YR Yield",
             "JPN CPI", "JPN FFR", "JPN Balance Sheet", "JPN 10YR Yield"),
  ADF_Statistic = c(adf.test(cpi_wide$cpi_us)$statistic,
                    adf.test(r_wide$ffr)$statistic,
                    adf.test(bs_wide$bs_us)$statistic,
                    adf.test(ty_join_wide$ty_us)$statistic,
                    adf.test(cpi_wide$cpi_j)$statistic,
                    adf.test(r_wide$jrate)$statistic,
                    adf.test(bs_wide$bs_j)$statistic,
                    adf.test(ty_join_wide$ty_j)$statistic),
  P_Value = c(adf.test(cpi_wide$cpi_us)$p.value,
              adf.test(r_wide$ffr)$p.value,
              adf.test(bs_wide$bs_us)$p.value,
              adf.test(ty_join_wide$ty_us)$p.value,
              adf.test(cpi_wide$cpi_j)$p.value,
              adf.test(r_wide$jrate)$p.value,
              adf.test(bs_wide$bs_j)$p.value,
              adf.test(ty_join_wide$ty_j)$p.value),
  Stationary = c("Stationary" , "Non-Stationary" , "Non-Stationary" , "Non-Stationary",
                 "Stationary" , "Stationary" , "Non-Stationary" , "Non-Stationary")
)





### Summary Stats for data ####
library(stargazer)
data_all_stats <- stargazer(cpi_wide,r_wide,bs_wide, ty_join_wide,type="text")
stargazer(cpi_wide,r_wide,bs_wide,ty_join_wide,
          title="Descriptive Statistics",
          type="html",
          digits = 3,
          out="tables/imps_stats1.doc"
)
system("open tables/imps_stats1.doc")

summary(ty_join_wide)
sd(ty_join_wide$ty_us)
sd(ty_join_wide$ty_j)
nrow(ty_join_wide)


#### AR + ARDL Models ####
# Convert cpi to a time series variable
cpi_ts_us <- ts(cpi_wide$cpi_us, start=c(1973,1), freq=12)
cpi_ts_j <- ts(cpi_wide$cpi_j, start=c(1973,1), freq=12)
# Estimate ARDL(1,1)_cpi
ardl.1.1_cpi <- dynlm(cpi_ts_us ~ L(cpi_ts_us,1) + L(cpi_ts_j,1))


# Convert policy rates to time series variables
r_ts_us <- ts(r_wide$ffr, start=c(1973,1), freq=12)
r_ts_j <- ts(r_wide$jrate, start=c(1973,1), freq=12)
# Estimate ARDL(1,1)_r
ardl.1.1_r <- dynlm(r_ts_us ~ L(r_ts_us,1) + L(r_ts_j,1))

# Convert balance sheets to time series variables
bs_ts_us <- ts(bs_wide$bs_us, start=c(1973,1))
bs_ts_j <- ts(bs_wide$bs_j, start=c(1973,1))
# Estimate ARDL(1,1)_bs
ardl.1.1_bs <- dynlm(bs_ts_us ~ L(bs_ts_us,1) + L(bs_ts_j,1))

# Convert balance sheets to time series variables
ty_ts_us <- ts(ty_join_wide$ty_us, start=c(1973,1))
ty_ts_j <- ts(ty_join_wide$ty_j, start=c(1973,1))
# Estimate ARDL(1,1)_bs
ardl.1.1_ty <- dynlm(ty_ts_us ~ L(ty_ts_us,1) + L(ty_ts_j,1))


# table
table <- stargazer(ardl.1.1_cpi, ardl.1.1_r, ardl.1.1_bs, ardl.1.1_ty, type = "text")

# Compile ARDL(1,1) for CPI, Policy Rate, Balance Sheet, and 10Yr Yield in single table
stargazer(ardl.1.1_cpi, ardl.1.1_r, ardl.1.1_bs, ardl.1.1_ty,
          title=c("ARDL, CPI", "ARDL, Policy Rates",
                  "ARDL, Balance Sheet", "ARDL, 10-Yr Yield"),
          type="html",
          digits= 3,
          out="tables/ardls.doc")
system("open tables/ardls.doc") 





#### REGRESSIONS? ####
#### GRANGER ####


cpi_granger <- grangertest(cpi_us ~ cpi_j, data=cpi_wide, order=8)
r_granger <- grangertest(ffr ~ jrate, data=r_wide, order=8)
bs_granger <- grangertest(bs_us ~ bs_j, data=bs_wide, order=3)
ty_granger <- grangertest(ty_us ~ ty_j, data=ty_join_wide, order=6)

granger_table <- stargazer(cpi_granger, r_granger, bs_granger, ty_granger, type = "text")

stargazer(cpi_granger, r_granger, bs_granger, ty_granger,
          title=c("Granger Causality Test, CPI", "Granger Causality Test, Policy Rates",
                  "Granger Causality Test, Balance Sheet", "Granger Causality Test, 10-Yr Yield"),
          type="html",
          digits= 3,
          out="tables/grangers.doc")
system("open tables/grangers.doc") 




