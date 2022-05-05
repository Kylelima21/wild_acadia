#!/usr/bin/env Rscript --vanilla

#------------------------------------------------#
####           Packages Required              ####
#------------------------------------------------#
require(utils)
require(tidyverse)
require(tidyr)
require(dplyr)
require(googledrive)
require(readxl)
require(purrr)
require(lubridate)

select <- dplyr::select



#------------------------------------------------#
####              Read in Files               ####
#------------------------------------------------#

well.precip <- read.csv("data/raw/Data_GMW/Shiny/well_prec_data_2013-2018.csv")

CBP.dat <- read_xlsx("data/raw/CBPstage_rawdata_20220326.xlsx", sheet = 1)
CBP.x <- read_xlsx("data/raw/CBPstage_rawdata_20220326.xlsx", sheet = 2)
CBS.dat <- read_xlsx("data/raw/CBSstage_rawdata_20220326.xlsx", sheet = 1)
CBS.x <- read_xlsx("data/raw/CBSstage_rawdata_20220326.xlsx", sheet = 2)
DB.dat <- read_xlsx("data/raw/DBstage_rawdata_20220326.xlsx", sheet = 1)
DB.x <- read_xlsx("data/raw/DBstage_rawdata_20220326.xlsx", sheet = 2)
JS.dat <- read_xlsx("data/raw/JSstage_rawdata_20220326.xlsx", sheet = 1)
JS.x <- read_xlsx("data/raw/JSstage_rawdata_20220326.xlsx", sheet = 2)
MB.dat <- read_xlsx("data/raw/MBstage_rawdata_20220326.xlsx", sheet = 1)
MB.x <- read_xlsx("data/raw/MBstage_rawdata_20220326.xlsx", sheet = 2)
SB.dat <- read_xlsx("data/raw/SBstage_rawdata_20220326.xlsx", sheet = 1)
SB.x <- read_xlsx("data/raw/SBstage_rawdata_20220326.xlsx", sheet = 2)

disch <- read_xlsx("data/processed/discharge_alldata_20220402.xlsx",)



#------------------------------------------------#
####                Data_GMW                  ####
#------------------------------------------------#

#Read Kate's WL function
calc_WL_stats <- function(df, from = 2013, to = 2019){
  
  EDT<-"America/New_York"
  well_prp <- df %>% mutate(timestamp = as.POSIXct(timestamp, format = "%m/%d/%Y %H:%M"),
                            month = lubridate::month(timestamp),
                            mon = months(timestamp, abbr = T)) %>%
    filter(doy > 134 & doy < 275) %>% droplevels()
  
  well_prp2 <- well_prp %>% group_by(Year) %>%
    mutate(lag.precip = lag(precip_cm, k = 1)) %>%
    ungroup()
  
  well_prp_yr <- well_prp2 %>% filter(between(Year, from, to)) %>% droplevels()
  
  # May 1 DOY= 121; May 15 = 135; Oct.1 = 274
  well_prp_long <- well_prp_yr %>% gather("site","water_level_cm",
                                          -timestamp, -Date, -doy, -Year, -hr,
                                          -doy_h, -month, -mon, -precip_cm, -lag.precip)
  
  well_prp_long2 <- well_prp_long %>% group_by(Year, site) %>%
    mutate(lag_WL = lag(water_level_cm),
           change_WL = water_level_cm-lag_WL)
  
  # Calculate growing season stats
  well_gs_stats <- well_prp_long2 %>% group_by(Year, site) %>%
    summarise(WL_mean = mean(water_level_cm, na.rm = TRUE),
              WL_sd = sd(water_level_cm, na.rm = TRUE),
              WL_min = suppressWarnings(min(water_level_cm, na.rm = TRUE)),
              WL_max = suppressWarnings(max(water_level_cm, na.rm = TRUE)),
              max_inc = suppressWarnings(max(change_WL, na.rm = TRUE)),
              max_dec = suppressWarnings(min(change_WL, na.rm = TRUE)),
              prop_GS_comp = length(which(!is.na(water_level_cm)))/n()*100)
  
  # Calculate change in WL from average Jun to average September
  well_gs_month <- well_prp_long2 %>% group_by(Year, mon, site) %>%
    summarise(WL_mean = mean(water_level_cm, na.rm = TRUE)) %>%
    filter(mon %in% c("Jun","Sep")) %>% droplevels() %>% spread(mon, WL_mean) %>%
    mutate(GS_change = Sep - Jun)
  
  
  well_gs_prop1 <- well_prp_long2 %>% mutate(over_0 = ifelse(water_level_cm >= 0 & !is.na(water_level_cm), 1, 0),
                                             bet_0_neg30 = ifelse(water_level_cm <= 0 & water_level_cm >= -30 &
                                                                    !is.na(water_level_cm), 1, 0),
                                             under_neg30 = ifelse(water_level_cm< -30 & !is.na(water_level_cm), 1, 0),
                                             num_logs = ifelse(!is.na(water_level_cm) & !is.na(water_level_cm), 1, NA))
  
  well_gs_prop <- well_gs_prop1 %>% group_by(Year, site) %>%
    summarise(prop_over_0cm = (sum(over_0, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100,
              prop_bet_0_neg30cm = (sum(bet_0_neg30, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100,
              prop_under_neg30cm = (sum(under_neg30, na.rm = TRUE)/sum(num_logs, na.rm = TRUE))*100)
  
  gs_WL_stats <- list(well_gs_stats, well_gs_month[,c("Year","site","GS_change")], well_gs_prop) %>%
    reduce(left_join, by = c("Year", "site"))
  
  # Missing water level data from 2017, change to NA
  metrics<-c("WL_mean","WL_sd","WL_min","WL_max", "max_inc","max_dec", "prop_GS_comp",
             "GS_change", "prop_over_0cm","prop_bet_0_neg30cm","prop_under_neg30cm" )
  
  gs_WL_stats[gs_WL_stats$site=="DUCK_WL" & gs_WL_stats$Year == 2017, metrics]<-NA
  # Logger failed in DUCK in 2017
  
  prop_complete_check <- length(gs_WL_stats$prop_GS_comp[gs_WL_stats$prop_GS_comp < 90])
  
  if(prop_complete_check > 0) {
    message(paste0("Warning: There are ", prop_complete_check, " sites that have water level measurements for less than 90% of growing season."))
  }
  
  return(gs_WL_stats)
}



#Fix year column to fit the function
well.precip <- well.precip %>% 
  rename(Year=year)

#Fix the timestamp to conform
well.precip$timestamp <- strptime(as.character(well.precip$timestamp), "%Y-%m-%d %H:%M:%S")

#Run the function
test <- calc_WL_stats(well.precip, from = 2013, to = 2018)

#Write the df to the processed file on google drive
write.csv(well.precip, "data/processed/well_prec_data_2013-2018_processed20220326.csv")



#------------------------------------------------#
####      Data_HYD - Individ. Measures        ####
#------------------------------------------------#

#Arrange data
disch <- disch %>% 
  arrange(MeterType, SiteCode, Date)


#Write out the processed files
write.csv(disch, "data/processed/discharge_alldata_20220402.csv")



#------------------------------------------------#
####            Data_HYD - Stage              ####
#------------------------------------------------#

#Combine all stage data for each site
stage.comb <- bind_rows(CBP.dat, CBS.dat, DB.dat, JS.dat, MB.dat, SB.dat)

#Combine all stage senor status data for each site
stage.x.comb <- bind_rows(CBP.x, CBS.x, DB.x, JS.x, MB.x, SB.x)

#Write out the processed files
write.csv(stage.comb, "data/processed/total_stagedata_20220326.csv")
write.csv(stage.x.comb, "data/processed/total_stagesensorstatus_20220326.csv")



#------------------------------------------------#
####  Datasets_BHM_2021 - Storm Surge Study   ####
#------------------------------------------------#

##Done manually due to lack of column headers



