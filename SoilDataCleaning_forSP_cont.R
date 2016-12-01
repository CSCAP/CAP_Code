load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/soil.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/plots.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/agro.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/crot.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/metadata.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/var_names.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/big_soil_data.RData")

library(tidyr)
library(ggplot2)

# BELOW DETACTION LIMIT data ---------------------------------------- <<< NEED TO ADD TO THE MAIN CODE
# get row numbers of BDL values
BDL_rows <- grep("<", SOIL_data$value)
# get rid off "< " sign in front of the BDL values and convert all values to numeric
SOIL_data$value <- as.double(sub("< ", "", SOIL_data$value))
# substitude BDL data with half of its values
SOIL_data$value[BDL_rows] <- SOIL_data$value[BDL_rows] / 2
# save as the df as soil data
soil <- SOIL_data

# see all SOIL variable codes and description
var_names %>% filter(grepl("SOIL", code))
  # SOIL1 = BD, SOIL13 = SOC, SOIL14 = TN

# select "soil" data with the (a) variables, and (b) sites and plots of interest
# average over replicates 
soil %>% 
  filter(varname %in% c("SOIL1", "SOIL13", "SOIL14")) %>%
#  filter(site %in% unique(plots$uniqueid)) %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  group_by(site, plotid, year, varname, depth) %>%
  summarise_at(vars(value), mean, na.rm = T) -> soil_all

#soil_all[soil_all$varname == "SOIL1", ] %>% head(10)

 
spread(soil_all, key = varname, value = value) %>%             # made each SOIL variable a column
  group_by(site, plotid, year) %>%
  summarise_at(vars(matches("SOIL")), mean, na.rm = T) %>%     # see if there is at least one measurement that year at any depth
  ungroup() %>% 
  group_by(site, plotid) %>%
  summarise(BD_count = sum(!is.na(SOIL1)), 
            SOC_count = sum(!is.na(SOIL13)), 
            TN_count = sum(!is.na(SOIL14))) %>%     # count how many years of measurement are available 
  filter(SOC_count < 2) -> soil_no_data_sites       # filter plots with 1 or no year measurement of SOC
  

# gives the list of sites that have plot(s) with not enough soil data (and number of plots with insufficient data)
soil_no_data_sites %>% group_by(site) %>% summarise(n())
soil_no_data_sites$id <- paste(soil_no_data_sites$site, soil_no_data_sites$plotid, sep = "_")

# remove plots with no soil data from "plots"
plots_data <- plots[!plots$id %in% soil_no_data_sites$id, ]

# list number of plots by crop rotation groups and sites
plots_data %>% 
  group_by(crot, uniqueid) %>% 
  summarise(total.plots = n()) %>% 
  spread(key = crot, value = total.plots) -> plots_summary


# # made each SOIL variable a column 
# spread(soil_all, key = varname, value = value) %>% 
#   group_by(site, plotid, year) %>%
#   summarise_at(vars(matches("SOIL")), mean, na.rm = T) %>%     # see if there is at least one measurement that year at any depth
#   group_by(site, plotid) %>%
#   summarise(BD_count = sum(!is.na(SOIL1)), 
#             SOC_count = sum(!is.na(SOIL13)), 
#             TN_count = sum(!is.na(SOIL14))) %>%     # count how many years of measurement are available 
#   filter(SOC_count > 1) -> soil_data_sites       # filter plots with more than 1 year measurement of SOC


# get renewed soil plots and values
soil %>%
  filter(varname %in% c("SOIL1", "SOIL13", "SOIL14")) %>%
  filter(site %in% unique(plots$uniqueid)) %>%
  mutate(id = paste(site, plotid, sep = "_")) %>%
  filter(id %in% unique(plots$id)) %>% 
  filter(!id %in% soil_no_data_sites$id) %>%              #additional filter to remove plots with no or 1 year soil data
  group_by(site, plotid, year, varname, depth) %>%
  summarise_at(vars(value), mean, na.rm = T) %>% 
  spread(key= varname, value = value) -> soil_all_data


## EXPLOR DATA 
# trying to see how many sites have data in each year
soil_all_data$site %>% unique() %>% length()
soil_all_data$site[soil_all_data$year == "2011"] %>% unique() %>% length()
soil_all_data$site[soil_all_data$year == "2012"] %>% unique() %>% length()
soil_all_data$site[soil_all_data$year == "2013"] %>% unique() %>% length()
soil_all_data$site[soil_all_data$year == "2014"] %>% unique() %>% length()
soil_all_data$site[soil_all_data$year == "2015"] %>% unique() %>% length()

setdiff(soil_all_data$site[soil_all_data$year == "2015"], soil_all_data$site[soil_all_data$year == "2011"])
soil_all_data$site[soil_all_data$year == "2012"] %>% unique()
  # all 28 sites have data collected in 2015
  # only 25 sites have first year soil data collected in 2011
  # 2 sites (ORR, WOOSTER.COV) have the first soil data collected in 2012
  # BRADFORD.A collected its first soil data in 2013
## END OF EXPLOR DATA

# FIND years with no data for each plot
soil_all_data %>%           
  group_by(site, plotid, year) %>%
  summarise_at(vars(matches("SOIL")), mean, na.rm = T) %>%
  ungroup() %>%
  mutate(id = paste(site, plotid, year, sep = "_")) %>%
  filter(is.na(SOIL13)) -> soil_no_data_years
  
# remove plot-years with no SOIL13 data from "soil_all_data" 
soil_all_data %>% 
  mutate(id = paste(site, plotid, year, sep = "_")) %>% 
  filter(!id %in% soil_no_data_years$id) -> soil_all_new_data


# FIRST YEAR of SITES/PLOTS --------------------------
# find the first and the last soil years 
soil_all_new_data %>% 
  group_by(site) %>% 
  summarise('First Year' = min(year), 
            'Last Year' = max(year),
            'delta(SOC)' = (max(year) - min(year))) -> first_last_year

soil_all_new_data %>% 
  group_by(site, plotid) %>% 
  summarise('First_Year' = min(year), 
            'Last_Year' = max(year),
            'delta(SOC)' = (max(year) - min(year))) -> first_last_year_new
# find sites that have plots with different first years for different plots
first_last_year_new %>% 
  group_by(site, First_Year) %>% 
  summarise() %>%
  group_by(site) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  filter(count > 1) -> tempo
# list of plots with the First Year different than the rest of the Site plots
first_last_year_new %>% 
  filter(site %in% tempo$site, First_Year != 2011)


# find sites that have plots with different last years for different plots
first_last_year_new %>% 
  group_by(site, Last_Year) %>% 
  summarise() %>%
  group_by(site) %>%
  summarise(count = n())%>%
  arrange(desc(count)) %>%
  filter(count > 1)
# There are no such a site

# creat ids for "first_last_year_new" and stor as "fly_first" and "fly_last"
first_last_year_new %>% 
  mutate(id  = paste(site, First_Year, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>% 
  mutate(id5 = paste(site, plotid, First_Year, sep = "_")) -> fly_first
first_last_year_new %>% 
  mutate(id  = paste(site, Last_Year, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>%
  mutate(id5 = paste(site, plotid, Last_Year, sep = "_")) -> fly_last


# AGGREGATE 20-30 and 30-40 depths into 20-40 ------------------
# WOOSTER.LTR, HOYTVILLE.LTR, and NAEW.WS (109, 111, 113 and 118) sites have mismatching depths for soil BD and SOC
# BD measurement were done at 20-30 and 30-40 cm depth intervals, while SOC is at 20-40cm
soil_all_new_data %>%
  ungroup() %>%
  filter(depth %in% c("20 - 30", "30 - 40")) %>% 
  group_by(site, plotid, year) %>%
  summarise_at(vars(SOIL1), mean, na.rm = T) %>%
  mutate(id = paste(site, plotid, year, sep = "_")) -> tempo    # calculate average BD for 20-40 cm depth based on 20-30 and 30-40

# assign calculated BDs to corresponding 20-40 BD 
soil_all_new_data$SOIL1[soil_all_new_data$depth == "20 - 40" & soil_all_new_data$id %in% tempo$id] <- tempo$SOIL1
soil_all_new_data <- soil_all_new_data[!soil_all_new_data$depth %in% c("20 - 30", "30 - 40"), ]


# get rid of the redundant depths in STJOHNS 
# in 2011 soil pH and CEC were measured at 0-30, 30-60, and 60-90 cm intervals
# while BD, SOC, and TN was measured at "regular" depths corresponding to the protocol 
# here we remove those artifactrs from the "soil_all_new_data"
soil_all_new_data <- 
  soil_all_new_data[-which(soil_all_new_data$site == "STJOHNS" & soil_all_new_data$depth %in% c("0 - 30", "30 - 60", "60 - 90")), ]


# SOC ---------------------------------------------------------------
# set "id"to be used for sorting of "soil_all_new_data"
soil_all_new_data %>% 
  mutate(id = paste(site, year, sep = "_")) %>%
  mutate(id2 = paste(site, plotid, depth, sep = "_")) %>%
  mutate(id3 = paste(site, plotid, year, depth, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> soil_all_new_data


# find rows with no SOC data in the "First Year"
soil_all_new_data %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL13)) -> first_missing

# find values corresponding to the "first_missing" data in non-first-year
soil_all_new_data %>% 
  filter(!id5 %in% fly_first$id5) %>%   #choose all no-first-year data
  filter(id2 %in% first_missing$id2) %>% 
  ungroup() %>%
  filter(year == 2013) %>% 
  select(id, id2, id3, SOIL13) -> first_missing_subs_13

# replace missing SOIL13 values in 2011 with substitutes from 2013
soil_all_new_data$SOIL13[soil_all_new_data$id3 %in% first_missing$id3] <- 
  first_missing_subs_13$SOIL13[first_missing_subs_13$id2 %in% first_missing$id2]


# Before searching for LAST MISSING data
# restore ARL data that was accidentally deleted from Google Sheets
# these values were also added back to the Google Sheet
# WHEN CSCAP DB IS UPDATED THIS CHUNK OF CODE WILL BE ABSOLETE
soil_all_new_data[soil_all_new_data$id3 == "ARL_113_2015_20 - 60", c("SOIL13", "SOIL14")] <- c(0.959591, 0.105528)


# find rows with no SOC data in the "Last Year"
soil_all_new_data %>%
  filter(id5 %in% fly_last$id5) %>%
  filter(is.na(SOIL13)) -> last_missing

# find values corresponding to the "last_missing" data in non-last-year
soil_all_new_data %>%
  filter(!id5 %in% fly_last$id5) %>%   #choose all no-first-year data
  filter(id2 %in% last_missing$id2) %>% 
  ungroup() %>%
  filter(year == 2011) %>%    # 2011 was choosen since there is no data in 2013
  select(id, id2, id3, SOIL13) -> last_missing_subs_13

# replace missing SOIL13 values in 2015 with substitutes from 2011
soil_all_new_data$SOIL13[soil_all_new_data$id3 %in% last_missing$id3] <- 
  last_missing_subs_13$SOIL13[last_missing_subs_13$id2 %in% last_missing$id2]


# BD ----------------------------------------------
# find rows with no BD data in the "First Year"
# iteration 1
soil_all_new_data %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL1)) -> first_missing_BD

# find values corresponding to the "first_missing_BD" data in non-first-year
soil_all_new_data %>%
  filter(!id5 %in% fly_first$id5) %>%   #choose all no-first-year data
  filter(id2 %in% first_missing_BD$id2) %>% 
  ungroup() %>%
  filter(year == 2013) %>% 
  filter(!is.na(SOIL1)) %>%
  select(id, id2, id3, SOIL1) -> first_missing_subs_1

#replace missing SOIL1 values in 2011 with substitutes from 2013
soil_all_new_data$SOIL1[soil_all_new_data$id3 %in% first_missing_BD$id3 & soil_all_new_data$id2 %in% first_missing_subs_1$id2] <- 
  first_missing_subs_1$SOIL1

# iteration 2
soil_all_new_data %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL1)) -> first_missing_BD2

# find values corresponding to the "first_missing_BD" data in non-first-year
soil_all_new_data %>%
  filter(!id5 %in% fly_first$id5) %>%   #choose all no-first-year data
  filter(id2 %in% first_missing_BD2$id2) %>% 
  ungroup() %>%
  filter(year == 2014) %>% 
  filter(!is.na(SOIL1)) %>%
  select(id, id2, id3, SOIL1) -> first_missing_subs_1

#replace missing SOIL1 values in 2011 with substitutes from 2014
soil_all_new_data$SOIL1[soil_all_new_data$id3 %in% first_missing_BD2$id3 & soil_all_new_data$id2 %in% first_missing_subs_1$id2] <- 
  first_missing_subs_1$SOIL1

# iteration 3
soil_all_new_data %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL1)) -> first_missing_BD3

# find values corresponding to the "first_missing_BD" data in non-first-year
soil_all_new_data %>%
  filter(!id5 %in% fly_first$id5) %>%   #choose all no-first-year data
  filter(id2 %in% first_missing_BD3$id2) %>% 
  ungroup() %>%
  filter(year == 2015) %>% 
  filter(!is.na(SOIL1)) %>%
  select(id, id2, id3, SOIL1) -> first_missing_subs_1

#replace missing SOIL1 values in 2011 with substitutes from 2013
soil_all_new_data$SOIL1[soil_all_new_data$id3 %in% first_missing_BD3$id3 & soil_all_new_data$id2 %in% first_missing_subs_1$id2] <- 
  first_missing_subs_1$SOIL1

# iteration 4
soil_all_new_data %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL1)) -> first_missing_BD4

first_missing_BD4 %>% group_by(site, plotid) %>% summarise(n())

# for NAEW sites 
# there was no soil sample collected at 40-60 depth at any given year, hence:
# - substitute 2013 BD data at 20-40 and 40-60  with BD values from 2011 at 20-40 cm
# - in 2015 assign 20-40 cm BD data to the 40-60 cm
soil_all_data %>% 
  filter(grepl("NAEW.WS", site), year == 2011, depth == "20 - 40") %>%
  mutate(id2 = paste(site, plotid, depth, sep = "_")) -> tempo
soil_all_new_data[soil_all_new_data$id2 %in% tempo$id2 & soil_all_new_data$year == 2013, ]$SOIL1 <- tempo$SOIL1
soil_all_new_data[grepl("NAEW", soil_all_new_data$site) & 
                    soil_all_new_data$depth == "40 - 60" & 
                    soil_all_new_data$year == 2013, ]$SOIL1 <- tempo$SOIL1
soil_all_new_data[grepl("NAEW", soil_all_new_data$site) & 
                    soil_all_new_data$depth == "40 - 60" & 
                    soil_all_new_data$year == 2015, ]$SOIL1 <- soil_all_new_data[grepl("NAEW", soil_all_new_data$site) & 
                                                                             soil_all_new_data$depth == "20 - 40" & 
                                                                             soil_all_new_data$year == 2015, ]$SOIL1

# iteration 5
soil_all_new_data %>% 
  filter(id5 %in% fly_first$id5) %>%
  filter(is.na(SOIL1)) -> first_missing_BD5

first_missing_BD5 %>% group_by(site, plotid) %>% summarise(n())

# Check if there is any soil data available for those plots at any year
soil_all_data %>% 
  filter(paste(site, plotid, sep = "_") %in% first_missing_BD5$id4) %>% 
  filter(!is.na(SOIL1)) %>%
  mutate(id2 = paste(site, plotid, depth, sep = "_")) %>%
  mutate(id4 = paste(site, plotid, sep = "_")) -> tempo
# only BRADFORD.C 104W has soil BD data in 2011
# there is no BD data for any other plots

# copy 2011 BD data in 2013 and 2015 years for BRADFORD.C plot 104W
soil_all_new_data[soil_all_new_data$id2 %in% tempo$id2 & soil_all_new_data$year == 2013, ]$SOIL1 <- tempo$SOIL1
soil_all_new_data[soil_all_new_data$id2 %in% tempo$id2 & soil_all_new_data$year == 2015, ]$SOIL1 <- tempo$SOIL1


# find rows with no BD data in the "Last Year"
# iteration 1
soil_all_new_data %>%
  filter(id5 %in% fly_last$id5) %>%
  filter(is.na(SOIL1)) -> last_missing_BD

last_missing_BD %>% group_by(site) %>% summarize(n())

# find values corresponding to the "last_missing" data in non-last-year
soil_all_new_data %>%
  filter(!id5 %in% fly_last$id5) %>%   #choose all no-first-year data
  filter(id2 %in% last_missing_BD$id2) %>% 
  ungroup() %>%
  filter(year == 2013) %>% 
  filter(!is.na(SOIL1)) %>%
  select(id, id2, id3, SOIL1) -> last_missing_subs_1

# replace missing SOIL1 values in 2015 with substitutes from 2013
soil_all_new_data[soil_all_new_data$id3 %in% last_missing_BD$id3 & soil_all_new_data$id2 %in% last_missing_subs_1$id2, ]$SOIL1 <- 
  last_missing_subs_1$SOIL1


# iteration 2
soil_all_new_data %>%
  filter(id5 %in% fly_last$id5) %>%
  filter(is.na(SOIL1)) -> last_missing_BD2

last_missing_BD2 %>% group_by(site) %>% summarize(n())

# find values corresponding to the "last_missing" data in non-last-year
soil_all_new_data %>%
  filter(!id5 %in% fly_last$id5) %>%   #choose all no-first-year data
  filter(id2 %in% last_missing_BD2$id2) %>% 
  ungroup() %>%
  filter(year == 2011) %>% 
  filter(!is.na(SOIL1)) %>%
  select(id, id2, id3, SOIL1) -> last_missing_subs_1

#replace missing SOIL1 values in 2015 with substitutes from 2011
soil_all_new_data[soil_all_new_data$id3 %in% last_missing_BD2$id3 & soil_all_new_data$id2 %in% last_missing_subs_1$id2, ]$SOIL1 <- 
  last_missing_subs_1$SOIL1


# iteration 3
soil_all_new_data %>%
  filter(id5 %in% fly_last$id5) %>%
  filter(is.na(SOIL1)) -> last_missing_BD3

last_missing_BD3 %>% group_by(site) %>% summarize(n())

last_missing_BD3 %>% group_by(site, plotid) %>% summarize(n())

# the END of the missing value substitutions


# C STOCK ------------------------

# filter all data so only first and last year values remain
first_last_year_new %>% 
  mutate(`delta(SOC)` = NULL) %>%
  ungroup() %>%
  gather(key = FLY, value = year, First_Year:Last_Year) %>%
  mutate(id5 = paste(site, plotid, year, sep = "_")) -> tempo
soil_all_new_data %>% 
  filter(id5 %in% tempo$id5) %>%
  ungroup() %>%
  select(site, plotid, year, depth, SOIL1, SOIL13) %>%
  filter(!is.na(SOIL1)) %>%                                # remove plots with no BD data (72 rows)
  mutate(top_depth = as.numeric(word(depth))) %>% 
  mutate(bot_depth = as.numeric(word(depth, 3))) %>% 
# convert BD from g/cm3 to kg/ha by multiplying it with depth and 100,000
# multiply above value by SOC (decimals) to get CS (carbon stocks) in kg C per ha (kg C/ha)
  mutate(CS = SOIL1*(bot_depth-top_depth)*100000*SOIL13/100) %>%
# DO WE NEED ANY ADJUSTMENT FOR GRAVEL CONTENT at deeper layers?                 <<< Q to LORI, MATT, RICK
  group_by(site, plotid, year) %>%
  summarize(CS = sum(CS)) %>%
  mutate(id = paste(site, plotid, sep = "_")) -> CS

merge(CS, plots_data[- which(names(plots_data) %in% c("plotid"))], by = "id", all.x = TRUE) %>%     # names()
  select(site, plotid, year, CS, crot, crot_name, rotation, tillage, drainage, nitrogen) -> CS_plot

merge(CS_plot, site_metadata[which(names(site_metadata) %in% c("UniqueID", "Latitude", "Longitude", "Plot Size (ha)"))], 
      by.x = "site", by.y = "UniqueID", all.x = TRUE) -> CS_plot

# PLOT TIME -------------------------------------
CS_plot %>% 
  group_by(crot, site) %>% 
  summarise(total.plots = n()/2) %>%            # divide by two because plots are counted twice for FIRST and LAST years
  spread(key = crot, value = total.plots) -> CS_summary


 

# EXAMPLE of modifying subset of df via dplr
#b %>% mutate(depth = ifelse(depth == "0 - 10" & year == "2015", "10 cm", depth))



