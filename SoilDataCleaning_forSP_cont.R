load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/soil.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/plots.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/agro.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/crot.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/metadata.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/var_names.RData")
load("C:/Users/Gio/Documents/Work/CSCAP/Sustainable Corn Paper/big_soil_data.RData")

library(tidyr)
library(ggplot2)
library(scales)
library(stringr)

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

# calculate average plot site for each site
site_metadata$`Plot Size (ha)` %>% 
  as.numeric() %>%
  is.na() %>%
  site_metadata[., c("UniqueID", "Number of Plots/ Subplots", "Site Area (ha)", "Plot Size (ha)")] 
site_metadata$`Ave Plot Size (ha)` <- as.numeric(site_metadata$`Plot Size (ha)`)
site_metadata$`Ave Plot Size (ha)`[site_metadata$UniqueID == "HICKS.B"] <- round(40.8 / 6, 1)
site_metadata$`Ave Plot Size (ha)`[site_metadata$UniqueID == "HICKS.G"] <- round(20.2 / 12, 1)
site_metadata$`Ave Plot Size (ha)`[site_metadata$UniqueID == "SERF"] <- round(17 / 24, 1)


merge(CS_plot, site_metadata[which(names(site_metadata) %in% c("UniqueID", 
                                                               "Latitude", 
                                                               "Longitude", 
                                                               "County", 
                                                               "State", 
                                                               "Ave Plot Size (ha)"))], 
      by.x = "site", by.y = "UniqueID", all.x = TRUE) -> CS_plot

# PLOT TIME ========================================================================================
setwd("C:/Users/Gio/Documents/GitHub/CSCAP/CAP_Fig/Soil_Paper")

CS_plot %>% 
  group_by(crot, site) %>% 
  summarise(total.plots = n()/2) %>%            # divide by two because plots are counted twice for FIRST and LAST years
  spread(key = crot, value = total.plots) -> CS_summary
# to print 
CS_summary[is.na(CS_summary)] <- "-"

# aggregated CS by sites --------------------
CS_plot %>%
  group_by(site) %>%
  select(site, CS, Latitude, Longitude, State, County, `Ave Plot Size (ha)`) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(site = factor(site, levels = site[order(Latitude)])) %>% 
  mutate(State = as.factor(State)) -> CS_site_plot

# PLOT 1
CS_site_plot %>%
  ggplot(aes(x=site, y=CS)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 250000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT1

#1.a
PLOT1 + ggtitle("Average CS by Site")
ggsave(filename = "plot01.png", width = 12, dpi = 300)

#1.b
PLOT1 + 
  facet_grid(~ State) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "plot01b.png", width = 12, dpi = 300)

#1.c
CS_site_plot %>%
  ggplot(aes(x=site, y=CS, fill = State)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 250000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light()
ggsave(filename = "plot01c.png", width = 12, dpi = 300)


# aggregated CS by sites and first-last year (fly) -------------------
CS_plot %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(site) %>%
  select(site, CS_diff, first, last, Latitude, Longitude, State, `Ave Plot Size (ha)`) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(site = factor(site, levels = site[order(Latitude)])) %>% 
  mutate(State = as.factor(State)) -> CS_site_fly_plot

# PLOT 2
CS_site_fly_plot %>%
  ggplot(aes(x=site, y=CS_diff)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(-100000, 100000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT2

#2.a
PLOT2 + ggtitle("Average CS Difference between the First and the Last Data Years by Site")
ggsave(filename = "plot02.png", width = 12, dpi = 300)

#2.b
PLOT2 + 
  facet_grid(~ State) + 
  theme(axis.text.x = element_text(angle = 90))
ggsave(filename = "plot02b.png", width = 12, dpi = 300)

#2.c
CS_site_fly_plot %>%
  ggplot(aes(x=site, y=CS_diff, fill = State)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "SITE ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(-100000, 100000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light()
ggsave(filename = "plot02c.png", width = 12, dpi = 300)






# aggregated CS by State --------------------
CS_plot %>%
  group_by(State) %>%
  select(CS, Latitude, Longitude, State) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(State = factor(State, levels = State[order(Latitude)])) -> CS_state_plot

# PLOT 3
CS_state_plot %>%
  ggplot(aes(x=State, y=CS)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "US STATE") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 250000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT3

#3.a
PLOT3 + ggtitle("Average CS by State")
ggsave(filename = "plot03.png", width = 12, dpi = 300)


# aggregated CS by State and first-last year (fly) -------------------
CS_plot %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, CS, Latitude, Longitude, State, `Ave Plot Size (ha)`, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(State) %>%
  select(State, CS_diff, first, last, Latitude, Longitude) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(State = factor(State, levels = State[order(Latitude)])) -> CS_state_fly_plot

# PLOT 4
CS_state_fly_plot %>%
  ggplot(aes(x=State, y=CS_diff)) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "US STATE") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(-50000, 50000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT4

#4.a
PLOT4 + ggtitle("Average CS Difference between the First and the Last Data Years by State")
ggsave(filename = "plot04.png", width = 12, dpi = 300)



# aggregated CS by Rotation --------------------
CS_plot %>%
  group_by(crot) %>%
  select(crot, crot_name, CS) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(crot = as.factor(crot)) -> CS_crot_plot

# PLOT 5
CS_crot_plot %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=CS, fill=I(color))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Crop Rotation ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 150000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  theme_bw() -> PLOT5

#5.a
PLOT5 + 
  ggtitle("Average CS by Crop Rotation") + 
  geom_text(aes(label=str_wrap(crot_name, width=8)), vjust=-0.5, size = 4)
ggsave(filename = "plot05.png", width = 12, dpi = 300)


# aggregated CS by Rotation and first-last year (fly) -------------------
CS_plot %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, CS, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  group_by(crot) %>%
  select(crot, crot_name, CS_diff, first, last) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  mutate(crot = as.factor(crot)) -> CS_crot_fly_plot

# PLOT 6
CS_crot_fly_plot %>%
  mutate(color = ifelse(crot == "CR05", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=CS_diff, fill = I(color))) + 
  geom_bar(stat = "identity") +
  scale_x_discrete(name = "Crop Rotaion ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(-15000, 15000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT6

#6.a
PLOT6 + 
  ggtitle("Average CS Difference between the First and the Last Data Years by Crop Rotation") +
  geom_text(aes(label=str_wrap(crot_name, width = 10)), hjust = "outward", size = 4)
ggsave(filename = "plot06.png", width = 12, dpi = 300)





# aggregated CS by Rotation and others --------------------
CS_plot %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  group_by(crot, tillage, drainage) %>%
  select(crot, crot_name, tillage, drainage, CS) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) -> CS_crot_plus_plot

# PLOT 7
CS_crot_plus_plot %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT" & drainage == "Not Drained", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=CS, fill=I(color))) + 
  geom_bar(stat = "identity") +
  facet_grid(tillage ~ drainage) +
  scale_x_discrete(name = "Crop Rotation ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(0, 200000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  theme_bw() -> PLOT7

#7.a
PLOT7 + 
  ggtitle("Average CS by Crop Rotation grouped by Drainage and Tillage") + 
  geom_text(aes(label=str_wrap(crot_name, width=12)), vjust=-0.5, size = 2.5)
ggsave(filename = "plot07.png", width = 12, dpi = 300)


# aggregated CS by Rotation and other in first-last year (fly) -------------------
CS_plot %>%
  group_by(site, plotid) %>%
  mutate(fly = ifelse(year == max(year), "last", "first")) %>% 
  select(site, plotid, crot, crot_name, tillage, drainage, CS, fly) %>%
  spread(fly, CS) %>%
  mutate(CS_diff = last - first) %>% 
  mutate(tillage = ifelse(tillage == "TIL2", "CT", "NT")) %>%       #TIL4 becomes No-Till too
  mutate(drainage = ifelse(drainage %in% c("DWM2", "DWM3", "DWM4", "DWM5"), "Drained", "Not Drained")) %>%
  group_by(crot, tillage, drainage) %>%
  select(crot, crot_name, tillage, drainage, CS_diff, first, last) %>%
  summarise_each(funs(if(is.numeric(.)) mean(., na.rm = TRUE) else first(.))) %>%
  ungroup() %>%
  mutate(crot = as.factor(crot)) %>%
  mutate(tillage = as.factor(tillage)) %>%
  mutate(drainage = as.factor(drainage)) -> CS_crot_plus_fly_plot


# PLOT 8
CS_crot_plus_fly_plot %>%
  mutate(color = ifelse(crot == "CR05" & tillage == "CT" & drainage == "Not Drained", "orange", "grey40")) %>%
  ggplot(aes(x=crot, y=CS_diff, fill=I(color))) + 
  geom_bar(stat = "identity") +
  facet_grid(tillage ~ drainage) +
  scale_x_discrete(name = "Crop Rotaion ID") +
  scale_y_continuous(name = "CS (kg C/ha)",
                     limits = c(-40000, 28000),
                     labels = comma) + 
  theme(axis.title.x = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.x  = element_text(angle=90, vjust=0.5, size=12),
        axis.title.y = element_text(face="bold", colour="goldenrod", size=16),
        axis.text.y  = element_text(size=12)) +
  coord_flip() +
  theme_light() -> PLOT8

#8.a
PLOT8 + 
  ggtitle("Average CS Difference between the First and the Last Data Years by Crop Rotation grouped by Drainage and Tillage") +
  geom_text(aes(label=str_wrap(crot_name, width = 10)), hjust = "outward", size = 2.5)
ggsave(filename = "plot08.png", width = 12, dpi = 300)


geom_text(aes(label=str_wrap(crot_name, width=12)), vjust=-0.5, size = 2.5)

#



