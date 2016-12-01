# LOAD DATA ---------------------------------------------------------
setwd("C://users/Gio/Documents")
dir()
# get table from database
library(ggplot2)
library(dplyr)
library(lubridate)
source("~/GitHub/R/My_Source_Codes/CSCAPpostgerDBconnect.R") 

# READ AGRO DATA ----------------------------------------------------
# select 6 for agronomic data and name the object as agro
wait(1)
6
agro

# READ PLOT MNGT DATA -----------------------------------------------
# select 1 for plot id and other mngt data and name the object as plot_mng
select_table()
Sys.sleep(1)
1
plot_mng

# add column to identify unique site-plot combinations
plot_mng$id <- as.factor(paste(plot_mng$uniqueid, plot_mng$plotid, sep = "_"))

# add column for unique site-plot combination and fix variable types
agro$id <- as.factor(paste(agro$site, agro$plotid, sep = "_"))
agro$site <- as.factor(agro$site)
agro$varname <- as.factor(agro$varname)
agro$updated <- NULL

# get site metadata
library(readr)
site_metadata <- read_delim("~/GitHub/CSCAP/CAP_Data/site_metadata.txt", 
                            "\t", escape_double = FALSE, 
                            col_types = cols(`City (nearest)` = col_skip(), 
                                             `Co-Leaders` = col_skip(), 
                                             `Notes 2011` = col_skip(), 
                                             `Notes 2012` = col_skip(), 
                                             `Notes 2013` = col_skip(), 
                                             `Notes 2014` = col_skip(), 
                                             `Notes 2015` = col_skip(), 
                                             `Raw LonLat` = col_skip()), 
                            trim_ws = TRUE)


# get variable full names
var_names <- read.table(file="~/GitHub/CSCAP/CAP_Data/varnames.txt", sep = "\t",
                        header = TRUE, 
                        strip.white = TRUE, 
                        #stringsAsFactors = FALSE,
                        blank.lines.skip = TRUE,
                        colClasses = c("factor", "character"))

# merge (inner join) agro and var_names on variable code
agro <- merge(agro, var_names, by.x = "varname", by.y = "code", all.x = TRUE)
agro$var_short_descr <- as.factor(agro$short_description)
agro$short_description <- NULL


# find uniqe non-numeric entries 
agro$newvalue <- as.double(agro$value)
#unique(agro$value[!is.na(agro$value) & is.na(agro$newvalue)])

# explor more about entries of "< 1" in the column "value"
agro[which(agro$value == "< 1"), ]
dim(agro[which(agro$value == "< 1"), ])[1]
summary(droplevels(agro[which(agro$value == "< 1"), ]))
# there are 32 entries of "< 1". All are for variable AGR7 (Cover crop biomass at termination) from
# two sites (GILMORE and WOOSTER.COV) in years 2014 and 2015.
# see comments in "Site Edits Needed" smartsheet for GILMORE and WOOSTER.COV by L.Abendroth
# Lori asks to put "<1 so it is clear to users that cover crop was planted but not enough for sampling". 
# Therefore it is justified to replace "<1" with 0. 

# replace "< 1" with 0
agro$newvalue[which(agro$value == "< 1")] <- 0

# copy "newvalue" to "value", and get rid off the "newvalue" column
agro$value <- agro$newvalue
agro$newvalue <- NULL

# look at the variation of AGR7
summary(agro$value[agro$varname == "AGR7"])
# 1st Quatile value is 370.8, which is way greater than detection limit of <0.1
# hence those values can be substituded with 0 without fear of loosing some important data

# plot boxplot to see destribution 
qplot(data=agro[agro$varname == "AGR7",], x=site, y=value, geom = "boxplot", na.rm=TRUE) + 
  ggtitle(var_names[var_names$code=="AGR7", 2])

# merge (inner join) agro and plot_mng on variable code
agro <- merge(agro, plot_mng[ , c("id", "rotation", "tillage", "drainage", "nitrogen")], 
              by = "id", all.x = TRUE)

# fix variable types
agro$rotation <- as.factor(agro$rotation)
agro$tillage <- as.factor(agro$tillage)
agro$drainage <- as.factor(agro$drainage)
agro$nitrogen <- as.factor(agro$nitrogen)

# READ ROTATION DATA ------------------------------------------------
# select 2 for plot rotation full names and name the object as rot
select_table()
2
rot

# merge (inner join) agro and rot on variable code
agro <- merge(agro, rot[ , c(1:2)], by.x = "rotation", by.y = "code", all.x = TRUE)

# get rid of rot
rm(rot)

# rename added column name 
names(agro)[grep("label", names(agro))] <- "rot_short_descr"

# rearrange columns of the agro df
agro <- agro[ , c("id", 
                  "site", 
                  "plotid", 
                  "varname", 
                  "year", 
                  "var_short_descr", 
                  "value", 
                  "rotation", 
                  "rot_short_descr", 
                  "tillage", 
                  "drainage", 
                  "nitrogen")]

# sort columns 
attach(agro)
agro <- agro[order(site, plotid, varname, year), ]
detach(agro)

# rename rows to make its sequence match with sorted arrangement 
rownames(agro) <- as.character(1:dim(agro)[1])

# add color-code for ONFARM and NAEW sites
agro$color <- "black"
agro$color[grepl("NAEW", agro$site)] <- "lightblue"
agro$color[grepl("ONFARM", agro$site)] <- "indianred"


# READ SOIL DATA ----------------------------------------------------
# select 7 for soil data and name the object as soil
select_table()
7
soil

soil$updated <- NULL
soil$value <- as.numeric(soil$value)
soil$sampledate <- as.Date(soil$sampledate)


# GROUPING ROTATIONS ------------------------------------------------
# combine identical roations in plots_mng
plot_mng$crot <- NA
plot_mng$crot[plot_mng$rotation %in% c("ROT1")] <- "CR01"     #continuous CORN
plot_mng$crot[plot_mng$rotation %in% c("ROT2")] <- "CR02"     #continuous SOY
plot_mng$crot[plot_mng$rotation %in% c("ROT38")] <- "CR03"    #continuous CORN  with RYE cover
plot_mng$crot[plot_mng$rotation %in% c("ROT39")] <- "CR04"    #continuous SOY   with RYE cover
plot_mng$crot[plot_mng$rotation %in% c("ROT4", 
                                      "ROT5",
                                      "ROT41", 
                                      "ROT54")] <- "CR05"    #CORN-SOY
plot_mng$crot[plot_mng$rotation %in% c("ROT36", 
                                      "ROT37", 
                                      "ROT55", 
                                      "ROT59", 
                                      "ROT60")] <- "CR06"    #CORN-SOY   with RYE cover
plot_mng$crot[plot_mng$rotation %in% c("ROT6",
                                      "ROT7",
                                      "ROT8",
                                      "ROT18", "ROT19", "ROT20",
                                      "ROT71", "ROT72", "ROT73",
                                      "ROT62")] <- "CR07"    #CORN-SOY-WHEAT
plot_mng$crot[plot_mng$rotation %in% c("ROT15",
                                      "ROT16",
                                      "ROT17")] <- "CR08"    #CORN-SOY-WHEAT/RED CLOVER
plot_mng$crot[plot_mng$rotation %in% c("ROT12",
                                      "ROT13",
                                      "ROT14")] <- "CR09"    #CORN-SOY-WHEAT/RED CLOVER (organic)
plot_mng$crot[plot_mng$rotation %in% c("ROT56",
                                      "ROT61")] <- "CR10"    #CORN-CRON-CRON-CRON-SOY
plot_mng$crot[plot_mng$rotation %in% c("ROT57",
                                      "ROT58")] <- "CR11"    #CORN-CRON-CRON-CRON-SOY with RYE cover
plot_mng$crot[plot_mng$rotation %in% c("ROT46",
                                      "ROT47",
                                      "ROT48",
                                      "ROT49")] <- "CR12"    #CORN-SOY-OATS/ALFALFA-ALFALFA
plot_mng$crot[plot_mng$rotation %in% c("ROT42", 
                                      "ROT43", 
                                      "ROT44", 
                                      "ROT45")] <- "CR13"    #CORN-SOY-OATS/ALFALFA-ALFALFA (organic)
plot_mng$crot[plot_mng$rotation %in% c("ROT40",
                                      "ROT50")] <- "CR14"    #PRAIRIE
plot_mng$crot[plot_mng$rotation %in% c("ROT51")] <- "CR15"    #WOODLAND


# naming combined roations in plots_mng
plot_mng$crot_name <- NA
plot_mng$crot_name[plot_mng$rotation %in% c("ROT1")] <- "continuous CORN"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT2")] <- "continuous SOY"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT38")] <- "continuous CORN with RYE cover"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT39")] <- "continuous SOY with RYE cover"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT4", 
                                       "ROT5",
                                       "ROT41", 
                                       "ROT54")] <- "CORN-SOY"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT36", 
                                       "ROT37", 
                                       "ROT55", 
                                       "ROT59", 
                                       "ROT60")] <- "CORN-SOY with RYE cover"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT6",
                                       "ROT7",
                                       "ROT8",
                                       "ROT18", "ROT19", "ROT20",
                                       "ROT71", "ROT72", "ROT73",
                                       "ROT62")] <- "CORN-SOY-WHEAT"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT15",
                                       "ROT16",
                                       "ROT17")] <- "CORN-SOY-WHEAT/RED CLOVER"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT12",
                                       "ROT13",
                                       "ROT14")] <- "CORN-SOY-WHEAT/RED CLOVER (organic)"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT56",
                                       "ROT61")] <- "CORN-CRON-CRON-CRON-SOY"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT57",
                                       "ROT58")] <- "CORN-CRON-CRON-CRON-SOY with RYE cover"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT46",
                                       "ROT47",
                                       "ROT48",
                                       "ROT49")] <- "CORN-SOY-OATS/ALFALFA-ALFALFA"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT42", 
                                       "ROT43", 
                                       "ROT44", 
                                       "ROT45")] <- "CORN-SOY-OATS/ALFALFA-ALFALFA (organic)"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT40",
                                       "ROT50")] <- "PRAIRIE"
plot_mng$crot_name[plot_mng$rotation %in% c("ROT51")] <- "WOODLAND"


# get rid of "north", "south", and "west" columns in plot_mng
plot_mng$north <- NULL
plot_mng$south <- NULL
plot_mng$west <- NULL

# get rid of "row" and "col"
plot_mng$row <- NULL
plot_mng$col <- NULL

# get rid of soil seris descriptions and taxonomic class
plot_mng <- plot_mng[ , - matches("taxon", vars = names(plot_mng))]
plot_mng <- plot_mng[ , - matches("descr", vars = names(plot_mng))]


# FILTERING PLOTS ---------------------------------------------------
# select code and labes of all organic rotations
rot_org <- rot[matches("\\(organic", ignore.case = TRUE, rot$label), 1:2]

# exclude plots with organic rotation from plot_mng
plots <- plot_mng %>% filter(!rotation %in% rot_org$code) #from 1038 to 1022

# exclude plots from ONFARM sites
plots <-  plots %>% filter(!grepl("ONFARM", uniqueid)) #from 1022 to 953

# exclude ISUAG.USB site (only 1 year of soil data in 2015)
plots <- plots %>% filter(uniqueid != "ISUAG.USB") # from 953 to 889

# exclude VICMS site (no soil data + organic site)
plots <- plots %>% filter(uniqueid != "VICMS") # from 889 to 882

# exclude HICKS.P site (prairie site)
plots <- plots %>% filter(uniqueid != "HICKS.P") # from 882 to 879

# exclude NAEW.Y102 site 
plots <- plots %>% filter(uniqueid != "NAEW.Y102") # from 879 to 878

# SUMMARISE PLOTS ---------------------------------------------------
library(tidyr)

# group "CCCCS" with "CC"
plots[plots$crot == "CR10", "crot_name"] <- "continuous CORN"
plots[plots$crot == "CR10", "crot"] <- "CR01"

# group "CCCCS w/ RYE cover" with "CC w/ RYE cover"
plots[plots$crot == "CR11", "crot_name"] <- "continuous CORN with RYE cover"
plots[plots$crot == "CR11", "crot"] <- "CR03"


# count number of plots in each category -----
plots %>% 
  group_by(crot, uniqueid) %>% 
  summarise(total.plots = n()) %>% 
  spread(key = crot, value = total.plots) -> CROT_by_SITE

plots %>% 
  group_by(crot) %>%
  select(crot_name) %>%
  unique() %>%
  arrange(crot) -> crot

CROT_by_SITE <- rbind(CROT_by_SITE, NA) 
CROT_by_SITE[dim(CROT_by_SITE)[1] , 1] <- "Total"
CROT_by_SITE[dim(CROT_by_SITE)[1] , -1] <- colSums(CROT_by_SITE[-1], na.rm = T)

CROT_by_SITE <- cbind(CROT_by_SITE, NA) 
names(CROT_by_SITE)[dim(CROT_by_SITE)[2]] <- "Total"
CROT_by_SITE$Total <- rowSums(CROT_by_SITE[-1], na.rm = T)

# to print ONLY
# CROT_by_SITE[is.na(CROT_by_SITE)] <- "-"

# CROT_by_SITE <- rbind(NA, CROT_by_SITE)
# CROT_by_SITE[1,1] <- "rotation name"
# CROT_by_SITE[1, grep(pattern = "CR", names(CROT_by_SITE))] <- 
#   crot$crot_name[crot$crot == names(CROT_by_SITE)[grep(pattern = "CR", names(CROT_by_SITE))]]
# 
# write.table(CROT_by_SITE, 
#             file ="C:/users/Gio/Documents/CSCAP/Analysis/Sustainable paper/Crop_Rotations_by_Site.txt",
#             quote = FALSE,
#             sep = "\t",
#             na = "-",
#             row.names = FALSE)


# count number of baseline plots in each category ----
plots %>% 
  filter(crot == "CR05" & 
           tillage == "TIL2" & 
           !drainage %in% c(paste0("DWM", 2:5)) & 
           !nitrogen %in% c("NIT3", "NIT4", "NIT1")) %>%
  group_by(uniqueid) %>% 
  summarise(total.plots = n()) ->a




  group_by(crot, uniqueid) %>% 
  summarise(total.plots = n()) %>% 
  spread(key = crot, value = total.plots) -> CROT_by_SITE

plots %>% 
  group_by(crot) %>%
  select(crot_name) %>%
  unique() %>%
  arrange(crot) -> crot

CROT_by_SITE <- rbind(CROT_by_SITE, NA) 
CROT_by_SITE[dim(CROT_by_SITE)[1] , 1] <- "Total"
CROT_by_SITE[dim(CROT_by_SITE)[1] , -1] <- colSums(CROT_by_SITE[-1], na.rm = T)

CROT_by_SITE <- cbind(CROT_by_SITE, NA) 
names(CROT_by_SITE)[dim(CROT_by_SITE)[2]] <- "Total"
CROT_by_SITE$Total <- rowSums(CROT_by_SITE[-1], na.rm = T)


# FILTERING SOIL ----------------------------------------------------

soil_bd <- soil %>% filter(varname == "SOIL1")
soil_oc <- soil %>% filter(varname == "SOIL13")

# PLOT DATA =========================================================
library(tidyverse)
library(scales)
library(lubridate)




save(agro, file = "C:/users/Gio/Documents/CSCAP/Analysis/Sustainable paper/agro.RData")
save(crot, file = "C:/users/Gio/Documents/CSCAP/Analysis/Sustainable paper/crot.RData")
save(plots, file = "C:/users/Gio/Documents/CSCAP/Analysis/Sustainable paper/plots.RData")
save(site_metadata, file = "C:/users/Gio/Documents/CSCAP/Analysis/Sustainable paper/metadata.RData")
save(var_names, file = "C:/users/Gio/Documents/CSCAP/Analysis/Sustainable paper/var_names.RData")
save(soil, file = "C:/users/Gio/Documents/CSCAP/Analysis/Sustainable paper/soil.RData")





