# LOAD DATA ---------------------------------------------------------
# get table from database
library(ggplot2)
library(dplyr)

setwd("C:/Users/Gio/Documents")
source("~/GitHub/R/My_Source_Codes/CSCAPpostgerDBconnect.R") 

# select 6 for soilnomic data and name the object as soil
#wait(1)
7
soil

# select 1 for plot mngt and other data and name the object as plot_mng
select_table()
Sys.sleep(1)
1
plot_mng

# add column to identify unique site-plot combinations
plot_mng$id <- as.factor(paste(plot_mng$uniqueid, plot_mng$plotid, sep = "_"))

# add column for unique site-plot combination and fix variable types
soil$id <- as.factor(paste(soil$site, soil$plotid, sep = "_"))
soil$site <- as.factor(soil$site)
soil$varname <- as.factor(soil$varname)
soil$updated <- NULL

# get variable full names
var_names <- read.table(file="~/GitHub/CSCAP/CAP_Data/varnames.txt", sep = "\t",
                        header = TRUE, 
                        strip.white = TRUE, 
                        #stringsAsFactors = FALSE,
                        blank.lines.skip = TRUE,
                        colClasses = c("factor", "character"))
var_names_SOIL19 <- read.table(file="~/GitHub/CSCAP/CAP_Data/varnames_SOIL19.txt", sep = "\t",
                        header = TRUE, 
                        strip.white = TRUE, 
                        #stringsAsFactors = FALSE,
                        blank.lines.skip = TRUE,
                        colClasses = c("factor", "character"))
var_names <- rbind(var_names, var_names_SOIL19)


# merge (inner join) soil and var_names on variable code
soil <- merge(soil, var_names, by.x = "varname", by.y = "code", all.x = TRUE)
soil$var_short_descr <- as.factor(soil$short_description)
soil$short_description <- NULL


# find uniqe non-numeric entries 
# get row numbers of BDL values
BDL_rows <- grep("<", soil$value)
# get rid off "< " sign in front of the BDL values and convert all values to numeric
soil$newvalue <- as.double(sub("< ", "", soil$value))
# substitude BDL data with half of its values
soil$newvalue[BDL_rows] <- soil$newvalue[BDL_rows] / 2
#unique(soil$value[!is.na(soil$value) & is.na(soil$newvalue)])

# get rid of soil texture (SOIL6)
soil <- soil[soil$varname != "SOIL6", ]

# merge (inner join) soil and plot_mng on variable code
soil <- merge(soil, plot_mng[ , c("id", "rotation", "tillage", "drainage", "nitrogen")], 
           by = "id", all.x = TRUE)

# fix variable types
soil$rotation <- as.factor(soil$rotation)
soil$tillage <- as.factor(soil$tillage)
soil$drainage <- as.factor(soil$drainage)
soil$nitrogen <- as.factor(soil$nitrogen)

# select 2 for plot rotation full names and name the object as rot
select_table()
2
rot

# merge (inner join) soil and rot on variable code
soil <- merge(soil, rot[ , c(1:2)], by.x = "rotation", by.y = "code", all.x = TRUE)

# get rid of rot
rm(rot)

# rename added column name 
names(soil)[grep("label", names(soil))] <- "rot_short_descr"

# replace value with newvalues
soil$value <- soil$newvalue
soil$newvalue <- NULL

# rearrange columns of the soil df
soil <- soil[ , c("id", 
                  "site", 
                  "plotid", 
                  "varname", 
                  "var_short_descr",
                  "year", 
                  "depth",
                  "subsample",
                  "value", 
                  "sampledate",
                  "rotation", 
                  "rot_short_descr", 
                  "tillage", 
                  "drainage", 
                  "nitrogen")]

# sort columns 
attach(soil)
soil <- soil[order(site, plotid, varname, year, depth, subsample), ]
detach(soil)

# rename rows to make its sequence match with sorted arrangement 
rownames(soil) <- as.character(1:dim(soil)[1])

# add color-code for ONFARM and NAEW sites
soil$color <- "black"
soil$color[grepl("NAEW", soil$site)] <- "lightblue"
soil$color[grepl("ONFARM", soil$site)] <- "indianred"

# PLOT DATA ---------------------------------------------------------
library(tidyverse)
library(scales)
library(lubridate)

# create directory/folder to store current boxplots
new_dir <- paste0(getwd(), "/GitHub/CSCAP/CAP_Fig/Soil/boxplots/", Sys.Date())
dir.create(new_dir, recursive = TRUE)
setwd(new_dir)

# count number of observations per site per variable
soil %>% 
  filter(!is.na(value)) %>% 
  group_by(site, varname) %>% 
  summarise(count = n(), 
            min = min(value),
            max = max(value)) -> obs 
# min and max value per variable
soil %>% 
  filter(!is.na(value)) %>%
  group_by(varname) %>%
  summarise(count = n(), 
            min = min(value),
            max = max(value)) %>%
  mutate(ylim = -(max -min)*0.07) -> ylims

# remove rear soil data from plotting
soil[!soil$varname %in% c("SOIL19.14", 
                          "SOIL19.15",
                          "SOIL19.16",
                          "SOIL19.19",
                          "SOIL19.20",
                          "SOIL19.22",
                          "SOIL19.23",
                          "SOIL19.24",
                          "SOIL19.25",
                          "SOIL19.26",
                          "SOIL19.27",
                          "SOIL19.28",
                          "SOIL19.29",
                          "SOIL19.30"), ] %>% 
  droplevels() -> soil


#for (i in 1:2) {
for (i in 1:nlevels(soil$varname)) {  
  varname <- (levels(soil$varname)[i])
  myplot <- ggplot(data = soil[soil$varname == varname, ], aes(x = site, y = value, colour = I(color))) +
    geom_boxplot(na.rm = TRUE) + 
    scale_y_continuous(limits = c(ylims$ylim[ylims$varname == varname], NA)) + 
    ggtitle(var_names$short_description[var_names$code == varname]) +
    geom_text(data = obs[obs$varname == varname,], 
              aes(y = 0, label = count, colour = I("goldenrod3")), 
              vjust = 2) +
    theme(axis.text.x = element_text(angle = 90, vjust = 1),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5))
  ggsave(myplot, filename = paste("CAP_", levels(soil$varname)[i], ".png", sep = ""), 
         path = new_dir,
         width = 12)
}    

rm(varname, i)







# PROBLEMATIC DATA (run 1 - 2016-10-26) ----

# AGR1 at ONFARM.SENECA1
ggplot(data = soil[soil$varname == "AGR1" & soil$site == "ONFARM.SENECA1", ],
       aes(x = as.factor(year), y = value)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR1"],
                     labels = comma) +
  ggtitle("ONFARM.SENECA1")
ggsave(filename = "AGR1_ONFARM.SENECA1.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)

# AGR10 at HOYTVILLE.LTR
ggplot(data = soil[soil$varname == "AGR10" & soil$site == "HOYTVILLE.LTR", ],
       aes(x = as.factor(year), y = value)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR10"],
                     labels = comma) +
  ggtitle("HOYTVILLE.LTR")
ggsave(filename = "AGR10_HOYTVILLE.LTR.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


# AGR14 at SEPAC
ggplot(data = soil[soil$varname == "AGR14" & soil$site == "SEPAC", ],
       aes(x = as.factor(year), y = value)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR14"],
                     labels = comma) +
  ggtitle("SEPAC")
ggsave(filename = "AGR14_SEPAC.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


# AGR18 at KELLOGG
ggplot(data = soil[soil$varname == "AGR18" & soil$site == "KELLOGG", ],
       aes(x = as.factor(year), y = value)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR18"],
                     labels = comma) +
  ggtitle("KELLOGG")
ggsave(filename = "AGR18_KELLOGG.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


# AGR20 at FREEMAN & KELLOGG
ggplot(data = soil[soil$varname == "AGR20" & soil$site %in% c("KELLOGG", "FREEMAN"), ],
       aes(x = as.factor(year), y = value)) +
  facet_grid(. ~ site) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR20"],
                     labels = comma)
ggsave(filename = "AGR20_FREEMAN&KELLOGG.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


# AGR24 at WOOSTER.COV
ggplot(data = soil[soil$varname == "AGR24" & soil$site == "WOOSTER.COV", ],
       aes(x = as.factor(year), y = value)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR24"],
                     labels = comma) +
  ggtitle("WOOSTER.COV")
ggsave(filename = "AGR24_WOOSTER.COV.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


# AGR25 at BRADFORD.B1
ggplot(data = soil[soil$varname == "AGR25" & soil$site == "BRADFORD.B1", ],
       aes(x = as.factor(year), y = value)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR25"],
                     labels = comma) +
  ggtitle("BRADFORD.B1")
ggsave(filename = "AGR25_BRADFORD.B1.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


# AGR26 at WOOSTER.COV
ggplot(data = soil[soil$varname == "AGR26" & soil$site == "WOOSTER.COV", ],
       aes(x = as.factor(year), y = value)) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR26"],
                     labels = comma) +
  ggtitle("WOOSTER.COV")
ggsave(filename = "AGR26_WOOSTER.COV.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


# AGR32 at STJOHNS & WOOSTER.COV
ggplot(data = soil[soil$varname == "AGR32" & soil$site %in% c("STJOHNS", "WOOSTER.COV"), ],
       aes(x = as.factor(year), y = value)) +
  facet_grid(. ~ site) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR32"],
                     labels = comma)
ggsave(filename = "AGR32_STJOHNS&WOOSTER.COV.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


# AGR32 at ONFARM.AUGLAIZE & ONFARM.SENECA1
ggplot(data = soil[soil$varname == "AGR32" & soil$site %in% c("ONFARM.AUGLAIZE", "ONFARM.SENECA1"), ],
       aes(x = as.factor(year), y = value)) +
  facet_grid(. ~ site) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR32"],
                     labels = comma)
ggsave(filename = "AGR32_ONFARM.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


