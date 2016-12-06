# LOAD DATA ---------------------------------------------------------
# get table from database
library(ggplot2)
source("~/GitHub/R/My_Source_Codes/CSCAPpostgerDBconnect.R") 

# select 6 for agronomic data and name the object as agro
wait(1)
6
agro

# select 1 for plot mngt and other data and name the object as plot_mng
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

# select 2 for plot rotation full names and name the object as rot
select_table()
wait(1)
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

# PLOT DATA ---------------------------------------------------------
library(tidyverse)
library(scales)
library(lubridate)

# create directory/folder to store current boxplots
new_dir <- paste0(getwd(), "/GitHub/CSCAP/CAP_Fig/Agro/boxplots/", Sys.Date())
dir.create(new_dir)

# count number of observations per site per variable
agro %>% 
  filter(!is.na(value)) %>% 
  group_by(site, varname) %>% 
  summarise(count = n(), 
            min = min(value),
            max = max(value)) -> obs 
# min and max value per variable
agro %>% 
  filter(!is.na(value)) %>%
  group_by(varname) %>%
  summarise(count = n(), 
            min = min(value),
            max = max(value)) %>%
  mutate(ylim = -(max -min)*0.07) -> ylims

for (i in 1:nlevels(agro$varname)) {
  varname <- (levels(agro$varname)[i])
  myplot <- ggplot(data = agro[agro$varname == varname, ], aes(x = site, y = value, colour = I(color))) +
    geom_boxplot(na.rm = TRUE) + 
    scale_y_continuous(limits = c(ylims$ylim[ylims$varname == varname], NA)) + 
    ggtitle(var_names$short_description[var_names$code == varname]) +
    theme(axis.text.x = element_text(angle = 90),
          axis.title.y = element_blank()) +
    geom_text(data = obs[obs$varname == varname,], 
              aes(y = 0, label = count, colour = I("goldenrod3")), 
              vjust = 2)
  ggsave(myplot, filename = paste("CAP_", levels(agro$varname)[i], ".png", sep = ""), 
         path = new_dir,
         width = 12)
}
rm(varname, i)


# PROBLEMATIC DATA (run 1 - 2016-10-26) ----

# AGR1 at ONFARM.SENECA1
ggplot(data = agro[agro$varname == "AGR1" & agro$site == "ONFARM.SENECA1", ],
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
ggplot(data = agro[agro$varname == "AGR10" & agro$site == "HOYTVILLE.LTR", ],
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
ggplot(data = agro[agro$varname == "AGR14" & agro$site == "SEPAC", ],
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
ggplot(data = agro[agro$varname == "AGR18" & agro$site == "KELLOGG", ],
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
ggplot(data = agro[agro$varname == "AGR20" & agro$site %in% c("KELLOGG", "FREEMAN"), ],
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
ggplot(data = agro[agro$varname == "AGR24" & agro$site == "WOOSTER.COV", ],
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
ggplot(data = agro[agro$varname == "AGR25" & agro$site == "BRADFORD.B1", ],
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
ggplot(data = agro[agro$varname == "AGR26" & agro$site == "WOOSTER.COV", ],
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
ggplot(data = agro[agro$varname == "AGR32" & agro$site %in% c("STJOHNS", "WOOSTER.COV"), ],
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
ggplot(data = agro[agro$varname == "AGR32" & agro$site %in% c("ONFARM.AUGLAIZE", "ONFARM.SENECA1"), ],
       aes(x = as.factor(year), y = value)) +
  facet_grid(. ~ site) +
  geom_boxplot(na.rm = TRUE) +
  scale_x_discrete(name = "Year") +
  scale_y_continuous(name = var_names$short_description[var_names$code == "AGR32"],
                     labels = comma)
ggsave(filename = "AGR32_ONFARM.png", 
       path = paste0(new_dir, "/issues"),
       width = 12)


