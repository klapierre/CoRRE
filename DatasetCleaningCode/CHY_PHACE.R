#######################
#### CHY_PHACE #####
#####################

setwd("~/Dropbox/CoRRE_database")
setwd('C:\\Users\\komatsuk\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database')

# library
library(readxl)
library(tidyr)

# data
dat <- read_excel("Data/OriginalData/Sites/CHY_PHACE_data.xlsx")

dat$treatment_year <- dat$YEAR - 2005
# How to add in warming treatment year?
# color?
dat$site_code <- "CHY"
dat$project_name <- "PHACE"
dat$data_type <- "biomass"

dat <- dat[-c(3,4,6,7,8,9)] # get rid of unnecessary columns
names(dat)[c(1:5)] <- c("calendar_year", "plot_id", "block", "treatment", "anpp") #rename columms

bio_dat <- dat[,-c(6:59)] #pull out composite anpp data

write.csv(bio_dat, "Data/CleanedData/Sites/ANPP csv/CHY_PHACE_anpp.csv", row.names = FALSE) #save anpp data

dat <- dat[,-5]
dat <- gather(dat, key = "genus_species", value = "abundance", 5:58) # wide to long format
dat <- dat[which(dat$abundance > 0),] # get rid of species with 0 biomass

write.csv(dat, "Data/CleanedData/Sites/Species csv/CHY_PHACE.csv", row.names = FALSE)


