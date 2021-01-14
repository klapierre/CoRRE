##################
## Sil_NASH ####
###############
setwd("/Users/kaitlinkimmel/Dropbox/CoRRE_database")

## libraries ##
library(tidyr)

# load data
dat <- read.delim("Data/OriginalData/2020 update/Data/Sil_NASH_data.txt")
# N.B. collected both biomass and cover data, depended on year

# wide to long format
dat <- gather(dat, key = "genus_species", value = "abundance", 10:125)

# get rid of species with 0 abundance
dat <- dat[which(dat$abundance > 0), ]

#change . to space in species names
dat$genus_species <- gsub("\\.", " ", dat$genus_species)

# add other necessary columns
dat$treatment_year <- dat$year - 1991
dat$site_code <- "Sil"
dat$project_name <- "NASH"

# need to make a unique plot_id column
dat$trt_temp <- paste(dat$plot, dat$insecticide, dat$molluscicide, dat$fencing, dat$lime, dat$herbicide, dat$nutrient, sep = ".")
plot_id <- data.frame("trt_temp" = unique(dat$trt_temp))
plot_id$plot_id <- seq(1:nrow(plot_id))
dat <- merge(dat, plot_id, by = "trt_temp")

# make treatment column
dat$treatment <- paste(dat$insecticide, dat$molluscicide, dat$fencing, dat$lime, dat$herbicide, dat$nutrient, sep = ".")
dat$trt_temp <- NULL
dat[,c(4:9)] <- NULL

# fix names

names(dat)[c(1,2,3)] <- c("calendar_year", "data_type", "block")

# save abundance data
write.csv(dat, "Data/CleanedData/Sites/Species csv/Sil_NASH.csv", row.names = FALSE)


# pull out ANPP data
### NB. 25 x 50 cm quadrats cut. Need to standardize to per m2
bio_dat <- dat[which(dat$data_type == "biomass"),] # collectected in 1992, 1993, 1994, 1995, 1996, 1997, 1999, 2000, 2013

bio_dat <- aggregate(bio_dat$abundance,by = list(calendar_year = bio_dat$calendar_year, block = bio_dat$block, 
                                    treatment_year = bio_dat$treatment_year, data_type = bio_dat$data_type,
                                    site_code = bio_dat$site_code, project_name = bio_dat$project_name, 
                                    plot_id = bio_dat$plot_id, treatment = bio_dat$treatment), FUN = sum)
names(bio_dat)[9] <- "anpp"

write.csv(bio_dat, "Data/CleanedData/Sites/ANPP csv/Sil_NASH_anpp.csv", row.names = FALSE)