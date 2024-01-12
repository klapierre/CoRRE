##################
#### KNZ_BGP ####
################
#setwd("~/Dropbox/CoRRE_database")

library(tidyverse)

####notes: We stopped mowing after 2003 because most of the plots that were mowed were heavily invaded by old worlds bluestem. If you are wanting to use data from across the entire 1989-2019 period, I would drop all of the mowed plots. Even before we stopped mowing, they were impacted by invasion to various degrees and ANPP sampling would have had to have been adjusted to account for that. I’m sure some of the species composition plots were impacted by invasion. 

#The nutrient treatments continued uninterrupted through 2016. Fertilization ceased in 2017 

#We are going only keep through 2016 the last year of fertilization and dropping all the mowing treatment

# biomass data
file <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.57.10&entityid=eaf1b05d4c7578a1fe1efc5173b80953"
df <- read.csv(file, header = TRUE, na.strings = c(NA, ""))


df$CUYRDD[df$CUYRDD == "."] <- 0
df$CUYRDD[which(is.na(df$CUYRDD))] <- 0
df$CUYRDD <- as.numeric(df$CUYRDD)

# change n+p to b to fit metadata
for (i in 1:nrow(df)){
  if(df$NUTRIENT[i] == "n+p"){
    df$NUTRIENT[i] = "b"
  }
}

# combine treatment data into single column
df$treatment <- paste(df$BURN, df$MOW, df$NUTRIENT, sep = "_")

df <- df[df$MOW == "u",]

df <- df[-which(is.na(df$FORBS) & is.na(df$LVGRASS)),]

df$anpp <- df$LVGRASS + df$FORBS + df$CUYRDD + df$WOODY

df <- aggregate(df$anpp, by = list(calendar_year = df$RECYEAR, plot_id = df$PLOT, 
                                     treatment = df$treatment), FUN = mean) #average across the two replicate clip strips
df$site_code <- "KNZ"
df$project_name <- "BGP"
df$data_type <- "anpp"
df$treatment_year <- df$calendar_year - 1985
names(df)[4] <- "anpp"
df$anpp <- df$anpp*10 # mulitply by 10 to get g/m2, currently g/0.1 m2

bgp.anpp <- df[,c(1,3,2,7,8,5,6,4)]

bgp.anpp2<-bgp.anpp %>% 
  filter(calendar_year<2017) 

#write.csv(df, "C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\CleanedData\\Sites\\ANPP csv\\KNZ_BGP_anpp.csv", row.names = FALSE)


file1 <- "https://pasta.lternet.edu/package/data/eml/knb-lter-knz/17/11/410e032a0651ce990c8c497be62c68f7"
df1 <- read.csv(file1, header = TRUE)
file2 <- "https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-knz.134.3&entityid=4abeadee64638e46bd5088f2fa7d832e"
spdf <- read.csv(file2, header = TRUE)
trtdf <- read.csv("C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\OriginalData\\Sites\\KNZ_BGP\\BGP_treatments.csv")
colnames(trtdf)[1] <- "plot_id"

## Convert cover class to mid-points
# 1 = 0.5, 2 = 3.5, 3 = 15, 4 = 37.5, 5=62.5, 6 = 85, 7 = 97.5

# Taking midpoints of cover classes 
df1$CoverClass[df1$CoverClass == 1] <- 0.5
df1$CoverClass[df1$CoverClass == 2] <- 3.5
df1$CoverClass[df1$CoverClass == 3] <- 15
df1$CoverClass[df1$CoverClass == 4] <- 37.5
df1$CoverClass[df1$CoverClass == 5] <- 62.5
df1$CoverClass[df1$CoverClass == 6] <- 85
df1$CoverClass[df1$CoverClass == 7] <- 97.5

 
df1 <- aggregate(df1$CoverClass, 
                  by = list(calendar_year = df1$RecYear, gen = df1$Ab_genus,
                            spec = df1$Ab_species, plot_id = df1$Plot), FUN = mean)

names(df1)[5] <- "abundance"
df1$site_code <- "KNZ"
df1$project_name <- "BGP"
df1$treatment_year <- df1$calendar_year - 1985

df1 <- merge(df1, spdf, all.x = TRUE)
df1$genus_species <- paste(df1$genus, df1$species, sep = " ")
df1$data_type <- "cover"

# need to add treatments from biomass data to cover data
df2 <- merge(df1, trtdf, by = "plot_id", all.x = TRUE)
df2 <- df2[df2$mowed == "u",]
df2$treatment <- paste(df2$burned, df2$mowed, df2$nutrient, sep = "_")

bgp.species <- df2[,c(1,4:8,19,23)]

bgp.species2<-bgp.species %>% 
  filter(calendar_year<2017) %>% 
  mutate(drop=ifelse(treatment %in% c('u_m_n', 'u_m_p', 'u_m_b', 'u_m_c','b_m_n', 'b_m_p', 'b_m_b', 'b_m_c'), 1, 0)) %>% 
  filter(drop!=1) %>% 
  select(-drop)

#write.csv(bgp.species, "C:\\Users\\mavolio2\\Dropbox\\CoRRE_database\\Data\\CleanedData\\Sites\\Species csv\\KNZ_BGP.csv", row.names = FALSE)

