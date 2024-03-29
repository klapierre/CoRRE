setwd("~/Dropbox/CoRRE_database")

setwd('C:\\Users\\kjkomatsu\\Dropbox (Smithsonian)\\working groups\\CoRRE\\CoRRE_database') #kim's laptop



library(tidyverse)

dat<-read.csv("Data/OriginalData/Sites/Edge/USA_EDGE.csv")
sev<-read.csv('Data/OriginalData/Sites/Edge/sev_fixed.csv')

splist<-dat%>%
  select(species)%>%
  unique()

write.csv(splist, "Data/OriginalData/Sites/Edge/USA_species.csv", row.names=F)
clean<-read.csv('Data/OriginalData/Sites/Edge/USA_species_filled.csv')

dat2<-dat%>%
  left_join(clean)%>%
  select(-species)%>%
  bind_rows(sev)

numreps<-dat2%>%
  select(site, plot, trt)%>%
  unique()

ave<-dat2%>%
  group_by(site, plot, trt, community_type, genus_species, year)%>%
  summarize(cover=mean(cover))

cleaned<-ave%>%
  rename(calendar_year=year,
         site_code=site,
         plot_id=plot,
         treatment=trt,
         abundance=cover)%>%
  mutate(project_name="EDGE",
         block=0)

trt_year_knz<-cleaned%>%
  filter(site_code=="KNZ")%>%
  ungroup()%>%
  select(calendar_year, site_code)%>%
  unique()%>%
  mutate(treatment_year=seq(0,4, by=1))

trt_year_sev<-cleaned%>%
  filter(site_code=="SEV")%>%
  ungroup()%>%
  select(calendar_year, site_code)%>%
  unique()%>%
  arrange(calendar_year)%>%
  mutate(treatment_year=seq(0,5, by=1))

trt_year_rest<-cleaned%>%
  filter(site_code!="SEV"&site_code!="KNZ")%>%
  ungroup()%>%
  select(calendar_year, site_code)%>%
  unique()%>%
  arrange(calendar_year)%>%
  mutate(treatment_year=ifelse(calendar_year==2012|calendar_year==2013, 0, ifelse(calendar_year==2014, 1, ifelse(calendar_year==2015, 2, ifelse(calendar_year==2016, 3, 4)))))

trt_year<-trt_year_knz%>%
  bind_rows(trt_year_sev, trt_year_rest)

cleaned2<-cleaned%>%
  left_join(trt_year)

test<-cleaned2%>%
  ungroup()%>%
  select(site_code, treatment_year, calendar_year)%>%
  unique()

write.csv(cleaned2, "CleanedData/Sites/USA_EDGE.csv", row.names = F)