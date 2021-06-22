#################################
## 00- Data Cleaning and Loading 
## Kristin Andrejko
## 11-30-20
#################################

# 1. Loading and Cleaning Data ------------------------------------------------------------

library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stats)
library(countrycode)
library(metafor)
library(meta)
library(ggplot2)
library(reshape)
library(ggpubr)
library(lme4)
library(gee) 
library(MASS)

setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe")
addt_93 <- read.csv("data/data_addt_93_final.csv") #Last pull of data from the 2007-2009 papers and 2020 update completed 12-2-20, note I manually removed Okade data from excel

#pcv_intro <- read_excel("data/PCV Vaccine Intro.xlsx") #found tiny errors on intro date, update to new sheet
pcv_intro <- read_excel("data/PCV Vaccine Intro Updated Nov2020.xlsx") #found tiny errors on intro date, update to new sheet
income_dta <- read_excel("data/income_status_worldbank.xlsx")
wide_dta <- read.csv("data/wide_data_121719.csv") #THIS IS ORIGINAL WIDE DATA SET- CLEANED 
gbd_region_new <-  read_excel("data/gbd_region_new_r.xlsx")
world_bank_gdp_new <- read_excel("data/world_bank_gdp_new2.xlsx")
extra_data <- read.csv("data/extra_data_011719.csv") #New pre 2007 data that Joe had us abstract over winter break 
extra_data2 <- read.csv("data/extra_kristin_data_012620.csv") #Extra abstracted data from re-review of titles that should've been included in review
extra_data3 <- read.csv("data/extra_data_2019.csv") #Buddhika data from 2019 studies (last search)

#0. Merge wide with extra_data 
all_data <- rbind(wide_dta, extra_data) 
all_data2 <- rbind(all_data, extra_data2)
all_data3 <- rbind(all_data2, extra_data3)

# addt_94 <- addt_94 %>% dplyr::select(-serotype.data.)
# all_data3 <- rbind(all_data3, addt_94)

#levels(factor(extra_data3$country[!(extra_data3$country %in% all_data2$country)]))   #this was used when merging names
levels(factor(all_data3$country[!(all_data3$country %in% gbd_region_new$country)]))   #this was used when merging names

#What new countries got added in last round of data abstraction?
levels(factor(addt_93$country[!(addt_93$country %in% all_data3$country)]))  


#Finding Unique ID's for Serotype Data 
# st_doi <- read.csv("fulldta_st_b_new.csv")
# fulldta_doi <- read_excel("AMr_DOI_CompleteList.xlsx")
# levels(factor(st_doi$doi[!(st_doi$doi %in% fulldta_doi$doi)]))   
# levels(factor(st_doi$studyID[!(st_doi$studyID %in% fulldta$studyID)]))   
# 
# length(unique(st_doi$doi)) #135 Serotype Studies 
# length(unique(st_doi$studyID)) #135 Serotype Studies 

#1. Merge Data 
newmerge <- merge(gbd_region_new, pcv_intro, by = "country", all = T) #replace gbd_region_new with who_region
newmerge <- merge(newmerge, income_dta, by = "country", all = T)

#2. Merge with wide_dta 
fulldta <- left_join(all_data3, newmerge, by = "country") #added all_data instead of wide_dta

length(unique(all_data3$country))
length(unique(fulldta$country))

#3. Merge with GDP per capital - haven't done this yet! 

#3.1 Items which are in fulldta which are not in world_bank_gdp_new 
levels(factor(fulldta$country[!(fulldta$country %in% gbd_region_new$country)]))  
levels(factor(fulldta$country[!(fulldta$country %in% pcv_intro$country)]))  
levels(factor(fulldta$country[!(fulldta$country %in% income_dta$country)]))  

#4. melt world_bank_gdp (need to convert world_bank_gdp into data frame for melt to work)

world_bank_gdp_new_v2 <- as.data.frame(world_bank_gdp_new)
world_bank_gdp_new_v2$'2020' <- world_bank_gdp_new_v2$"2019" #add 2020 data that is the same as 2019

world_bank_gdp_melt <- melt(data = world_bank_gdp_new_v2, 
                            id.vars= "country",
                            measure.vars= c("1973", "1974", "1974", "1978", "1979", "1980", "1981", "1982", 
                                            "1983", "1984", "1985", "1986", "1987", "1989", "1990", "1991", 
                                            "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", 
                                            "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", 
                                            "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", 
                                            "2016", "2017", "2018", "2019", "2020"),
                            variable.name = "midpoint_yr", 
                            value.name = "gdp")


colnames(world_bank_gdp_melt)[2] <- "midpoint_yr"
colnames(world_bank_gdp_melt)[3] <- "gdp"

#4.1. convert world_bank_gdp_melt from factor to numeric 
world_bank_gdp_melt$midpoint_yr <- as.numeric(levels(world_bank_gdp_melt$midpoint_yr))[world_bank_gdp_melt$midpoint_yr]

#5 Create midpoint_yr value 
fulldta <- fulldta %>%
  mutate(midpoint_yr = ((sample_collection_endyear - sample_collection_startyear) / 2) + sample_collection_startyear)

#5.1 Round midpoint_year up
fulldta$midpoint_yr <- round(fulldta$midpoint_yr, digits = 0) 

#6. Merge GDP with data set- a lot of these are NA's becuase midpoint year is not a whole number
fulldta <- left_join(fulldta, world_bank_gdp_melt, by = c("country", "midpoint_yr"))

#Check where there are not values of GDP and manually replace 
na_df <- fulldta[is.na(fulldta$gdp),]
View(na_df) 

fulldta[5061, "gdp"] <- 2068.51039847413 #Thailand 
fulldta[5263, "gdp"] <- 16837 #New Caledonia 2002 from http://data.un.org/Data.aspx?q=New+Caledonia&d=SNAAMA&f=grID%3A101%3BcurrID%3AUSD%3BpcFlag%3A1%3BcrID%3A540

#7. Add yr since vax  (used start year of sample collection rather than midpoint yr bc otherwise 90 studies had pre = 0 and yr since vax >0)
fulldta <- fulldta %>%
  mutate(yr_since_vax = case_when(as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) > 0 ~ sample_collection_startyear -  as.numeric(pcv_intro_year), 
                                  as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) == 0 ~ 0, 
                                  as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) < 0 ~ 0, 
                                  pcv_intro_year == "NI" ~ 0)) #pre

length(which(is.na(fulldta$yr_since_vax))) 

check <- fulldta %>% 
  dplyr::select(pcv_intro_year, sample_collection_endyear, sample_collection_startyear, yr_since_vax)
View(check)

#8. Add prepost variable
fulldta <- fulldta %>% 
  mutate(prepost = case_when(as.numeric(pcv_intro_year) == sample_collection_startyear ~ "no_analysis", 
                             as.numeric(pcv_intro_year) == sample_collection_endyear ~ "no_analysis", 
                             sample_collection_startyear - as.numeric(pcv_intro_year) == 1 ~ "no_analysis", 
                             as.numeric(pcv_intro_year) > sample_collection_endyear ~ "naive",
                             sample_collection_startyear - as.numeric(pcv_intro_year) >= 3 ~ "not_naive", #changed from 5
                             #sample_collection_startyear - as.numeric(pcv_intro_year) == 4 ~ "no_analysis", 
                             #sample_collection_startyear - as.numeric(pcv_intro_year) == 3 ~ "no_analysis", 
                             sample_collection_startyear - as.numeric(pcv_intro_year) == 2 ~ "no_analysis", 
                             pcv_intro_year == "NI" ~ "naive")) 

fulldta$prepost[is.na(fulldta$prepost)] <- "no_analysis"
length(which(is.na(fulldta$prepost))) 


#8.1 Add prepost variable for meta-regression analysis 
fulldta <- fulldta %>% 
  mutate(prepost_meta = case_when(yr_since_vax >= 1 ~ "post", 
                                  yr_since_vax == 0 ~ "pre"))

# check <- fulldta %>% 
#   dplyr::select(prepost_meta, yr_since_vax)
# View(check)

#9. Recode study ID to r_id
studyid_df <- fulldta %>% 
  group_by(studyID) %>% 
  summarize(var = mean(gdp)) 

studyid_df <- data.frame(studyid_df)

studyid_df <- studyid_df %>% 
  mutate(r_id = row_number())

length(unique(studyid_df$studyID))
length(unique(studyid_df$r_id)) #392

studyid_df <- studyid_df[,-c(2)]

fulldta <- merge(studyid_df, fulldta, by = "studyID")
nrow(fulldta)
names(fulldta)
#View(fulldta)

#10. Deal with CLSI standards and NCCLS ###

#1. Add CLSI As the method for ALL of the ATLAS data sets (row id 9999)

#2. Edit the susceptibility testing method of Stovall doi 10.1097/00006454-200110000-00007 becuase left blank by accident
# From paper: Susceptibility testing was performed by microbroth dilution with Mueller-Hinton media supplemented with 3% lysed horse blood.
#Susceptibility was defined according to the 2000 National Committee for Clinical Laboratory Standards guidelines
#studyid 2876 | r_id 377 | rowID 267, set Criteria = CLSI (1)

fulldta[fulldta$studyID == 2876 & fulldta$r_id== 377 & fulldta$rowID == 267, "criteria"] <- 1 #We forgot to code criteria for this study- adding CLSI (1)
fulldta[fulldta$studyID == 2876 & fulldta$r_id== 377 & fulldta$rowID == 267, "method_1"] <- 3 #We forgot to code method for this study- adding microbroth dilution (3)

#recode all of ATLAS to be classified as CLSI data 
fulldta[fulldta$studyID == 9999, "criteria"] <- 1 #We forgot to code that all ATLAS data is CLSI 

clsi_list <- c("National Committee for Clinical Laboratory Standards", 
               "National Committee for Clinical Laboratory Standards " ,
               "NCCLS", 
               "CLSI were used for sus, int, res; EUCAST was used for benzylpenicillin")

for(i in 1:nrow(fulldta)){
  fulldta$criteria_cl[i] <- ifelse(fulldta$criteria_specify[i] %in%  clsi_list, 1, fulldta$criteria[i])
}

clsi_nccls <- fulldta %>% 
  dplyr::group_by(criteria_cl, criteria_specify) %>% 
  dplyr::summarize(freq = n())
clsi_nccls

clsi_noinclude <- fulldta %>% 
  dplyr::filter(criteria_cl == 4)
#View(clsi_noinclude)

clsi_noinclud_nodup <- clsi_noinclude %>% 
  distinct(r_id, arm_id, .keep_all = TRUE)
#write.csv(clsi_noinclud_nodup, file = "clsi_noinclud_nodup.csv")

#Recode criteria_cl for values ofr criteria that previously were set to 4 to criteria = 3 or CLSI based off of re-reviewing papers listed as criteria = 4
# file with decisions and reasoning is "clsi_noinclud_nodup.csv" from 03-10-20
fulldta[fulldta$studyID == 508 & fulldta$r_id== 55 & is.na(fulldta$rowID), "criteria_cl"] <- 1 #CLSI
fulldta[fulldta$studyID == 717 & fulldta$r_id== 82 & is.na(fulldta$rowID), "criteria_cl"] <- 3 #Other 
fulldta[fulldta$studyID == 783 & fulldta$r_id== 96 & is.na(fulldta$rowID), "criteria_cl"] <- 3 
fulldta[fulldta$studyID == 1752 & fulldta$r_id== 244 & fulldta$rowID == 510, "criteria_cl"] <- 3 

fulldta[fulldta$studyID == 1881, "criteria_cl"] <- 1 
fulldta[fulldta$studyID == 1954 & fulldta$r_id== 274 & fulldta$rowID == 648, "criteria_cl"] <- 1 

fulldta[fulldta$studyID == 1978 & fulldta$r_id== 276 & fulldta$rowID == 652, "criteria_cl"] <- 3 
fulldta[fulldta$studyID == 1997 & fulldta$r_id== 279 & fulldta$rowID == 660, "criteria_cl"] <- 3 
fulldta[fulldta$studyID == 1999, "criteria_cl"] <- 3 
fulldta[fulldta$studyID == 2009 & fulldta$r_id== 283 & fulldta$rowID == 667, "criteria_cl"] <- 3 

fulldta[fulldta$studyID == 2119, "criteria_cl"] <- 3  
fulldta[fulldta$studyID == 2362 & fulldta$r_id== 317 & fulldta$rowID == 771, "criteria_cl"] <- 3 
fulldta[fulldta$studyID == 486 , "criteria_cl"] <- 1 #For studyID486, change from criteria_cl = 4 to criteria_cl = 1 (CSLI guidelines instead of "CLSI" guidelines reported in paper)

#Test if recoding worked- there should be 13 criteria_cl = 4 
test <- fulldta %>% 
  dplyr::filter(criteria_cl == 4)
nrow(test)

#11. Other data changes 

#These are the checks I used while creating variables 
# length(which(fulldta$studyID == 533 & fulldta$r_id==61 & fulldta$rowID == 701)) 
# subset(fulldta, studyID==533 & r_id==61 & rowID == 701, select=c("no_sus"))#Check that there is only 1 value for this condition 

#test <- subset(fulldta, studyID==422) 
#View(test)

#This is the DataCleaning_020520 from https://docs.google.com/spreadsheets/d/1oLpd74abGxVZNvTBcBOYG0QayP4n5yepVQtr3JtR1Aw/edit#gid=0
fulldta[fulldta$studyID == 422 & fulldta$r_id==38 & fulldta$rowID == 113, "no_ns"] <- 80 #replace value given conditional statements
fulldta[fulldta$studyID == 533 & fulldta$r_id==61 & fulldta$rowID == 701, "no_sus"] <- 42 #
fulldta[fulldta$studyID == 717 & fulldta$r_id==82 & fulldta$drug_class == "penicillin", "no_ns"] <- 54 
fulldta[fulldta$studyID == 847 & fulldta$r_id==108 & fulldta$rowID == 792, "no_sus"] <- 24 #
fulldta[fulldta$studyID == 847 & fulldta$r_id==108 & fulldta$rowID == 791, "no_ns"] <- 1 
fulldta[fulldta$studyID == 847 & fulldta$r_id==108 & fulldta$rowID == 791, "no_sus"] <- 11 
fulldta[fulldta$studyID == 953 & fulldta$r_id==119 & fulldta$rowID == 819, "no_res"] <- 19 
fulldta[fulldta$studyID == 1160 & fulldta$r_id==154 & fulldta$rowID == 1423, "no_ns"] <- 0 
fulldta[fulldta$studyID == 1160 & fulldta$r_id==154 & fulldta$rowID == 1423, "no_sus"] <- 96 
fulldta[fulldta$studyID == 1160 & fulldta$r_id==154 & fulldta$rowID == 1419, "no_ns"] <- 28 
fulldta[fulldta$studyID == 1190 & fulldta$r_id==158 & fulldta$rowID == 1380, "no_ns"] <- 2 
fulldta[fulldta$studyID == 1190 & fulldta$r_id==158 & fulldta$rowID == 1380, "no_sus"] <- 133 
fulldta[fulldta$studyID == 1190 & fulldta$r_id==158 & fulldta$rowID == 1378, "no_ns"] <- 6 
fulldta[fulldta$studyID == 1190 & fulldta$r_id==158 & fulldta$rowID == 1378, "no_sus"] <- 129 
fulldta[fulldta$studyID == 1369 & fulldta$r_id==185 & fulldta$rowID == 1074, "total_isolates_drug"] <- 101 
fulldta[fulldta$studyID == 1369 & fulldta$r_id==185 & fulldta$rowID == 1074, "no_ns"] <- 25 
fulldta[fulldta$studyID == 1369 & fulldta$r_id==185 & fulldta$rowID == 1074, "no_res"] <- 23 
fulldta[fulldta$studyID == 1369 & fulldta$r_id==185 & fulldta$rowID == 1072, "total_isolates_drug"] <- 101 
fulldta[fulldta$studyID == 1369 & fulldta$r_id==185 & fulldta$rowID == 1072, "no_ns"] <- 34 
fulldta[fulldta$studyID == 1413 & fulldta$r_id==192 & fulldta$rowID == 1090, "no_ns"] <- 9 
fulldta[fulldta$studyID == 1413 & fulldta$r_id==192 & fulldta$rowID == 1090, "no_res"] <- 2 
fulldta[fulldta$studyID == 1413 & fulldta$r_id==192 & fulldta$rowID == 1090, "no_sus"] <- 7 
fulldta[fulldta$studyID == 1440 & fulldta$r_id==195 & fulldta$rowID == 1109, "no_ns"] <- 57 
fulldta[fulldta$studyID == 1440 & fulldta$r_id==195 & fulldta$rowID == 1109, "no_sus"] <- 6 
fulldta[fulldta$studyID == 1737 & fulldta$r_id==243 & fulldta$rowID == 509, "total_isolates_drug"] <- 67 
fulldta[fulldta$studyID == 1737 & fulldta$r_id==243 & fulldta$rowID == 509, "no_ns"] <- 47 
fulldta[fulldta$studyID == 1737 & fulldta$r_id==243 & fulldta$rowID == 509, "no_int"] <- 47 
fulldta[fulldta$studyID == 1737 & fulldta$r_id==243 & fulldta$rowID == 509, "no_sus"] <- 20 
fulldta[fulldta$studyID == 1769 & fulldta$r_id==246 & fulldta$rowID == 516, "no_int"] <- 12 
fulldta[fulldta$studyID == 1822 & fulldta$r_id==254 & fulldta$rowID == 529, "no_ns"] <- 28 
fulldta[fulldta$studyID == 1822 & fulldta$r_id==254 & fulldta$rowID == 529, "no_int"] <- 28 
fulldta[fulldta$studyID == 1822 & fulldta$r_id==254 & fulldta$rowID == 529, "no_sus"] <- 170 
fulldta[fulldta$studyID == 1846 & fulldta$r_id==258 & fulldta$rowID == 543, "no_ns"] <- 398 
fulldta[fulldta$studyID == 2194 & fulldta$r_id==302 & fulldta$rowID == 726, "no_ns"] <- 665 
fulldta[fulldta$studyID == 2413 & fulldta$r_id==324 & fulldta$rowID == 479, "no_ns"] <- 20 
fulldta[fulldta$studyID == 2682 & fulldta$r_id==354 & fulldta$rowID == 344, "no_ns"] <- 2 
fulldta[fulldta$studyID == 2960 & fulldta$r_id==383 & fulldta$rowID == 229, "no_sus"] <- 78 
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 182, "no_sus"] <- 85 
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 166, "no_res"] <- 0 
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 150, "no_ns"] <- 20 
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 150, "no_res"] <- 8 
fulldta[fulldta$studyID == 3127 & fulldta$r_id==409 & fulldta$rowID == 42, "total_isolates_drug"] <- 16 
fulldta[fulldta$studyID == 3127 & fulldta$r_id==409 & fulldta$rowID == 42, "no_ns"] <- 6 
fulldta[fulldta$studyID == 3127 & fulldta$r_id==409 & fulldta$rowID == 42, "no_res"] <- 6 
fulldta[fulldta$studyID == 3127 & fulldta$r_id==409 & fulldta$rowID == 42, "no_int"] <- 0
fulldta[fulldta$studyID == 3127 & fulldta$r_id==409 & fulldta$rowID == 42, "no_sus"] <- 11
fulldta[fulldta$studyID == 3153 & fulldta$r_id==414 & fulldta$rowID == 23, "no_int"] <- 24
fulldta[fulldta$studyID == 3341 & fulldta$r_id==435 & fulldta$drug_class == "chloramphenicol", "no_sus"] <- 490 
fulldta[fulldta$studyID == 3341 & fulldta$r_id==435 & fulldta$drug_class == "levofloxacin", "no_sus"] <- 445 
fulldta[fulldta$studyID == 3341 & fulldta$r_id==435 & fulldta$drug == "Penicillin", "no_ns"] <- 19 
fulldta[fulldta$studyID == 3341 & fulldta$r_id==435 & fulldta$drug == "Penicillin", "no_sus"] <- 49 
fulldta[fulldta$studyID == 3341 & fulldta$r_id==435 & fulldta$drug == "Amoxicillin", "no_ns"] <- 24 
fulldta[fulldta$studyID == 3341 & fulldta$r_id==435 & fulldta$drug == "Penicillin", "no_sus"] <- 45 


fulldta[fulldta$studyID == 792 & fulldta$r_id==99 & fulldta$rowID == 266, "total_isolates_drug"] <- NA  
fulldta[fulldta$studyID == 792 & fulldta$r_id==99 & fulldta$rowID == 266, "no_ns"] <- NA  
fulldta[fulldta$studyID == 792 & fulldta$r_id==99 & fulldta$rowID == 266, "no_res"] <- NA  
fulldta[fulldta$studyID == 792 & fulldta$r_id==99 & fulldta$rowID == 266, "no_int"] <- NA  


fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1349, "total_isolates_drug"] <- NA  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1349, "no_ns"] <- NA  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1349, "no_int"] <- NA  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1349, "no_sus"] <- NA  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1349, "no_res"] <- NA  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1350, "total_isolates_drug"] <- 288  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1350, "no_ns"] <- 132  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1350, "no_int"] <- 130  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1350, "no_sus"] <- 156  
fulldta[fulldta$studyID == 1135 & fulldta$r_id==151 & fulldta$rowID == 1350, "no_res"] <- 2  
fulldta[fulldta$studyID == 1786 & fulldta$r_id==249 & fulldta$rowID == 520, "no_sus"] <- 547  
fulldta[fulldta$studyID == 1441 & fulldta$r_id==196 & fulldta$rowID == 1565, "no_ns"] <- 606  
fulldta[fulldta$studyID == 1441 & fulldta$r_id==196 & fulldta$rowID == 1565, "no_res"] <- 90  


#Replace study 385 arm_id with a K 
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 191, "arm_id"] <- "K"   
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 192, "arm_id"] <- "K"   
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 193, "arm_id"] <- "K"   
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 194, "arm_id"] <- "K"   
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 195, "arm_id"] <- "K"   
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 196, "arm_id"] <- "K"   
fulldta[fulldta$studyID == 2968 & fulldta$r_id==385 & fulldta$rowID == 197, "arm_id"] <- "K"   


#12. Remove rows where studyID is 1411 because on 19A isolates

#test <- subset(fulldta, studyID==1411) #This is code to find what rows of data need to be deleeted
#View(test)

fulldta <- fulldta[-c(1333, 1334, 1335, 1336), ]


#13. Make data changes from Buddhika on 2/14/20

# studyID 1023 - DELETE whole study (only serotype 5 isolates)
#test <- subset(fulldta, studyID==1023) #This is code to find what rows of data need to be deleeted
#View(test) #row 907
fulldta <- fulldta[-c(907), ]
length(which(fulldta$studyID==1023))

# studyID 867 - DELETE whole study (symptomatic nasopharyngeal isolates)
#test <- subset(fulldta, studyID==867) #This is code to find what rows of data need to be deleeted
#View(test) #row 907
fulldta <- fulldta[-c(780, 781, 782, 783, 784, 785), ]
length(which(fulldta$studyID==867))

# studyID 546 - CHANGE sample_end_collection date to January 2003
#test <- subset(fulldta, studyID==546) #This is code to find what rows of data need to be deleeted
#View(test)
fulldta[fulldta$studyID == 546, "sample_collection_endyear"] <- 2003   
fulldta[fulldta$studyID == 546, "sample_collection_endmo"] <- 1   

# studyID 1047 - CHANGE Pen no_res -> no_ns
test <- subset(fulldta, studyID == 1047)
#View(test)

fulldta[fulldta$studyID == 1047 & fulldta$rowID == 856, "no_res"] <- NA 
fulldta[fulldta$studyID == 1047 & fulldta$rowID == 856, "no_ns"] <- 27   

fulldta[fulldta$studyID == 1047 & fulldta$rowID == 859, "no_res"] <- NA   
fulldta[fulldta$studyID == 1047 & fulldta$rowID == 859, "no_ns"] <- 22   

fulldta[fulldta$studyID == 1047 & fulldta$rowID == 858, "no_res"] <- NA   
fulldta[fulldta$studyID == 1047 & fulldta$rowID == 858, "no_ns"] <- 27   

fulldta[fulldta$studyID == 1047 & fulldta$rowID == 860, "no_res"] <- NA   
fulldta[fulldta$studyID == 1047 & fulldta$rowID == 860, "no_ns"] <- 26   

fulldta[fulldta$studyID == 1047 & fulldta$rowID == 857, "no_res"] <- NA    
fulldta[fulldta$studyID == 1047 & fulldta$rowID == 857, "no_ns"] <- 22   

fulldta[fulldta$studyID == 1047 & fulldta$rowID == 855, "no_res"] <- NA   
fulldta[fulldta$studyID == 1047 & fulldta$rowID == 855, "no_ns"] <- 23   

length(unique(fulldta$doi)) #468 DOI's, 447 Studies, 447, r_ID's #THIS MUST NOT INCLUDE THE SEROTYPE ONLY DATA  

#Remove income status from fulldta 
fulldta <- dplyr::select(fulldta, -income_status)

### #Add in last bit of data abstraction
load(file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/data_120220.rda")
#View(extra_data4)
nrow(extra_data4) + nrow(fulldta)

fulldta <- rbind(fulldta, extra_data4)
nrow(fulldta)

#14.  #Use ifelse statements to fill in the holes of data set 

#1. If no_ns has a value, and no_sus is empty, then no_sus is total_isolates - no_ns 
#2. If no_ns is missing, but no_int and no_res are full, create new no_ns that is sum of no_int and no_res
#Test if step 2 is necessary using: length(which(fulldta_test$no_ns != fulldta_test$no_ns_new))

fulldta <- fulldta %>% 
  mutate(no_sus_new = ifelse(!is.na(no_ns) & is.na(no_sus), total_isolates_drug - no_ns, no_sus)) 
# mutate(no_ns_new = ifelse(!is.na(no_int) & !is.na(no_res) & is.na(no_ns), no_res + no_int, no_ns))  

unique(fulldta$drug_class)
length(unique(fulldta$doi))
length(unique(fulldta$r_id)) #447 + 93: this excludes the serotype only studies 

### SAVE FILE 
save(fulldta, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_120220.Rdata")

test <- fulldta %>% 
  dplyr::select(studyID, r_id, doi)
View(test)

#Export full data with 1 row per unique studyid to compile DOI
fulldta_unique <- distinct(fulldta, r_id, .keep_all = TRUE) 
#write.csv(fulldta_unique, file = "fulldta_unique_031020.csv")
save(fulldta_unique, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_unique_120220.Rdata")

# fulldta_032410 <- read_excel("fulldta_032410.xlsx")
# 
# fulldta_doi <- distinct(fulldta, doi, .keep_all = TRUE) 
# fulldta_doi_group <- fulldta_doi %>%
#   dplyr::group_by(studyID) %>%
#   dplyr::select(studyID, doi)
# View(fulldta_doi_group)
# 
# fulldta_doi <- distinct(fulldta_032410, doi, .keep_all = TRUE) 
# fulldta_doi_group <- fulldta_doi %>%
#   dplyr::group_by(studyID) %>%
#   dplyr::select(studyID, doi)
# View(fulldta_doi_group)
# 
# st_doi_distinct <- distinct(st_doi, doi, .keep_all = TRUE) 
# st_doi_group <- st_doi_distinct %>%
#   dplyr::group_by(studyID) %>%
#   dplyr::select(studyID, doi)
# View(st_doi_group)
# 
# fulldta_032410_final <- read_excel("fulldta_032410_final.xlsx")
# length(unique(fulldta_032410_final$doi)) #144 Serotype Studies 
# length(unique(fulldta_032410_final$studyID)) #144 Serotype Studies 
# 
# test <- fulldta_032410_final %>%
#   dplyr::group_by(doi) %>%
#   dplyr::select(studyID, doi)
# View(test)
# 
# #Create max number of isolates per study/ arm
# 
# test <- fulldta_032410_final %>% distinct(studyID, arm_id, .keep_all = TRUE)
# View(test)
# 
# uniqueid_updated <- fulldta_032410_final %>% distinct(studyID, .keep_all = TRUE)
# write.csv(uniqueid_updated, "uniqueid_updated.csv")


# 2.1 Create full data set with only nonsusceptible isolates  ----
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_120220.Rdata")
length(unique(fulldta$r_id)) #540 (exlcudes serotype only studies)

fulldta_ns <- fulldta
fulldta_ns <- fulldta_ns[!(is.na(fulldta_ns$no_ns)), ] 

length(which(is.na(fulldta_ns$no_ns))) 
save(fulldta_ns, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_ns.rda")

# 2.2 Create full data set with only resistant isoltaes ----
fulldta_res <- fulldta
fulldta_res <- fulldta_res[!(is.na(fulldta_res$no_res)), ] 

length(which(is.na(fulldta_res$no_res))) 
save(fulldta_res, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_res.rda")

# 2.3 Reshape Data for resistant isolates ------------------------------------------------------------
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_ns.rda")

length(unique(fulldta_ns$studyID))
length(unique(fulldta_res$studyID))

#2.1 Initalize for loop vectors 
prepost_meta = midpoint_yr = r_id = prepost = isolate_type = drug  = yr_since_vax = sregion = gdp = income = region = sregion = id = res = c()

for (i in 1:length(fulldta_res$no_res)) {
  res = c(res,rep(1,(fulldta_res$no_res[i])),
          rep(0,fulldta_res$total_isolates_drug[i] - fulldta_res$no_res[i]))
  id = c(id,rep(i,fulldta_res$total_isolates_drug[i])) 
  region = c(region, rep(fulldta_res$region[i], fulldta_res$total_isolates_drug[i]))
  sregion = c(sregion, rep(fulldta_res$super_region[i], fulldta_res$total_isolates_drug[i]))
  income = c(income, rep(fulldta_res$income_status[i], fulldta_res$total_isolates_drug[i]))
  gdp = c(gdp, rep(fulldta_res$gdp[i], fulldta_res$total_isolates_drug[i]))
  yr_since_vax = c(yr_since_vax, rep(fulldta_res$yr_since_vax[i], fulldta_res$total_isolates_drug[i]))
  #pre = c(pre, rep(fulldta_res$pre[i], fulldta_res$total_isolates_drug[i]))
  drug = c(drug, rep(fulldta_res$drug_class[i], fulldta_res$total_isolates_drug[i]))
  isolate_type = c(isolate_type, rep(fulldta_res$isolate_type[i], fulldta_res$total_isolates_drug[i]))
  prepost = c(prepost, rep(fulldta_res$prepost[i], fulldta_res$total_isolates_drug[i]))
  r_id = c(r_id, rep(fulldta_res$r_id[i], fulldta_res$total_isolates_drug[i]))
  midpoint_yr = c(midpoint_yr, rep(fulldta_res$midpoint_yr[i], fulldta_res$total_isolates_drug[i]))
  prepost_meta = c(prepost_meta, rep(fulldta_res$prepost_meta[i], fulldta_res$total_isolates_drug[i]))
  
}


out_fulldta_res <- data.frame(res, id, region, sregion, gdp, yr_since_vax, drug, isolate_type, prepost, r_id, midpoint_yr, prepost_meta) #exclude income
names(out_fulldta_res) #check if data reshaped 
str(out_fulldta_res) #check if data reshaped 


# 2.4 Reshape data for nonsusceptible isolates----

#3.1 Initalize for loop vectors  #something is happening here whereby the new data drugs are coded as NA
midpoint_yr_ns = r_id_ns = prepost_ns = isolate_type_ns = drug_ns = 
  yr_since_vax_ns = sregion_ns = income_ns = region_ns = id_ns = ns = gdp_ns = prepost_meta_ns = c()

for (i in 1:length(fulldta_ns$no_ns)) {
  ns = c(ns,rep(1,(fulldta_ns$no_ns[i])),
         rep(0,fulldta_ns$total_isolates_drug[i] - fulldta_ns$no_ns[i]))
  id_ns = c(id_ns,rep(i,fulldta_ns$total_isolates_drug[i])) 
  region_ns = c(region_ns, rep(fulldta_ns$region[i], fulldta_ns$total_isolates_drug[i]))
  sregion_ns = c(sregion_ns, rep(fulldta_ns$super_region[i], fulldta_ns$total_isolates_drug[i]))
  income_ns = c(income_ns, rep(fulldta_ns$income_status[i], fulldta_ns$total_isolates_drug[i]))
  gdp_ns = c(gdp_ns, rep(fulldta_ns$gdp[i], fulldta_ns$total_isolates_drug[i]))
  yr_since_vax_ns = c(yr_since_vax_ns, rep(fulldta_ns$yr_since_vax[i], fulldta_ns$total_isolates_drug[i]))
  drug_ns = c(drug_ns, rep(fulldta_ns$drug_class[i], fulldta_ns$total_isolates_drug[i]))
  isolate_type_ns = c(isolate_type_ns, rep(fulldta_ns$isolate_type[i], fulldta_ns$total_isolates_drug[i]))
  prepost_ns = c(prepost_ns, rep(fulldta_ns$prepost[i], fulldta_ns$total_isolates_drug[i]))
  r_id_ns = c(r_id_ns, rep(fulldta_ns$r_id[i], fulldta_ns$total_isolates_drug[i])) 
  midpoint_yr_ns = c(midpoint_yr_ns, rep(fulldta_ns$midpoint_yr[i], fulldta_ns$total_isolates_drug[i]))
  prepost_meta_ns = c(prepost_meta_ns, rep(fulldta_ns$prepost_meta[i], fulldta_ns$total_isolates_drug[i]))
  
}

length(ns) 
length(region_ns)
length(sregion_ns)
length(income_ns)
length(gdp_ns)
length(yr_since_vax_ns)
length(drug_ns)
length(isolate_type_ns)
length(prepost_ns)
length(r_id_ns)
length(midpoint_yr_ns)
length(prepost_meta_ns)
length(id_ns)

out_fulldta_ns <- data.frame(ns, id_ns, region_ns, sregion_ns, yr_since_vax_ns, drug_ns, 
                             isolate_type_ns, r_id_ns, gdp_ns, prepost_ns, midpoint_yr_ns, prepost_meta_ns)

str(out_fulldta_ns)
names(out_fulldta_ns)

# 3. Pre-Post Data Frames ----

# 4.1 Create GBD Regions and GBD Super Regions
gbd_regions <- factor(out_fulldta_ns$region)
gbd_list <- levels(gbd_regions)

sregions <- factor(out_fulldta_ns$sregion)
sregion_list <- levels(sregions)

save(sregion_list, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/sregion_list.rda")
save(gbd_list, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/gbd_list.rda")

# 3.1 Create data frames for unequivocally PRE and POST Vaccine ----
colnames(out_fulldta_ns) <- c("ns", "id", "region",  "sregion" , "yr_since_vax", "drug", "isolate_type", "r_id", "gdp", "prepost", "midpoint_yr", "prepost_meta")

out_fulldta_ns$region <- as.character(levels(out_fulldta_ns$region))[out_fulldta_ns$region]
out_fulldta_ns$sregion <- as.character(levels(out_fulldta_ns$sregion))[out_fulldta_ns$sregion]
#out_fulldta_ns$income <- as.character(levels(out_fulldta_ns$income))[out_fulldta_ns$income]
out_fulldta_ns$prepost <- as.character(levels(out_fulldta_ns$prepost))[out_fulldta_ns$prepost]
out_fulldta_ns$prepost_meta <- as.character(levels(out_fulldta_ns$prepost_meta))[out_fulldta_ns$prepost_meta]

out_fulldta_res$region <- as.character(levels(out_fulldta_res$region))[out_fulldta_res$region]
out_fulldta_res$sregion <- as.character(levels(out_fulldta_res$sregion))[out_fulldta_res$sregion]
#out_fulldta_res$income <- as.character(levels(out_fulldta_res$income))[out_fulldta_res$income]
out_fulldta_res$prepost <- as.character(levels(out_fulldta_res$prepost))[out_fulldta_res$prepost]
out_fulldta_res$prepost_meta <- as.character(levels(out_fulldta_res$prepost_meta))[out_fulldta_res$prepost_meta]

save(out_fulldta_ns, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/out_fulldta_ns.rda")
save(out_fulldta_res, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/out_fulldta_res.rda")

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/out_fulldta_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/out_fulldta_res.rda")


#1. Save pre and post data frames 

novax_res <- out_fulldta_res %>% 
  filter(prepost == "naive") 

save(novax_res, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/novax_res.rda")

novax_ns <- out_fulldta_ns %>% 
  filter(prepost == "naive") 

save(novax_ns, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/novax_ns.rda")

postvax_res <- out_fulldta_res %>% 
  filter(prepost == "not_naive") 

save(postvax_res, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/postvax_res.rda")

postvax_ns <- out_fulldta_ns %>% 
  filter(prepost == "not_naive") 

save(postvax_ns, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/postvax_ns.rda")


# 3. Create data frames for meta-regression -- this is the individual data frame ----
#Data frames created below are plugged into models in 02-11-20_code_AMr_PLOT-MetaRegression and models will also be added to final code file for the plot 
# Data frames created below are used to create Figure 4 in the 05-10-20_code_AMR_MetaRegPlot

#New data frames 5.13.20
mod_pen_ns <- out_fulldta_ns %>% 
  filter(drug == 9) %>%  #9 = penicillin 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3) 

mod_pen_res <- out_fulldta_res %>% 
  filter(drug == 9) %>%  #9 = penicillin 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3)  

mod_mac_ns <- out_fulldta_ns %>% 
  filter(drug == 8) %>%  #8 = macrolide 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3)  

mod_mac_res <- out_fulldta_res %>% 
  filter(drug == 8) %>%  #8 = macrolide 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3)

save(mod_pen_ns, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_pen_ns.rda")
save(mod_pen_res, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_pen_res.rda")
save(mod_mac_ns, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_mac_ns.rda")
save(mod_mac_res, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_mac_res.rda")


mod_sxt_ns <- out_fulldta_ns %>% 
  filter(drug == 10) %>%  #10 = SXT 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3) 

mod_sxt_res <- out_fulldta_res %>% 
  filter(drug == 10) %>%  #10 = SXT 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3) 

save(mod_sxt_res, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_sxt_res.rda")
save(mod_sxt_ns, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_sxt_ns.rda")

# Create data frame of 3gen_cephalosporin 
#To determine what drug number: #levels(fulldta$drug_class)

mod_ceph_ns <- out_fulldta_ns %>% 
  filter(drug == 2) %>%  #2 = ceph 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3) 

mod_ceph_res <- out_fulldta_res %>% 
  filter(drug == 2) %>%  #2 = ceph
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3)

# Create data frame of just tetraccycline 
mod_tet_ns <- out_fulldta_ns %>% 
  filter(drug == 11) %>%  #11 = tetracycline 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3) 

mod_tet_res <- out_fulldta_res %>% 
  filter(drug == 11) %>%  #11 = tetracycline 
  mutate(yr_2 = yr_since_vax^2) %>%  
  mutate(yr_3 = yr_since_vax^3) 


save(mod_ceph_res, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_ceph_res.rda")
save(mod_ceph_ns, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_ceph_ns.rda")
save(mod_tet_ns, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_tet_ns.rda")
save(mod_tet_res, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_tet_res.rda")



### 4. START HERE TO LOAD DATA TO CREATE TABLE 1, TABLE 2, TABLE 1S ### ----

setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/01-SPAMR-RFiles/SPAMR-Box/output/03-10-20")
load("gbd_list.rda")
load("sregion_list.rda")
load("out_fulldta_ns.rda")
load("out_fulldta_res.rda")
load("novax_ns.rda")
load("novax_res.rda")
load("fulldta_ns.rda")
load("fulldta_res.rda")
load("postvax_ns.rda")
load("postvax_res.rda")
load("fulldta_031020.Rdata")

#Build table of pre vs. 3 years post vaccine introduction by country and region 

#Save region order as a r object
region_order <- unique(dat$region) #dat is from the forest plotting code 
save(region_order, file = "region_order.Rdata") #This is saved as an object to 04-13-20

supp_tb <- fulldta %>% 
  group_by(country, region, prepost) %>% 
  filter(prepost == "naive" | prepost == "not_naive") %>% 
  dplyr::summarize(study = length(unique(studyID)), 
                   isolates = sum(total_isolates_drug, na.rm = TRUE)) %>% 
  arrange(match(region, region_order))
View(supp_tb)

setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/01-SPAMR-RFiles/SPAMR-Box/output/04-13-20")
write.csv(supp_tb, "supp_tb.csv")

#Test how mnay countries there are in full data set 
length(unique(fulldta$country)) #98 countries 

atlasdta <- fulldta %>% 
  filter(studyID == 9999)

length(unique(atlasdta$country)) #58 countries 

atlasdta_nodup <- atlasdta %>% 
  distinct(country, sample_collection_startyear, .keep_all = TRUE)
View(atlasdta_nodup)

#Sum total isolates from atlasdta_nodup
sum(atlasdta$total_isolates_drug) #14, 162 isolates 

#Filter how much data came from ATLAS for results section 

# 4.1. CREATE TABLE 1- DESCRIBING DATA  ----

#THINK ABOUT HOW WE ARE SUMMING ISOLATES
# WE WILL INFLATE THE NUMBER OF ISOLATES BY DOUBLE COUNTING THE SAME ISOLATES TESTED FOR MULTIPLE DRUGS. 
# WILL NEED TO SOMEHOW FILTER BY UNIQUE ID + UNIQUE ARMS, THEN COUNT THE NUMBER OF STUDIES 

#FILTER DATA HERE: 
#test <- unique(fulldta[c("r_id", "arm_id")]) #this gives you what the columsn should be, but not the full data farme


fulldta_table1_032410 <- read_excel("fulldta_table1_032410_final.xlsx") #this is final table excel for these tables- includes serotype data- do not use for analysis
length(unique(fulldta_table1_032410$studyID))
length(unique(fulldta_table1_032410$doi))

fulldta_nodup <- fulldta_table1_032410 %>% distinct(studyID, arm_id, .keep_all = TRUE) #need to use studyID
length(unique(fulldta_nodup$studyID))
length(unique(fulldta_nodup$doi)) #START HERE! 

#OLD BELOW
# fulldta_nodup <- fulldta %>% distinct(r_id, arm_id, .keep_all = TRUE)
# save(fulldta_nodup, file = "fulldta_nodup.rda") #Can also use this to get a complete list of DOI's 
# load("fulldta_nodup.rda")

#How to change the order of the output: 
order_region <- c("southern_latin_america", "western_europe", "hi_north_america", "australasia", 
                  "hi_asia_pacific", "caribbean", "central_latin_america", "tropical_latin_america", 
                  "andean_latin_america", "southern_subsaharan_africa", "western_subsaharan_africa", 
                  "central_subsaharan_africa", "eastern_subsaharan_africa", "north_africa_middle_east", 
                  "south_asia","east_asia","southeast_asia",  "oceania", "eastern_europe", "central_europe")

order_region_s <- c("southern_latin_america", "western_europe", "hi_north_america", "australasia", 
                    "hi_asia_pacific","high_income","caribbean", "central_latin_america", "tropical_latin_america", 
                    "andean_latin_america", "latin_america_caribbean", "southern_subsaharan_africa", "western_subsaharan_africa", 
                    "central_subsaharan_africa", "eastern_subsaharan_africa", "sub_saharan_africa", "north_africa_middle_east", 
                    "south_asia","east_asia","southeast_asia",  "oceania","seasia_easia_oceania", "eastern_europe", "central_europe", "ceurope_eeurope_casia")

#1. Super Region counts 
count_sregion <- fulldta_nodup %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarise(study_id = length(unique(studyID)), #previously used r_id 
                   (isolate_count = sum(total_isolates_drug, na.rm = TRUE)))
#View(count_sregion)

count_region <- fulldta_nodup %>% 
  dplyr::group_by(region) %>%
  dplyr::summarise(study_id = length(unique(studyID)),  #previously used r_id
                   (isolate_count = sum(total_isolates_drug, na.rm = TRUE)))
#View(count_region) 

count_region <- count_region %>% 
  arrange(match(region, order_region))
View(count_region)

count_sregion <- count_sregion %>% 
  arrange(match(super_region, order_region_s))
View(count_sregion)


# STEP 2: Summarize Invasive vs. Carriage 

count_isolate_type <- fulldta_nodup %>% 
  dplyr::select(isolate_type, total_isolates_drug, studyID) %>% 
  group_by(isolate_type) %>% 
  dplyr::summarize(study_id = length(unique(studyID)),
                   (isolate_count = sum(total_isolates_drug, na.rm = TRUE)))
View(count_isolate_type)

# STEP 3: susceptibility testing method 

count_sus_method1 <- fulldta_nodup %>% 
  dplyr::select(method_1, method_2, studyID, total_isolates_drug) %>% 
  group_by(method_1) %>% 
  dplyr::summarize(study_id = length(unique(studyID)),
                   (isolate_count = sum(total_isolates_drug, na.rm = TRUE)))
View(count_sus_method1)

#append a column to count_sus_method1 with labels 
labels_count_sus_method <- c("1: Disk Diffusion", "2: E-test", "3: Broth microdilution", "4: Agar dilution", "5: Molecular Assay", "6: Other", "7: Not specified", "NR", "blank", "not listed", "is.na")
count_sus_method1$labels  = labels_count_sus_method 
names(count_sus_method1)[3] <- "count"
View(count_sus_method1)

#Next repeat process for method_2
count_sus_method2 <- fulldta_nodup %>% 
  dplyr::select(method_1, method_2, r_id, total_isolates_drug) %>% 
  group_by(method_2) %>% 
  dplyr::summarize(study_id = length(unique(r_id)),
                   (count = sum(total_isolates_drug, na.rm = TRUE)))
(count_sus_method2)

labels_count_sus_method_2 <- c("1: Disk Diffusion", "2: E-test", "3: Broth microdilution", "4: Agar dilution", "6: Other", "is.na")
names(count_sus_method2)[3] <- "count"
count_sus_method2
count_sus_method2$labels  = labels_count_sus_method_2 
View(count_sus_method2)

#Need to add values of count_sus_method2 to count_sus_method1

#First index the number of studies and the count of isolates utilizing >1 method from method_2 
study_extra_method1 <- count_sus_method2$study_id[1] 
study_extra_method2 <- count_sus_method2$study_id[2] 
study_extra_method3 <- count_sus_method2$study_id[3] 
study_extra_method4 <- count_sus_method2$study_id[4] 
study_extra_method6 <- count_sus_method2$study_id[5] #be careful: row 5 of count_sus_method2 corrsponds to method 6 (other)

isolates_extra_method1 <- count_sus_method2$count[1] 
isolates_extra_method2 <- count_sus_method2$count[2] 
isolates_extra_method3 <- count_sus_method2$count[3] 
isolates_extra_method4 <- count_sus_method2$count[4] 
isolates_extra_method6 <- count_sus_method2$count[5] 

#Next add these to the value in count_sus_method1
count_sus_method1$study_id[1] = count_sus_method1$study_id[1] + study_extra_method1
count_sus_method1$study_id[2] = count_sus_method1$study_id[2] + study_extra_method2
count_sus_method1$study_id[3] = count_sus_method1$study_id[3] + study_extra_method3
count_sus_method1$study_id[4] = count_sus_method1$study_id[4] + study_extra_method4
count_sus_method1$study_id[6] = count_sus_method1$study_id[6] + study_extra_method6

count_sus_method1$count[1] = count_sus_method1$count[1] + isolates_extra_method1
count_sus_method1$count[2] = count_sus_method1$count[2] + isolates_extra_method2
count_sus_method1$count[3] = count_sus_method1$count[3] + isolates_extra_method3
count_sus_method1$count[4] = count_sus_method1$count[4] + isolates_extra_method4
count_sus_method1$count[6] = count_sus_method1$count[6] + isolates_extra_method6

#Clean up count_sus_method1 by combining molecular assay (5) with other and all the not specified (7) with NR, "", not listed, and is.na

notlisted_study_final <- sum(count_sus_method1$study_id[7:11]) #sum of all the "not listed" values for count_sus_method1
notlisted_count_final <- sum(count_sus_method1$count[7:11]) #sum of all the "not listed" values for count_sus_method1

count_sus_method1 <- data.frame(count_sus_method1)
count_sus_method1[nrow(count_sus_method1) + 1,] = c("Final Value for 7", notlisted_study_final, notlisted_count_final, "Updated-Final Not Listed Counts")
View(count_sus_method1)

#Remove rows 7:11 of count_sus_method1
count_sus_method1 <- count_sus_method1[-c(7:11),]

# STEP 4: test standards 
count_standards <- fulldta_nodup %>% 
  dplyr::select(criteria_cl, studyID, total_isolates_drug) %>% 
  group_by(criteria_cl) %>% 
  dplyr::summarize(study_id = length(unique(studyID)),
                   (isolate_count = sum(total_isolates_drug, na.rm = TRUE)))
View(count_standards)

#one study that used both CLSI and EUCAST -> counted as "other" 
view <- fulldta_nodup[which(fulldta_nodup$criteria == 12),] #2 unique regions covered by 370 (southern latin america, central latin america)
View(view)

# STEP 5: pre vs post 
count_prepost <- fulldta_nodup %>% 
  dplyr::select(prepost, studyID, total_isolates_drug) %>% 
  group_by(prepost) %>% 
  dplyr::summarize(study_id = length(unique(studyID)),
                   (isolate_count = sum(total_isolates_drug, na.rm = TRUE)))
View(count_prepost)

#New for adding data for 18 - 032420
# serotype18 <- read_excel("18_addt_serotype.xlsx")
# #7. Add yr since vax  (used start year of sample collection rather than midpoint yr bc otherwise 90 studies had pre = 0 and yr since vax >0)
# serotype18 <- serotype18 %>%
#   mutate(yr_since_vax = case_when(as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) > 0 ~ sample_collection_startyear -  as.numeric(pcv_intro_year), 
#                                   as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) == 0 ~ 0, 
#                                   as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) < 0 ~ 0, 
#                                   pcv_intro_year == "NI" ~ 0)) #pre
# 
# length(which(is.na(serotype18$yr_since_vax))) 
# 
# serotype18 <- serotype18 %>% 
#   mutate(prepost = case_when(as.numeric(pcv_intro_year) == sample_collection_startyear ~ "no_analysis", 
#                              as.numeric(pcv_intro_year) == sample_collection_endyear ~ "no_analysis", 
#                              sample_collection_startyear - as.numeric(pcv_intro_year) == 1 ~ "no_analysis", 
#                              as.numeric(pcv_intro_year) > sample_collection_endyear ~ "naive",
#                              sample_collection_startyear - as.numeric(pcv_intro_year) >= 3 ~ "not_naive", #changed from 5
#                              #sample_collection_startyear - as.numeric(pcv_intro_year) == 4 ~ "no_analysis", 
#                              #sample_collection_startyear - as.numeric(pcv_intro_year) == 3 ~ "no_analysis", 
#                              sample_collection_startyear - as.numeric(pcv_intro_year) == 2 ~ "no_analysis", 
#                              pcv_intro_year == "NI" ~ "naive")) 
# 
# serotype18$prepost[is.na(serotype18$prepost)] <- "no_analysis"
# length(which(is.na(serotype18$prepost))) 
# 
# 
# #8.1 Add prepost variable for meta-regression analysis 
# serotype18 <- serotype18 %>% 
#   mutate(prepost_meta = case_when(yr_since_vax >= 1 ~ "post", 
#                                   yr_since_vax == 0 ~ "pre"))
# 
# view <- serotype18 %>%
#   dplyr::select(studyID, yr_since_vax, prepost, prepost_meta)
# View(view)
# 
# write.csv(view, "view.csv")

# 4.2 CREATE TABLE 2- POOLED NUMBERS OF NONSUSCEPTIBLE AND RESISTANCE  ----

#Dplyr Method- this is for invasive, macorlide, naive isolates 
gbd_region_list_str <- c("Andean Latin America", "Australasia", "Caribbean", "Central Europe", "Central Latin America", 
                         "Central Subsaharan Africa", "East Asia", 
                         "Eastern Europe", "Eastern Sub-Saharan Africa", "High Income Asia Pacific", "High Income North America", 
                         "North Africa and Middle East", "Oceania", "South Asia", "Southeast Asia", "Southern Latin America", "Southern Sub-Saharan Africa", 
                         "Tropical Latin America", "Western Europe", "Western Sub-Saharan Africa")

###NEW - filter by drug THEN unique data  
length(unique(fulldta_table1_032410$studyID))
length(unique(fulldta_table1_032410$doi))

### Create Data Sets ###

#1. Penicillin 
fulldta_table1_pen <- fulldta_table1_032410 %>% 
  dplyr::filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, .keep_all = TRUE)

fulldta_table1_pen_ns <- fulldta_table1_pen
fulldta_table1_pen_ns <- fulldta_table1_pen_ns[!(is.na(fulldta_table1_pen_ns$no_ns)), ] 

fulldta_table1_pen_res <- fulldta_table1_pen
fulldta_table1_pen_res <- fulldta_table1_pen_res[!(is.na(fulldta_table1_pen_res$no_res)), ] 

#2. Macrolide 
fulldta_table1_mac <- fulldta_table1_032410 %>% 
  dplyr::filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, .keep_all = TRUE)

fulldta_table1_mac_ns <- fulldta_table1_mac
fulldta_table1_mac_ns <- fulldta_table1_mac_ns[!(is.na(fulldta_table1_mac_ns$no_ns)), ] 

fulldta_table1_mac_res <- fulldta_table1_mac
fulldta_table1_mac_res <- fulldta_table1_mac_res[!(is.na(fulldta_table1_mac_res$no_res)), ] 

#3. SXT 
fulldta_table1_sxt <- fulldta_table1_032410 %>% 
  dplyr::filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, .keep_all = TRUE)

fulldta_table1_sxt_ns <- fulldta_table1_sxt
fulldta_table1_sxt_ns <- fulldta_table1_sxt_ns[!(is.na(fulldta_table1_sxt_ns$no_ns)), ] 

fulldta_table1_sxt_res <- fulldta_table1_sxt
fulldta_table1_sxt_res <- fulldta_table1_sxt_res[!(is.na(fulldta_table1_sxt_res$no_res)), ] 

#4. CEPH  
fulldta_table1_ceph <- fulldta_table1_032410 %>% 
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  distinct(studyID, arm_id, .keep_all = TRUE)

fulldta_table1_ceph_ns <- fulldta_table1_ceph
fulldta_table1_ceph_ns <- fulldta_table1_ceph_ns[!(is.na(fulldta_table1_ceph_ns$no_ns)), ] 

fulldta_table1_ceph_res <- fulldta_table1_ceph
fulldta_table1_ceph_res <- fulldta_table1_ceph_res[!(is.na(fulldta_table1_ceph_res$no_res)), ] 

#5. TETRACYCLINE  
fulldta_table1_tet <- fulldta_table1_032410 %>% 
  dplyr::filter(drug_class == "tetracycline") %>% 
  distinct(studyID, arm_id, .keep_all = TRUE)

fulldta_table1_tet_ns <- fulldta_table1_tet
fulldta_table1_tet_ns <- fulldta_table1_tet_ns[!(is.na(fulldta_table1_tet_ns$no_ns)), ] 

fulldta_table1_tet_res <- fulldta_table1_tet
fulldta_table1_tet_res <- fulldta_table1_tet_res[!(is.na(fulldta_table1_tet_res$no_res)), ] 

#Create Functions 

ns <- function(ns){
  tbl_mac_ns <- ns %>% 
    dplyr::group_by(region) %>%
    dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                     total_ns = sum(no_ns, na.rm = TRUE)) %>% 
    dplyr::mutate(perc_ns = (total_ns / total_isolates_sum) * 100) 
  
  tbl_mac_ns_sregion <- ns %>% 
    dplyr::group_by(super_region) %>%
    dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                     total_ns = sum(no_ns, na.rm = TRUE)) %>% 
    dplyr::mutate(perc_ns = (total_ns / total_isolates_sum) * 100) 
  names(tbl_mac_ns_sregion)[1] <- "region"
  tb_mac_ns_comb <- rbind(tbl_mac_ns, tbl_mac_ns_sregion)
  
  tb_mac_ns_comb <- tb_mac_ns_comb %>% 
    arrange(match(region, order_region_s))
  
  tb_mac_ns_comb$fin_ns_mac <- rep(NA, nrow(tb_mac_ns_comb)) #previously c()
  
  for (i in 1:nrow(tb_mac_ns_comb)){
    tb_mac_ns_comb$fin_ns_mac[i] <- c(paste0(round(as.numeric(tb_mac_ns_comb$total_ns[i], 0)), 
                                             " (", 
                                             round(as.numeric(tb_mac_ns_comb$perc_ns[i],0)),
                                             ")"))
  }
  
  tb_mac_ns_comb <- tb_mac_ns_comb %>% 
    arrange(match(region, order_region_s)) 
  
  tb_mac_ns_comb <- unique(tb_mac_ns_comb[,c(1:3, 5)])
  
  return(tb_mac_ns_comb)
}

res <- function(res){
  tbl_mac_res <- res %>% 
    dplyr::group_by(region) %>%
    dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                     total_res = sum(no_res, na.rm = TRUE)) %>% 
    dplyr::mutate(perc_res = (total_res / total_isolates_sum) * 100) 
  
  tbl_mac_res_sregion <- res %>% 
    dplyr::group_by(super_region) %>%
    dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                     total_res = sum(no_res, na.rm = TRUE)) %>% 
    dplyr::mutate(perc_res = (total_res / total_isolates_sum) * 100) 
  names(tbl_mac_res_sregion)[1] <- "region"
  tb_mac_res_comb <- rbind(tbl_mac_res, tbl_mac_res_sregion)
  
  tb_mac_res_comb <- tb_mac_res_comb %>% 
    arrange(match(region, order_region_s))
  
  tb_mac_res_comb$fin_res_mac <- rep(NA, nrow(tb_mac_res_comb)) #previously c()
  
  for (i in 1:nrow(tb_mac_res_comb)){
    tb_mac_res_comb$fin_res_mac[i] <- c(paste0(round(as.numeric(tb_mac_res_comb$total_res[i], 0)), 
                                               " (", 
                                               round(as.numeric(tb_mac_res_comb$perc_res[i],0)),
                                               ")"))
  }
  
  tb_mac_res_comb <- tb_mac_res_comb %>% 
    arrange(match(region, order_region_s)) 
  
  tb_mac_res_comb <- unique(tb_mac_res_comb[,c(1:3, 5)])
  
  return(tb_mac_res_comb)
}


#Run Functions

#1. Pen 
ns_tb <- (ns(fulldta_table1_pen_ns))
res_tb <- (res(fulldta_table1_pen_res))

View(ns_tb)
View(res_tb)

ns <- sum(ns_tb$total_ns)
total <- sum(ns_tb$total_isolates_sum)
perc <- (ns/ total) * 100
ns; total; perc;

res <- sum(res_tb$total_res)
total <- sum(res_tb$total_isolates_sum)
perc <- (res/ total) * 100
res; total; perc;

#2. Mac 
ns_tb <- (ns(fulldta_table1_mac_ns))
res_tb <- (res(fulldta_table1_mac_res))

View(ns_tb)
View(res_tb)

ns <- sum(ns_tb$total_ns)
total <- sum(ns_tb$total_isolates_sum)
perc <- (ns/ total) * 100
ns; total; perc;

res <- sum(res_tb$total_res)
total <- sum(res_tb$total_isolates_sum)
perc <- (res/ total) * 100
res; total; perc;

#3. SXT 
ns_tb <- (ns(fulldta_table1_sxt_ns))
res_tb <- (res(fulldta_table1_sxt_res))

View(ns_tb)
View(res_tb)

ns <- sum(ns_tb$total_ns)
total <- sum(ns_tb$total_isolates_sum)
perc <- (ns/ total) * 100
ns; total; perc;

res <- sum(res_tb$total_res)
total <- sum(res_tb$total_isolates_sum)
perc <- (res/ total) * 100
res; total; perc;

#4. Ceph
ns_tb <- (ns(fulldta_table1_ceph_ns))
res_tb <- (res(fulldta_table1_ceph_res))

View(ns_tb)
View(res_tb)

ns <- sum(ns_tb$total_ns)
total <- sum(ns_tb$total_isolates_sum)
perc <- (ns/ total) * 100
ns; total; perc;

res <- sum(res_tb$total_res)
total <- sum(res_tb$total_isolates_sum)
perc <- (res/ total) * 100
res; total; perc;

#5. Tet

ns_tb <- (ns(fulldta_table1_tet_ns))
res_tb <- (res(fulldta_table1_tet_res))

View(ns_tb)
View(res_tb)

ns <- sum(ns_tb$total_ns)
total <- sum(ns_tb$total_isolates_sum)
perc <- (ns/ total) * 100
ns; total; perc;

res <- sum(res_tb$total_res)
total <- sum(res_tb$total_isolates_sum)
perc <- (res/ total) * 100
res; total; perc;



#NEW BELOW
tb_mac_ns_comb <- tb_mac_ns_comb %>% 
  arrange(match(region, order_region_s)) 
View(tb_mac_ns_comb)

ns <- sum(tb_mac_ns_comb$total_ns)
total <- sum(tb_mac_ns_comb$total_isolates_sum)
perc <- (ns/ total) * 100
ns; total; perc;

tb_mac_res_comb <- tb_mac_res_comb %>% 
  arrange(match(region, order_region_s)) 
View(tb_mac_res_comb)

res <- sum(tb_mac_res_comb$total_res)
total <- sum(tb_pen_res_comb$total_isolates_sum)
perc <- (res/ total) * 100
res; total; perc;
#NEW ABOVe

tb_mac <- full_join(tb_mac_ns_comb, tb_mac_res_comb, by = "region")

#SXT estimates
tbl_sxt_ns <- fulldta_ns %>% 
  dplyr::select(region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "SXT") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_ns = sum(no_ns, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_ns = (total_ns / total_isolates_sum) * 100) 

tbl_sxt_ns_sregion <- fulldta_ns %>% 
  dplyr::filter(drug_class == "SXT") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_ns = sum(no_ns, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_ns = (total_ns / total_isolates_sum) * 100) 
names(tbl_sxt_ns_sregion)[1] <- "region"
tb_sxt_ns_comb <- rbind(tbl_sxt_ns, tbl_sxt_ns_sregion)

tbl_sxt_res <- fulldta_res %>% 
  dplyr::select(region, super_region, drug_class, total_isolates_drug, 
                no_sus, no_sus_new, no_int, no_res, no_ns, isolate_type, prepost) %>% #no_sus_new, no_ns_new
  dplyr::filter(drug_class == "SXT") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_res = sum(no_res, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_res = (total_res / total_isolates_sum) * 100) 

tbl_sxt_res_sregion <- fulldta_res %>% 
  dplyr::filter(drug_class == "SXT") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_res = sum(no_res, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_res = (total_res / total_isolates_sum) * 100) 

names(tbl_sxt_res_sregion)[1] <- "region"
tb_sxt_res_comb <- rbind(tbl_sxt_res, tbl_sxt_res_sregion)

#New strategy when nrow(ns) is not the same as nrow(res) (have to have two different for loops)
tb_sxt_ns_comb$fin_ns_sxt <- rep(NA, nrow(tb_sxt_ns_comb)) #previously c()
tb_sxt_res_comb$fin_res_sxt <- rep(NA, nrow(tb_sxt_res_comb)) #previously c()


for (i in 1:nrow(tb_sxt_ns_comb)){
  tb_sxt_ns_comb$fin_ns_sxt[i] <- c(paste0(round(as.numeric(tb_sxt_ns_comb$total_ns[i], 0)), 
                                           " (", 
                                           round(as.numeric(tb_sxt_ns_comb$perc_ns[i],0)),
                                           ")"))
}
for (i in 1:nrow(tb_sxt_res_comb)){
  tb_sxt_res_comb$fin_res_sxt[i] <- c(paste0(round(as.numeric(tb_sxt_res_comb$total_res[i], 0)), 
                                             " (", 
                                             round(as.numeric(tb_sxt_res_comb$perc_res[i],0)),
                                             ")"))
} 
#View(tb_sxt_ns_comb)
#View(tb_sxt_res_comb)
#NEW BELOW
tb_sxt_ns_comb <- tb_sxt_ns_comb %>% 
  arrange(match(region, order_region_s)) 
View(tb_sxt_ns_comb)

ns <- sum(tb_sxt_ns_comb$total_ns)
total <- sum(tb_sxt_ns_comb$total_isolates_sum)
perc <- (ns/ total) * 100
ns; total; perc;

tb_sxt_res_comb <- tb_sxt_res_comb %>% 
  arrange(match(region, order_region_s)) 
View(tb_sxt_res_comb)

res <- sum(tb_sxt_res_comb$total_res)
total <- sum(tb_pen_res_comb$total_isolates_sum)
perc <- (res/ total) * 100
res; total; perc;
#NEW ABOVe

tb_sxt <- full_join(tb_sxt_ns_comb, tb_sxt_res_comb, by = "region")
View(tb_sxt)

#Cephalosporin estimates
tbl_ceph_ns <- fulldta_ns %>% 
  dplyr::select(region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_ns = sum(no_ns, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_ns = (total_ns / total_isolates_sum) * 100) 

tbl_ceph_ns_sregion <- fulldta_ns %>% 
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_ns = sum(no_ns, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_ns = (total_ns / total_isolates_sum) * 100) 
names(tbl_ceph_ns_sregion)[1] <- "region"
tb_ceph_ns_comb <- rbind(tbl_ceph_ns, tbl_ceph_ns_sregion)

tbl_ceph_res <- fulldta_res %>% 
  dplyr::select(region, super_region, drug_class, total_isolates_drug, 
                no_sus, no_sus_new, no_int, no_res, no_ns, isolate_type, prepost) %>% #no_sus_new, no_ns_new
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_res = sum(no_res, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_res = (total_res / total_isolates_sum) * 100) 

tbl_ceph_res_sregion <- fulldta_res %>% 
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_res = sum(no_res, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_res = (total_res / total_isolates_sum) * 100) 

names(tbl_ceph_res_sregion)[1] <- "region"
tb_ceph_res_comb <- rbind(tbl_ceph_res, tbl_ceph_res_sregion)

#New strategy when nrow(ns) is not the same as nrow(res) (have to have two different for loops)
tb_ceph_ns_comb$fin_ns_ceph <- rep(NA, nrow(tb_ceph_ns_comb)) #previously c()
tb_ceph_res_comb$fin_res_ceph <- rep(NA, nrow(tb_ceph_res_comb)) #previously c()


for (i in 1:nrow(tb_ceph_ns_comb)){
  tb_ceph_ns_comb$fin_ns_ceph[i] <- c(paste0(round(as.numeric(tb_ceph_ns_comb$total_ns[i], 0)), 
                                             " (", 
                                             round(as.numeric(tb_ceph_ns_comb$perc_ns[i],0)),
                                             ")"))
}
for (i in 1:nrow(tb_ceph_res_comb)){
  tb_ceph_res_comb$fin_res_ceph[i] <- c(paste0(round(as.numeric(tb_ceph_res_comb$total_res[i], 0)), 
                                               " (", 
                                               round(as.numeric(tb_ceph_res_comb$perc_res[i],0)),
                                               ")"))
} 
#View(tb_ceph_ns_comb)
#View(tb_ceph_res_comb)
tb_ceph <- full_join(tb_ceph_ns_comb, tb_ceph_res_comb, by = "region")
#View(tb_ceph)

#tetracycline estimates
tbl_tet_ns <- fulldta_ns %>% 
  dplyr::select(region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "tetracycline") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_ns = sum(no_ns, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_ns = (total_ns / total_isolates_sum) * 100) 

tbl_tet_ns_sregion <- fulldta_ns %>% 
  dplyr::filter(drug_class == "tetracycline") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_ns = sum(no_ns, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_ns = (total_ns / total_isolates_sum) * 100) 
names(tbl_tet_ns_sregion)[1] <- "region"
tb_tet_ns_comb <- rbind(tbl_tet_ns, tbl_tet_ns_sregion)

tbl_tet_res <- fulldta_res %>% 
  dplyr::select(region, super_region, drug_class, total_isolates_drug, 
                no_sus, no_sus_new, no_int, no_res, no_ns, isolate_type, prepost) %>% #no_sus_new, no_ns_new
  dplyr::filter(drug_class == "tetracycline") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_res = sum(no_res, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_res = (total_res / total_isolates_sum) * 100) 

tbl_tet_res_sregion <- fulldta_res %>% 
  dplyr::filter(drug_class == "tetracycline") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(total_isolates_sum = sum(total_isolates_drug, na.rm = TRUE),
                   total_res = sum(no_res, na.rm = TRUE)) %>% 
  dplyr::mutate(perc_res = (total_res / total_isolates_sum) * 100) 

names(tbl_tet_res_sregion)[1] <- "region"
tb_tet_res_comb <- rbind(tbl_tet_res, tbl_tet_res_sregion)

#New strategy when nrow(ns) is not the same as nrow(res) (have to have two different for loops)
tb_tet_ns_comb$fin_ns_tet <-  rep(NA, nrow(tb_tet_ns_comb)) #previously c()
tb_tet_res_comb$fin_res_tet <- rep(NA, nrow(tb_tet_res_comb)) #previously c()


for (i in 1:nrow(tb_tet_ns_comb)){
  tb_tet_ns_comb$fin_ns_tet[i] <- c(paste0(round(as.numeric(tb_tet_ns_comb$total_ns[i], 0)), 
                                           " (", 
                                           round(as.numeric(tb_tet_ns_comb$perc_ns[i],0)),
                                           ")"))
}
for (i in 1:nrow(tb_tet_res_comb)){
  tb_tet_res_comb$fin_res_tet[i] <- c(paste0(round(as.numeric(tb_tet_res_comb$total_res[i], 0)), 
                                             " (", 
                                             round(as.numeric(tb_tet_res_comb$perc_res[i],0)),
                                             ")"))
} 
#View(tb_tet_ns_comb)
#View(tb_tet_res_comb)
tb_tet <- full_join(tb_tet_ns_comb, tb_tet_res_comb, by = "region")
View(tb_tet)

#Merge pen + mac ns and res data (and drop colmns)- DO THIS WITH ALL THE DRUGS AT END 
#tbl_pen_new <- tbl_pen[-c(1:4, 6:8)] #drops columns 
#tbl_mac_new <- tb_mac[-c(1:4, 6:8)] #dropb columns 


tbl_pen_new <- tb_pen[-c(2:4, 6:8)] #drops columns 
tbl_pen_new <- tbl_pen_new %>% 
  plyr::arrange(match(region, order_region_s))
tbl_mac_new <- tb_mac[-c(2:4, 6:8)] #drops columns 
tbl_mac_new <- tbl_mac_new %>% 
  plyr::arrange(match(region, order_region_s))
tbl_ceph_new <- tb_ceph[-c(2:4, 6:8)] #drops columns 
tbl_ceph_new <- tbl_ceph_new %>% 
  plyr::arrange(match(region, order_region_s))
tbl_sxt_new <- tb_sxt[-c(2:4, 6:8)] #dropb columns 
tbl_sxt_new <- tbl_sxt_new %>% 
  plyr::arrange(match(region, order_region_s))
tbl_tet_new <- tb_tet[-c(2:4, 6:8)] #drops columns 
tbl_tet_new <- tbl_tet_new %>% 
  plyr::arrange(match(region, order_region_s))

#Remove SE asia + north africa middle east duplicates 
tbl_pen_new <- tbl_pen_new[-c(17:22),] #18:20, 22:24
tbl_mac_new <- tbl_mac_new[-c(18:20, 22:24),]
tbl_ceph_new <- tbl_ceph_new[-c(18:20, 22:24),]
tbl_sxt_new <- tbl_sxt_new[-c(18:20, 22:24),]
tbl_tet_new <- tbl_tet_new[-c(18:20, 22:24),]


tb1 <- full_join(tbl_pen_new, tbl_mac_new, by = "region")
tb1 <- full_join(tb1, tbl_sxt_new, by = "region")
tb1 <- full_join(tb1, tbl_ceph_new, by = "region")
tb1 <- full_join(tb1, tbl_tet_new, by = "region")

write.csv(tb1, "tb2.csv")

#calculate totals
amt_pen_ns <- sum(tbl_pen_ns$total_ns)
perc_pen_ns <- (sum(tbl_pen_ns$total_ns) / sum(tbl_pen_ns$total_isolates_sum)) * 100 
amt_pen_res <- sum(tbl_pen_res$total_res)
perc_pen_res <- (sum(tbl_pen_res$total_res) / sum(tbl_pen_res$total_isolates_sum)) * 100 
amt_pen_ns
perc_pen_ns
amt_pen_res
perc_pen_res

amt_mac_ns <- sum(tbl_mac_ns$total_ns)
perc_mac_ns <- (sum(tbl_mac_ns$total_ns) / sum(tbl_mac_ns$total_isolates_sum)) * 100 
amt_mac_res <- sum(tbl_mac_res$total_res)
perc_mac_res <- (sum(tbl_mac_res$total_res) / sum(tbl_mac_res$total_isolates_sum)) * 100 
amt_mac_ns
perc_mac_ns
amt_mac_res
perc_mac_res

amt_ceph_ns <- sum(tbl_ceph_ns$total_ns)
perc_ceph_ns <- (sum(tbl_ceph_ns$total_ns) / sum(tbl_ceph_ns$total_isolates_sum)) * 100 
amt_ceph_res <- sum(tbl_ceph_res$total_res)
perc_ceph_res <- (sum(tbl_ceph_res$total_res) / sum(tbl_ceph_res$total_isolates_sum)) * 100 
amt_ceph_ns
perc_ceph_ns
amt_ceph_res
perc_ceph_res

amt_tet_ns <- sum(tbl_tet_ns$total_ns)
perc_tet_ns <- (sum(tbl_tet_ns$total_ns) / sum(tbl_tet_ns$total_isolates_sum)) * 100 
amt_tet_res <- sum(tbl_tet_res$total_res)
perc_tet_res <- (sum(tbl_tet_res$total_res) / sum(tbl_tet_res$total_isolates_sum)) * 100 
amt_tet_ns
perc_tet_ns
amt_tet_res
perc_tet_res

amt_sxt_ns <- sum(tbl_sxt_ns$total_ns)
perc_sxt_ns <- (sum(tbl_sxt_ns$total_ns) / sum(tbl_sxt_ns$total_isolates_sum)) * 100 
amt_sxt_res <- sum(tbl_sxt_res$total_res)
perc_sxt_res <- (sum(tbl_sxt_res$total_res) / sum(tbl_sxt_res$total_isolates_sum)) * 100 
amt_sxt_ns
perc_sxt_ns
amt_sxt_res
perc_sxt_res



# 4.3 CREATE TABLE 1S- TOTAL NUMBER OF STUDIES STRATIFIED BY PRE VS. POST ----

#New 

#Create Functions 
ns_studies <- function(ns){
  tbl_region <- ns %>% 
    dplyr::group_by(region) %>%
    dplyr::summarize(study_no = length(unique(studyID)))
  
  tbl_super_region <- ns %>% 
    dplyr::group_by(super_region) %>%
    dplyr::summarize(study_no = length(unique(studyID)))
  names(tbl_super_region)[1] <- "region"
  
  tb_comb <- rbind(tbl_region, tbl_super_region)
  
  tb_comb <- tb_comb %>% 
    arrange(match(region, order_region_s))
  
  return(tb_comb)
}

res_studies <- function(res){
  tbl_region <- res %>% 
    dplyr::group_by(region) %>%
    dplyr::summarize(study_no = length(unique(studyID)))
  
  tbl_super_region <- res %>% 
    dplyr::group_by(super_region) %>%
    dplyr::summarize(study_no = length(unique(studyID)))
  names(tbl_super_region)[1] <- "region"
  
  tb_comb <- rbind(tbl_region, tbl_super_region)
  
  tb_comb <- tb_comb %>% 
    arrange(match(region, order_region_s))
  
  return(tb_comb)
}

#Loop through data frames 

#1. Penicillin 
ns_tb_study <- ns_studies(fulldta_table1_pen_ns)
res_tb_study <- res_studies(fulldta_table1_pen_res)

View(ns_tb_study)
View(res_tb_study)

#2. Macrolides 
ns_tb_study <- ns_studies(fulldta_table1_mac_ns)
res_tb_study <- res_studies(fulldta_table1_mac_res)

View(ns_tb_study)
View(res_tb_study)

#3. SXT 
ns_tb_study <- ns_studies(fulldta_table1_sxt_ns)
res_tb_study <- res_studies(fulldta_table1_sxt_res)

View(ns_tb_study)
View(res_tb_study)

#4. CEPH 
ns_tb_study <- ns_studies(fulldta_table1_ceph_ns)
res_tb_study <- res_studies(fulldta_table1_ceph_res)

View(ns_tb_study)
View(res_tb_study)

#5. TET 
ns_tb_study <- ns_studies(fulldta_table1_tet_ns)
res_tb_study <- res_studies(fulldta_table1_tet_res)

View(ns_tb_study)
View(res_tb_study)


#To get total studies with nonsusceptible and resistant data we do NOT want to filter by fulldta_nodup
fulldta_ns <- fulldta_table1_032410
fulldta_ns <- fulldta_ns[!(is.na(fulldta_ns$no_ns)), ] 

fulldta_res <- fulldta_table1_032410
fulldta_res <- fulldta_res[!(is.na(fulldta_res$no_res)), ] 

#NEW PEN
tbl_pen_ns_study <- fulldta_ns %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "penicillin") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_pen_ns = length(unique(studyID)))
View(tbl_pen_ns_study)

tbl_pen_ns_sregion_study <- fulldta_ns %>% 
  dplyr::filter(drug_class == "penicillin") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_pen_ns = length(unique(studyID)))
names(tbl_pen_ns_sregion_study)[1] <- "region"
tb_pen_ns_comb_study <- rbind(tbl_pen_ns_study, tbl_pen_ns_sregion_study)

tbl_pen_res_study <- fulldta_res %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "penicillin") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_pen_res = length(unique(studyID)))
#View(tbl_pen_ns_study)

tbl_pen_res_sregion_study <- fulldta_res %>% 
  dplyr::filter(drug_class == "penicillin") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_pen_res = length(unique(r_id)))

names(tbl_pen_res_sregion_study)[1] <- "region"
tb_pen_res_comb_study <- rbind(tbl_pen_res_study, tbl_pen_res_sregion_study)

tb_pen_study <- full_join(tb_pen_ns_comb_study, tb_pen_res_comb_study, by = "region")
#View(tb_pen_study)

#NEW Mac
tbl_mac_ns_study <- fulldta_ns %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "macrolide") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_mac_ns = length(unique(studyID)))
#View(tbl_mac_ns_study)

tbl_mac_ns_sregion_study <- fulldta_ns %>% 
  dplyr::filter(drug_class == "macrolide") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_mac_ns = length(unique(studyID)))
names(tbl_mac_ns_sregion_study)[1] <- "region"
tb_mac_ns_comb_study <- rbind(tbl_mac_ns_study, tbl_mac_ns_sregion_study)

tbl_mac_res_study <- fulldta_res %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "macrolide") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_mac_res = length(unique(studyID)))
#View(tbl_mac_ns_study)

tbl_mac_res_sregion_study <- fulldta_res %>% 
  dplyr::filter(drug_class == "macrolide") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_mac_res = length(unique(studyID)))

names(tbl_mac_res_sregion_study)[1] <- "region"
tb_mac_res_comb_study <- rbind(tbl_mac_res_study, tbl_mac_res_sregion_study)

tb_mac_study <- full_join(tb_mac_ns_comb_study, tb_mac_res_comb_study, by = "region")
#View(tb_mac_study)

#NEW SXT
tbl_sxt_ns_study <- fulldta_ns %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "SXT") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_sxt_ns = length(unique(studyID)))
View(tbl_sxt_ns_study)

tbl_sxt_ns_sregion_study <- fulldta_ns %>% 
  dplyr::filter(drug_class == "SXT") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_sxt_ns = length(unique(studyID)))
names(tbl_sxt_ns_sregion_study)[1] <- "region"
tb_sxt_ns_comb_study <- rbind(tbl_sxt_ns_study, tbl_sxt_ns_sregion_study)

tbl_sxt_res_study <- fulldta_res %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "SXT") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_sxt_res = length(unique(studyID)))
#View(tbl_sxt_ns_study)

tbl_sxt_res_sregion_study <- fulldta_res %>% 
  dplyr::filter(drug_class == "SXT") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_sxt_res = length(unique(studyID)))

names(tbl_sxt_res_sregion_study)[1] <- "region"
tb_sxt_res_comb_study <- rbind(tbl_sxt_res_study, tbl_sxt_res_sregion_study)

tb_sxt_study <- full_join(tb_sxt_ns_comb_study, tb_sxt_res_comb_study, by = "region")
#View(tb_sxt_study)

#NEW CEPH
tbl_ceph_ns_study <- fulldta_ns %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_ceph_ns = length(unique(studyID)))
#View(tbl_ceph_ns_study)

tbl_ceph_ns_sregion_study <- fulldta_ns %>% 
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_ceph_ns = length(unique(studyID)))
names(tbl_ceph_ns_sregion_study)[1] <- "region"
tb_ceph_ns_comb_study <- rbind(tbl_ceph_ns_study, tbl_ceph_ns_sregion_study)

tbl_ceph_res_study <- fulldta_res %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_ceph_res = length(unique(studyID)))
#View(tbl_ceph_ns_study)

tbl_ceph_res_sregion_study <- fulldta_res %>% 
  dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_ceph_res = length(unique(r_id)))

names(tbl_ceph_res_sregion_study)[1] <- "region"
tb_ceph_res_comb_study <- rbind(tbl_ceph_res_study, tbl_ceph_res_sregion_study)

tb_ceph_study <- full_join(tb_ceph_ns_comb_study, tb_ceph_res_comb_study, by = "region")
#View(tb_ceph_study)

#NEW TET
tbl_tet_ns_study <- fulldta_ns %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "tetracycline") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_tet_ns = length(unique(studyID)))
View(tbl_tet_ns_study)

tbl_tet_ns_sregion_study <- fulldta_ns %>% 
  dplyr::filter(drug_class == "tetracycline") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_tet_ns = length(unique(studyID)))
names(tbl_tet_ns_sregion_study)[1] <- "region"
tb_tet_ns_comb_study <- rbind(tbl_tet_ns_study, tbl_tet_ns_sregion_study)

tbl_tet_res_study <- fulldta_res %>% 
  dplyr::select(studyID, region, super_region, drug_class, total_isolates_drug, no_res, no_ns) %>% 
  dplyr::filter(drug_class == "tetracycline") %>% 
  dplyr::group_by(region) %>%
  dplyr::summarize(study_tet_res = length(unique(studyID)))
#View(tbl_tet_ns_study)

tbl_tet_res_sregion_study <- fulldta_res %>% 
  dplyr::filter(drug_class == "tetracycline") %>% 
  dplyr::group_by(super_region) %>%
  dplyr::summarize(study_tet_res = length(unique(studyID)))

names(tbl_tet_res_sregion_study)[1] <- "region"
tb_tet_res_comb_study <- rbind(tbl_tet_res_study, tbl_tet_res_sregion_study)

tb_tet_study <- full_join(tb_tet_ns_comb_study, tb_tet_res_comb_study, by = "region")
#View(tb_tet_study)

#Bind data frames together, but first ORDER 


tb_pen_study_new <- tb_pen_study %>% 
  plyr::arrange(match(region, order_region_s))
tb_mac_study_new <- tb_mac_study %>% 
  plyr::arrange(match(region, order_region_s))
tb_sxt_study_new <- tb_sxt_study %>% 
  plyr::arrange(match(region, order_region_s))
tb_ceph_study_new <- tb_ceph_study %>% 
  plyr::arrange(match(region, order_region_s))
tb_tet_study_new <- tb_tet_study %>% 
  plyr::arrange(match(region, order_region_s))

#Remove SE asia + north africa middle east duplicates 
tb_pen_study_new <- tb_pen_study_new[-c(18:20, 22:24),]
tb_mac_study_new <- tb_mac_study_new[-c(18:20, 22:24),]
tb_sxt_study_new <- tb_sxt_study_new[-c(18:20, 22:24),]
tb_ceph_study_new <- tb_ceph_study_new[-c(18:20, 22:24),]
tb_tet_study_new <- tb_tet_study_new[-c(18:20, 22:24),]


tb1s <- full_join(tb_pen_study_new, tb_mac_study_new, by = "region")
tb1s <- full_join(tb1s, tb_sxt_study_new, by = "region")
tb1s <- full_join(tb1s, tb_ceph_study_new, by = "region")
tb1s <- full_join(tb1s, tb_tet_study_new, by = "region")

write.csv(tb1s, "tb2_main.csv")



#4.4 Create Tabls S6- total serotype studies ----

st_dta <- read.csv("fulldta_st_b_new.csv")
length(unique(st_dta$studyID))
length(unique(st_dta$doi))


#5. RUNNING **PENICILLIN**  MODELS FOR FOREST PLOTS + CREATING DATA FILE FOR FOREST PLOT CODE  ----
# 5.2 Penicillin + Nonsusceptible + Invasive + Pre,boundary singular fit error   ----


# check <- fulldta_ns %>% 
#   filter(prepost == "naive") %>% 
#   filter(drug_class == "penicillin") %>% 
#   filter(isolate_type == 1) %>% 
#   filter(region == "southern_subsaharan_africa") 
# nrow(check)
# check <- check %>% 
#   dplyr::select(r_id, total_isolates_drug, no_ns)
# View(check)

###Test adding isolate type as fixed effect  
# dta_loop_pre <- novax_ns %>% 
#   filter(drug == 9)
# 
# model_pre_loop <- lmer(ns ~ (1 | r_id) + isolate_type + region, data = dta_loop_pre)
# model_int <- lmer(ns ~ (1 | r_id) + isolate_type * region, data = dta_loop_pre) #AIC LOWER 
# AIC(model_pre_loop, model_int)

####

# save(novax_ns, file = "novax_ns.rda")

matrix_pen_inv_ns_pre <- matrix(rep(NA), nrow= 20, ncol= 11)

#load("output/02-03-20/novax_ns.rda") 
#load("output/02-03-20/postvax_ns.rda") 
#load("output/02-03-20/df_pen_ninv_ns_prepost_comb.RData")

for (i in 1:length(gbd_list)){
  dta_loop_pre <- novax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  # View(dta_loop_pre)
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre) #, family = poisson(link = 'log')
    #model_pre_loop <- glm(ns ~ r_id, family = poisson(link = 'log'), data = (dta_loop_pre))
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    # id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    #model_pre_loop <- geeglm(ns ~ r_id, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #id=r_id, corstr = "exchangeable") 
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    #coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    #vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1]) 
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output) #remove exp
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_inv_ns_pre[i, 1] <- output_exp[1]
    matrix_pen_inv_ns_pre[i, 2] <- output_exp[2]
    matrix_pen_inv_ns_pre[i, 3] <- "penicillin"
    matrix_pen_inv_ns_pre[i, 4] <- gbd_list[i]
    matrix_pen_inv_ns_pre[i, 5] <- "invasive"
    matrix_pen_inv_ns_pre[i, 6] <- "ns"
    matrix_pen_inv_ns_pre[i, 7] <- total_cases
    matrix_pen_inv_ns_pre[i, 8] <- total_denom
    matrix_pen_inv_ns_pre[i, 9] <- study_arms
    matrix_pen_inv_ns_pre[i, 10] <- "pre"
    matrix_pen_inv_ns_pre[i, 11] <- output_exp[3]
    
  }
  else {
    mean <- mean(dta_loop_pre$ns)
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_inv_ns_pre[i, 1] <- mean
    matrix_pen_inv_ns_pre[i, 2] <- ci_lb_prop
    matrix_pen_inv_ns_pre[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_inv_ns_pre[i, 3] <- "penicillin"
    matrix_pen_inv_ns_pre[i, 4] <- gbd_list[i]
    matrix_pen_inv_ns_pre[i, 7] <- total_cases
    matrix_pen_inv_ns_pre[i, 8] <- total_denom
    matrix_pen_inv_ns_pre[i, 9] <- study_arms
    matrix_pen_inv_ns_pre[i, 5] <- "invasive"
    matrix_pen_inv_ns_pre[i, 6] <- "ns"
    matrix_pen_inv_ns_pre[i, 10] <- "pre"
    
  }
}


#Create cleaned frame 
df_pen_inv_ns_pre<- data.frame(matrix_pen_inv_ns_pre, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_inv_ns_pre) <- list_rownames
df_pen_inv_ns_pre[,4] <- gbd_list
#
# 5.3 Penicillin + Nonsusceptible + Invasive + Post, boundary singular fit error  ---- 

matrix_pen_inv_ns_post <- matrix(rep(NA), nrow= 20, ncol= 11) #change to 20

for (i in 1:length(gbd_list)){
  dta_loop_post <- postvax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_post)
  
  #if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
  #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
  #if((length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) != length(unique(dta_loop_post$r_id))) ){ 
  #model_post_loop <- lmer(ns ~ (1 | id), data = dta_loop_post)
  if (length(unique(dta_loop_post$r_id))>1) {
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop) #for glmer
    vars <- (diag(vcov(model_post_loop))) #for glmer
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    # id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    
    #coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    #vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_inv_ns_post[i, 1] <- output_exp[1]
    matrix_pen_inv_ns_post[i, 2] <- output_exp[2]
    matrix_pen_inv_ns_post[i, 3] <- "penicillin"
    matrix_pen_inv_ns_post[i, 4] <- gbd_list[i]
    matrix_pen_inv_ns_post[i, 5] <- "invasive"
    matrix_pen_inv_ns_post[i, 6] <- "ns"
    matrix_pen_inv_ns_post[i, 7] <- total_cases
    matrix_pen_inv_ns_post[i, 8] <- total_denom
    matrix_pen_inv_ns_post[i, 9] <- study_arms
    matrix_pen_inv_ns_post[i, 10] <- "post"
    matrix_pen_inv_ns_post[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_inv_ns_post[i, 1] <- mean
    matrix_pen_inv_ns_post[i, 2] <- ci_lb_prop
    matrix_pen_inv_ns_post[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_inv_ns_post[i, 3] <- "penicillin"
    matrix_pen_inv_ns_post[i, 4] <- gbd_list[i]
    matrix_pen_inv_ns_post[i, 7] <- total_cases
    matrix_pen_inv_ns_post[i, 8] <- total_denom
    matrix_pen_inv_ns_post[i, 9] <- study_arms
    matrix_pen_inv_ns_post[i, 5] <- "invasive"
    matrix_pen_inv_ns_post[i, 6] <- "ns"
    matrix_pen_inv_ns_post[i, 10] <- "post"
  }
}


#Create cleaned frame 
df_pen_inv_ns_post<- data.frame(matrix_pen_inv_ns_post, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", 
                   "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_inv_ns_post) <- list_rownames
df_pen_inv_ns_post[,4] <- gbd_list


# 5.4 Penicillin + Nonsusceptible + Noninvasive + Pre  ---- 
matrix_pen_ninv_ns_pre <- matrix(rep(NA), nrow= 20, ncol= 11)

for (i in 1:length(gbd_list)){
  dta_loop_pre <- novax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive  
  #View(dta_loop_pre)
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    #model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #    id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_ninv_ns_pre[i, 1] <- output_exp[1]
    matrix_pen_ninv_ns_pre[i, 2] <- output_exp[2]
    matrix_pen_ninv_ns_pre[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_pre[i, 4] <- gbd_list[i]
    matrix_pen_ninv_ns_pre[i, 5] <- "noninvasive"
    matrix_pen_inv_ns_pre[i, 6] <- "ns"
    matrix_pen_ninv_ns_pre[i, 7] <- total_cases
    matrix_pen_ninv_ns_pre[i, 8] <- total_denom
    matrix_pen_ninv_ns_pre[i, 9] <- study_arms
    matrix_pen_ninv_ns_pre[i, 10] <- "pre"
    matrix_pen_ninv_ns_pre[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_ninv_ns_pre[i, 1] <- mean
    matrix_pen_ninv_ns_pre[i, 2] <- ci_lb_prop
    matrix_pen_ninv_ns_pre[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_ninv_ns_pre[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_pre[i, 4] <- gbd_list[i]
    matrix_pen_ninv_ns_pre[i, 7] <- total_cases
    matrix_pen_ninv_ns_pre[i, 8] <- total_denom
    matrix_pen_ninv_ns_pre[i, 9] <- study_arms
    matrix_pen_ninv_ns_pre[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_pre[i, 6] <- "ns"
    matrix_pen_ninv_ns_pre[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_pen_ninv_ns_pre<- data.frame(matrix_pen_ninv_ns_pre, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_ninv_ns_pre) <- list_rownames
df_pen_ninv_ns_pre[,4] <- gbd_list

# 5.5 Penicillin + Nonsusceptible + Noninvasive + Post  ----

matrix_pen_ninv_ns_post <- matrix(rep(NA), nrow= 20, ncol= 11)

for (i in 1:length(gbd_list)){
  dta_loop_post <- postvax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive 
  #View(dta_loop_post)
  
  #if(length(unique(dta_loop_post$id))>1){ #use new if statement below to account of studies with <1 value
  #if( (length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
  #model_post_loop <- lmer(ns ~ (1 | id), data = dta_loop_post)
  if (length(unique(dta_loop_post$r_id))>1) {
    model_post_loop <- lmer(ns ~ (1 | r_id),  data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    # 
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_ninv_ns_post[i, 1] <- output_exp[1]
    matrix_pen_ninv_ns_post[i, 2] <- output_exp[2] 
    matrix_pen_ninv_ns_post[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_post[i, 4] <- gbd_list[i]
    matrix_pen_ninv_ns_post[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_post[i, 6] <- "ns"
    matrix_pen_ninv_ns_post[i, 7] <- total_cases
    matrix_pen_ninv_ns_post[i, 8] <- total_denom
    matrix_pen_ninv_ns_post[i, 9] <- study_arms
    matrix_pen_ninv_ns_post[i, 10] <- "post"
    matrix_pen_ninv_ns_post[i, 11] <- output_exp[3] 
    
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_ninv_ns_post[i, 1] <- mean
    matrix_pen_ninv_ns_post[i, 2] <- ci_lb_prop
    matrix_pen_ninv_ns_post[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_ninv_ns_post[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_post[i, 4] <- gbd_list[i]
    matrix_pen_ninv_ns_post[i, 7] <- total_cases
    matrix_pen_ninv_ns_post[i, 8] <- total_denom
    matrix_pen_ninv_ns_post[i, 9] <- study_arms
    matrix_pen_ninv_ns_post[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_post[i, 6] <- "ns"
    matrix_pen_ninv_ns_post[i, 10] <- "post"
    
  }
}


#Create cleaned frame 
df_pen_ninv_ns_post<- data.frame(matrix_pen_ninv_ns_post, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_ninv_ns_post) <- list_rownames
df_pen_ninv_ns_post[,4] <- gbd_list

# 5.6 Penicillin + Nonsusceptible + Invasive + Pre SUPER REGION  ----

matrix_pen_inv_ns_pre_sregion <- matrix(rep(NA), nrow= 7, ncol= 11) #change to super region + 7 rows 

for (i in 1:length(sregion_list)){
  dta_loop_pre <- novax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_pre) 
  
  if (length(unique(dta_loop_pre$r_id))>1){
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    #model_pre_loop <- glm(ns ~ r_id, family = poisson(link = 'log'), data = (dta_loop_pre))
    
    coefs <- fixef(model_pre_loop) #for glmer
    vars <- (diag(vcov(model_pre_loop))) #for glmer and glm
    
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_pre_loop) #for gee and glm
    # vars <- model_pre_loop$robust.variance #for gee
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_inv_ns_pre_sregion[i, 1] <- output_exp[1]
    matrix_pen_inv_ns_pre_sregion[i, 2] <- output_exp[2]
    matrix_pen_inv_ns_pre_sregion[i, 3] <- "penicillin"
    matrix_pen_inv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_pen_inv_ns_pre_sregion[i, 5] <- "invasive"
    matrix_pen_inv_ns_pre_sregion[i, 6] <- "ns"
    matrix_pen_inv_ns_pre_sregion[i, 7] <- total_cases
    matrix_pen_inv_ns_pre_sregion[i, 8] <- total_denom
    matrix_pen_inv_ns_pre_sregion[i, 9] <- study_arms
    matrix_pen_inv_ns_pre_sregion[i, 10] <- "pre"
    matrix_pen_inv_ns_pre_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_inv_ns_pre_sregion[i, 1] <- mean
    matrix_pen_inv_ns_pre_sregion[i, 2] <- ci_lb_prop
    matrix_pen_inv_ns_pre_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_inv_ns_pre_sregion[i, 3] <- "penicillin"
    matrix_pen_inv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_pen_inv_ns_pre_sregion[i, 7] <- total_cases
    matrix_pen_inv_ns_pre_sregion[i, 8] <- total_denom
    matrix_pen_inv_ns_pre_sregion[i, 9] <- study_arms
    matrix_pen_inv_ns_pre_sregion[i, 5] <- "invasive"
    matrix_pen_inv_ns_pre_sregion[i, 6] <- "ns" #temporarily change this to sregion
    matrix_pen_inv_ns_pre_sregion[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_pen_inv_ns_pre_sregion<- data.frame(matrix_pen_inv_ns_pre_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_inv_ns_pre_sregion) <- list_rownames
df_pen_inv_ns_pre_sregion[,4] <- sregion_list


# 5.7 Penicillin + Nonsusceptible + Invasive + Post- SUPER REGION ---- 

matrix_pen_inv_ns_post_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop 
for (i in 1:length(sregion_list)){
  dta_loop_post <- postvax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_inv_ns_post_sregion[i, 1] <- output_exp[1]
    matrix_pen_inv_ns_post_sregion[i, 2] <- output_exp[2]
    matrix_pen_inv_ns_post_sregion[i, 3] <- "penicillin"
    matrix_pen_inv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_pen_inv_ns_post_sregion[i, 5] <- "invasive"
    matrix_pen_inv_ns_post_sregion[i, 6] <- "ns"
    matrix_pen_inv_ns_post_sregion[i, 7] <- total_cases
    matrix_pen_inv_ns_post_sregion[i, 8] <- total_denom
    matrix_pen_inv_ns_post_sregion[i, 9] <- study_arms
    matrix_pen_inv_ns_post_sregion[i, 10] <- "post"
    matrix_pen_inv_ns_post_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_inv_ns_post_sregion[i, 1] <- mean
    matrix_pen_inv_ns_post_sregion[i, 2] <- ci_lb_prop
    matrix_pen_inv_ns_post_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_inv_ns_post_sregion[i, 3] <- "penicillin"
    matrix_pen_inv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_pen_inv_ns_post_sregion[i, 7] <- total_cases
    matrix_pen_inv_ns_post_sregion[i, 8] <- total_denom
    matrix_pen_inv_ns_post_sregion[i, 9] <- study_arms
    matrix_pen_inv_ns_post_sregion[i, 5] <- "invasive"
    matrix_pen_inv_ns_post_sregion[i, 6] <- "ns" 
    matrix_pen_inv_ns_post_sregion[i, 10] <- "post"
    
  }
}  


#Create cleaned frame 
df_pen_inv_ns_post_sregion<- data.frame(matrix_pen_inv_ns_post_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", 
                   "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_inv_ns_post_sregion) <- list_rownames
df_pen_inv_ns_post_sregion[,4] <- sregion_list
#View(df_pen_inv_ns_post_sregion)


# 5.8 Penicillin + Nonsusceptible + Noninvasive + Pre + SUPER REGION ----

# check <- fulldta_ns %>% 
#   filter(prepost == "naive") %>% 
#   filter(drug_class == "penicillin") %>% 
#   filter(isolate_type == 1) %>% 
#   filter(super_region == "high_income") 
# nrow(check)
# check <- check %>% 
#   dplyr::select(r_id, total_isolates_drug, no_ns)
# View(check)

matrix_pen_ninv_ns_pre_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop  
for (i in 1:length(sregion_list)){
  dta_loop_pre <- novax_ns %>%
    filter(sregion == sregion_list[i]) %>% 
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive  
  #View(dta_loop_pre)
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    
    #  model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    #  
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_ninv_ns_pre_sregion[i, 1] <- output_exp[1]
    matrix_pen_ninv_ns_pre_sregion[i, 2] <- output_exp[2]
    matrix_pen_ninv_ns_pre_sregion[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_pen_ninv_ns_pre_sregion[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_pre_sregion[i, 6] <- "ns"
    matrix_pen_ninv_ns_pre_sregion[i, 7] <- total_cases
    matrix_pen_ninv_ns_pre_sregion[i, 8] <- total_denom
    matrix_pen_ninv_ns_pre_sregion[i, 9] <- study_arms
    matrix_pen_ninv_ns_pre_sregion[i, 10] <- "pre"
    matrix_pen_ninv_ns_pre_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_ninv_ns_pre_sregion[i, 1] <- mean
    matrix_pen_ninv_ns_pre_sregion[i, 2] <- ci_lb_prop
    matrix_pen_ninv_ns_pre_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_pen_ninv_ns_pre_sregion[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_pen_ninv_ns_pre_sregion[i, 7] <- total_cases
    matrix_pen_ninv_ns_pre_sregion[i, 8] <- total_denom
    matrix_pen_ninv_ns_pre_sregion[i, 9] <- study_arms
    matrix_pen_ninv_ns_pre_sregion[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_pre_sregion[i, 6] <- "ns"
    matrix_pen_ninv_ns_pre_sregion[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_pen_ninv_ns_pre_sregion<- data.frame(matrix_pen_ninv_ns_pre_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_ninv_ns_pre_sregion) <- list_rownames
df_pen_ninv_ns_pre_sregion[,4] <- sregion_list
#View(df_pen_ninv_ns_pre_sregion)

# 5.9 Penicillin + Nonsusceptible + Noninvasive + Post + SUPER REGION ----
matrix_pen_ninv_ns_post_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop 
for (i in 1:length(sregion_list)){
  dta_loop_post <- postvax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 9) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive 
  #View(dta_loop_post)
  
  #if(length(unique(dta_loop_post$id))>1){ #use new if statement below to account of studies with <1 value
  #if( (length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
  if (length(unique(dta_loop_post$r_id))>1) {
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post)
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                         id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    #  
    #  coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    #  vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_ninv_ns_post_sregion[i, 1] <- output_exp[1]
    matrix_pen_ninv_ns_post_sregion[i, 2] <- output_exp[2] 
    matrix_pen_ninv_ns_post_sregion[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_pen_ninv_ns_post_sregion[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_post_sregion[i, 6] <- "ns"
    matrix_pen_ninv_ns_post_sregion[i, 7] <- total_cases
    matrix_pen_ninv_ns_post_sregion[i, 8] <- total_denom
    matrix_pen_ninv_ns_post_sregion[i, 9] <- study_arms
    matrix_pen_ninv_ns_post_sregion[i, 10] <- "post"
    matrix_pen_ninv_ns_post_sregion[i, 11] <- output_exp[3] 
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_pen_ninv_ns_post_sregion[i, 1] <- mean
    matrix_pen_ninv_ns_post_sregion[i, 2] <- ci_lb_prop
    matrix_pen_ninv_ns_post_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_pen_ninv_ns_post_sregion[i, 3] <- "penicillin"
    matrix_pen_ninv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_pen_ninv_ns_post_sregion[i, 7] <- total_cases
    matrix_pen_ninv_ns_post_sregion[i, 8] <- total_denom
    matrix_pen_ninv_ns_post_sregion[i, 9] <- study_arms
    matrix_pen_ninv_ns_post_sregion[i, 5] <- "noninvasive"
    matrix_pen_ninv_ns_post_sregion[i, 6] <- "ns"
    matrix_pen_ninv_ns_post_sregion[i, 10] <- "post"
    
  }
}  

#Create cleaned frame 
df_pen_ninv_ns_post_sregion<- data.frame(matrix_pen_ninv_ns_post_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_pen_ninv_ns_post_sregion) <- list_rownames
df_pen_ninv_ns_post_sregion[,4] <- sregion_list
#View(df_pen_ninv_ns_post_sregion)



# 5.10. Bind together pre-post region & super region eestimates for penicillin, nonsusceptible to create final data frames ----

# 1. Penicillin + Nonsusceptible + Invasive + Pre/ Post- Super Region 

df_pen_inv_ns_prepost_sregion <- rbind(df_pen_inv_ns_pre_sregion, df_pen_inv_ns_post_sregion)
#View(df_pen_inv_ns_prepost_sregion)

df_pen_inv_ns_prepost_sregion <- df_pen_inv_ns_prepost_sregion %>% 
  arrange(sregion) 

# df_pen_inv_ns_prepost_sregion <- df_pen_inv_ns_prepost_sregion %>% 
#   mutate(ci_ub_new = ci_ub) #create new variable called ci_ub_new 

names(df_pen_inv_ns_prepost_sregion)[4] <- "region"  #rename from sregion to region 

df_pen_inv_ns_prepost_region <- rbind(df_pen_inv_ns_pre, df_pen_inv_ns_post)
df_pen_inv_ns_prepost_region <- df_pen_inv_ns_prepost_region %>% 
  arrange(region)
#View(df_pen_inv_ns_prepost_region)

df_pen_inv_ns_prepost_comb <- rbind(df_pen_inv_ns_prepost_sregion, df_pen_inv_ns_prepost_region) 
#df_pen_inv_ns_prepost_comb[nrow(df_pen_inv_ns_prepost_comb)+21,] <- NA
#View(df_pen_inv_ns_prepost_comb)

df_pen_inv_ns_prepost_comb <- df_pen_inv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 3:4, 19:20, 23:24, 49:50, 15:16, 5:6, 47:48, 
                                                           53:54, 25:26, 31:32, 13:14, 37:38, 7:8, 41:42, 11:12, 
                                                           27:28, 43:44, 39:40, 9:10, 29:30, 21:22, 1:2),]
View(df_pen_inv_ns_prepost_comb) #FINAL DATA FRAME 
save(df_pen_inv_ns_prepost_comb, file = "df_pen_inv_ns_prepost_comb.RData")

#2. Penicillin + Nonsusceptible + Noninvasive + Pre/ Post - Super Region 
df_pen_ninv_ns_prepost_sregion <- rbind(df_pen_ninv_ns_pre_sregion, df_pen_ninv_ns_post_sregion)
#View(df_pen_ninv_ns_prepost_sregion)

df_pen_ninv_ns_prepost_sregion <- df_pen_ninv_ns_prepost_sregion %>% 
  arrange(sregion) 

# df_pen_ninv_ns_prepost_sregion <- df_pen_ninv_ns_prepost_sregion %>% 
#   mutate(ci_ub_new = ci_ub) #create new variable called ci_ub_new 

names(df_pen_ninv_ns_prepost_sregion)[4] <- "region"  #rename from sregion to region 

df_pen_ninv_ns_prepost_region <- rbind(df_pen_ninv_ns_pre, df_pen_ninv_ns_post)
df_pen_ninv_ns_prepost_region <- df_pen_ninv_ns_prepost_region %>% 
  arrange(region)
#View(df_pen_ninv_ns_prepost_region)

df_pen_ninv_ns_prepost_comb <- rbind(df_pen_ninv_ns_prepost_sregion, df_pen_ninv_ns_prepost_region) 
#df_pen_ninv_ns_prepost_comb[nrow(df_pen_ninv_ns_prepost_comb)+21,] <- NA
#View(df_pen_ninv_ns_prepost_comb)

# df_pen_ninv_ns_prepost_comb <- df_pen_ninv_ns_prepost_comb[c(41:42, 47:48, 33:34, 17:18, 31:32, 51,  3:4, 52:53, 19:20, 23:24, 45:46, 15:16, 54, 5:6, 
#                                                              55:56, 43:44, 49:50, 29:30, 57, 13:14, 58:59, 35:36, 60, 7:8, 61:62, 37:38, 63, 11:12, 
#                                                              64:65, 25:26, 39:40, 67, 9:10, 68:69, 27:28, 21:22, 70, 1:2, 71),]
# View(df_pen_ninv_ns_prepost_comb) #FINAL DATA FRAME 


# df_pen_ninv_ns_prepost_comb <- df_pen_ninv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 55, 3:4, 56:57, 19:20, 23:24, 49:50, 58, 5:6, 59:60, 47:48, 
#                                                              53:54, 25:26, 31:32, 61, 13:14, 62:63, 37:38, 64, 7:8, 65:66, 41:42, 67, 9:10, 68:69, 
#                                                              27:28, 43:44, 39:40, 70, 9:10, 71:72, 29:30, 21:22, 73, 1:2),]

#NEW 
df_pen_ninv_ns_prepost_comb <- df_pen_ninv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 3:4, 19:20, 23:24, 49:50, 15:16, 5:6, 47:48, 
                                                             53:54, 25:26, 31:32, 13:14, 37:38, 7:8, 41:42, 11:12, 
                                                             27:28, 43:44, 39:40, 9:10, 29:30, 21:22, 1:2),]

View(df_pen_ninv_ns_prepost_comb) #FINAL DATA FRAME 

save(df_pen_ninv_ns_prepost_comb, file = "df_pen_ninv_ns_prepost_comb.RData")

# 5.11 Clean data file (dat and dat1) before inputting to plots ----

load("df_pen_inv_ns_prepost_comb.Rdata")
load("df_pen_ninv_ns_prepost_comb.Rdata")

dat = df_pen_inv_ns_prepost_comb #1. ORIGINAL - Penicillin Invasive Nonsusceptible 
dat2 = df_pen_ninv_ns_prepost_comb #2. New - Penicillin Noninvasive Nonsusceptible 

# Clean data for 0/n isolates

row.names(dat) <- 1:nrow(dat)

dat <- dat %>% 
  mutate(ci_lb_new = ifelse(ci_lb <= 0, 0, ci_lb)) %>% 
  mutate(ci_ub_new = ci_ub) 

#Make this an ifelse statement to filter out plots < 20 isolates
for (i in 1:nrow(dat)) {
  ifelse((as.numeric(dat$total_denom[i])<20), dat$ee[i] <- NaN, dat$ee[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$total_denom[i] <- 0, dat$total_denom[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$total_cases[i] <- 0, dat$total_cases[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$ci_ub_new[i] <- 0, dat$ci_ub_new[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$ci_lb_new[i] <- 0, dat$ci_lb_new[i]) 
  ifelse((as.numeric(dat$total_denom[i])<20), dat$study_arms[i] <- 0, dat$study_arms[i]) 
}
View(dat)

#Check confidence intervals here:
for (i in 1:nrow(dat)){
  ifelse(dat$total_cases[i] == 0 & dat$total_denom[i] != 0,  
         dat$ci_ub_new[i] <- qbeta(c(0.5, 0.025, 0.975), 0.5, as.numeric(dat$total_denom[i])+0.5)[3], 
         dat$ci_ub_new[i])
}

#dat[8, "ci_ub_new"] <- qbeta(c(0.5, 0.025, 0.975), 0.5, 14.5)[3] #Old manual CI code 

#Repeat process for dat2
dat2 = df_pen_ninv_ns_prepost_comb #2. New - Penicillin Noninvasive Nonsusceptible 
row.names(dat2) <- 1:nrow(dat2)

dat2 <- dat2 %>% 
  mutate(ci_lb_new = ifelse(ci_lb <= 0, 0, ci_lb)) %>% 
  mutate(ci_ub_new = ci_ub) 

#Reset confidence intervals using qbeta 
#dat2[16, "ci_ub_new"] <- qbeta(c(0.5, 0.025, 0.975), 0.5, 194.5)[3]

#Make this an ifelse statement to filter out plots < 20 isolates
for (i in 1:nrow(dat2)) {
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$ee[i] <- NaN, dat2$ee[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$total_denom[i] <- 0, dat2$total_denom[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$total_cases[i] <- 0, dat2$total_cases[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$ci_ub_new[i] <- 0, dat2$ci_ub_new[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$ci_lb_new[i] <- 0, dat2$ci_lb_new[i]) 
  ifelse((as.numeric(dat2$total_denom[i])<20), dat2$study_arms[i] <- 0, dat2$study_arms[i]) 
}
View(dat2)

#Check confidence intervals here:
for (i in 1:nrow(dat2)){
  ifelse(dat2$total_cases[i] == 0 & dat2$total_denom[i] != 0,  
         dat2$ci_ub_new[i] <- qbeta(c(0.5, 0.025, 0.975), 0.5, as.numeric(dat2$total_denom[i])+0.5)[3], 
         dat2$ci_ub_new[i])
}

save(dat, file ="forest_pen_inv.Rdata")
save(dat2, file = "forest_pen_ninv.Rdata")




# 6. RUNNING **MACROLIDE** MODELS FOR FOREST PLOTS + CREATING DATA FILE FOR FOREST PLOT CODE  ----
# 6.1 Macrolide + Nonsusceptible + Invasive + Pre, boundary singular fit   ----

# Note that this MODEL did not fit DUE TO CARRIBEAN 0/49.5 isolates from 2 studies
# Work around was removing carribean from loop and hard coding 0/49.5 w/ qbeta estimate for confidence intervals. 

#Test for interaction - model w/ interaction is stronger than model w/o interaction 
# dta_loop_pre <- novax_ns %>%
#   filter(drug == 8) #penicillin = 9, macrolide = 8
# 
# model_pre_loop <- lmer(ns ~ (1 | r_id) + isolate_type + region, data = dta_loop_pre)
# model_int <- lmer(ns ~ (1 | r_id) + isolate_type * region, data = dta_loop_pre) #AIC IS LOWER 
# AIC(model_pre_loop, model_int)
# 

matrix_mac_inv_ns_pre <- matrix(rep(NA), nrow= 19, ncol= 11) #change nrow to 19 to account for custom GBD list

#make custom GBD_list to exclude estimate for the carribean 

gbd_list_custom <- gbd_list[-3]

for (i in 1:length(gbd_list_custom)){
  dta_loop_pre <- novax_ns %>%
    filter(region == gbd_list_custom[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive
  #View(dta_loop_pre)
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_inv_ns_pre[i, 1] <- output_exp[1]
    matrix_mac_inv_ns_pre[i, 2] <- output_exp[2]
    matrix_mac_inv_ns_pre[i, 3] <- "macrolide"
    matrix_mac_inv_ns_pre[i, 4] <- gbd_list_custom[i]
    matrix_mac_inv_ns_pre[i, 5] <- "invasive"
    matrix_mac_inv_ns_pre[i, 6] <- "ns"
    matrix_mac_inv_ns_pre[i, 7] <- total_cases
    matrix_mac_inv_ns_pre[i, 8] <- total_denom
    matrix_mac_inv_ns_pre[i, 9] <- study_arms
    matrix_mac_inv_ns_pre[i, 10] <- "pre"
    matrix_mac_inv_ns_pre[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_inv_ns_pre[i, 1] <- mean
    matrix_mac_inv_ns_pre[i, 2] <- ci_lb_prop
    matrix_mac_inv_ns_pre[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_inv_ns_pre[i, 3] <- "macrolide"
    matrix_mac_inv_ns_pre[i, 4] <- gbd_list_custom[i]
    matrix_mac_inv_ns_pre[i, 7] <- total_cases
    matrix_mac_inv_ns_pre[i, 8] <- total_denom
    matrix_mac_inv_ns_pre[i, 9] <- study_arms
    matrix_mac_inv_ns_pre[i, 5] <- "invasive"
    matrix_mac_inv_ns_pre[i, 6] <- "ns"
    matrix_mac_inv_ns_pre[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_mac_inv_ns_pre<- data.frame(matrix_mac_inv_ns_pre, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_inv_ns_pre) <- list_rownames
df_mac_inv_ns_pre[,4] <- gbd_list_custom

#Add in estimate for carribean 
df_mac_inv_ns_pre[nrow(df_mac_inv_ns_pre) +1,] <- NA 
#Fill in values for the row manually 
df_mac_inv_ns_pre[20,1] <- 0
df_mac_inv_ns_pre[20,2] <- 0
df_mac_inv_ns_pre[20,3] <- "macrolide"
df_mac_inv_ns_pre[20,4] <- gbd_list[3]
df_mac_inv_ns_pre[20,5] <- "invasive"
df_mac_inv_ns_pre[20,6] <- "ns"
df_mac_inv_ns_pre[20,7] <- 0 
df_mac_inv_ns_pre[20,8] <- 49
df_mac_inv_ns_pre[20,9] <- 2
df_mac_inv_ns_pre[20,10] <- "pre"
df_mac_inv_ns_pre[20,11] <-  qbeta(c(0.975), 0.5, 49.5)

#Re-order to push carribean back up 
df_mac_inv_ns_pre <- df_mac_inv_ns_pre[c(1,2,20, 3:19),]
#View(df_mac_inv_ns_pre)

# 6.2 Macrolide + Nonsusceptible + Invasive + Post boundary singular fit   ---- 

#Checking that # of isolates reported in table is correct w/ fulldta set 
# check <- fulldta %>%
#   filter(prepost == "not_naive") %>%
#   filter(drug_class == "macrolide") %>%
#   filter(isolate_type == 1) %>%
#   filter(region == "hi_asia_pacific")
# nrow(check)
# View(check)
# check <- check %>%
#   dplyr::select(r_id, total_isolates_drug, no_ns)
# View(check)

matrix_mac_inv_ns_post <- matrix(rep(NA), nrow= 20, ncol= 11)

for (i in 1:length(gbd_list)){
  dta_loop_post <- postvax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region, gbd_list[10] for hi_asia_pacific 
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  
  # dta_loop_post2 <- dta_loop_post %>% 
  #   filter(r_id == 418) %>% 
  #   summarise(count = sum(ns))
  # dta_loop_post2
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    #if((length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) != length(unique(dta_loop_post$id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post) #see if this runs with r_id instead of id 
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_inv_ns_post[i, 1] <- output_exp[1]
    matrix_mac_inv_ns_post[i, 2] <- output_exp[2]
    matrix_mac_inv_ns_post[i, 3] <- "macrolide"
    matrix_mac_inv_ns_post[i, 4] <- gbd_list[i]
    matrix_mac_inv_ns_post[i, 5] <- "invasive"
    matrix_mac_inv_ns_post[i, 6] <- "ns"
    matrix_mac_inv_ns_post[i, 7] <- total_cases
    matrix_mac_inv_ns_post[i, 8] <- total_denom
    matrix_mac_inv_ns_post[i, 9] <- study_arms
    matrix_mac_inv_ns_post[i, 10] <- "post"
    matrix_mac_inv_ns_post[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_inv_ns_post[i, 1] <- mean
    matrix_mac_inv_ns_post[i, 2] <- ci_lb_prop
    matrix_mac_inv_ns_post[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_inv_ns_post[i, 3] <- "macrolide"
    matrix_mac_inv_ns_post[i, 4] <- gbd_list[i]
    matrix_mac_inv_ns_post[i, 7] <- total_cases
    matrix_mac_inv_ns_post[i, 8] <- total_denom
    matrix_mac_inv_ns_post[i, 9] <- study_arms
    matrix_mac_inv_ns_post[i, 5] <- "invasive"
    matrix_mac_inv_ns_post[i, 6] <- "ns"
    matrix_mac_inv_ns_post[i, 10] <- "post"
    
  }
}  


#Create cleaned frame 
df_mac_inv_ns_post<- data.frame(matrix_mac_inv_ns_post, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", 
                   "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_inv_ns_post) <- list_rownames
df_mac_inv_ns_post[,4] <- gbd_list


# 6.3 Macrolide + Nonsusceptible + Noninvasive + Pre, boundary singular fit  ---- 
matrix_mac_ninv_ns_pre <- matrix(rep(NA), nrow= 20, ncol= 11)

#loop 
for (i in 1:length(gbd_list)){
  dta_loop_pre <- novax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive  
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_ninv_ns_pre[i, 1] <- output_exp[1]
    matrix_mac_ninv_ns_pre[i, 2] <- output_exp[2]
    matrix_mac_ninv_ns_pre[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_pre[i, 4] <- gbd_list[i]
    matrix_mac_ninv_ns_pre[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_pre[i, 6] <- "ns"
    matrix_mac_ninv_ns_pre[i, 7] <- total_cases
    matrix_mac_ninv_ns_pre[i, 8] <- total_denom
    matrix_mac_ninv_ns_pre[i, 9] <- study_arms
    matrix_mac_ninv_ns_pre[i, 10] <- "pre"
    matrix_mac_ninv_ns_pre[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_ninv_ns_pre[i, 1] <- mean
    matrix_mac_ninv_ns_pre[i, 2] <- ci_lb_prop
    matrix_mac_ninv_ns_pre[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_ninv_ns_pre[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_pre[i, 4] <- gbd_list[i]
    matrix_mac_ninv_ns_pre[i, 7] <- total_cases
    matrix_mac_ninv_ns_pre[i, 8] <- total_denom
    matrix_mac_ninv_ns_pre[i, 9] <- study_arms
    matrix_mac_ninv_ns_pre[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_pre[i, 6] <- "ns"
    matrix_mac_ninv_ns_pre[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_mac_ninv_ns_pre<- data.frame(matrix_mac_ninv_ns_pre, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_ninv_ns_pre) <- list_rownames
df_mac_ninv_ns_pre[,4] <- gbd_list

# 6.4 Macrolide + Nonsusceptible + Noninvasive + Post, boundary singular fit   ----

matrix_mac_ninv_ns_post <- matrix(rep(NA), nrow= 20, ncol= 11)

#loop
for (i in 1:length(gbd_list)){
  dta_loop_post <- postvax_ns %>%
    filter(region == gbd_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive 
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_post)
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_ninv_ns_post[i, 1] <- output_exp[1]
    matrix_mac_ninv_ns_post[i, 2] <- output_exp[2] 
    matrix_mac_ninv_ns_post[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_post[i, 4] <- gbd_list[i]
    matrix_mac_ninv_ns_post[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_post[i, 6] <- "ns"
    matrix_mac_ninv_ns_post[i, 7] <- total_cases
    matrix_mac_ninv_ns_post[i, 8] <- total_denom
    matrix_mac_ninv_ns_post[i, 9] <- study_arms
    matrix_mac_ninv_ns_post[i, 10] <- "post"
    matrix_mac_ninv_ns_post[i, 11] <- output_exp[3] 
    
  }
  
  else {
    mean <- mean(dta_loop_post$ns)
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_ninv_ns_post[i, 1] <- mean
    matrix_mac_ninv_ns_post[i, 2] <- ci_lb_prop
    matrix_mac_ninv_ns_post[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_ninv_ns_post[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_post[i, 4] <- gbd_list[i]
    matrix_mac_ninv_ns_post[i, 7] <- total_cases
    matrix_mac_ninv_ns_post[i, 8] <- total_denom
    matrix_mac_ninv_ns_post[i, 9] <- study_arms
    matrix_mac_ninv_ns_post[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_post[i, 6] <- "ns"
    matrix_mac_ninv_ns_post[i, 10] <- "post"
    
  }
}  

#Create cleaned frame 
df_mac_ninv_ns_post<- data.frame(matrix_mac_ninv_ns_post, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "region", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_ninv_ns_post) <- list_rownames
df_mac_ninv_ns_post[,4] <- gbd_list

# 6.6 Macrolide + Nonsusceptible + Invasive + Pre SUPER REGION ----

matrix_mac_inv_ns_pre_sregion <- matrix(rep(NA), nrow= 7, ncol= 11) #change to super region + 7 rows 

for (i in 1:length(sregion_list)){
  dta_loop_pre <- novax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_pre) #has out when not using 
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_inv_ns_pre_sregion[i, 1] <- output_exp[1]
    matrix_mac_inv_ns_pre_sregion[i, 2] <- output_exp[2]
    matrix_mac_inv_ns_pre_sregion[i, 3] <- "macrolide"
    matrix_mac_inv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_mac_inv_ns_pre_sregion[i, 5] <- "invasive"
    matrix_mac_inv_ns_pre_sregion[i, 6] <- "ns"
    matrix_mac_inv_ns_pre_sregion[i, 7] <- total_cases
    matrix_mac_inv_ns_pre_sregion[i, 8] <- total_denom
    matrix_mac_inv_ns_pre_sregion[i, 9] <- study_arms
    matrix_mac_inv_ns_pre_sregion[i, 10] <- "pre"
    matrix_mac_inv_ns_pre_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_inv_ns_pre_sregion[i, 1] <- mean
    matrix_mac_inv_ns_pre_sregion[i, 2] <- ci_lb_prop
    matrix_mac_inv_ns_pre_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_inv_ns_pre_sregion[i, 3] <- "macrolide"
    matrix_mac_inv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_mac_inv_ns_pre_sregion[i, 7] <- total_cases
    matrix_mac_inv_ns_pre_sregion[i, 8] <- total_denom
    matrix_mac_inv_ns_pre_sregion[i, 9] <- study_arms
    matrix_mac_inv_ns_pre_sregion[i, 5] <- "invasive"
    matrix_mac_inv_ns_pre_sregion[i, 6] <- "ns" #temporarily change this to sregion
    matrix_mac_inv_ns_pre_sregion[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_mac_inv_ns_pre_sregion<- data.frame(matrix_mac_inv_ns_pre_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_inv_ns_pre_sregion) <- list_rownames
df_mac_inv_ns_pre_sregion[,4] <- sregion_list
#View(df_mac_inv_ns_pre_sregion)

# 6.7 Macrolide + Nonsusceptible + Invasive + Post- SUPER REGION ---- 

matrix_mac_inv_ns_post_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop 
for (i in 1:length(sregion_list)){
  dta_loop_post <- postvax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 1) #invasive 
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$r_id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    #if((length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) != length(unique(dta_loop_post$r_id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | id), data = dta_loop_post)
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_inv_ns_post_sregion[i, 1] <- output_exp[1]
    matrix_mac_inv_ns_post_sregion[i, 2] <- output_exp[2]
    matrix_mac_inv_ns_post_sregion[i, 3] <- "macrolide"
    matrix_mac_inv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_mac_inv_ns_post_sregion[i, 5] <- "invasive"
    matrix_mac_inv_ns_post_sregion[i, 6] <- "ns"
    matrix_mac_inv_ns_post_sregion[i, 7] <- total_cases
    matrix_mac_inv_ns_post_sregion[i, 8] <- total_denom
    matrix_mac_inv_ns_post_sregion[i, 9] <- study_arms
    matrix_mac_inv_ns_post_sregion[i, 10] <- "post"
    matrix_mac_inv_ns_post_sregion[i, 11] <- output_exp[3]
    
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_inv_ns_post_sregion[i, 1] <- mean
    matrix_mac_inv_ns_post_sregion[i, 2] <- ci_lb_prop
    matrix_mac_inv_ns_post_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_inv_ns_post_sregion[i, 3] <- "macrolide"
    matrix_mac_inv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_mac_inv_ns_post_sregion[i, 7] <- total_cases
    matrix_mac_inv_ns_post_sregion[i, 8] <- total_denom
    matrix_mac_inv_ns_post_sregion[i, 9] <- study_arms
    matrix_mac_inv_ns_post_sregion[i, 5] <- "invasive"
    matrix_mac_inv_ns_post_sregion[i, 6] <- "ns" 
    matrix_mac_inv_ns_post_sregion[i, 10] <- "post"
    
  }
}  

#Create cleaned frame 
df_mac_inv_ns_post_sregion<- data.frame(matrix_mac_inv_ns_post_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", 
                   "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_inv_ns_post_sregion) <- list_rownames
df_mac_inv_ns_post_sregion[,4] <- sregion_list
#View(df_mac_inv_ns_post_sregion)


# 6.8 Macrolide + Nonsusceptible + Noninvasive + Pre + SUPER REGION ----

matrix_mac_ninv_ns_pre_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop  
for (i in 1:length(sregion_list)){
  dta_loop_pre <- novax_ns %>%
    filter(sregion == sregion_list[i]) %>% 
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive  
  #View(dta_loop_pre)
  
  if (length(unique(dta_loop_pre$r_id))>1) {
    model_pre_loop <- lmer(ns ~ (1 | r_id), data = dta_loop_pre)
    
    coefs <- fixef(model_pre_loop)
    vars <- (diag(vcov(model_pre_loop))) 
    
    # model_pre_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_pre), 
    #                       id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_pre_loop) #THESE ARE THE NEWEST
    # vars <- model_pre_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_ninv_ns_pre_sregion[i, 1] <- output_exp[1]
    matrix_mac_ninv_ns_pre_sregion[i, 2] <- output_exp[2]
    matrix_mac_ninv_ns_pre_sregion[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_mac_ninv_ns_pre_sregion[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_pre_sregion[i, 6] <- "ns"
    matrix_mac_ninv_ns_pre_sregion[i, 7] <- total_cases
    matrix_mac_ninv_ns_pre_sregion[i, 8] <- total_denom
    matrix_mac_ninv_ns_pre_sregion[i, 9] <- study_arms
    matrix_mac_ninv_ns_pre_sregion[i, 10] <- "pre"
    matrix_mac_ninv_ns_pre_sregion[i, 11] <- output_exp[3]
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_pre$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_pre$ns)
    n <- length(dta_loop_pre$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_ninv_ns_pre_sregion[i, 1] <- mean
    matrix_mac_ninv_ns_pre_sregion[i, 2] <- ci_lb_prop
    matrix_mac_ninv_ns_pre_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_pre$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_pre$ns) 
    study_arms <- length(unique(dta_loop_pre$r_id))
    
    matrix_mac_ninv_ns_pre_sregion[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_pre_sregion[i, 4] <- sregion_list[i]
    matrix_mac_ninv_ns_pre_sregion[i, 7] <- total_cases
    matrix_mac_ninv_ns_pre_sregion[i, 8] <- total_denom
    matrix_mac_ninv_ns_pre_sregion[i, 9] <- study_arms
    matrix_mac_ninv_ns_pre_sregion[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_pre_sregion[i, 6] <- "ns"
    matrix_mac_ninv_ns_pre_sregion[i, 10] <- "pre"
    
  }
}

#Create cleaned frame 
df_mac_ninv_ns_pre_sregion<- data.frame(matrix_mac_ninv_ns_pre_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_ninv_ns_pre_sregion) <- list_rownames
df_mac_ninv_ns_pre_sregion[,4] <- sregion_list
#View(df_mac_ninv_ns_pre_sregion)

# 6.9 Macrolide + Nonsusceptible + Noninvasive + Post + SUPER REGION ----
matrix_mac_ninv_ns_post_sregion <- matrix(rep(NA), nrow= 7, ncol= 11)

#Loop 
for (i in 1:length(sregion_list)){
  dta_loop_post <- postvax_ns %>%
    filter(sregion == sregion_list[i]) %>% #create an estimate for each region
    filter(drug == 8) %>% #penicillin = 9, macrolide = 8
    filter(isolate_type == 0) #noninvasive 
  #View(dta_loop_post)
  
  if(length(unique(dta_loop_post$id))>1){ #use new if statement below to account of studies with <1 value
    #if( (length(unique(dta_loop_post$r_id))>1) & (sum(dta_loop_post$ns) > length(unique(dta_loop_post$id))) ){ 
    model_post_loop <- lmer(ns ~ (1 | id), data = dta_loop_post)
    #model_post_loop <- glmer(ns ~ (1 | r_id), family = poisson(link = 'log'), data = dta_loop_post)
    
    coefs <- fixef(model_post_loop)
    vars <- (diag(vcov(model_post_loop))) 
    
    # model_post_loop <- gee(ns ~ 1, family = poisson(link = 'log'), data = (dta_loop_post), 
    #                        id = r_id, corstr = "exchangeable") #THIS IS THE NEWEST MODEL
    # 
    # coefs <- coef(model_post_loop) #THESE ARE THE NEWEST
    # vars <- model_post_loop$robust.variance #THESE ARE THE NEWEST 
    # 
    library(MASS)
    set.seed(123)
    pars = mvrnorm(1e5, coefs[1], vars[1])
    est = pars[,1]
    output = quantile(est, c(0.5, 0.025, 0.975))
    output_exp = (output)
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_ninv_ns_post_sregion[i, 1] <- output_exp[1]
    matrix_mac_ninv_ns_post_sregion[i, 2] <- output_exp[2] 
    matrix_mac_ninv_ns_post_sregion[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_mac_ninv_ns_post_sregion[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_post_sregion[i, 6] <- "ns"
    matrix_mac_ninv_ns_post_sregion[i, 7] <- total_cases
    matrix_mac_ninv_ns_post_sregion[i, 8] <- total_denom
    matrix_mac_ninv_ns_post_sregion[i, 9] <- study_arms
    matrix_mac_ninv_ns_post_sregion[i, 10] <- "post"
    matrix_mac_ninv_ns_post_sregion[i, 11] <- output_exp[3] 
    
  }#closes if statement 
  else {
    mean <- mean(dta_loop_post$ns)
    #n <- length(dta_loop_pre$ns)
    #ci_lb_prop <- mean - 1.96 * sqrt( ((mean)*(1-mean)) / n)
    #ci_ub_prop <- mean + 1.96 * sqrt( ((mean)*(1-mean)) / n)
    
    x <- sum(dta_loop_post$ns)
    n <- length(dta_loop_post$ns)
    ci_lb_prop <- qbeta(.025, x , n-x+1 ) #qbeta(alpha/2,x,n-x+1) x=num of successes and n=num of trial
    ci_ub_prop <- qbeta(1-.025,x ,n-x+1) #Clopper-Pearson interval
    
    matrix_mac_ninv_ns_post_sregion[i, 1] <- mean
    matrix_mac_ninv_ns_post_sregion[i, 2] <- ci_lb_prop
    matrix_mac_ninv_ns_post_sregion[i, 11] <- ci_ub_prop
    
    total_cases <- sum(dta_loop_post$ns, na.rm = TRUE)
    total_denom <- length(dta_loop_post$ns) 
    study_arms <- length(unique(dta_loop_post$r_id))
    
    matrix_mac_ninv_ns_post_sregion[i, 3] <- "macrolide"
    matrix_mac_ninv_ns_post_sregion[i, 4] <- sregion_list[i]
    matrix_mac_ninv_ns_post_sregion[i, 7] <- total_cases
    matrix_mac_ninv_ns_post_sregion[i, 8] <- total_denom
    matrix_mac_ninv_ns_post_sregion[i, 9] <- study_arms
    matrix_mac_ninv_ns_post_sregion[i, 5] <- "noninvasive"
    matrix_mac_ninv_ns_post_sregion[i, 6] <- "ns"
    matrix_mac_ninv_ns_post_sregion[i, 10] <- "post"
    
  }
}  

#Create cleaned frame 
df_mac_ninv_ns_post_sregion<- data.frame(matrix_mac_ninv_ns_post_sregion, stringsAsFactors = FALSE) #edited to add stringAsFactors = FALSE 
list_rownames <- c("ee", "ci_lb", "drug", "sregion", "inv_ninv", "res_ns", "total_cases", "total_denom", "study_arms", "pre_post", "ci_ub") 
colnames(df_mac_ninv_ns_post_sregion) <- list_rownames
df_mac_ninv_ns_post_sregion[,4] <- sregion_list
#View(df_mac_ninv_ns_post_sregion)


# 6.10 Bind together pre-post region & super region eestimates for Macrolide, nonsusceptible to create final data frames ----

#1. Macrolide + Nonsusceptible + Invasive + Pre/ Post - Super Region 

df_mac_inv_ns_prepost_sregion <- rbind(df_mac_inv_ns_pre_sregion, df_mac_inv_ns_post_sregion)
#View(df_mac_inv_ns_prepost_sregion)

df_mac_inv_ns_prepost_sregion <- df_mac_inv_ns_prepost_sregion %>% 
  arrange(sregion) 

# df_mac_inv_ns_prepost_sregion <- df_mac_inv_ns_prepost_sregion %>% 
#   mutate(ci_ub_new = ci_ub) #create new variable called ci_ub_new 

names(df_mac_inv_ns_prepost_sregion)[4] <- "region"  #rename from sregion to region 

df_mac_inv_ns_prepost_region <- rbind(df_mac_inv_ns_pre, df_mac_inv_ns_post)
df_mac_inv_ns_prepost_region <- df_mac_inv_ns_prepost_region %>% 
  arrange(region)
#View(df_pen_inv_ns_prepost_region)

df_mac_inv_ns_prepost_comb <- rbind(df_mac_inv_ns_prepost_sregion, df_mac_inv_ns_prepost_region) 
#df_mac_inv_ns_prepost_comb[nrow(df_mac_inv_ns_prepost_comb)+21,] <- NA
#View(df_mac_inv_ns_prepost_comb)

#reorder 
df_mac_inv_ns_prepost_comb <- df_mac_inv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 3:4, 19:20, 23:24, 49:50, 15:16, 5:6, 47:48, 
                                                           53:54, 25:26, 31:32, 13:14, 37:38, 7:8, 41:42, 11:12, 
                                                           27:28, 43:44, 39:40, 9:10, 29:30, 21:22, 1:2),]

#View(df_mac_inv_ns_prepost_comb)
save(df_mac_inv_ns_prepost_comb, file = "df_mac_inv_ns_prepost_comb.RData")

#2. Macrolide + Nonsusceptible + Noninvasive + Pre/ Post - Super Region 
df_mac_ninv_ns_prepost_sregion <- rbind(df_mac_ninv_ns_pre_sregion, df_mac_ninv_ns_post_sregion)
#View(df_pen_ninv_ns_prepost_sregion)

df_mac_ninv_ns_prepost_sregion <- df_mac_ninv_ns_prepost_sregion %>% 
  arrange(sregion) 

# df_mac_ninv_ns_prepost_sregion <- df_mac_ninv_ns_prepost_sregion %>% 
#   mutate(ci_ub_new = ci_ub) #create new variable called ci_ub_new 

names(df_mac_ninv_ns_prepost_sregion)[4] <- "region"  #rename from sregion to region 

df_mac_ninv_ns_prepost_region <- rbind(df_mac_ninv_ns_pre, df_mac_ninv_ns_post)
df_mac_ninv_ns_prepost_region <- df_mac_ninv_ns_prepost_region %>% 
  arrange(region)
#View(df_pen_ninv_ns_prepost_region)

df_mac_ninv_ns_prepost_comb <- rbind(df_mac_ninv_ns_prepost_sregion, df_mac_ninv_ns_prepost_region) 
#df_mac_ninv_ns_prepost_comb[nrow(df_mac_ninv_ns_prepost_comb)+21,] <- NA
#View(df_mac_ninv_ns_prepost_comb)

df_mac_ninv_ns_prepost_comb <- df_mac_ninv_ns_prepost_comb[c(45:46, 51:52, 35:36, 17:18, 33:34, 3:4, 19:20, 23:24, 49:50, 15:16, 5:6, 47:48, 
                                                             53:54, 25:26, 31:32, 13:14, 37:38, 7:8, 41:42, 11:12, 
                                                             27:28, 43:44, 39:40, 9:10, 29:30, 21:22, 1:2),]

#View(df_mac_ninv_ns_prepost_comb)

save(df_mac_ninv_ns_prepost_comb, file = "df_mac_ninv_ns_prepost_comb.RData")


# 6.11 Clean Macrolide Data file before uploading to plots ----

load("df_mac_inv_ns_prepost_comb.Rdata")
load("df_mac_ninv_ns_prepost_comb.Rdata")

dat3 = df_mac_inv_ns_prepost_comb #3. New - Macrolide Invasive Nonsusceptible- have not created
row.names(dat3) <- 1:nrow(dat3)

dat3 <- dat3 %>% 
  mutate(ci_lb_new = ifelse(ci_lb <= 0, 0, ci_lb)) %>% 
  mutate(ci_ub_new = ci_ub) 


#Make this an ifelse statement to filter out plots < 20 isolates
for (i in 1:nrow(dat3)) {
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$ee[i] <- NaN, dat3$ee[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$total_denom[i] <- 0, dat3$total_denom[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$total_cases[i] <- 0, dat3$total_cases[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$ci_ub_new[i] <- 0, dat3$ci_ub_new[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$ci_lb_new[i] <- 0, dat3$ci_lb_new[i]) 
  ifelse((as.numeric(dat3$total_denom[i])<20), dat3$study_arms[i] <- 0, dat3$study_arms[i]) 
}
#View(dat3)

#Check confidence intervals here:
for (i in 1:nrow(dat3)){
  ifelse(dat3$total_cases[i] == 0 & dat3$total_denom[i] != 0,  
         dat3$ci_ub_new[i] <- qbeta(c(0.5, 0.025, 0.975), 0.5, as.numeric(dat3$total_denom[i])+0.5)[3], 
         dat3$ci_ub_new[i])
}

#Repeat for dat5
dat4 = df_mac_ninv_ns_prepost_comb #4. New - Macrolide Noninvasive Nonsusceptible - have note created
row.names(dat4) <- 1:nrow(dat4)

dat4 <- dat4 %>% 
  mutate(ci_lb_new = ifelse(ci_lb <= 0, 0, ci_lb)) %>% 
  mutate(ci_ub_new = ci_ub) 

#Make this an ifelse statement to filter out plots < 20 isolates
for (i in 1:nrow(dat4)) {
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$ee[i] <- NaN, dat4$ee[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$total_denom[i] <- 0, dat4$total_denom[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$total_cases[i] <- 0, dat4$total_cases[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$ci_ub_new[i] <- 0, dat4$ci_ub_new[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$ci_lb_new[i] <- 0, dat4$ci_lb_new[i]) 
  ifelse((as.numeric(dat4$total_denom[i])<20), dat4$study_arms[i] <- 0, dat4$study_arms[i]) 
}
#View(dat4)

#Check confidence intervals here:
for (i in 1:nrow(dat4)){
  ifelse(dat4$total_cases[i] == 0 & dat4$total_denom[i] != 0,  
         dat4$ci_ub_new[i] <- qbeta(c(0.5, 0.025, 0.975), 0.5, as.numeric(dat4$total_denom[i])+0.5)[3], 
         dat4$ci_ub_new[i])
}

save(dat3, file = "forest_mac_inv.Rdata") #use these files in the forest plot code 
save(dat4, file = "forest_mac_ninv.Rdata") #use these files in the forest plot code 
