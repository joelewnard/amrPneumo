#######################
## ADDITIONAL GPS DATA- DATA CLEANING FILE 
## DEC 08 2020
## Kristin Andrejko
#######################

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
library(varhandle)
library(MASS)
library(gee)

setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe")
fulldta_st <- read.csv("data/serotype_data_study3307.csv") #This included study ID 3307 
length(unique(fulldta_st$studyID)) #1
length(unique(fulldta_st$country)) #6
nrow(fulldta_st)

#Find total isolates (keep only unique arm_id)
gps_isolates <- fulldta_st %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE)
nrow(gps_isolates)

#Load and Clean Data ----
#1. Load Data- first  need to merge with region, income, GDP 
setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe")
gbd_region_new <-  read_excel("data/gbd_region_new_r.xlsx")
world_bank_gdp_new <- read_excel("data/world_bank_gdp_new2.xlsx")
#pcv_intro <- read_excel("data/IVAC_PCV_Product_2020Feb11_Edited.xlsx") #I think the serotype analysis used a different IVAC sheet 
pcv_intro <- read_excel("data/IVAC_PCV_Product_2020Nov_Edited.xlsx") #I think the serotype analysis used a different IVAC sheet 

#Check that there aren't any countries that are not in PCV_Intro
levels(factor(fulldta_st$country[!(fulldta_st$country %in% pcv_intro$country)]))  #Taiwan , United States
levels(factor(fulldta_st$country[!(fulldta_st$country %in% gbd_region_new$country)]))  #United States
levels(factor(fulldta_st$country[!(fulldta_st$country %in% world_bank_gdp_new$country)]))  #Taiwan , United States

#Clean drug_class variable!!
fulldta_st$drug_class <- as.character(fulldta_st$drug)
unique(fulldta_st$drug_class)
#fulldta$drug_class <- as.character(fulldta$drug_class)

fulldta_st$drug_class[fulldta_st$drug_class == "Penicillin"] <- "penicillin"
fulldta_st$drug_class[fulldta_st$drug_class == "Erythromycin"] <- "macrolide"
fulldta_st$drug_class[fulldta_st$drug_class == "SXT"] <- "SXT"
unique(fulldta_st$drug_class)

#1. Merge Data 
newmerge <- merge(gbd_region_new, pcv_intro, by = "country", all = T) #replace gbd_region_new with who_region
fulldta_st <- left_join(fulldta_st, newmerge, by = "country") #added all_data instead of wide_dta

length(unique(fulldta_st$studyID))

#2. Change format 
fulldta_st$serotype <- as.character(fulldta_st$serotype)
fulldta_st$current_formula <- as.character(fulldta_st$current_formula)
fulldta_st$old_formula <- as.character(fulldta_st$old_formula)

#3. Create midpoint_yr value 

fulldta_st <- fulldta_st %>%
  mutate(midpoint_yr = ((sample_collection_endyear - sample_collection_startyear) / 2) + sample_collection_startyear)

fulldta_st$midpoint_yr <- round(fulldta_st$midpoint_yr, digits = 0) #round midpoint yera up 

#4. Merge with GDP 
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

world_bank_gdp_melt$midpoint_yr <- as.numeric(levels(world_bank_gdp_melt$midpoint_yr))[world_bank_gdp_melt$midpoint_yr]

fulldta_st <- left_join(fulldta_st, world_bank_gdp_melt, by = c("country", "midpoint_yr"))
na_df <- fulldta_st[is.na(fulldta_st$gdp),] #check that this is zero 

#5. Create OLD Years Since Vax Variable- this is year since intro of any PCV product  
# fulldta_st <- fulldta_st %>%
#   mutate(yr_since_vax = case_when(as.numeric(sample_collection_startyear) -  as.numeric(intro_year) > 0 ~ sample_collection_startyear -  as.numeric(intro_year), 
#                                   as.numeric(sample_collection_startyear) -  as.numeric(intro_year) == 0 ~ 0, 
#                                   as.numeric(sample_collection_startyear) -  as.numeric(intro_year) < 0 ~  0,
#                                   is.na(intro_year) ~ 0)) #pre
# length(which(is.na(fulldta_st$yr_since_vax))) 
# 
# #6. Create OLD PrePost Variable
# fulldta_st <- fulldta_st %>% 
#   mutate(prepost_meta = case_when(yr_since_vax >= 1 ~ "post", 
#                                   yr_since_vax <= 0 ~ "pre"))

#7. Create Vaccine Type Variable Using PCV7 
PCV7_VT <- c("4", "6B", "9V", "14", "18C", "19F", "23F", "PCV7") 
fulldta_st <- fulldta_st %>%  
  dplyr::mutate(vac_type_pcv7 = ifelse (fulldta_st$serotype %in% PCV7_VT, "PCV7" , "Non-PCV7"))

#8 Identify PCV product in use during time study was conducted 

for (i in 1:nrow(fulldta_st)){ #sample_collection_startyear
  fulldta_st$prod[i] <-  ifelse(fulldta_st$intro_year[i] <= fulldta_st$sample_collection_startyear[i], ifelse(fulldta_st$product_switch[i] ==1, 
                                                                                                              ifelse(fulldta_st$sample_collection_endyear[i] <= fulldta_st$switch_date[i], fulldta_st$old_formula[i], fulldta_st$current_formula[i]), 
                                                                                                              fulldta_st$current_formula[i]), "no_vax_intro") #sample_collection_endyear to midpoint_year
}

view <- fulldta_st %>%
  group_by(prod, intro_year, sample_collection_startyear,sample_collection_endyear, switch_date, product_switch, old_formula_start_new, old_formula,current_formula) %>%
  summarize(n = n())
#View(view) #come back here and determine if this is appropriate, consider using midpoint h

#8.1 Classify isolates as VT vs. NVT, using information about what PCV product was in use during time study was conducted 

`%notin%` <- Negate(`%in%`) #create a not in function 
PCV7_VT <- c("4", "6B", "9V", "14", "18C", "19F", "23F", "PCV7") #create vector of possible values to identify "PCV7" serotypes 
PCV10_VT <-  c("1", "4", "5", "6B", "7F", "9V", "14", "18C", "19F", "23F", "PCV7", "PCV10")
PCV13_VT <- c("1", "3", "4", "5", "6A", "6B", "6A/B", "7F", "9V", "14", "18C","19A", "19F", "19A/F", "23F", "PCV7", "PCV10", "PCV13")


#8.2 create new variable vac_type_new that identifies serotypes as vt or nvt based off of prod introduced during study 

fulldta_st <- fulldta_st %>%  
  mutate(vac_type_new = case_when(fulldta_st$prod == "PCV7" & fulldta_st$serotype %in% PCV7_VT ~ "vt" , 
                                  fulldta_st$prod == "PCV10" & fulldta_st$serotype %in%  PCV10_VT ~ "vt", 
                                  fulldta_st$prod == "PCV13" & fulldta_st$serotype %in% PCV13_VT ~ "vt" , 
                                  fulldta_st$prod == "PCV7"  & fulldta_st$serotype %notin% PCV7_VT ~ "nvt" , 
                                  fulldta_st$prod == "PCV10" & fulldta_st$serotype %notin% PCV10_VT  ~ "nvt", 
                                  fulldta_st$prod == "PCV13" & fulldta_st$serotype %notin% PCV13_VT ~ "nvt", 
                                  fulldta_st$prod == "no_vax_intro" ~ "nvt",
                                  is.na(fulldta_st$prod) ~ "nvt"))

length(which(is.na(fulldta_st$vac_type_new)))
length(which(is.na(fulldta_st$studyID)))
table(fulldta_st$vac_type_new)
# view <- fulldta_st %>% #test to see if vac_type worked
#   dplyr::select(prod, serotype, vac_type, intro_year, sample_collection_startyear, sample_collection_endyear, product_switch, switch_date, old_formula, current_formula)
# View(view)

#Summarize issues from above classification (ex. where PCV7 is listed as the product, classified as PCV7_VT, but study explicitly states PCV13 or non-PCV13 serotypes)
view <- fulldta_st %>%
  group_by(prod, vac_type_new, serotype) %>%
  summarize(n = n())
#View(view)

table(fulldta_st$prod, fulldta_st$vac_type_new)

# Remove rows of data where serotpe is listed as "PCV10" or "PCV13" and product introduced during study is PCV7: unclear whether this is vt or nvt 
#Note: this is NOT an issue when prod is PCV10 and serotype is PCV13

fulldta_st <- fulldta_st[!(fulldta_st$prod == "PCV7" & fulldta_st$serotype == "PCV10"),]
length(which(is.na(fulldta_st$studyID)))
fulldta_st <- fulldta_st[!(fulldta_st$prod == "PCV7" & fulldta_st$serotype == "PCV13"),]
fulldta_st <- fulldta_st[!(is.na(fulldta_st$studyID)),]

(unique(fulldta_st$vac_type_new))
length(which(is.na(fulldta_st$vac_type_new)))


#NEW: create yr_since_vax that takes into account the vaccine introduced for each row. 

fulldta_st$old_formula_start_new <- format(as.Date(fulldta_st$old_formula_start, format="%m/%d/%y"),"%y")
fulldta_st$old_formula_start_new <- as.numeric(fulldta_st$old_formula_start_new) + 2000

fulldta_st$current_formula_start_new <- format(as.Date(fulldta_st$current_formula_start, format="%m/%d/%y"),"%y")
fulldta_st$current_formula_start_new <- as.numeric(fulldta_st$current_formula_start_new) + 2000

PCV13_7 <- c("1", "3", "5", "6A", "6A/B", "7F", "19A", "19A/F") #Serotypes in PCV13 but not PCV7
PCV10_7 <- c("1", "5", "7F") #Serotypes in PCV10 but not PCV7
PCV13_10 <- c("3", "6A", "6A/B" ,"19A", "19A/F")  #Serotypes in PCV13 but not PCV10

fulldta_st <- fulldta_st %>%  
  mutate(intro_year_new = ifelse( (fulldta_st$product_switch == 0), fulldta_st$intro_year, #if product switch = 0, use intro_date  
                                  ifelse( (fulldta_st$prod == "PCV13" & fulldta_st$old_formula == "PCV7" & serotype %in% PCV13_7), fulldta_st$current_formula_start_new,
                                          ifelse( (fulldta_st$prod == "PCV10" & fulldta_st$old_formula == "PCV7" & serotype %in% PCV10_7), fulldta_st$current_formula_start_new,
                                                  ifelse( (fulldta_st$prod == "PCV13" & fulldta_st$old_formula == "PCV10" & serotype %in% PCV13_10), fulldta_st$current_formula_start_new,
                                                          fulldta_st$intro_year)))))


test <- fulldta_st %>% 
  dplyr::select(prod, serotype, product_switch, old_formula, current_formula, current_formula_start,current_formula_start_new, intro_year, old_formula, old_formula_start_new, intro_year, intro_year_new)
#View(test)   

length(which(is.na(fulldta_st$intro_year_new))); length(which(is.na(fulldta_st$intro_year)))

#Now create new year_since_vax_new using custom intro_year_new
fulldta_st <- fulldta_st %>%
  mutate(yr_since_vax_new = case_when(as.numeric(sample_collection_startyear) -  as.numeric(intro_year_new) > 0 ~ sample_collection_startyear -  as.numeric(intro_year_new), 
                                      as.numeric(sample_collection_startyear) -  as.numeric(intro_year_new) == 0 ~ 0, 
                                      as.numeric(sample_collection_startyear) -  as.numeric(intro_year_new) < 0 ~  0,
                                      is.na(intro_year_new) ~ 0)) #pre
length(which(is.na(fulldta_st$yr_since_vax_new)))
table(fulldta_st$yr_since_vax_new)

#For VT: this is, for serotype X, years since introduction of PCV with serotype X”
#For NVT, this is years of vaccine use that did NOT contain this serotype 
#If there is no vaccine use- there is a zero for years since vaccination for both NVT and VT serotypes
#If there IS vaccine use and serotype is NVT- years of vaccine use with NVT serotype. 


#Try excluding studies where intro_during_study ==1
#fulldta_filt <- fulldta_st %>% 
#filter(intro_during_study == 0 | is.na(intro_during_study)) 
#length(unique(fulldta_filt$studyID)); length(unique(fulldta_st$studyID))
#View(fulldta_filt$intro_during_study)

#create new pre_post variable from new year_since_vax_new

# fulldta_st <- fulldta_st %>% 
#   mutate(prepost_new = case_when(yr_since_vax_new >= 1 ~ "post", 
#                                  yr_since_vax_new <= 0 ~ "pre"))
# table(fulldta_st$yr_since_vax_new, fulldta_st$prepost_new)

#Re do pre-post vairable such that it takes into account whether vaccine was introduced in same year as start = "post" 
#load("fulldta_st.rda")

fulldta_st <- fulldta_st %>% 
  mutate(prepost_new = case_when(yr_since_vax_new >= 1 ~ "post", 
                                 ( (yr_since_vax_new == 0)  & (sample_collection_startyear == intro_year_new) ) ~ "post", 
                                 ( (yr_since_vax_new == 0) & (intro_year_new > sample_collection_startyear) ) ~"pre"))

#table(fulldta_st$prepost_new2, fulldta_st$yr_since_vax_new, fulldta_st$vac_type_new)
table(fulldta_st$prepost_new, fulldta_st$yr_since_vax_new, fulldta_st$vac_type_new)

test <- fulldta_st %>% 
  filter(prepost_new == "pre") %>% 
  filter(vac_type_new == "vt") 
# dplyr::select(studyID, rowID, country, region, intro_during_study, vac_type_new, prepost_new2, product_switch, yr_since_vax_new, sample_collection_startyear, sample_collection_endyear, intro_year_new, prod,  serotype, old_formula, old_formula_start, old_formula_start_new, current_formula, current_formula_start, current_formula_start_new, intro_year)
#View(test)

test2 <- fulldta_st %>% 
  filter(prepost_new == "pre") %>% 
  filter(vac_type_new == "vt") %>% 
  dplyr::select(studyID, rowID, country, region, intro_during_study, vac_type_new, prepost_new, product_switch, yr_since_vax_new, sample_collection_startyear, sample_collection_endyear, intro_year_new, prod,  serotype, old_formula, old_formula_start, old_formula_start_new, current_formula, current_formula_start, current_formula_start_new, intro_year)
nrow(test2)#should be zero 

table(fulldta_st$prepost_new, fulldta_st$yr_since_vax_new, fulldta_st$vac_type_new) #should have 0 pre VT studies

#Variable I need for model: NS, vac_type, gdp, isolate_type, midpoint_yr, r_id (just studyID- make sure it doesn't overlap), region, yr_since_vax
table(fulldta_st$vac_type_new)
table(fulldta_st$gdp)
table(fulldta_st$isolate_type)
table(fulldta_st$midpoint_yr)
table(fulldta_st$studyID)
table(fulldta_st$region)
table(fulldta_st$yr_since_vax)

clsi_list <- c("National Committee for Clinical Laboratory Standards", 
               "National Committee for Clinical Laboratory Standards " ,
               "NCCLS", 
               "CLSI were used for sus, int, res; EUCAST was used for benzylpenicillin")

for(i in 1:nrow(fulldta_st)){
  fulldta_st$criteria[i] <- ifelse(fulldta_st$criteria_specify[i] %in%  clsi_list, 1, fulldta_st$criteria[i])
}

table(fulldta_st$criteria)

fulldta_st_gps2020 <- fulldta_st
save(fulldta_st_gps2020, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_st_gps2020.rData")

#### Create version of serotype file NOT for analysis, but just for COUNTING STUDIES and ISOLATES ####

library(dplyr)
setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe")
addt_93 <- read.csv("data/serotype_data_study3307.csv") #This included study ID 3307 


#pcv_intro <- read_excel("data/PCV Vaccine Intro.xlsx") #found tiny errors on intro date, update to new sheet
pcv_intro <- read_excel("data/PCV Vaccine Intro Updated Nov2020.xlsx") #found tiny errors on intro date, update to new sheetincome_dta <- read_excel("data/income_status_worldbank.xlsx")
gbd_region_new <-  read_excel("data/gbd_region_new_r.xlsx")
world_bank_gdp_new <- read_excel("data/world_bank_gdp_new2.xlsx")

all_data4 <- addt_93
length(unique(all_data4$studyID)) #93

#levels(factor(extra_data3$country[!(extra_data3$country %in% all_data2$country)]))   #this was used when merging names
levels(factor(all_data4$country[!(all_data4$country %in% world_bank_gdp_new$country)]))   #this was used when merging names

#1. Merge Data 
newmerge <- merge(gbd_region_new, pcv_intro, by = "country", all = T) #replace gbd_region_new with who_region

#2. Merge with wide_dta 
fulldta <- left_join(all_data4, newmerge, by = "country") #added all_data instead of wide_dta

length(unique(all_data4$country))
length(unique(fulldta$country))

#3. Merge with GDP per capital - haven't done this yet! 

#3.1 Items which are in fulldta which are not in world_bank_gdp_new 
levels(factor(fulldta$country[!(fulldta$country %in% gbd_region_new$country)]))  
levels(factor(fulldta$country[!(fulldta$country %in% pcv_intro$country)]))  
levels(factor(fulldta$country[!(fulldta$country %in% world_bank_gdp_new$country)]))  

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
fulldta[293, "gdp"] <- 15068.982 #Turkey GDP in 2020 
nrow(na_df)

#7. Add yr since vax  (used start year of sample collection rather than midpoint yr bc otherwise 90 studies had pre = 0 and yr since vax >0)
fulldta <- fulldta %>%
  mutate(yr_since_vax = case_when(as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) > 0 ~ sample_collection_startyear -  as.numeric(pcv_intro_year), 
                                  as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) == 0 ~ 0, 
                                  as.numeric(sample_collection_startyear) -  as.numeric(pcv_intro_year) < 0 ~ 0, 
                                  pcv_intro_year == "NI" ~ 0)) #pre

length(which(is.na(fulldta$yr_since_vax))) 

check <- fulldta %>% 
  dplyr::select(pcv_intro_year, sample_collection_endyear, sample_collection_startyear, yr_since_vax)
#View(check)

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

fulldta$r_id <- fulldta$r_id  + 6000 #add 450 to each of the r_id to not mess up the earlier stuff -> CHANGE TO 5000
unique(fulldta$r_id)

clsi_list <- c("National Committee for Clinical Laboratory Standards", 
               "National Committee for Clinical Laboratory Standards " ,
               "NCCLS", 
               "CLSI were used for sus, int, res; EUCAST was used for benzylpenicillin")

for(i in 1:nrow(fulldta)){
  fulldta$criteria_cl[i] <- ifelse(fulldta$criteria_specify[i] %in%  clsi_list, 1, fulldta$criteria[i])
}

#Clean drug_class variable!!
fulldta$drug_class <- as.character(fulldta$drug)

table(fulldta$drug, fulldta$drug_class)
extra_data_gps <- fulldta
length(unique(extra_data_gps$studyID)) #93
length(unique(extra_data_gps$r_id)) #93

save(extra_data_gps, file = "data/serotypeData_gps_tb1.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/serotypeData_gps_tb1.rda")