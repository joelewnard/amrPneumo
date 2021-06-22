# Clean last bunch of Serotype Only Studies 
# Buddhika abstracted these late November, early December 

#Libraries
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

#Load and Clean Data ----
#1. Load Data- first  need to merge with region, income, GDP 
setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe")
gbd_region_new <-  read_excel("data/gbd_region_new_r.xlsx")
world_bank_gdp_new <- read_excel("data/world_bank_gdp_new2.xlsx")
#pcv_intro <- read_excel("data/IVAC_PCV_Product_2020Feb11_Edited.xlsx") #I think the serotype analysis used a different IVAC sheet 
pcv_intro <- read_excel("data/IVAC_PCV_Product_2020Nov_Edited.xlsx") #I think the serotype analysis used a different IVAC sheet 


fulldta_st <- read.csv("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/data_st_dec2020.csv")
length(unique(fulldta_st$studyID)); length(unique(fulldta_st$doi)) #14 ST studies
length(which(is.na(fulldta_st$studyID)))

#Check that there aren't any countries that are not in PCV_Intro
levels(factor(fulldta_st$country[!(fulldta_st$country %in% pcv_intro$country)]))  #Taiwan , United States
levels(factor(fulldta_st$country[!(fulldta_st$country %in% gbd_region_new$country)]))  #United States
levels(factor(fulldta_st$country[!(fulldta_st$country %in% world_bank_gdp_new$country)]))  #Taiwan , United States

#Recode names of country 
fulldta_st$country <- as.character(fulldta_st$country)
fulldta_st$country[fulldta_st$country == "United States "] <- "United States of America"
fulldta_st$country[fulldta_st$country == "Taiwan"] <- "China"

#Recode drugs

#Clean drug_class variable!!
fulldta_st$drug_class <- as.character(fulldta_st$drug)
unique(fulldta_st$drug_class)
#fulldta$drug_class <- as.character(fulldta$drug_class)

fulldta_st$drug_class[fulldta_st$drug_class == "Penicillin"] <- "penicillin"
fulldta_st$drug_class[fulldta_st$drug_class == "Clarithromycin"] <- "macrolide"
fulldta_st$drug_class[fulldta_st$drug_class == "Erythromycin"] <- "macrolide"
fulldta_st$drug_class[fulldta_st$drug_class == "Amoxicillin"] <- "penicillin"
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

#Check this first: table(fulldta_st$prod, fulldta_st$yr_since_vax_new) 
table(fulldta_st$prod, fulldta_st$yr_since_vax_new, fulldta_st$vac_type_new) 

test <- fulldta_st %>% 
  filter(prod == "PCV7" & vac_type_new == "nvt" & serotype == "19A" |
           prod == "PCV10" & vac_type_new == "nvt" & serotype == "19A"|
           prod == "no_vax_intro" & vac_type_new == "nvt" & serotype == "19A")
#View(test)
length(unique(test$studyID)) #62 studies 
hist(test$yr_since_vax_new) #majority of these studies are before PCV has been implemented 
length(unique(test$studyID[test$yr_since_vax_new == 0])) #51 

test <- fulldta_st %>% 
  filter(prod == "PCV13" & vac_type_new == "vt" & serotype == "19A")
length(unique(test$studyID)) #6 studies 
hist(test$yr_since_vax_new) #pretty much no studies for 19A in post era! 

test <- fulldta_st %>% 
  filter(prod == "PCV13" & vac_type_new == "vt" )
hist(test$yr_since_vax_new) #only go 3 years out to years since vax for VT from PCV13 

test <- fulldta_st %>% 
  filter(prod == "PCV7" & vac_type_new == "vt" )
hist(test$yr_since_vax_new) #only go 3 years out to years since vax for VT from PCV13 
length(unique(test$studyID)) #16 studies

test <- fulldta_st %>% 
  group_by(vac_type_new, prod) %>% 
  dplyr::summarize(n = length(unique(studyID)))
#View(test)

test <- fulldta_ns_st %>% 
  group_by(country, vac_type_new) %>% 
  dplyr::summarize(n = length(unique(studyID)))
#View(test)

#Check germany issue
test <- fulldta_ns_st %>% 
  filter(country == "Germany") %>% 
  dplyr::select(studyID, country, region, vac_type_new, product_switch, yr_since_vax_new, sample_collection_startyear, sample_collection_endyear, intro_year_new, prod,  serotype, old_formula, old_formula_start, old_formula_start_new, current_formula, current_formula_start, current_formula_start_new, intro_year)
#View(test)


#Check how many studies had no vaccine introduced at start of study, but vaccine introduced by the end of the study
test <- fulldta_st %>% 
  mutate(intro_during_study = ifelse( (fulldta_st$sample_collection_startyear <  fulldta_st$intro_year & 
                                         fulldta_st$intro_year < fulldta_st$sample_collection_endyear), 1, 0)) %>% 
  dplyr::select(studyID, intro_during_study, sample_collection_startyear, sample_collection_endyear, intro_year) %>% 
  group_by(intro_during_study) %>% 
  dplyr::summarize(n = length(unique(studyID)))
#View(test)

fulldta_st <-  fulldta_st %>% 
  mutate(intro_during_study = ifelse( (fulldta_st$sample_collection_startyear <  fulldta_st$intro_year & 
                                         fulldta_st$intro_year < fulldta_st$sample_collection_endyear), 1, 0))
length(unique(fulldta_st$studyID[fulldta_st$intro_during_study ==1]))#17 studies 

fulldta_st_ids <- fulldta_st %>% 
  filter(intro_during_study == 1) %>% 
  dplyr::select(studyID, country, region, intro_during_study, vac_type_new, product_switch, yr_since_vax_new, sample_collection_startyear, sample_collection_endyear, intro_year_new, prod,  serotype, old_formula, old_formula_start, old_formula_start_new, current_formula, current_formula_start, current_formula_start_new, intro_year)
#View(fulldta_st_ids)

test <- fulldta_st %>% 
  dplyr::filter(drug_class == "macrolide" ) %>% 
  dplyr::select(studyID, country, region, vac_type_new, product_switch, yr_since_vax_new, sample_collection_startyear, intro_year_new, prod,  serotype, old_formula, old_formula_start, old_formula_start_new, current_formula, current_formula_start, current_formula_start_new, intro_year) 
#View(test)   

test <- fulldta_st %>% 
  dplyr::select(studyID, prod, serotype, vac_type_new, intro_year, sample_collection_startyear, sample_collection_endyear, product_switch, switch_date, old_formula, current_formula, intro_year_new, sample_collection_startyear, yr_since_vax_new)
#View(test)

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
#Try checking whether these classifications make sense

# SAVE FILE 
fulldta_st_nov2020 <- fulldta_st
save(fulldta_st_nov2020, file =  "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_st_nov2020.rData")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_st_nov2020.rData")
unique(fulldta_st_nov2020$studyID)
