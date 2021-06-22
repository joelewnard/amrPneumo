#AMR SEROTYPE Full Analysis
#New Years Since Vax + Pre-Post Vairable Code
#A clean version of thils file from 04-14-20_code_AMr_STFinalAnalysis
#This is final code file to create the data frames for the ST plots in the main analysis!! 
# Also creates Table S9

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
#1. Load Data- this data set is cleaned and already has income, region, PCV data merged
fulldta_st <- read.csv("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_st_tbs6_041620.csv")
length(unique(fulldta_st$studyID)); length(unique(fulldta_st$doi)) #should be 135
length(which(is.na(fulldta_st$studyID)))

#2. Change format 
fulldta_st$serotype <- as.character(fulldta_st$serotype)
fulldta_st$current_formula <- as.character(fulldta_st$current_formula)
fulldta_st$old_formula <- as.character(fulldta_st$old_formula)

#3. Create midpoint_yr value 

fulldta_st <- fulldta_st %>%
  mutate(midpoint_yr = ((sample_collection_endyear - sample_collection_startyear) / 2) + sample_collection_startyear)

fulldta_st$midpoint_yr <- round(fulldta_st$midpoint_yr, digits = 0) #round midpoint yera up 

#4. Merge with GDP 
setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/01-SPAMR-RFiles/SPAMR-Box")
world_bank_gdp_new <- read_excel("merge_data/world_bank_gdp_new2.xlsx")
world_bank_gdp_new_v2 <- as.data.frame(world_bank_gdp_new)
world_bank_gdp_melt <- melt(data = world_bank_gdp_new_v2, 
                            id.vars= "country",
                            measure.vars= c("1973", "1974", "1974", "1978", "1979", "1980", "1981", "1982", 
                                            "1983", "1984", "1985", "1986", "1987", "1989", "1990", "1991", 
                                            "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999", 
                                            "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", 
                                            "2008", "2009", "2010", "2011", "2012", "2013", "2014", "2015", 
                                            "2016", "2017", "2018", "2019"),
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
View(view) #come back here and determine if this is appropriate, consider using midpoint h

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

#change teh five "pre" VT studies to POST 
fulldta_st[fulldta_st$studyID == 465 & fulldta_st$X == 268, "prepost_new"] <- "post"   
fulldta_st[fulldta_st$studyID == 465 & fulldta_st$X == 269, "prepost_new"] <- "post"   
fulldta_st[fulldta_st$studyID == 465 & fulldta_st$X == 270, "prepost_new"] <- "post"   
fulldta_st[fulldta_st$studyID == 406 & fulldta_st$X == 1089, "prepost_new"] <- "post"   
fulldta_st[fulldta_st$studyID == 406 & fulldta_st$X == 1090, "prepost_new"] <- "post"   

test2 <- fulldta_st %>% 
  filter(prepost_new == "pre") %>% 
  filter(vac_type_new == "vt") %>% 
  dplyr::select(studyID, rowID, country, region, intro_during_study, vac_type_new, prepost_new, product_switch, yr_since_vax_new, sample_collection_startyear, sample_collection_endyear, intro_year_new, prod,  serotype, old_formula, old_formula_start, old_formula_start_new, current_formula, current_formula_start, current_formula_start_new, intro_year)
nrow(test2)#should be zero 

table(fulldta_st$prepost_new, fulldta_st$yr_since_vax_new, fulldta_st$vac_type_new) #should have 0 pre VT studies

#  ------------------------ - FILL IN ------------------------ #
#ADD IN THE LAST BUNCH OF SEROTYPE ONLY STUDIES -> start studyID at 7000
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_st_nov2020.rData")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_st_gps2020.rData")

gpsData <- fulldta_st_gps2020 %>% 
  dplyr::select(studyID, arm_id, region, super_region, criteria, isolate_type, drug_class, serotype, serotype_isolates, no_ns, no_res, gdp, midpoint_yr, vac_type_new, yr_since_vax_new)

novData <- fulldta_st_nov2020 %>% 
  dplyr::select(studyID, arm_id, region, super_region, criteria, isolate_type, drug_class, serotype, serotype_isolates, no_ns, no_res, gdp, midpoint_yr, vac_type_new, yr_since_vax_new)

fulldta_st2 <- fulldta_st %>% 
  dplyr::select(studyID, arm_id, region, super_region, criteria, isolate_type, drug_class, serotype, serotype_isolates, no_ns, no_res, gdp, midpoint_yr, vac_type_new, yr_since_vax_new)

totalSerotype <- rbind(fulldta_st2, novData)
nrow(novData) + nrow(fulldta_st2); nrow(totalSerotype)

totalSerotype2 <- rbind(totalSerotype, gpsData)
nrow(totalSerotype) + nrow(gpsData); nrow(totalSerotype2)

totalSerotype <- totalSerotype2

#8.1 Number of TOTAL NS studies  
fulldta_ns_st <- totalSerotype
fulldta_ns_st <- fulldta_ns_st[!(is.na(fulldta_ns_st$no_ns)), ] 
length(unique(fulldta_ns_st$studyID)) #117 studies -> 131 -> 132

#8.2 Number of TOTAL RESISTANT studies  
fulldta_res_st <- totalSerotype
fulldta_res_st <- fulldta_res_st[!(is.na(fulldta_res_st$no_res)), ] 
length(unique(fulldta_res_st$studyID)) #60 studies -> 70 -> 71

save(totalSerotype, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_st.rda")
save(fulldta_ns_st, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_ns_st.rda")
save(fulldta_res_st, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_res_st.rda")

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_st.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_ns_st.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_res_st.rda")

length(unique(totalSerotype$studyID)) #150

#Initalize for loop vectors for nonsusceptible- only include variables included in final model:
# penNS_new = lmer(ns~vac_type+log(gdp)+isolate_type+midpoint_yr+(1|r_id)+(1|region)+
#                    sqrt(yr_since_vax)*vac_type,data=subset_penNS); summary(penNS_new); BIC(penNS_new)

serotype_ns = isolate_type_ns = drug_ns = ns = vac_type_ns = gdp_ns  = midpoint_yr_ns = r_id_ns = region_ns = yr_since_vax_ns = c()

for (i in 1:length(fulldta_ns_st$no_ns)) {
  ns = c(ns,rep(1,(fulldta_ns_st$no_ns[i])),
         rep(0,fulldta_ns_st$serotype_isolates[i] - fulldta_ns_st$no_ns[i])) #INCLUDE
  region_ns = c(region_ns, rep(fulldta_ns_st$region[i], fulldta_ns_st$serotype_isolates[i]))
  gdp_ns = c(gdp_ns, rep(fulldta_ns_st$gdp[i], fulldta_ns_st$serotype_isolates[i]))
  yr_since_vax_ns = c(yr_since_vax_ns, rep(fulldta_ns_st$yr_since_vax_new[i], fulldta_ns_st$serotype_isolates[i]))
  drug_ns = c(drug_ns, rep(fulldta_ns_st$drug_class[i], fulldta_ns_st$serotype_isolates[i]))
  isolate_type_ns = c(isolate_type_ns, rep(fulldta_ns_st$isolate_type[i], fulldta_ns_st$serotype_isolates[i]))
  r_id_ns = c(r_id_ns, rep(fulldta_ns_st$studyID[i], fulldta_ns_st$serotype_isolates[i]))
  midpoint_yr_ns = c(midpoint_yr_ns, rep(fulldta_ns_st$midpoint_yr[i], fulldta_ns_st$serotype_isolates[i]))
  serotype_ns = c(serotype_ns, rep(fulldta_ns_st$serotype[i], fulldta_ns_st$serotype_isolates[i]))
  vac_type_ns = c(vac_type_ns, rep(fulldta_ns_st$vac_type_new[i], fulldta_ns_st$serotype_isolates[i]))
  
}

out_fulldta_ns_st <- data.frame(ns, r_id_ns, region_ns, gdp_ns, yr_since_vax_ns, drug_ns, isolate_type_ns, midpoint_yr_ns, serotype_ns, vac_type_ns)
                      
          
#Repeat for resistant 
serotype_res = isolate_type_res = drug_res = res = vac_type_res = gdp_res  = midpoint_yr_res = r_id_res = region_res = yr_since_vax_res = c()

for (i in 1:length(fulldta_res_st$no_res)) {
  res = c(res,rep(1,(fulldta_res_st$no_res[i])),
         rep(0,fulldta_res_st$serotype_isolates[i] - fulldta_res_st$no_res[i])) #INCLUDE
  region_res = c(region_res, rep(fulldta_res_st$region[i], fulldta_res_st$serotype_isolates[i]))
  gdp_res = c(gdp_res, rep(fulldta_res_st$gdp[i], fulldta_res_st$serotype_isolates[i]))
  yr_since_vax_res = c(yr_since_vax_res, rep(fulldta_res_st$yr_since_vax_new[i], fulldta_res_st$serotype_isolates[i]))
  drug_res = c(drug_res, rep(fulldta_res_st$drug_class[i], fulldta_res_st$serotype_isolates[i]))
  isolate_type_res = c(isolate_type_res, rep(fulldta_res_st$isolate_type[i], fulldta_res_st$serotype_isolates[i]))
  r_id_res = c(r_id_res, rep(fulldta_res_st$studyID[i], fulldta_res_st$serotype_isolates[i]))
  midpoint_yr_res = c(midpoint_yr_res, rep(fulldta_res_st$midpoint_yr[i], fulldta_res_st$serotype_isolates[i]))
  serotype_res = c(serotype_res, rep(fulldta_res_st$serotype[i], fulldta_res_st$serotype_isolates[i]))
  vac_type_res = c(vac_type_res, rep(fulldta_res_st$vac_type_new[i], fulldta_res_st$serotype_isolates[i]))
}

out_fulldta_res_st <- data.frame(res, r_id_res, region_res, gdp_res, yr_since_vax_res, drug_res, isolate_type_res, midpoint_yr_res, serotype_res, vac_type_res)

colnames(out_fulldta_ns_st) <- c("ns", "r_id", "region", "gdp", "yr_since_vax", "drug", "isolate_type", "midpoint_yr", "serotype","vac_type")
colnames(out_fulldta_res_st) <- c("res", "r_id", "region", "gdp", "yr_since_vax", "drug", "isolate_type", "midpoint_yr", "serotype","vac_type")

out_fulldta_ns_st$serotype <- as.character(levels(out_fulldta_ns_st$serotype))[out_fulldta_ns_st$serotype]
out_fulldta_res_st$serotype <- as.character(levels(out_fulldta_res_st$serotype))[out_fulldta_res_st$serotype]

save(out_fulldta_ns_st, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/out_fulldta_ns_st.rda")
save(out_fulldta_res_st, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/out_fulldta_res_st.rda")
      
#Create Data frames for meta-regression models
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/out_fulldta_ns_st.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/out_fulldta_res_st.rda")

mod_pen_ns_st <- out_fulldta_ns_st %>%
  filter(drug == "2") %>%
  mutate(yr_2 = yr_since_vax^2) %>%
  mutate(yr_3 = yr_since_vax^3)

mod_mac_ns_st <- out_fulldta_ns_st %>%
  filter(drug == "1") %>%
  mutate(yr_2 = yr_since_vax^2) %>%
  mutate(yr_3 = yr_since_vax^3)

save(mod_pen_ns_st, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/mod_pen_ns_st.rda")
save(mod_mac_ns_st, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/mod_mac_ns_st.rda")


###############################################
#Counting the  number of studies for table S9 #
###############################################

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_ns_st.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_res_st.rda")

#8.3 Total number of NS isolates 
fulldta_st_ns_dist <- fulldta_ns_st %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) #filtering by serotype helps but removes other drugs 
sum(fulldta_st_ns_dist$serotype_isolates, na.rm = TRUE) #77113
length(unique(fulldta_ns_st$studyID))

#8.4 Total number of RES isolates 
fulldta_st_res_dist <- fulldta_res_st %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) #filtering by serotype helps but removes other drugs 
sum(fulldta_st_res_dist$serotype_isolates, na.rm = TRUE) #20,721
length(unique(fulldta_res_st$studyID))

#8.5 Total number of Penicillin Nonsusceptible Studies 
ns1_study <- fulldta_ns_st %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_new == "vt")  #vt serotype
# filter(prepost_new == "pre") #PCV not yet implemented 
length(unique(ns1_study$studyID)) 
#View(ns1_study)

ns2_study <- fulldta_ns_st %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_new == "nvt")  #nvt serotype
length(unique(ns2_study$studyID)) 

ns3_study <- fulldta_ns_st %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_new == "vt")  #vt serotype
length(unique(ns3_study$studyID)) 

ns4_study <- fulldta_ns_st %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_new == "nvt")  #nvt serotype
length(unique(ns4_study$studyID)) 

ns5_study <- fulldta_ns_st %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_new == "vt")  #vt serotype
length(unique(ns5_study$studyID)) 

ns6_study <- fulldta_ns_st %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_new == "nvt")  #nvt serotype
length(unique(ns6_study$studyID)) 

#Count isolates 

ns1_isolate <- fulldta_ns_st %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "vt") #VT 
sum(ns1_isolate$serotype_isolates, na.rm = TRUE) 

ns2_isolate <- fulldta_ns_st %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "nvt") #nVT 
sum(ns2_isolate$serotype_isolates, na.rm = TRUE) 

ns3_isolate <- fulldta_ns_st %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "vt") #VT 
sum(ns3_isolate$serotype_isolates, na.rm = TRUE) 

ns4_isolate <- fulldta_ns_st %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "nvt") #nVT 
sum(ns4_isolate$serotype_isolates, na.rm = TRUE) 

ns5_isolate <- fulldta_ns_st %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "vt") #VT 
sum(ns5_isolate$serotype_isolates, na.rm = TRUE) 

ns6_isolate <- fulldta_ns_st %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "nvt") #nVT 
sum(ns6_isolate$serotype_isolates, na.rm = TRUE) 


#Repest for RES: 
res1_study <- fulldta_res_st %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_new == "vt")  #vt serotype
length(unique(res1_study$studyID)) 

res2_study <- fulldta_res_st %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_new == "nvt")  #nvt serotype
length(unique(res2_study$studyID)) 

res3_study <- fulldta_res_st %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_new == "vt")  #vt serotype
length(unique(res3_study$studyID)) 

res4_study <- fulldta_res_st %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_new == "nvt")  #nvt serotype
length(unique(res4_study$studyID)) 

res5_study <- fulldta_res_st %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_new == "vt")  #vt serotype
length(unique(res5_study$studyID)) 

res6_study <- fulldta_res_st %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_new == "nvt")  #nvt serotype
length(unique(res6_study$studyID)) 

#Count isolates 

res1_isolate <- fulldta_res_st %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "vt") #VT 
sum(res1_isolate$serotype_isolates, na.rm = TRUE) 

res2_isolate <- fulldta_res_st %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "nvt") #nVT 
sum(res2_isolate$serotype_isolates, na.rm = TRUE) 

res3_isolate <- fulldta_res_st %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "vt") #VT 
sum(res3_isolate$serotype_isolates, na.rm = TRUE) 

res4_isolate <- fulldta_res_st %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "nvt") #nVT 
sum(res4_isolate$serotype_isolates, na.rm = TRUE) 

res5_isolate <- fulldta_res_st %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "vt") #VT 
sum(res5_isolate$serotype_isolates, na.rm = TRUE) 

res6_isolate <- fulldta_res_st %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "nvt") #nVT 
sum(res6_isolate$serotype_isolates, na.rm = TRUE) 



#Old below:

ns2_study <- fulldta_ns_st %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_new == "PCV7") %>% #PCV7 serotype
  filter(prepost_new == "post") #PCV implemented
length(unique(ns2_study$studyID)) #73

ns3_study <- fulldta_st_ns %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns3_study$studyID)) #90

ns4_study <- fulldta_st_ns %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
length(unique(ns4_study$studyID)) #22

#8.6 Total number of Penicillin Nonsusceptible ISOLATES  
#Note- that in this data set, each serotype is its own row so we don't want to use a distinct data set 
# ns1_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "penicillin") %>% 
#   filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
#   filter(prepost_meta == "pre") #PCV not yet implemented 
# sum(ns1_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns2_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "penicillin") %>% 
#   filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
#   filter(prepost_meta == "post") #PCV implemented 
# sum(ns2_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns3_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "penicillin") %>% 
#   filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
#   filter(prepost_meta == "pre") #PCV not yet implemented 
# sum(ns3_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns4_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "penicillin") %>% 
#   filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
#   filter(prepost_meta == "post") #PCV implemented 
# sum(ns4_isolate$serotype_isolates, na.rm = TRUE)
# 

#TEST 2- using new filter then distinct strategy
ns1_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_new == "vt") %>% #PCV7 serotype
  # filter(prepost_meta == "pre") #PCV not yet implemented 
  sum(ns1_isolate$serotype_isolates, na.rm = TRUE) #21,011

ns2_isolate <- fulldta_st_ns_dist %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns2_isolate$serotype_isolates, na.rm = TRUE) #1684

ns3_isolate <- fulldta_st_ns_dist %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns3_isolate$serotype_isolates, na.rm = TRUE) #41,419

ns4_isolate <- fulldta_st_ns_dist %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns4_isolate$serotype_isolates, na.rm = TRUE)


#8.7 Total number of Macrolide Nonsusceptible Studies 
ns1_study <- fulldta_st_ns %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns1_study$studyID)) #23

ns2_study <- fulldta_st_ns %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented
length(unique(ns2_study$studyID)) #8

ns3_study <- fulldta_st_ns %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns3_study$studyID)) #36

ns4_study <- fulldta_st_ns %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
length(unique(ns4_study$studyID)) #14

#8.8 Total number of Macrolide Nonsusceptible ISOLATES  
# ns1_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "macrolide") %>% 
#   filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
#   filter(prepost_meta == "pre") #PCV not yet implemented 
# sum(ns1_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns2_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "macrolide") %>% 
#   filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
#   filter(prepost_meta == "post") #PCV implemented 
# sum(ns2_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns3_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "macrolide") %>% 
#   filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
#   filter(prepost_meta == "pre") #PCV not yet implemented 
# sum(ns3_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns4_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "macrolide") %>% 
#   filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
#   filter(prepost_meta == "post") #PCV implemented 
# sum(ns4_isolate$serotype_isolates, na.rm = TRUE)

#TEST New 
ns1_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns1_isolate$serotype_isolates, na.rm = TRUE)

ns2_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns2_isolate$serotype_isolates, na.rm = TRUE)

ns3_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns3_isolate$serotype_isolates, na.rm = TRUE)

ns4_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns4_isolate$serotype_isolates, na.rm = TRUE)

#8.9 Total number of SXT Nonsusceptible Studies 
ns1_study <- fulldta_st_ns %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns1_study$studyID)) #15

ns2_study <- fulldta_st_ns %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented
length(unique(ns2_study$studyID)) #5

ns3_study <- fulldta_st_ns %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns3_study$studyID)) #21

ns4_study <- fulldta_st_ns %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
length(unique(ns4_study$studyID)) #9

#8.10 Total number of SXT Nonsusceptible ISOLATES  
# ns1_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "SXT") %>% 
#   filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
#   filter(prepost_meta == "pre") #PCV not yet implemented 
# sum(ns1_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns2_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "SXT") %>% 
#   filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
#   filter(prepost_meta == "post") #PCV implemented 
# sum(ns2_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns3_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "SXT") %>% 
#   filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
#   filter(prepost_meta == "pre") #PCV not yet implemented 
# sum(ns3_isolate$serotype_isolates, na.rm = TRUE)
# 
# ns4_isolate <- fulldta_st_ns_dist %>% 
#   filter(drug_class == "SXT") %>% 
#   filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
#   filter(prepost_meta == "post") #PCV implemented 
# sum(ns4_isolate$serotype_isolates, na.rm = TRUE)
# 

#Test 3
ns1_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns1_isolate$serotype_isolates, na.rm = TRUE)

ns2_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns2_isolate$serotype_isolates, na.rm = TRUE)

ns3_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns3_isolate$serotype_isolates, na.rm = TRUE)

ns4_isolate <- fulldta_st_ns %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns4_isolate$serotype_isolates, na.rm = TRUE)

### #REPEAT FOR RESISTANCE ####

#8.5 Total number of Penicillin Nonsusceptible Studies 
ns1_study <- fulldta_st_res %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns1_study$studyID)) #29

ns2_study <- fulldta_st_res %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented
length(unique(ns2_study$studyID)) #6

ns3_study <- fulldta_st_res %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns3_study$studyID)) #38

ns4_study <- fulldta_st_res %>% 
  filter(drug_class == "penicillin") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
length(unique(ns4_study$studyID)) #13

#8.6 Total number of Penicillin Nonsusceptible ISOLATES  
ns1_isolate <- fulldta_st_res %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns1_isolate$serotype_isolates, na.rm = TRUE)

ns2_isolate <- fulldta_st_res %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns2_isolate$serotype_isolates, na.rm = TRUE)

ns3_isolate <- fulldta_st_res %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns3_isolate$serotype_isolates, na.rm = TRUE)

ns4_isolate <- fulldta_st_res %>% 
  filter(drug_class == "penicillin") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns4_isolate$serotype_isolates, na.rm = TRUE)

#8.7 Total number of Macrolide Nonsusceptible Studies 
ns1_study <- fulldta_st_res %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns1_study$studyID)) #15

ns2_study <- fulldta_st_res %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented
length(unique(ns2_study$studyID)) #7

ns3_study <- fulldta_st_res %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns3_study$studyID)) #22

ns4_study <- fulldta_st_res %>% 
  filter(drug_class == "macrolide") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
length(unique(ns4_study$studyID)) #10

#8.8 Total number of Macrolide Nonsusceptible ISOLATES  
ns1_isolate <- fulldta_st_res %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns1_isolate$serotype_isolates, na.rm = TRUE)

ns2_isolate <- fulldta_st_res %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns2_isolate$serotype_isolates, na.rm = TRUE)

ns3_isolate <- fulldta_st_res %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns3_isolate$serotype_isolates, na.rm = TRUE)

ns4_isolate <- fulldta_st_res %>% 
  filter(drug_class == "macrolide") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns4_isolate$serotype_isolates, na.rm = TRUE)

#8.9 Total number of SXT Nonsusceptible Studies 
ns1_study <- fulldta_st_res %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns1_study$studyID)) #7

ns2_study <- fulldta_st_res %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented
length(unique(ns2_study$studyID)) #6

ns3_study <- fulldta_st_res %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
length(unique(ns3_study$studyID)) #12

ns4_study <- fulldta_st_res %>% 
  filter(drug_class == "SXT") %>% 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
length(unique(ns4_study$studyID)) #9

#8.10 Total number of SXT Nonsusceptible ISOLATES  

ns1_isolate <- fulldta_st_res %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns1_isolate$serotype_isolates, na.rm = TRUE)

ns2_isolate <- fulldta_st_res %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "PCV7") %>% #PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns2_isolate$serotype_isolates, na.rm = TRUE)

ns3_isolate <- fulldta_st_res %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "pre") #PCV not yet implemented 
sum(ns3_isolate$serotype_isolates, na.rm = TRUE)

ns4_isolate <- fulldta_st_res %>% 
  filter(drug_class == "SXT") %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% #filtering by serotype helps but removes other drugs 
  filter(vac_type_pcv7 == "Non-PCV7") %>% #Non-PCV7 serotype
  filter(prepost_meta == "post") #PCV implemented 
sum(ns4_isolate$serotype_isolates, na.rm = TRUE)



