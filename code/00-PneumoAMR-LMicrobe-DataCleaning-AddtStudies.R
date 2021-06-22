# 1. Loading and Cleaning Data ------------------------------------------------------------
# This creates a clean data file to merge with pre-NOV 2020 abstractions 
# This cleans data from final NOV 2020 abstraction (N=93 aggregate studies)

library(dplyr)

setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe")
addt_93 <- read.csv("data/data_addt_93_final.csv") #Last pull of data from the 2007-2009 papers and 2020 update completed 12-2-20, note I manually removed Okade data from excel
length(unique(addt_93$studyID)) #93

#pcv_intro <- read_excel("data/PCV Vaccine Intro.xlsx") #found tiny errors on intro date, update to new sheet
pcv_intro <- read_excel("data/PCV Vaccine Intro Updated Nov2020.xlsx") #found tiny errors on intro date, update to new sheetincome_dta <- read_excel("data/income_status_worldbank.xlsx")
gbd_region_new <-  read_excel("data/gbd_region_new_r.xlsx")
world_bank_gdp_new <- read_excel("data/world_bank_gdp_new2.xlsx")

addt_93 <- addt_93 %>% dplyr::select(-serotype.data.)
all_data4 <- addt_93
length(unique(all_data4$studyID)) #93

#levels(factor(extra_data3$country[!(extra_data3$country %in% all_data2$country)]))   #this was used when merging names
levels(factor(all_data4$country[!(all_data4$country %in% world_bank_gdp_new$country)]))   #this was used when merging names

#Countries to fix from addt_93:
# England -> United Kingdom
# United States -> United States of America 
# Taiwan 
all_data4$country <- as.character(all_data4$country)
all_data4$country[all_data4$country == "United States"] <- "United States of America"
all_data4$country[all_data4$country == "United States "] <- "United States of America"
all_data4$country[all_data4$country == "Taiwan"] <- "China"
all_data4$country[all_data4$country == "England"] <- "United Kingdom"

#1. Merge Data 
newmerge <- merge(gbd_region_new, pcv_intro, by = "country", all = T) #replace gbd_region_new with who_region
newmerge <- merge(newmerge, income_dta, by = "country", all = T)

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

fulldta$r_id <- fulldta$r_id  + 5000 #add 450 to each of the r_id to not mess up the earlier stuff -> CHANGE TO 5000
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
#fulldta$drug_class <- as.character(fulldta$drug_class)

fulldta$drug_class[fulldta$drug_class == "Ampicillin"] <- "penicillin"
fulldta$drug_class[fulldta$drug_class == "Amoxacillin"] <- "penicillin"
fulldta$drug_class[fulldta$drug_class == "Penicilin"] <- "penicillin"
fulldta$drug_class[fulldta$drug_class == "penicillin"] <- "penicillin"
fulldta$drug_class[fulldta$drug_class == "Ceftriaxone"] <- "3gen_cephalosporin"
fulldta$drug_class[fulldta$drug_class == "Tetracycline"] <- "tetracycline"
fulldta$drug_class[fulldta$drug_class == "Cotrimoxazole"] <- "SXT"
fulldta$drug_class[fulldta$drug_class == "Cefotaxime"] <- "3gen_cephalosporin"
fulldta$drug_class[fulldta$drug_class == "Ceftoxamine"] <- "3gen_cephalosporin"
fulldta$drug_class[fulldta$drug_class == "Ceftiaxone"] <- "3gen_cephalosporin"
fulldta$drug_class[fulldta$drug_class == "Cefuroxime"] <- "3gen_cephalosporin"
fulldta$drug_class[fulldta$drug_class == "Cefuroxime "] <- "3gen_cephalosporin"

fulldta$drug_class[fulldta$drug_class == "Amoxicillin"] <- "penicillin"
fulldta$drug_class[fulldta$drug_class == "Clindamycin"] <- "tetracycline" #CHECK
fulldta$drug_class[fulldta$drug_class == "Cefaclor"] <- "3gen_cephalosporin" #CHECK
fulldta$drug_class[fulldta$drug_class == "Imipenem"] <- "imipenem" #REMOVE
fulldta$drug_class[fulldta$drug_class == "Levofloxacin"] <- "fluoroquinolone" #CHECK
fulldta$drug_class[fulldta$drug_class == "Vancomycin"] <- "vancomycin" #CHECK
fulldta$drug_class[fulldta$drug_class == "Chloramphenicol"] <- "chloramphenicol" #CHECK
fulldta$drug_class[fulldta$drug_class == "Ciprofloxacin"] <- "ciprofloxacin" #CHECK
fulldta$drug_class[fulldta$drug_class == "Linezolid"] <- "linezolid" 
fulldta$drug_class[fulldta$drug_class == "Penicillin"] <- "penicillin"
fulldta$drug_class[fulldta$drug_class == "Erythromycin"] <- "macrolide"
fulldta$drug_class[fulldta$drug_class == "Azithromycn"] <- "macrolide"

table(fulldta$drug, fulldta$drug_class)
extra_data4 <- fulldta
length(unique(extra_data4$studyID)) #93
length(unique(extra_data4$r_id)) #93
range(extra_data4$studyID, na.rm = T); range(extra_data4$r_id, na.rm = T)

save(extra_data4, file = "data/data_120220.rda")
View(extra_data4)

#Take extra_data4 and select doi, country, sample_collection_startyear, sample_collection_endyear, prepost
finalTable <- extra_data4 %>% 
  dplyr::select(author, doi, pub_year, country, sample_collection_startyear, sample_collection_endyear, prepost, total_isolates_drug)
View(finalTable)

