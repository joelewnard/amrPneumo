# Create Table 1 of PneumoAMRSys Review
# Kristin Andrejko
# 12-1-20

library(readxl)

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/gbd_list.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/sregion_list.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/out_fulldta_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/out_fulldta_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/novax_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/novax_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/postvax_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/postvax_res.rda")
fulldta <- read_excel("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_table1_032410_final.xlsx")


#Test how mnay countries there are in full data set 
length(unique(fulldta$country)) #99 countries 

atlasdta <- fulldta %>% 
  filter(studyID == 9999)

length(unique(atlasdta$country)) #58 countries 

atlasdta_nodup <- atlasdta %>% 
  distinct(country, sample_collection_startyear, .keep_all = TRUE)
View(atlasdta_nodup)

#Sum total isolates from atlasdta_nodup
sum(atlasdta$total_isolates_drug) #14, 162 isolates 

# 4.1. CREATE TABLE 1- DESCRIBING DATA  ----

#THINK ABOUT HOW WE ARE SUMMING ISOLATES
# WE WILL INFLATE THE NUMBER OF ISOLATES BY DOUBLE COUNTING THE SAME ISOLATES TESTED FOR MULTIPLE DRUGS. 
# WILL NEED TO SOMEHOW FILTER BY UNIQUE ID + UNIQUE ARMS, THEN COUNT THE NUMBER OF STUDIES 

#FILTER DATA HERE: 
#Use fulldta file 
#test <- unique(fulldta[c("r_id", "arm_id")]) #this gives you what the columsn should be, but not the full data farme

#Need to merge this table1 with additional 93 articles we abstracted Dec 1
# Columnsn to keep: 
# studyID, r_id, super_region, region, total_isolates_drug, isolate_type, method_1, method_2, criteria_cl, criteria_cl

fulldta <- fulldta %>% dplyr::select(country, studyID, arm_id, r_id, super_region, prepost,
                                     region, total_isolates_drug, isolate_type, method_1, method_2, criteria_cl,doi)
fulldta2 <- fulldta
length(unique(fulldta$studyID)) #465 (include serotype only)
length(unique(fulldta$doi))


#Merge in additional studies (this will include both ST and non-ST studies)

#This is the additional data from November 2020, not including serotype #from 00-PneumoAMR-DataCleaning-AddtStudies
load(file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/data_120220.rda")
extra_data4_keep <- extra_data4 %>% 
  dplyr::select(country, studyID, arm_id, r_id, super_region, prepost,
                region, total_isolates_drug, isolate_type, method_1, method_2, criteria_cl,doi)
length(unique(extra_data4_keep$studyID)) #This is the additional data from November 2020, 

#Merge extra_data4, fulldta
fulldta2 <- rbind(fulldta, extra_data4_keep)
length(unique(fulldta2$studyID)) #558
length(unique(fulldta2$country)) #558

#Merge GPS data - START HERE 
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/serotypeData_gps_tb1.rda")
gps_data <- extra_data_gps

gps_isolates <- gps_data %>% 
  distinct(studyID, arm_id, serotype, .keep_all = TRUE) %>% 
  group_by(arm_id) %>% 
  summarize(total_isolates_drug = sum(serotype_isolates))
sum(gps_isolates$total_isolates_drug)

gps_isolates2 <- gps_isolates %>% dplyr::select(arm_id, total_isolates_drug)

gps_data2 <- merge(gps_isolates2, gps_data, by = c("arm_id"))
View(gps_data2)

gps_data_keep <- gps_data2 %>% 
  dplyr::select(country, studyID, arm_id, r_id, super_region, prepost,
                region, total_isolates_drug, isolate_type, method_1, method_2, criteria_cl,doi)

fulldta3 <- rbind(fulldta2, gps_data_keep)
fulldta2 <- fulldta3

test <- fulldta2 %>%  filter(studyID ==3307 )
View(test)

#Create nonduplicated version to use for code below 
fulldta_nodup <- fulldta2 %>% distinct(studyID, arm_id, .keep_all = TRUE) #need to use studyID
length(unique(fulldta_nodup$studyID))
length(unique(fulldta_nodup$doi)) #START HERE! 

sum(fulldta_nodup$total_isolates_drug, na.rm= T)

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
#View(count_sus_method1)

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
