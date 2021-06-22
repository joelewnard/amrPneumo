# Create Table 2- Total number of studies included in systeamtic review by region
# Kristin Andrejko
# Last updated: 12-1-20

### Need to add in studies from the 2020 review (both ST only and nonST from 05-PneumoAMR-T1-LMicrobe)
fulldta_table1_032410 <- read_excel("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_table1_032410_final.xlsx")
load(file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/data_120220.rda")

fulldta_table1_032410 <- fulldta_table1_032410 %>% dplyr::select(studyID, arm_id, r_id, no_ns, no_res,drug_class, super_region, prepost, region, isolate_type, method_1, method_2, criteria_cl,doi)
extra_data4_keep <- extra_data4 %>% 
  dplyr::select(studyID, arm_id, r_id, no_ns, no_res, drug_class, super_region, prepost, region, isolate_type, method_1, method_2, criteria_cl,doi)

#Merge extra_data4, fulldta, 
fulldta_table1_032410 <- rbind(fulldta_table1_032410, extra_data4_keep)
length(unique(fulldta_table1_032410$studyID)) #558

#Merge the last serotype data
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/serotypeData_gps_tb1.rda")
gps_data <- extra_data_gps

gps_data_keep <- gps_data %>% 
  dplyr::select(studyID, arm_id, r_id, no_ns, no_res, drug_class, super_region, prepost, region, isolate_type, method_1, method_2, criteria_cl,doi)
gps_data

fulldta_table1_032410_v2 <- rbind(fulldta_table1_032410, gps_data_keep)
nrow(fulldta_table1_032410) + nrow(gps_data_keep); nrow(fulldta_table1_032410_v2)
fulldta_table1_032410 <- fulldta_table1_032410_v2

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


# #1. Penicillin 
# fulldta_table1_pen <- fulldta_table1_032410 %>% 
#   dplyr::filter(drug_class == "penicillin") %>% 
#   distinct(studyID, arm_id, .keep_all = TRUE)
# 
# fulldta_table1_pen_ns <- fulldta_table1_pen
# fulldta_table1_pen_ns <- fulldta_table1_pen_ns[!(is.na(fulldta_table1_pen_ns$no_ns)), ] 
# 
# fulldta_table1_pen_res <- fulldta_table1_pen
# fulldta_table1_pen_res <- fulldta_table1_pen_res[!(is.na(fulldta_table1_pen_res$no_res)), ] 
# 
# #2. Macrolide 
# fulldta_table1_mac <- fulldta_table1_032410 %>% 
#   dplyr::filter(drug_class == "macrolide") %>% 
#   distinct(studyID, arm_id, .keep_all = TRUE)
# 
# fulldta_table1_mac_ns <- fulldta_table1_mac
# fulldta_table1_mac_ns <- fulldta_table1_mac_ns[!(is.na(fulldta_table1_mac_ns$no_ns)), ] 
# 
# fulldta_table1_mac_res <- fulldta_table1_mac
# fulldta_table1_mac_res <- fulldta_table1_mac_res[!(is.na(fulldta_table1_mac_res$no_res)), ] 
# 
# #3. SXT 
# fulldta_table1_sxt <- fulldta_table1_032410 %>% 
#   dplyr::filter(drug_class == "SXT") %>% 
#   distinct(studyID, arm_id, .keep_all = TRUE)
# 
# fulldta_table1_sxt_ns <- fulldta_table1_sxt
# fulldta_table1_sxt_ns <- fulldta_table1_sxt_ns[!(is.na(fulldta_table1_sxt_ns$no_ns)), ] 
# 
# fulldta_table1_sxt_res <- fulldta_table1_sxt
# fulldta_table1_sxt_res <- fulldta_table1_sxt_res[!(is.na(fulldta_table1_sxt_res$no_res)), ] 
# 
# #4. CEPH  
# fulldta_table1_ceph <- fulldta_table1_032410 %>% 
#   dplyr::filter(drug_class == "3gen_cephalosporin") %>% 
#   distinct(studyID, arm_id, .keep_all = TRUE)
# 
# fulldta_table1_ceph_ns <- fulldta_table1_ceph
# fulldta_table1_ceph_ns <- fulldta_table1_ceph_ns[!(is.na(fulldta_table1_ceph_ns$no_ns)), ] 
# 
# fulldta_table1_ceph_res <- fulldta_table1_ceph
# fulldta_table1_ceph_res <- fulldta_table1_ceph_res[!(is.na(fulldta_table1_ceph_res$no_res)), ] 
# 
# #5. TETRACYCLINE  
# fulldta_table1_tet <- fulldta_table1_032410 %>% 
#   dplyr::filter(drug_class == "tetracycline") %>% 
#   distinct(studyID, arm_id, .keep_all = TRUE)
# 
# #Create Functions 
# ns_studies <- function(ns){
#   tbl_region <- ns %>% 
#     dplyr::group_by(region) %>%
#     dplyr::summarize(study_no = length(unique(studyID)))
#   
#   tbl_super_region <- ns %>% 
#     dplyr::group_by(super_region) %>%
#     dplyr::summarize(study_no = length(unique(studyID)))
#   names(tbl_super_region)[1] <- "region"
#   
#   tb_comb <- rbind(tbl_region, tbl_super_region)
#   
#   tb_comb <- tb_comb %>% 
#     arrange(match(region, order_region_s))
#   
#   return(tb_comb)
# }
# 
# res_studies <- function(res){
#   tbl_region <- res %>% 
#     dplyr::group_by(region) %>%
#     dplyr::summarize(study_no = length(unique(studyID)))
#   
#   tbl_super_region <- res %>% 
#     dplyr::group_by(super_region) %>%
#     dplyr::summarize(study_no = length(unique(studyID)))
#   names(tbl_super_region)[1] <- "region"
#   
#   tb_comb <- rbind(tbl_region, tbl_super_region)
#   
#   tb_comb <- tb_comb %>% 
#     arrange(match(region, order_region_s))
#   
#   return(tb_comb)
# }
# 
# #Loop through data frames 
# 
# #1. Penicillin 
# ns_tb_study <- ns_studies(fulldta_table1_pen_ns)
# res_tb_study <- res_studies(fulldta_table1_pen_res)
# 
# View(ns_tb_study)
# View(res_tb_study)
# 
# #2. Macrolides 
# ns_tb_study <- ns_studies(fulldta_table1_mac_ns)
# res_tb_study <- res_studies(fulldta_table1_mac_res)
# 
# View(ns_tb_study)
# View(res_tb_study)
# 
# #3. SXT 
# ns_tb_study <- ns_studies(fulldta_table1_sxt_ns)
# res_tb_study <- res_studies(fulldta_table1_sxt_res)
# 
# View(ns_tb_study)
# View(res_tb_study)
# 
# #4. CEPH 
# ns_tb_study <- ns_studies(fulldta_table1_ceph_ns)
# res_tb_study <- res_studies(fulldta_table1_ceph_res)
# 
# View(ns_tb_study)
# View(res_tb_study)
# 
# #5. TET 
# ns_tb_study <- ns_studies(fulldta_table1_tet_ns)
# res_tb_study <- res_studies(fulldta_table1_tet_res)
# 
# View(ns_tb_study)
# View(res_tb_study)


#To get total studies with nonsusceptible and resistant data we do NOT want to filter by fulldta_nodup
fulldta_ns <- fulldta_table1_032410
fulldta_ns <- fulldta_ns[!(is.na(fulldta_ns$no_ns)), ] 

fulldta_res <- fulldta_table1_032410
fulldta_res <- fulldta_res[!(is.na(fulldta_res$no_res)), ] 

#NEW PEN
tbl_pen_ns_study <- fulldta_ns %>% 
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class,  no_res, no_ns) %>% 
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
  dplyr::select(studyID, region, super_region, drug_class, no_res, no_ns) %>% 
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
View(tb1s)
write.csv(tb1s, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb2.csv")
