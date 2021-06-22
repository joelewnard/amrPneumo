# Create Table S5 (S6 in final manuscript)- Excluding ATLAS database 
# Kristin Andrejko
# Last updated 12-8-20


### CAREFULLY LOAD DATA: 
#To use data frame that does NOT include ATLAS for supplemental table
fulldta_table1_032410 <- read_excel("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/fulldta_table1_032410_final.xlsx")
length(unique(fulldta_table1_032410$studyID))

# Exclude studyID 9999
fulldta_table1_032410 <- fulldta_table1_032410 %>%
  filter(studyID != 9999)

length(unique(fulldta_table1_032410$studyID)) #should be 1 less than previously 
length(unique(fulldta_table1_032410$doi))

#See what happens when we EXCLUDE teh serotype_only ones
fulldta_table1_032410 <- fulldta_table1_032410 %>% dplyr::filter(is.na(serotype_only))
length(unique(fulldta_table1_032410$studyID)) #Shoudl be 446 (excludes serotype only papers)

#NEW: Need to add in the Nov 2020 abstractions and Nov 2020 serotype only abstractions 
fulldta_table1_032410 <- fulldta_table1_032410 %>%   dplyr::select(studyID, arm_id, r_id, super_region, drug_class,
                                                                   region, total_isolates_drug, no_ns, no_res)


#Merge in additional studies (this will include both ST and non-ST studies)

#This is the additional data from November 2020, not including serotype 
load(file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/data_120220.rda")
extra_data4_keep <- extra_data4 %>% 
  dplyr::select(studyID, arm_id, r_id, super_region, drug_class,
                region, total_isolates_drug, no_ns, no_res)
length(unique(extra_data4_keep$studyID))


#Merge extra_data4, fulldta, and the last extra_data_serotype
fulldta2 <- rbind(fulldta_table1_032410, extra_data4_keep) #USE THIS FILE TO CREATE TABLE 3 in the MAIN TEXT 
length(unique(fulldta2$studyID)) 
fulldta_table1_032410 <- fulldta2


#Dplyr Method- this is for invasive, macorlide, naive isolates 
gbd_region_list_str <- c("Andean Latin America", "Australasia", "Caribbean", "Central Europe", "Central Latin America", 
                         "Central Subsaharan Africa", "East Asia", 
                         "Eastern Europe", "Eastern Sub-Saharan Africa", "High Income Asia Pacific", "High Income North America", 
                         "North Africa and Middle East", "Oceania", "South Asia", "Southeast Asia", "Southern Latin America", "Southern Sub-Saharan Africa", 
                         "Tropical Latin America", "Western Europe", "Western Sub-Saharan Africa")

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
ns_tb <- ns(fulldta_table1_pen_ns)
res_tb <- (res(fulldta_table1_pen_res))

View(ns_tb)
View(res_tb)

pen_tb3 <- data.frame(merge(ns_tb, res_tb, by = "region")) %>% dplyr::select(region, fin_ns_mac, fin_res_mac) %>% 
  arrange(match(region, order_region_s)) 

write.csv(pen_tb3, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/ns_tb_pen_noAtlas.csv")
write.csv(ns_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/ns_tb_pen_noAtlas.csv")
write.csv(res_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/res_tb_pen_noAtlas.csv")

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

mac_tb3 <- data.frame(merge(ns_tb, res_tb, by = "region")) %>% dplyr::select(region, fin_ns_mac, fin_res_mac) %>% 
  arrange(match(region, order_region_s)) 

write.csv(mac_tb3, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/ns_tb_mac_noAtlas.csv")
write.csv(ns_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/ns_tb_mac_noAtlas.csv")
write.csv(res_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/res_tb_mac_noAtlas.csv")

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

#3. SXT - does NOT change w/ addition of ATLAS data 
ns_tb <- (ns(fulldta_table1_sxt_ns))
res_tb <- (res(fulldta_table1_sxt_res))

write.csv(ns_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/ns_tb_sxt_noAtlas.csv")
write.csv(res_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/res_tb_sxt_noAtlas.csv")

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

write.csv(ns_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/ns_tb_ceph_noAtlas.csv")
write.csv(res_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/res_tb_ceph_noAtlas.csv")
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

write.csv(ns_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/ns_tb_tet_noAtlas.csv")
write.csv(res_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tb3/res_tb_tet_noAtlas.csv")


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
View(tb1)

#write.csv(tb1, "tb2.csv")

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
