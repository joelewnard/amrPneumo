load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_pen_inv.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_pen_ninv.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_mac_inv.Rdata") #dat3
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_mac_ninv.Rdata") #dat4

regs = unique(dat$region[is.na(dat$region)==F])

regs_clean = c("Southern Latin America", 
               "Western Europe", 
               "High Income North America", 
               "Australasia", 
               "High Income Asia Pacific", 
               "High Income", 
               "Caribbean", 
               "Central Latin America", 
               "Tropical Latin America", 
               "Andean Latin America", 
               "Latin America and Caribbean",
               "Southern  sub-Saharan Africa", 
               "Western  sub-Saharan Africa", 
               "Central sub-Saharan Africa", 
               "Eastern  sub-Saharan Africa", 
               "Sub-Saharan Africa", 
               "North Africa & Middle East",
               "South Asia", 
               "East Asia", 
               "Southeast Asia",
               "Oceania", 
               "Southeast Asia, East Asia, & Oceania", 
               "Eastern Europe", 
               "Central Europe", 
               "Central Europe, Eastern Europe") 
eras = c('pre','post')

ests = array(NA,dim=c(length(regs),3,2))
nums = array(NA,dim=c(length(regs),2))
for (i in 1:length(regs)) for (j in 1:length(eras)){
  ests[i,1,j] = dat$ee[which(dat$region==regs[i]&dat$pre_post==eras[j])[1]]
  ests[i,2,j] = dat$ci_lb_new[which(dat$region==regs[i]&dat$pre_post==eras[j])[1]]
  ests[i,3,j] = dat$ci_ub_new[which(dat$region==regs[i]&dat$pre_post==eras[j])[1]]
}
ests[ests<0] = 0

studynums = sampsizes = array(NA,dim=c(length(regs),2))
for (i in 1:length(regs)) for (j in 1:length(eras)){
  studynums[i,j] = dat$study_arms[which(dat$region==regs[i]&dat$pre_post==eras[j])[1]]
  sampsizes[i,j] = dat$total_denom[which(dat$region==regs[i]&dat$pre_post==eras[j])[1]]
}

ests_2 = array(NA,dim=c(length(regs),3,2))
nums_2 = array(NA,dim=c(length(regs),2))
for (i in 1:length(regs)) for (j in 1:length(eras)){
  ests_2[i,1,j] = dat2$ee[which(dat2$region==regs[i]&dat2$pre_post==eras[j])[1]]
  ests_2[i,2,j] = dat2$ci_lb[which(dat2$region==regs[i]&dat2$pre_post==eras[j])[1]]
  ests_2[i,3,j] = dat2$ci_ub_new[which(dat2$region==regs[i]&dat2$pre_post==eras[j])[1]] 
}
ests_2[ests_2<0] = 0

studynums_2 = sampsizes_2 = array(NA,dim=c(length(regs),2))
for (i in 1:length(regs)) for (j in 1:length(eras)){
  studynums_2[i,j] = dat2$study_arms[which(dat2$region==regs[i]&dat2$pre_post==eras[j])[1]]
  sampsizes_2[i,j] = dat2$total_denom[which(dat2$region==regs[i]&dat2$pre_post==eras[j])[1]]
}

ests_3 = array(NA,dim=c(length(regs),3,2))
nums_3 = array(NA,dim=c(length(regs),2))
for (i in 1:length(regs)) for (j in 1:length(eras)){
  ests_3[i,1,j] = dat3$ee[which(dat3$region==regs[i]&dat3$pre_post==eras[j])[1]]
  ests_3[i,2,j] = dat3$ci_lb_new[which(dat3$region==regs[i]&dat3$pre_post==eras[j])[1]]
  ests_3[i,3,j] = dat3$ci_ub_new[which(dat3$region==regs[i]&dat3$pre_post==eras[j])[1]]
}
ests_3[ests_3<0] = 0

studynums_3 = sampsizes_3 = array(NA,dim=c(length(regs),2))
for (i in 1:length(regs)) for (j in 1:length(eras)){
  studynums_3[i,j] = dat3$study_arms[which(dat3$region==regs[i]&dat3$pre_post==eras[j])[1]]
  sampsizes_3[i,j] = dat3$total_denom[which(dat3$region==regs[i]&dat3$pre_post==eras[j])[1]]
}

ests_4 = array(NA,dim=c(length(regs),3,2))
nums_4 = array(NA,dim=c(length(regs),2))
for (i in 1:length(regs)) for (j in 1:length(eras)){
  ests_4[i,1,j] = dat4$ee[which(dat4$region==regs[i]&dat4$pre_post==eras[j])[1]]
  ests_4[i,2,j] = dat4$ci_lb[which(dat4$region==regs[i]&dat4$pre_post==eras[j])[1]]
  ests_4[i,3,j] = dat4$ci_ub_new[which(dat4$region==regs[i]&dat4$pre_post==eras[j])[1]]
}
ests_4[ests_4<0] = 0

studynums_4 = sampsizes_4 = array(NA,dim=c(length(regs),2))
for (i in 1:length(regs)) for (j in 1:length(eras)){
  studynums_4[i,j] = dat4$study_arms[which(dat4$region==regs[i]&dat4$pre_post==eras[j])[1]]
  sampsizes_4[i,j] = dat4$total_denom[which(dat4$region==regs[i]&dat4$pre_post==eras[j])[1]]
}

#### Create Table S6 and Table S7 #
#ests = penicillin invasive
#ests_2 = penicillin noninvasive
#ests_3 = macrolide invasive
#ests_4 = macrolide noninvasive 
pen_inv_pre <- data.frame(cbind(regs,ests[,,1])) %>% mutate(era = "pre")   #For Pen-Inv-Pre

pen_inv_post <- data.frame(cbind(regs,ests[,,2])) %>% mutate(era = "post")   #For Pen-Inv-Post

pen_ninv_pre <- data.frame(cbind(regs,ests_2[,,1])) %>% mutate(era = "pre")   #For Pen-NInv-Pre

pen_ninv_post <- data.frame(cbind(regs,ests_2[,,2])) %>% mutate(era = "post")   #For Pen-NInv-Post

mac_inv_pre <- data.frame(cbind(regs,ests_3[,,1]))%>%  mutate(era = "pre")   #For Mac-Inv-Pre

mac_inv_post <- data.frame(cbind(regs,ests_3[,,2])) %>% mutate(era = "post")   #For Mac-Inv-Post

mac_ninv_pre <- data.frame(cbind(regs,ests_4[,,1])) %>%  mutate(era = "pre")   #For Mac-NInv-Pre

mac_ninv_post <- data.frame(cbind(regs,ests_4[,,2]))%>% mutate(era = "post")   #For Mac-NInv-Post

order_region_s <- c("southern_latin_america", "western_europe", "hi_north_america", "australasia", 
                    "hi_asia_pacific","high_income","caribbean", "central_latin_america", "tropical_latin_america", 
                    "andean_latin_america", "latin_america_caribbean", "southern_subsaharan_africa", "western_subsaharan_africa", 
                    "central_subsaharan_africa", "eastern_subsaharan_africa", "sub_saharan_africa", "north_africa_middle_east", 
                    "south_asia","east_asia","southeast_asia",  "oceania","seasia_easia_oceania", "eastern_europe", "central_europe", "ceurope_eeurope_casia")

pen_inv <- rbind(pen_inv_pre, pen_inv_post)
pen_inv <- pen_inv %>% arrange(match(regs, order_region_s))
names(pen_inv)[2] <- "EE"
pen_inv <- pen_inv %>% dplyr::select(regs, EE, era)

pen_ninv <- rbind(pen_ninv_pre, pen_ninv_post)
pen_ninv <- pen_ninv %>% arrange(match(regs, order_region_s))
names(pen_ninv)[2] <- "EE"
pen_ninv <- pen_ninv %>% dplyr::select(regs, EE, era)

mac_inv <- rbind(mac_inv_pre, mac_inv_post)
mac_inv <- mac_inv %>% arrange(match(regs, order_region_s))
names(mac_inv)[2] <- "EE"
mac_inv <- mac_inv %>% dplyr::select(regs, EE, era)

mac_ninv <- rbind(mac_ninv_pre, mac_ninv_post)
mac_ninv <- mac_ninv %>% arrange(match(regs, order_region_s))
names(mac_ninv)[2] <- "EE"
mac_ninv <- mac_ninv %>% dplyr::select(regs, EE, era)

save(pen_inv, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/pen_inv_consump.Rdata")
save(pen_ninv, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/pen_ninv_consump.Rdata")
save(mac_inv, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/mac_inv_consump.Rdata")
save(mac_ninv, file = "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/mac_ninv_consump.Rdata")
