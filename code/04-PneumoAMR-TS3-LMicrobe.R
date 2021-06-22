# Create Table S3: Summary of available studies for comparisons of isolate susceptibility ≥1 year before and ≥3 years after PCV implementation.  
# Kristin Andrejko
# 12-3-20


load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_120220.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_pen_inv.Rdata")

#Build table of pre vs. 3 years post vaccine introduction by country and region 

#Save region order as a r object
region_order <- unique(dat$region) #dat is from the forest plotting code 
#save(region_order, file = "region_order.Rdata") #This is saved as an object to 04-13-20

supp_tb <- fulldta %>% 
  group_by(country, region, prepost) %>% 
  filter(prepost == "naive" | prepost == "not_naive") %>% 
  dplyr::summarize(study = length(unique(studyID)), 
                   isolates = sum(total_isolates_drug, na.rm = TRUE)) %>% 
  arrange(match(region, region_order))
View(supp_tb)

write.csv(supp_tb, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/tabS3.csv")
