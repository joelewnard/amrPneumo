# Addressing reviewer comments for LMicrobe

#Finding pre-post studies
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/fulldta_113020.Rdata")
length(unique(fulldta$studyID))
length(which(fulldta$studyID == 9999))
#Note this data set has 447 (now 535) rows, including 1 row of ATLAS becuase it doesn't include the serotype only studies 
#ATLAS data is r_id = 450

#### 1. Reviewer #4: Pre-Post Analysis####
# 1. Create a variable for whether PCV was introduced DURING the study collection ####
View(head(fulldta, 25))

#Is sample_collection_startyear < pcv_intro_year < sample_collection_endyear
fulldta$pcvDuring <- ifelse(fulldta$sample_collection_startyear < fulldta$pcv_intro_year &  
                              fulldta$pcv_intro_year < fulldta$sample_collection_endyear, 1, 0)
length(which(fulldta$pcvDuring == 1))

uniquePen <- fulldta %>% filter(fulldta$pcvDuring==1) %>%  filter(drug_class == "penicillin") # #24 penicillin studies, 31 rows
uniqueMac <- fulldta %>% filter(fulldta$pcvDuring==1) %>%  filter(drug_class == "macrolide") #15 macrolide studies, 23 rows 
length(unique(uniqueMac$r_id)) 

#Once I checked that there were not multipl locations per arm, I just filtered to only have 1 row/ study
uniquePenNoDup <- uniquePen %>% distinct(r_id, .keep_all = TRUE)
uniqueMacNoDup <- uniqueMac %>% distinct(r_id, .keep_all = TRUE)

table(uniquePenNoDup$country) #Analysis within the US= 3, Spain = 3, Israel = 2
table(uniqueMacNoDup$country) #Analysis within Germany = 2, Spain = 2

#Pull numbers of studies with pre-post data 
studyNum <- uniquePenNoDup %>% 
  dplyr::select(r_id, studyID, abstractor, country, author, doi)
View(studyNum)

## GDP per capita 2000-2020 (range, 2010 USD)
setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe")

world_bank_gdp_new <- read_excel("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/world_bank_gdp_new2.xlsx")

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

world_bank_gdp_melt2 <- world_bank_gdp_melt %>% 
  filter(midpoint_yr == 2000| midpoint_yr == 2018)
View(world_bank_gdp_melt2)
View(world_bank_gdp_melt)
