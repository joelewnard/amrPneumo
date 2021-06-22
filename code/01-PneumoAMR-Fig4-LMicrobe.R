# Meta-Regression Plots - PCV/ AMR Meta-Analysis - Updated Fifugre4 models to reflect the same quadratic terms as Fig4
# Kristin Andrejko
# Updated 11-30-20
# This code produces Runs the meta-regression models, produces updated Figure 4 in AMR_Tables-Figures and is adapted from 03-10-20_code_AMR_MetaRegression
# This is the LOOP version of Figure 4 

# 1. Run Models ----
#Load data frames for the models- these come from 00-PneumoAMR-LMicrobe-DataCleaning

setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe")

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_pen_ns.rda") #This is the data that gets inputted into the model 
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_pen_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_mac_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_mac_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_sxt_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_sxt_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_ceph_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_ceph_res.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_tet_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/mod_tet_res.rda")

#Run fixed and random effects models : 

modPen_NS_npp = lmer(ns~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                       log(gdp) + isolate_type, data = mod_pen_ns) 
modPen_Res_npp = lmer(res~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                        log(gdp) + isolate_type, data = mod_pen_res) 
modMac_NS_npp = lmer(ns~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                       log(gdp) + isolate_type, data = mod_mac_ns) 
modMac_Res_npp = lmer(res~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                        log(gdp) + isolate_type, data = mod_mac_res) 
modSXT_NS_npp = lmer(ns~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                       log(gdp) + isolate_type, data = mod_sxt_ns) 
modSXT_Res_npp = lmer(res~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                        log(gdp) + isolate_type, data = mod_sxt_res) 
modCeph_NS_npp = lmer(ns~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                        log(gdp) + isolate_type, data = mod_ceph_ns) 
modCeph_Res_npp = lmer(res~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                         log(gdp) + isolate_type, data = mod_ceph_res) 
modTet_NS_npp = lmer(ns~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                       log(gdp) + isolate_type, data = mod_tet_ns) 
modTet_Res_npp = lmer(res~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                        log(gdp) + isolate_type, data = mod_tet_res) 


BIC(modPen_NS_npp,modPen_Res_npp,modMac_NS_npp,modMac_Res_npp,modSXT_NS_npp ,modSXT_Res_npp,modCeph_NS_npp,modCeph_Res_npp,modTet_NS_npp,modTet_Res_npp, 
    modPen_NS_npp2,modPen_Res_npp2,modMac_NS_npp2,modMac_Res_npp2,modSXT_NS_npp2 ,modSXT_Res_npp2,modCeph_NS_npp2,modCeph_Res_npp2,modTet_NS_npp2,modTet_Res_npp2)



round.fn = function(x){
  a1 = round(100*quantile(x,0.5),1)
  b = round(100*quantile(x,0.5))
  if (a1==b){
    a1 = paste(a1,'.0',sep='')
  }
  
  a2 = round(100*quantile(x,0.025),1)
  b = round(100*quantile(x,0.025))
  if (a2==b){
    a2 = paste(a2,'.0',sep='')
  }
  
  a3 = round(100*quantile(x,0.975),1)
  b = round(100*quantile(x,0.975))
  if (a3==b){
    a3 = paste(a3,'.0',sep='')
  }
  
  return(paste(a1,' (',a2,', ',a3,')',sep=''))
}

# 3. Create the plots ----
#Set Layout

## NEW- Clean up code
set.seed(252)
parsPenRes = mvrnorm(1e4,fixef(modPen_Res_npp),vcov(modPen_Res_npp))
parsPenNs = mvrnorm(1e4,fixef(modPen_NS_npp),vcov(modPen_NS_npp))
parsMacRes = mvrnorm(1e4,fixef(modMac_Res_npp),vcov(modMac_Res_npp))
parsMacNs = mvrnorm(1e4,fixef(modMac_NS_npp),vcov(modMac_NS_npp))
parsSxtRes = mvrnorm(1e4,fixef(modSXT_Res_npp),vcov(modSXT_Res_npp))
parsSxtNs = mvrnorm(1e4,fixef(modSXT_NS_npp),vcov(modSXT_NS_npp))
parsTetRes = mvrnorm(1e4,fixef(modTet_Res_npp),vcov(modTet_Res_npp))
parsTetNs = mvrnorm(1e4,fixef(modTet_NS_npp),vcov(modTet_NS_npp))
parsCephRes = mvrnorm(1e4,fixef(modCeph_Res_npp),vcov(modCeph_Res_npp))
parsCephNs = mvrnorm(1e4,fixef(modCeph_NS_npp),vcov(modCeph_NS_npp))

t = seq(0,17,0.1) 
outPenRes2 = outPenNs2 = outPenRes = outPenNs = 
  outMacRes2 = outMacNs2 = outMacRes = outMacNs = 
  outSxtRes2 = outSxtNs2 = outSxtRes = outSxtNs = 
  outTetRes2 = outTetNs2 = outTetRes = outTetNs = 
  outCephRes2 = outCephNs2 = outCephRes = outCephNs = 
  array(NA,dim=c(1e4,length(t)))

for (i in 1:length(t)){
  outPenNs[,i] = parsPenNs[,1] + parsPenNs[,2]*sqrt(t[i])+ parsPenNs[,3]*2006 + parsPenNs[,4]*10 
  outPenRes[,i] = parsPenRes[,1] + parsPenRes[,2]*sqrt(t[i])+ parsPenRes[,3]*2006 + parsPenRes[,4]*10 
  outPenNs2[,i] = parsPenNs[,1] + parsPenNs[,2]*sqrt(t[i])+ parsPenNs[,3]*(2006 + t[i]) + parsPenNs[,4]*10 
  outPenRes2[,i] =parsPenRes[,1] + parsPenRes[,2]*sqrt(t[i])+ parsPenRes[,3]*(2006 + t[i])  + parsPenRes[,4]*10 
  
  outMacNs[,i] = parsMacNs[,1] + parsMacNs[,2]*sqrt(t[i])+ parsMacNs[,3]*2006 + parsMacNs[,4]*10 
  outMacRes[,i] = parsMacRes[,1] + parsMacRes[,2]*sqrt(t[i])+ parsMacRes[,3]*2006 + parsMacRes[,4]*10 
  outMacNs2[,i] = parsMacNs[,1] + parsMacNs[,2]*sqrt(t[i])+ parsMacNs[,3]*(2006 + t[i]) + parsMacNs[,4]*10 
  outMacRes2[,i] =parsMacRes[,1] + parsMacRes[,2]*sqrt(t[i])+ parsMacRes[,3]*(2006 + t[i])  + parsMacRes[,4]*10 
  
  outSxtNs[,i] = parsSxtNs[,1] + parsSxtNs[,2]*sqrt(t[i])+ parsSxtNs[,3]*2006 + parsSxtNs[,4]*10 
  outSxtRes[,i] = parsSxtRes[,1] + parsSxtRes[,2]*sqrt(t[i])+ parsSxtRes[,3]*2006 + parsSxtRes[,4]*10 
  outSxtNs2[,i] = parsSxtNs[,1] + parsSxtNs[,2]*sqrt(t[i])+ parsSxtNs[,3]*(2006 + t[i]) + parsSxtNs[,4]*10 
  outSxtRes2[,i] =parsSxtRes[,1] + parsSxtRes[,2]*sqrt(t[i])+ parsSxtRes[,3]*(2006 + t[i])  + parsSxtRes[,4]*10 
  
  outTetNs[,i] = parsTetNs[,1] + parsTetNs[,2]*sqrt(t[i])+ parsTetNs[,3]*2006 + parsTetNs[,4]*10 
  outTetRes[,i] = parsTetRes[,1] + parsTetRes[,2]*sqrt(t[i])+ parsTetRes[,3]*2006 + parsTetRes[,4]*10 
  outTetNs2[,i] = parsTetNs[,1] + parsTetNs[,2]*sqrt(t[i])+ parsTetNs[,3]*(2006 + t[i]) + parsTetNs[,4]*10 
  outTetRes2[,i] =parsTetRes[,1] + parsTetRes[,2]*sqrt(t[i])+ parsTetRes[,3]*(2006 + t[i])  + parsTetRes[,4]*10 
  
  outCephNs[,i] = parsCephNs[,1] + parsCephNs[,2]*sqrt(t[i])+ parsCephNs[,3]*2006 + parsCephNs[,4]*10 
  outCephRes[,i] = parsCephRes[,1] + parsCephRes[,2]*sqrt(t[i])+ parsCephRes[,3]*2006 + parsCephRes[,4]*10 
  outCephNs2[,i] = parsCephNs[,1] + parsCephNs[,2]*sqrt(t[i])+ parsCephNs[,3]*(2006 + t[i]) + parsCephNs[,4]*10 
  outCephRes2[,i] =parsCephRes[,1] + parsCephRes[,2]*sqrt(t[i])+ parsCephRes[,3]*(2006 + t[i])  + parsCephRes[,4]*10 
  
}


# for (i in 1:length(t)){
#   outPenNs[,i] = parsPenNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsPenNs[,5]*2006 + parsPenNs[,6]*10 
#   outPenRes[,i] = parsPenRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsPenRes[,5]*2006 + parsPenRes[,6]*10
#   outPenNs2[,i] = parsPenNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsPenNs[,5]*(2006 + t[i]) + parsPenNs[,6]*10
#   outPenRes2[,i] = parsPenRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsPenRes[,5]*(2006 + t[i]) + parsPenRes[,6]*10
#   
#   outMacNs[,i] = parsMacNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsMacNs[,5]*2006 + parsMacNs[,6]*10
#   outMacRes[,i] = parsMacRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsMacRes[,5]*2006 + parsMacRes[,6]*10
#   outMacNs2[,i] = parsMacNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsMacNs[,5]*(2006 + t[i]) + parsMacNs[,6]*10
#   outMacRes2[,i] = parsMacRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsMacRes[,5]*(2006 + t[i]) + parsMacRes[,6]*10
#   
#   outSxtNs[,i] = parsSxtNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsSxtNs[,5]*2006 + parsSxtNs[,6]*10
#   outSxtRes[,i] = parsSxtRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsSxtRes[,5]*2006 + parsSxtRes[,6]*10
#   outSxtNs2[,i] = parsSxtNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsSxtNs[,5]*(2006 + t[i]) + parsSxtNs[,6]*10
#   outSxtRes2[,i] = parsSxtRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsSxtRes[,5]*(2006 + t[i]) + parsSxtRes[,6]*10
#   
#   outTetNs[,i] = parsTetNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsTetNs[,5]*2006 + parsTetNs[,6]*10
#   outTetRes[,i] = parsTetRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsTetRes[,5]*2006 + parsTetRes[,6]*10
#   outTetNs2[,i] = parsTetNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsTetNs[,5]*(2006 + t[i]) + parsTetNs[,6]*10
#   outTetRes2[,i] = parsTetRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsTetRes[,5]*(2006 + t[i]) + parsTetRes[,6]*10
#   
#   outCephNs[,i] = parsCephNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsCephNs[,5]*2006 + parsCephNs[,6]*10
#   outCephRes[,i] = parsCephRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsCephRes[,5]*2006 + parsCephRes[,6]*10
#   outCephNs2[,i] = parsCephNs[,c(1:4)]%*%(t[i]^c(0:3)) + parsCephNs[,5]*(2006 + t[i]) + parsCephNs[,6]*10
#   outCephRes2[,i] = parsCephRes[,c(1:4)]%*%(t[i]^c(0:3)) + parsCephRes[,5]*(2006 + t[i]) + parsCephRes[,6]*10
# }

q95fn = function(x){return(quantile(x,c(0.5,0.025,0.975)))}
estsPenNs = apply(outPenNs,2,q95fn); estsPenRes = apply(outPenRes,2,q95fn) 
estsPenNs2 = apply(outPenNs2,2,q95fn); estsPenRes2 = apply(outPenRes2,2,q95fn) 

estsMacNs = apply(outMacNs,2,q95fn); estsMacRes = apply(outMacRes,2,q95fn) 
estsMacNs2 = apply(outMacNs2,2,q95fn); estsMacRes2 = apply(outMacRes2,2,q95fn) 

estsSxtNs = apply(outSxtNs,2,q95fn); estsSxtRes = apply(outSxtRes,2,q95fn) 
estsSxtNs2 = apply(outSxtNs2,2,q95fn); estsSxtRes2 = apply(outSxtRes2,2,q95fn) 

estsTetNs = apply(outTetNs,2,q95fn); estsTetRes = apply(outTetRes,2,q95fn) 
estsTetNs2 = apply(outTetNs2,2,q95fn); estsTetRes2 = apply(outTetRes2,2,q95fn) 

estsCephNs = apply(outCephNs,2,q95fn); estsCephRes = apply(outCephRes,2,q95fn) 
estsCephNs2 = apply(outCephNs2,2,q95fn); estsCephRes2 = apply(outCephRes2,2,q95fn) 

round.fn = function(x){
  a1 = round(100*quantile(x,0.5),1)
  b = round(100*quantile(x,0.5))
  if (a1==b){
    a1 = paste(a1,'.0',sep='')
  }
  
  a2 = round(100*quantile(x,0.025),1)
  b = round(100*quantile(x,0.025))
  if (a2==b){
    a2 = paste(a2,'.0',sep='')
  }
  
  a3 = round(100*quantile(x,0.975),1)
  b = round(100*quantile(x,0.975))
  if (a3==b){
    a3 = paste(a3,'.0',sep='')
  }
  
  return(paste(a1,' (',a2,', ',a3,')',sep=''))
}


#STORE EVERYTHING IN ARRAYS 
vaxonlyNS <- array(c(estsPenNs, estsMacNs, estsSxtNs, estsTetNs, estsCephNs), 
                   dim = c(3,171,10)) #no secular trend 
vax_calNS <- array(c(estsPenNs2, estsMacNs2, estsSxtNs2, estsTetNs2, estsCephNs2),
                   dim = c(3, 171,10)) #secular trend
vaxonlyRES <- array(c(estsPenRes, estsMacRes , estsSxtRes, estsTetRes, estsCephRes), 
                    dim = c(3,171,10))
vax_calRES <- array(c(estsPenRes2, estsMacRes2 , estsSxtRes2 , estsTetRes2 , estsCephRes2),
                    dim = c(3, 171,10))
plot_names <- c("Penicillin", "Macrolides", "Sulfamethoxazole/trimethoprin", "Third generation cephalosporins", "Tetracycline")
diff_namesNS <- array(c(outPenNs, outMacNs, outSxtNs,outTetNs,outCephNs), 
                      dim = c(1e4, 171, 10))
diff_namesRES <- array(c(outPenRes, outMacRes, outSxtRes, outTetRes, outCephRes), 
                       dim = c(1e4, 171, 10))

# DATA FOR TABLE S6 on MetaReg Parameters 

modTet_Res_npp = lmer(res~(1 | r_id ) + (1 | region) +  sqrt(yr_since_vax) + midpoint_yr +
                        log(gdp) + isolate_type, data = mod_tet_res) 

round(100*quantile(parsPenNs[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR prevoiusly 5
round(100*quantile(parsPenNs[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP preiovusl 6
round(100*quantile(parsPenNs[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE previously 7

round(100*quantile(parsPenRes[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsPenRes[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsPenRes[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

round(100*quantile(parsMacNs[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsMacNs[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsMacNs[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

round(100*quantile(parsMacRes[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsMacRes[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsMacRes[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

round(100*quantile(parsSxtNs[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsSxtNs[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsSxtNs[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

round(100*quantile(parsSxtRes[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsSxtRes[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsSxtRes[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

round(100*quantile(parsCephNs[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsCephNs[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsCephNs[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

round(100*quantile(parsCephRes[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsCephRes[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsCephRes[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

round(100*quantile(parsTetNs[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsTetNs[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsTetNs[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

round(100*quantile(parsTetRes[,3],c(0.5,0.025,0.975)),2) #Use this for the tableS6- MIDPOINT YEAR
round(100*quantile(parsTetRes[,4],c(0.5,0.025,0.975)),2) #Use this for the tableS6- GDP
round(100*quantile(parsTetRes[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6- ISOLATE TYPE

#5: midpoint year, 6: GDP, 7: isolate type

# LOOP FOR FIGURE 4
#setwd("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/01-SPAMR-RFiles/SPAMR-Box/output/11-21-20")
pdf("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/fig4_120220.pdf", width=5, height=4.5) #OLD 

mat <- matrix(c(1,1,   17, 4,4,
                2,3,   17, 5,6,
                7,7,   17, 10,10,
                8,9,   17, 11,12,
                13,13, 17, 16,16,
                14,15, 17, 16,16), nrow = 6, byrow = T)
layout(mat,heights=rep(c(0.1,1),3),widths=c(1,1,0.1,1,1))
par(tck=-0.025)

for (i in 1:5){ #plot names 
  
  par(mar=c(0, 0, 0, 0)) ### b, l, t, r
  plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
  text(x =0, y =0.5, plot_names[i],cex=0.85,font=1,adj=0) 
  
  par(mar=c(3,2.5,0.5,0.5)) ### b, l, t, 
  
  #PLOT NONSUSCEPTIBLE 
  
  #Just Vaccine Effect 
  plot(y=(vaxonlyNS[,,i][1,]),x=t,ylim=c(0,1),xlim=c(-1,10),type='n',axes=F,ann=F) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
  polygon(y=c(vaxonlyNS[,,i][2,],rev(vaxonlyNS[,,i][3,])),x=c(t,rev(t)),
          col=rgb(0,0.1,1,0.25),lty=0)
  lines(y=vaxonlyNS[,,i][1,],x=t,col='darkblue',lwd=0.5)
  # lines(y=vaxonlyNS[,,i][2,],x=t,col='darkblue',lwd=0.25)
  # lines(y=vaxonlyNS[,,i][3,],x=t,col='darkblue',lwd=0.25)
  
  t_star = seq(-1, 0, 0.1)
  polygon(y=c(rep(vaxonlyNS[,,i][2,1], 11), rev(rep(vaxonlyNS[,,i][3,1], 11))),x=c(t_star, rev(t_star)),
          col=rgb(0,0.1,1,0.15),lty=0)
  lines(y=(rep(vaxonlyNS[,,i][1,1], 11)),x=t_star,col='darkblue',lwd=0.5)
  # lines(y=(rep(vaxonlyNS[,,i][2,1],11)), x=t_star,col='darkblue',lwd=0.25)
  # lines(y=(rep(vaxonlyNS[,,i][3,1], 11)), x=t_star,col='darkblue',lwd=0.25)
  abline(v=0,lty='dotted',col='grey')
  
  #Vaccine + Calendar Effect
  polygon(y=c(vax_calNS[,,i][2,],rev(vax_calNS[,,i][3,])),x=c(t,rev(t)),
          col=rgb(0,0.1,1,0.45),lty=0)
  lines(y=vax_calNS[,,i][1,],x=t,col='darkblue',lwd=0.5)
  lines(y=vax_calNS[,,i][2,],x=t,col='darkblue',lwd=0.25)
  lines(y=vax_calNS[,,i][3,],x=t,col='darkblue',lwd=0.25)
  
  t_star = seq(-1, 0, 0.1)
  polygon(y=c(rep(vax_calNS[,,i][2,1], 11), rev(rep(vax_calNS[,,i][3,1], 11))),x=c(t_star, rev(t_star)),
          col=rgb(0,0.1,1,0.45),lty=0) #change to 0.45 from 0.15
  lines(y=(rep(vax_calNS[,,i][1,1], 11)),x=t_star,col='darkblue',lwd=0.5)
  lines(y=(rep(vax_calNS[,,i][2,1],11)), x=t_star,col='darkblue',lwd=0.25)
  lines(y=(rep(vax_calNS[,,i][3,1], 11)), x=t_star,col='darkblue',lwd=0.25)
  abline(v=0,lty='dotted',col='grey')
  
  box(bty='l',lwd=0.5)
  par(mgp=c(3,0.25,0))
  axis(side=2,las=1,cex.axis=0.65,lwd=0,lwd.ticks=0.5, #NEW - this adds labels too far away from the axis 
       at=seq(0, 1, .25),labels=seq(0,100,25))
  axis(side=1,at = seq(0,12,2),cex.axis = 0.5, lwd=0,lwd.ticks=0.5, labels = NA, xpd = T) 
  text(x=seq(0,10,2),y= -0.1,adj=1,seq(0,10,2),xpd=T,cex=0.65,srt=45) #adds  labels to the plots 
  mtext(side=2, line = 1.5, 'Non-susceptible (%)', cex = 0.5, font =1)
  text('10 year difference',x=9.5,adj=1,cex=0.65,y=0.95,xpd=T)
  text(round.fn(diff_namesNS[,,i][,101]-diff_namesNS[,,i][,1]),x=9.5,adj=1,cex=0.65,y=0.85,xpd=T)
  mtext(side=1, line = 0.75, 'Years from introduction', cex = 0.5, font =1)
  
  #PLOT RESISTANCE  
  
  #Just Vaccine Effect 
  plot(y=(vaxonlyRES[,,i][1,]),x=t,ylim=c(0,1),xlim=c(-1,10),type='n',axes=F,ann=F) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
  polygon(y=c(vaxonlyRES[,,i][2,],rev(vaxonlyRES[,,i][3,])),x=c(t,rev(t)),
          col=rgb(1,0,0,0.25),lty=0) 
  lines(y=vaxonlyRES[,,i][1,],x=t,col='darkred',lwd=0.5)
  # lines(y=vaxonlyRES[,,i][2,],x=t,col='darkred',lwd=0.25)
  # lines(y=vaxonlyRES[,,i][3,],x=t,col='darkred',lwd=0.25)
  
  t_star = seq(-1, 0, 0.1)
  polygon(y=c(rep(vaxonlyRES[,,i][2,1], 11), rev(rep(vaxonlyRES[,,i][3,1], 11))),x=c(t_star, rev(t_star)),
          col=rgb(1,0,0,0.15),lty=0)
  lines(y=(rep(vaxonlyRES[,,i][1,1], 11)),x=t_star,col='darkred',lwd=0.5)
  # lines(y=(rep(vaxonlyRES[,,i][2,1],11)), x=t_star,col='darkred',lwd=0.25)
  # lines(y=(rep(vaxonlyRES[,,i][3,1], 11)), x=t_star,col='darkred',lwd=0.25)
  abline(v=0,lty='dotted',col='grey')
  
  #Vaccine + Calendar Effect
  polygon(y=c(vax_calRES[,,i][2,],rev(vax_calRES[,,i][3,])),x=c(t,rev(t)),
          col=rgb(1,0,0,0.35),lty=0)
  lines(y=vax_calRES[,,i][1,],x=t,col='darkred',lwd=0.5)
  lines(y=vax_calRES[,,i][2,],x=t,col='darkred',lwd=0.25)
  lines(y=vax_calRES[,,i][3,],x=t,col='darkred',lwd=0.25)
  
  t_star = seq(-1, 0, 0.1)
  polygon(y=c(rep(vax_calRES[,,i][2,1], 11), rev(rep(vax_calRES[,,i][3,1], 11))),x=c(t_star, rev(t_star)),
          col=rgb(1,0,0,0.35),lty=0) #changed from 0.15 to 0.35
  lines(y=(rep(vax_calRES[,,i][1,1], 11)),x=t_star,col='darkred',lwd=0.5)
  lines(y=(rep(vax_calRES[,,i][2,1],11)), x=t_star,col='darkred',lwd=0.25)
  lines(y=(rep(vax_calRES[,,i][3,1], 11)), x=t_star,col='darkred',lwd=0.25)
  abline(v=0,lty='dotted',col='grey')
  
  box(bty='l',lwd=0.5)
  par(mgp=c(3,0.25,0))
  axis(side=2,las=1,cex.axis=0.65,lwd=0,lwd.ticks=0.5, #NEW - this adds labels too far away from the axis 
       at=seq(0, 1, .25),labels=seq(0,100,25))
  axis(side=1,at = seq(0,12,2),cex.axis = 0.5, lwd=0,lwd.ticks=0.5, labels = NA, xpd = T) 
  text(x=seq(0,10,2),y= -0.1,adj=1,seq(0,10,2),xpd=T,cex=0.65,srt=45) #adds  labels to the plots 
  mtext(side=2, line = 1.5, 'Resistance (%)', cex = 0.5, font =1)
  text('10 year difference',x=9.5,adj=1,cex=0.65,y=0.95,xpd=T)
  text(round.fn(diff_namesRES[,,i][,101]-diff_namesRES[,,i][,1]),x=9.5,adj=1,cex=0.65,y=0.85,xpd=T)
  mtext(side=1, line = 0.75, 'Years from introduction', cex = 0.5, font =1)
  
}
## ADD LEGEND
par(mar=c(0,0,0,0))
plot(1,axes=F,ann=F,type='n',ylim=c(0,1),xlim=c(0,1))

#Nonsusceptible
polygon(x=c(0.35,0.45,0.45,0.35),y=c(0.5,0.5,0.6,0.6),col=rgb(0,0.1,1,0.15),lty=0) #pre pcv
polygon(x=c(0.6,0.7,0.7,0.6),y=c(0.5,0.5,0.6,0.6),col=rgb(0,0.1,1,0.45),lty=0) #Post PCV 

lines(x=c(0.35,0.45,NA,0.6,0.7),y=rep(0.6,5),col='dark blue',lwd=0.25) #0.6 is upper bound 
lines(x=c(0.35,0.45,NA,0.6,0.7),y=rep(0.55,5),col='dark blue',lwd=0.5) #median
lines(x=c(0.35,0.45,NA,0.6,0.7),y=rep(0.5,5),col='dark blue',lwd=0.25) #0.5 is lower bound
text(x=0.275,y=0.55,adj=1,'Prevalence\n(non-susceptible)',cex=0.65)
#text(x=c(0.425,0.725),y=rep(0.55,2),adj=0,c('Without secular \n Trend','With Secular \n Trend'),cex=0.65)
text(x=c(0.4,0.65),y=rep(0.7,2),adj=0.5,c('Without \n secular trend','With \n secular trend'),cex=0.65)

#Resistance
polygon(x=c(0.35,0.45,0.45,0.35),y=c(0.25,0.25,0.35,0.35),col=rgb(1,0,0,0.15),lty=0) #pre pcv
polygon(x=c(0.6,0.7,0.7,0.6),y=c(0.25,0.25,0.35,0.35),col=rgb(1,0,0,0.45),lty=0) #Post PCV

lines(x=c(0.35,0.45,NA,0.6,0.7),y=rep(0.35,5),col='dark red',lwd=0.25) 
lines(x=c(0.35,0.45,NA,0.6,0.7),y=rep(0.30,5),col='dark red',lwd=0.5) #median
lines(x=c(0.35,0.45,NA,0.6,0.7),y=rep(0.25,5),col='dark red',lwd=0.25) 
text(x=0.275,y=0.3,adj=1,'Prevalence\n(resistance)',cex=0.65)
#text(x=c(0.425,0.725),y=rep(0.3,2),adj=0,c('Vaccine \n Effect','Calendar &\n Vaccine Effect'),cex=0.65)

dev.off()

#To get values for each coefficient for the table: 
round(100*quantile(parsPenNs[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS8
round(100*quantile(parsPenRes[,2],c(0.5,0.025,0.975)),2) #Use this for the tableS8

#parsPenNs; parsPenRes; parsMacNs
#1 (intercept), 2 (years since vax), 3 (yr_2), 4 (yr_3), 5 (midpoint_year), 6 (log gdp), 7 (isolate type)
