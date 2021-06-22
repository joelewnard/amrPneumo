# Combined figure 4 and figure 5 script 
# This uses figure 4 corpus
# Updated on 2/23/21 to add in secular and non-secular trends 

library(dplyr)
library(lme4)
library(MASS)

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/out_fulldta_ns.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/out_fulldta_res.rda")

#Find values of r_id that are in both NS and Res
ns_id <- unique(out_fulldta_ns$r_id) #453
res_id <- unique(out_fulldta_res$r_id) #379

keep <- ns_id[ns_id %in% res_id] #297 studies 

#Filter out_fulldta_ns and out_fulldta_res to only include r_id in keep
out_fulldta_ns2 <- out_fulldta_ns %>% 
  filter(r_id %in% keep)
out_fulldta_res2 <- out_fulldta_res %>% 
  filter(r_id %in% keep)

nrow(out_fulldta_ns2) + nrow(out_fulldta_res2)

#drug == X
#9 = penicillin 
#8 = macrolide 
#10 = SXT 
#2 = ceph 
#11 = tetracycline 

mod_pen_ns <- out_fulldta_ns2 %>% filter(drug == 9)
mod_mac_ns <- out_fulldta_ns2 %>% filter(drug == 8)
mod_sxt_ns <- out_fulldta_ns2 %>% filter(drug == 10)
mod_ceph_ns <- out_fulldta_ns2 %>% filter(drug == 2)
mod_tet_ns <- out_fulldta_ns2 %>% filter(drug == 11)

mod_pen_res <- out_fulldta_res2 %>% filter(drug == 9)
mod_mac_res <- out_fulldta_res2 %>% filter(drug == 8)
mod_sxt_res <- out_fulldta_res2 %>% filter(drug == 10)
mod_ceph_res <- out_fulldta_res2 %>% filter(drug == 2)
mod_tet_res <- out_fulldta_res2 %>% filter(drug == 11)


## Create new models where data is isolatedtot studies with both RES and NS data

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
plot_names <- c("A.  Penicillin, all isolates", "Macrolides, all isolates", "Sulfamethoxazole/trimethoprin, all isolates", 
                "Third generation cephalosporins, all isolates", "Tetracycline, all isolates")
diff_namesNS <- array(c(outPenNs, outMacNs, outSxtNs,outTetNs,outCephNs), 
                      dim = c(1e4, 171, 10))
diff_namesRES <- array(c(outPenRes, outMacRes, outSxtRes, outTetRes, outCephRes), 
                       dim = c(1e4, 171, 10))
diff_namesNSCal <- array(c(outPenNs2, outMacNs2, outSxtNs2,outTetNs2,outCephNs2), 
                      dim = c(1e4, 171, 10))
diff_namesRESCal <- array(c(outPenRes2, outMacRes2, outSxtRes2, outTetRes2, outCephRes2), 
                       dim = c(1e4, 171, 10))

# DATA FOR TABLE S6 on MetaReg Parameters 
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
pdf("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/plot_fig4fig5Combined_022321.pdf", width=5, height=6.5) 


# mat <- matrix(c(1,1,   18, 4,4,
#                 2,3,   18, 5,6,
#                 7,7,   18, 10,10,
#                 8,9,   18, 11,12,
#                 13,13, 18, 16,16,
#                 14,15, 18, 17,17), nrow = 6, byrow = T)
# mat <- matrix(c(1,1,   17, 4,4,
#                 2,3,   17, 5,6,
#                 7,7,   17, 10,10,
#                 8,9,   17, 11,12,
#                 13,13, 17, 16,16,
#                 14,15, 17, 16,16), nrow = 6, byrow = T)
# layout(mat,heights=rep(c(0.1,1),3),widths=c(1,1,0.1,1,1))

mat2 <- matrix(c(1,1,   28, 4,4,
                 2,3,   28, 5,6,
                 7,7,   28, 10,10,
                 8,9,   28, 11,12,
                 13,13, 28, 16,16,
                 14,15, 28, 16,16, 
                 17,17, 28, 17,17, 
                 18,18, 28, 23,23, 
                 19,20, 28, 24,25,
                 21,22, 28, 26,27), nrow = 10, byrow = T)

layout(mat2,heights=c(rep(c(0.1,1),3), 0.1, 0.2, 0.1, 1),widths=c(1,1,0.1,1,1))
#layout.show(n= 28)
par(tck=-0.025)

yValues <- c(0.8, 0.8, 0.86, 0.8, 0.8)
yValues2 <- c(0.6, 0.6, 0.17, 0.6, 0.6)

for (i in 1:5){ #plot names 
  
  par(mar=c(0, 0, 0, 0)) ### b, l, t, r
  plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
  text(x =0, y =0.5, plot_names[i],cex=0.85,font=2,adj=0) 
  
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
  text('10 year difference',x=10.9,adj=1,cex=0.65,y=1,xpd=T)
  text(paste("Without secular trend:\n",(round.fn(diff_namesNS[,,i][,101]-diff_namesNS[,,i][,1]))),x=10.9,adj=1,cex=0.65,y=yValues[i],xpd=T)
  text(paste("With secular trend:\n",(round.fn(diff_namesNSCal[,,i][,101]-diff_namesNSCal[,,i][,1]))),x=10.9,adj=1,cex=0.65,y=yValues2[i],xpd=T)
  mtext(side=1, line = 0.75, 'Years of PCV use', cex = 0.5, font =1)
  
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
  text('10 year difference',x=10.9,adj=1,cex=0.65,y=1,xpd=T)
  text(paste("Without secular trend:\n",(round.fn(diff_namesRES[,,i][,101]-diff_namesRES[,,i][,1]))),x=10.9,adj=1,cex=0.65,y=yValues[i],xpd=T)
  text(paste("With secular trend:\n",(round.fn(diff_namesRESCal[,,i][,101]-diff_namesRESCal[,,i][,1]))),x=10.9,adj=1,cex=0.65,y=yValues2[i],xpd=T)
  mtext(side=1, line = 0.75, 'Years of PCV use', cex = 0.5, font =1)
  
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

#dev.off()

#To get values for each coefficient for the table: 
round(100*quantile(parsPenNs[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS6
round(100*quantile(parsPenRes[,2],c(0.5,0.025,0.975)),2) #Use this for the tableS6

#parsPenNs; parsPenRes; parsMacNs
#1 (intercept), 2 (years since vax), 3 (yr_2), 4 (yr_3), 5 (midpoint_year), 6 (log gdp), 7 (isolate type)


### SWITCH TO VT OR NVT 
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/mod_pen_ns_st.rda")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/data/mod_mac_ns_st.rda")
subset_penNS = mod_pen_ns_st
subset_macNS = mod_mac_ns_st

penNS_new = lmer(ns~vac_type+log(gdp)+isolate_type+midpoint_yr+(1|r_id)+(1|region)+
                   sqrt(yr_since_vax)*vac_type,data=subset_penNS); summary(penNS_new); BIC(penNS_new)
macNS_new = lmer(ns~vac_type+log(gdp)+isolate_type+midpoint_yr+(1|r_id)+(1|region)+
                   sqrt(yr_since_vax)*vac_type,data=subset_macNS); summary(macNS_new); BIC(macNS_new)
set.seed(252)
parsPenNs = mvrnorm(1e4,fixef(penNS_new),vcov(penNS_new))
parsMacNs = mvrnorm(1e4,fixef(macNS_new),vcov(macNS_new))

######################################################
### CALCULATE VALUES FOR PARAMETERS FOR TABLS S8 now S10 ####
######################################################

round(100*quantile(parsPenNs[,2],c(0.5,0.025,0.975)),2) #Use this for the tableS8
round(100*quantile(parsMacNs[,5],c(0.5,0.025,0.975)),2) #Use this for the tableS8
#calendar time is coef 5, GDP log is 3, invasive source is 4, vaccine type type isolate 2

#remove 8th parameter if not running the VT model and add back: parsPenNs[,7]*sqrt(t[i])
t = seq(-2,10,0.1); t[t<0] = 0
outPenNs = outMacNs = outPenNs2 = outMacNs2 = array(NA,dim=c(1e4,length(t),2))

for (i in 1:length(t)){
  outPenNs[,i,2] = parsPenNs[,1] + parsPenNs[,3]*10 + parsPenNs[,4] + parsPenNs[,5]*2006 + parsPenNs[,6]*sqrt(t[i]) #nvt- OLD USE THESE
  outPenNs[,i,1] = outPenNs[,i,2] + parsPenNs[,2] + parsPenNs[,7]*sqrt(t[i]) #vaccine type- no secular trend
  
  outMacNs[,i,2] = parsMacNs[,1] + parsMacNs[,3]*10 + parsMacNs[,4] + parsMacNs[,5]*2006 + parsMacNs[,6]*sqrt(t[i])
  outMacNs[,i,1] = outMacNs[,i,2] + parsMacNs[,2] + parsMacNs[,7]*sqrt(t[i])
  
  outPenNs2[,i,2] = parsPenNs[,1] + parsPenNs[,3]*10 + parsPenNs[,4] + parsPenNs[,5]*(2006 + t[i]) + parsPenNs[,6]*sqrt(t[i]) #nvt + secular trend
  outPenNs2[,i,1] = outPenNs2[,i,2] + parsPenNs[,2] + parsPenNs[,7]*sqrt(t[i]) #vt + secular trend 
  
  outMacNs2[,i,2] = parsMacNs[,1] + parsMacNs[,3]*10 + parsMacNs[,4] + parsMacNs[,5]*(2006 + t[i]) + parsMacNs[,6]*sqrt(t[i])
  outMacNs2[,i,1] = outMacNs2[,i,2] + parsMacNs[,2] + parsMacNs[,7]*sqrt(t[i])
}


q95fn = function(x){return(quantile(x,c(0.5,0.025,0.975)))}

#Calculate the 10 year difference for Penicillin VT and NVT for TEXT in PAPER 
nvt_diff <- (outPenNs2[,121,2]-outPenNs2[,1,2]); q95fn(nvt_diff) #NVT + Secular Trend
vt_diff <- (outPenNs2[,121,1]-outPenNs2[,1,1]); q95fn(vt_diff) #VT + Secular Trend

#### NEW ADDITION SECULAR TREND AND NO SECULAR TREND TO FIGURE ##### START HERE KRISTIN
text(round.fn(outPenNs[,121,1]-outPenNs[,1,1]),x=10,adj=1,cex=0.65,y=0.85,xpd=T) #PEN VT WITHOUT SECULAR TREND
# ADD PEN VT WITH SECULAR TREND: (outPenNs2[,121,1]-outPenNs2[,1,1])

text(round.fn(outPenNs[,121,2]-outPenNs[,1,2]),x=10,adj=1,cex=0.65,y=0.85,xpd=T) #PEN NVT WITHOUT SECULAR TREND
# ADD PEN NVT WITH SECULAR TRNED: (outPenNs2[,121,2]-outPenNs2[,1,2])

text(round.fn(outMacNs[,121,1]-outMacNs[,1,1]),x=10,adj=1,cex=0.65,y=0.15,xpd=T) #VT Macrolide without secular trend
# ADD VT MACROLIDE WITH SECULAR TREND: (outMacNs2[,121,1]-outMacNs2[,1,1])

text(round.fn(outMacNs[,121,2]-outMacNs[,1,2]),x=10,adj=1,cex=0.65,y=0.85,xpd=T) #NVT Macrolide without secular trend
# AD NVT MACROLIDE WITHOUT SECULAR TREND: (outMacNs2[,121,2]-outMacNs2[,1,2])

estsPenNsVt = apply(outPenNs[,,1],2,q95fn); estsPenNsNvt = apply(outPenNs[,,2],2,q95fn) #does not take into account calendar time 
estsPenNsVt2 = apply(outPenNs2[,,1],2,q95fn); estsPenNsNvt2 = apply(outPenNs2[,,2],2,q95fn) #dark- takes into account calendar time

estsMacNsVt = apply(outMacNs[,,1],2,q95fn); estsMacNsNvt = apply(outMacNs[,,2],2,q95fn)
estsMacNsVt2 = apply(outMacNs2[,,1],2,q95fn); estsMacNsNvt2 = apply(outMacNs2[,,2],2,q95fn)

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

t_star = seq(-1, 0, 0.1)

########################################################
# Create Plots #
########################################################
#Update layout:
#pdf("plot_st_metareg_051320.pdf", width=5, height=1.75) #OLD
# pdf("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/fig5_120820.pdf", width=5, height=1.75) #NEW
# 
# mat <- matrix(c(1,1,   12, 6,6,
#                 2,3,   12, 7,8,
#                 4,5,   12, 9,10,
#                 11,11, 12, 11, 11), nrow = 4, byrow = T)
# #layout(mat,heights=c(rep(c(0.125,0.1,1),2),0.25),widths=c(1,1,0.1,1,1))
# layout(mat,heights=c(0.125,0.1,1,0.175),widths=c(1,1,0.1,1,1))
# layout.show(n= 12)
# #layout.show(n=11)
# par(tck=-0.025)


par(mar=c(0, 0, 0, 0)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #for blank dividing plot

plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0, y =0.5, 'B.  Penicillin, by PCV inclusion',cex=0.85,font=2,adj=0) 

par(mar=c(0, 0, 0, 0)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0.5, y =0.5, 'Vaccine Type',cex=0.75,font=3,adj=0.45) 

par(mar=c(0, 0, 0, 0)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0.5, y =0.5, 'Non-Vaccine Type',cex=0.75,font=3,adj=0.25) 

#dev.off()

######################################################
## Plot 1: Penicillin 
#######################################################
par(mar=c(2.5,2.5,0.5,0.5)) ### b, l, t, r


## VACCINE TYPE PLOTS ## 

## JUST VACCINE EFFECT = LIGHT 
plot(1,ylim=c(0,1),xlim=c(-1,10),type='n',axes=F,ann=F) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
polygon(y=c(estsPenNsVt[2,],rev(estsPenNsVt[3,])),
        x=c(t,rev(t)),col=rgb(0,0.1,1,0.25),lty=0)

lines(y=estsPenNsVt[1,],x=t,col='darkblue',lwd=0.5)

polygon(y=c(rep(estsPenNsVt[2,1],11),rev(rep(estsPenNsVt[3,1], 11))),
        x=c(t_star, rev(t_star)),col=rgb(0,0.1,1,0.15),lty=0) 
lines(y=(rep(estsPenNsVt[1,1],11)),x=t_star,col='darkblue',lwd=0.5)

## ADD IN DARK LINES- TREND + VACCINE = DARK  (try green instead of red 0.9,0.6,0.7,0.25)
polygon(y=c(estsPenNsVt2[2,],rev(estsPenNsVt2[3,])),
        x=c(t,rev(t)),col=rgb(0,0.1,1,0.45),lty=0)

lines(y=estsPenNsVt2[1,],x=t,col='darkblue',lwd=0.5)
lines(y=estsPenNsVt2[2,],x=t,col='darkblue',lwd=0.25)
lines(y=estsPenNsVt2[3,],x=t,col='darkblue',lwd=0.25)

polygon(y=c(rep(estsPenNsVt2[2,1],11),rev(rep(estsPenNsVt2[3,1], 11))),
        x=c(t_star, rev(t_star)),col=rgb(0,0.1,1,0.45),lty=0) #NEW 05-12-20 changed to 0.45 from 0.15!!!!!!!!!!!!!!!
lines(y=(rep(estsPenNsVt2[1,1],11)),x=t_star,col='darkblue',lwd=0.5)
lines(y=(rep(estsPenNsVt2[2,1],11)), x=t_star,col='darkblue',lwd=0.25)
lines(y=(rep(estsPenNsVt2[3,1],11)), x=t_star,col='darkblue',lwd=0.25)
###

abline(v=0,lty='dotted',col='grey')

box(bty='l',lwd=0.5)
par(mgp=c(3,0.25,0))
axis(side=2,las=1,cex.axis=0.65,lwd=0,lwd.ticks=0.5, #NEW - this adds labels too far away from the axis 
     at=seq(0, 1, .25),labels=seq(0,100,25))


axis(side=1,at = seq(0,10,1),lwd=0,lwd.ticks=0.5, labels = NA, xpd = T) 
text(x=seq(0,10,2),y= -0.1,adj=1,seq(0,10,2),xpd=T,cex=0.65,srt=45) #adds  labels to the plots 
mtext(side=2, line = 1.25, 'Non-susceptible (%)', cex = 0.48, font =1) #change cex to 0.4 from 0.5
text('10 year difference',x=10,adj=1,cex=0.65,y=1,xpd=T)
text(paste("Without secular trend:\n",(round.fn(outPenNs[,121,1]-outPenNs[,1,1]))),x=10,adj=1,cex=0.65,y=0.85,xpd=T) #WITHOUT SECULAR TREND?
text(paste("With secular trend:\n",(round.fn(outPenNs2[,121,2]-outPenNs2[,1,2]))),x=10,adj=1,cex=0.65,y=0.65,xpd=T)
mtext(side=1, line = 1.25, 'Years of PCV use \n with serotype', cex = 0.48, font =1)

## NON-VACCINE TYPE PLOTS ## 

par(mar=c(2.5,2.5,0.5,0.5)) ### b, l, t, r
plot(1,ylim=c(0,1),xlim=c(-1,10),type='n',axes=F,ann=F) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)

## JUST VACCINE EFFECT = LIGHT 

polygon(y=c(estsPenNsNvt[2,],rev(estsPenNsNvt[3,])),
        x=c(t,rev(t)),col=rgb(0,0.1,1,0.25),lty=0)
lines(y=estsPenNsNvt[1,],x=t,col='darkblue',lwd=0.5)

polygon(y=c(rep(estsPenNsNvt[2,1],11),rev(rep(estsPenNsNvt[3,1], 11))),
        x=c(t_star, rev(t_star)),col=rgb(0,0.1,1,0.15),lty=0)
lines(y=(rep(estsPenNsNvt[1,1],11)),x=t_star,col='darkblue',lwd=0.5)

## ADD IN DARK LINES- TREND + VACCINE = DARK  
polygon(y=c(estsPenNsNvt2[2,],rev(estsPenNsNvt2[3,])),
        x=c(t,rev(t)),col=rgb(0,0.1,1,0.45),lty=0)
lines(y=estsPenNsNvt2[1,],x=t,col='darkblue',lwd=0.5)
lines(y=estsPenNsNvt2[2,],x=t,col='darkblue',lwd=0.25)
lines(y=estsPenNsNvt2[3,],x=t,col='darkblue',lwd=0.25)

polygon(y=c(rep(estsPenNsNvt2[2,1],11),rev(rep(estsPenNsNvt2[3,1], 11))),
        x=c(t_star, rev(t_star)),col=rgb(0,0.1,1,0.45),lty=0)
lines(y=(rep(estsPenNsNvt2[1,1],11)),x=t_star,col='darkblue',lwd=0.5)
lines(y=(rep(estsPenNsNvt2[2,1],11)), x=t_star,col='darkblue',lwd=0.25)
lines(y=(rep(estsPenNsNvt2[3,1],11)), x=t_star,col='darkblue',lwd=0.25)

abline(v=0,lty='dotted',col='grey')

box(bty='l',lwd=0.5)
par(mgp=c(3,0.25,0))
axis(side=2,las=1,cex.axis=0.65,lwd=0,lwd.ticks=0.5, #NEW - this adds labels too far away from the axis 
     at=seq(0, 1, .25),labels=seq(0,100,25))


axis(side=1,at = seq(0,10,1),lwd=0,lwd.ticks=0.5, labels = NA, xpd = T) 
text(x=seq(0,10,2),y= -0.1,adj=1,seq(0,10,2),xpd=T,cex=0.65,srt=45) #adds  labels to the plots 
mtext(side=2, line = 1.25, 'Non-susceptible (%)', cex = 0.48, font =1)
text('10 year difference',x=10,adj=1,cex=0.65,y=1,xpd=T)
text(paste("Without secular trend:\n",(round.fn(outPenNs[,121,2]-outPenNs[,1,2]))),x=10,adj=1,cex=0.65,y=0.85,xpd=T) #WITHOUT SECULAR TREND?
text(paste("With secular trend:\n",(round.fn(outPenNs2[,121,2]-outPenNs2[,1,2]))),x=10,adj=1,cex=0.65,y=0.65,xpd=T)
mtext(side=1, line = 1.25, 'Years of PCV use \n without serotype', cex = 0.46, font =1)


######################################################
## Plot 2: Mac
#######################################################

par(mar=c(0, 0, 0, 0)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0, y =0.5, 'Macrolides, by PCV inclusion',cex=0.85,font=2,adj=0) 

par(mar=c(0,2.5,0,0.5)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0.5, y =0.5, 'Vaccine Type',cex=0.75,font=3,adj=0.7) 

plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0.5, y =0.5, 'Non-Vaccine Type',cex=0.75,font=3,adj=0.5) 


par(mar=c(2.5,2.5,0.5,0.5)) ### b, l, t, r

## VACCINE TYPE PLOTS ## 
plot(1,ylim=c(0,1),xlim=c(-1,10),type='n',axes=F,ann=F) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)

## JUST VACCINE EFFECT = LIGHT 
polygon(y=c(estsMacNsVt[2,],rev(estsMacNsVt[3,])),
        x=c(t,rev(t)),col=rgb(0,0.1,1,0.25),lty=0)
lines(y=estsMacNsVt[1,],x=t,col='darkblue',lwd=0.5)

polygon(y=c(rep(estsMacNsVt[2,1],11),rev(rep(estsMacNsVt[3,1], 11))),
        x=c(t_star, rev(t_star)),col=rgb(0,0.1,1,0.15),lty=0)
lines(y=(rep(estsMacNsVt[1,1],11)),x=t_star,col='darkblue',lwd=0.5)

## ADD IN DARK LINES- TREND + VACCINE = DARK  (try green instead of red 0.9,0.6,0.7,0.25)
polygon(y=c(estsMacNsVt2[2,],rev(estsMacNsVt2[3,])),
        x=c(t,rev(t)),col=rgb(0,0.1,1,0.45),lty=0)

lines(y=estsMacNsVt2[1,],x=t,col='darkblue',lwd=0.5)
lines(y=estsMacNsVt2[2,],x=t,col='darkblue',lwd=0.25)
lines(y=estsMacNsVt2[3,],x=t,col='darkblue',lwd=0.25)

polygon(y=c(rep(estsMacNsVt2[2,1],11),rev(rep(estsMacNsVt2[3,1], 11))),
        x=c(t_star, rev(t_star)),col=rgb(0,0.1,1,0.45),lty=0)
lines(y=(rep(estsMacNsVt2[1,1],11)),x=t_star,col='darkblue',lwd=0.5)
lines(y=(rep(estsMacNsVt2[2,1],11)), x=t_star,col='darkblue',lwd=0.25)
lines(y=(rep(estsMacNsVt2[3,1],11)), x=t_star,col='darkblue',lwd=0.25)

abline(v=0,lty='dotted',col='grey')

box(bty='l',lwd=0.5)
par(mgp=c(3,0.25,0))
axis(side=2,las=1,cex.axis=0.65,lwd=0,lwd.ticks=0.5, #NEW - this adds labels too far away from the axis 
     at=seq(0, 1, .25),labels=seq(0,100,25))


axis(side=1,at = seq(0,10,1),lwd=0,lwd.ticks=0.5, labels = NA, xpd = T) 
text(x=seq(0,10,2),y= -0.1,adj=1,seq(0,10,2),xpd=T,cex=0.65,srt=45) #adds  labels to the plots 
mtext(side=2, line = 1.25, 'Non-susceptible (%)', cex = 0.48, font =1) #change cex to 0.4 from 0.5
text('10 year difference',x=10,adj=1,cex=0.65,y=1,xpd=T)
text(paste("Without secular trend:\n",(round.fn(outMacNs[,121,1]-outMacNs[,1,1]))),x=10,adj=1,cex=0.65,y=0.28,xpd=T) #WITHOUT SECULAR TREND?
text(paste("With secular trend:\n",(round.fn(outMacNs2[,121,1]-outMacNs2[,1,1]))),x=10,adj=1,cex=0.65,y=0.08,xpd=T) #VT MAC WITH SECULAR TREND
mtext(side=1, line = 1.25, 'Years of PCV use \n with serotype', cex = 0.46, font =1)


## NON-VACCINE TYPE PLOTS ## 

par(mar=c(2.5,2.5,0.5,0.5)) ### b, l, t, r
plot(1,ylim=c(0,1),xlim=c(-1,10),type='n',axes=F,ann=F) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)

## JUST VACCINE EFFECT = LIGHT 

polygon(y=c(estsMacNsNvt[2,],rev(estsMacNsNvt[3,])),
        x=c(t,rev(t)),col=rgb(0,0.1,1,0.25),lty=0)

lines(y=estsMacNsNvt[1,],x=t,col='darkblue',lwd=0.5)
#lines(y=estsMacNsNvt[2,],x=t,col='darkblue',lwd=0.25)
#lines(y=estsMacNsNvt[3,],x=t,col='darkblue',lwd=0.25)

polygon(y=c(rep(estsMacNsNvt[2,1],11),rev(rep(estsMacNsNvt[3,1], 11))),
        x=c(t_star, rev(t_star)),col=rgb(0,0.1,1,0.15),lty=0)
lines(y=(rep(estsMacNsNvt[1,1],11)),x=t_star,col='darkblue',lwd=0.5)
#lines(y=(rep(estsMacNsNvt[2,1],11)), x=t_star,col='darkblue',lwd=0.25)
#lines(y=(rep(estsMacNsNvt[3,1],11)), x=t_star,col='darkblue',lwd=0.25)

## ADD IN DARK LINES- TREND + VACCINE = DARK  (try green instead of red 0.9,0.6,0.7,0.25)
polygon(y=c(estsMacNsNvt2[2,],rev(estsMacNsNvt2[3,])),
        x=c(t,rev(t)),col=rgb(0,0.1,1,0.45),lty=0)

lines(y=estsMacNsNvt2[1,],x=t,col='darkblue',lwd=0.5)
lines(y=estsMacNsNvt2[2,],x=t,col='darkblue',lwd=0.25)
lines(y=estsMacNsNvt2[3,],x=t,col='darkblue',lwd=0.25)

polygon(y=c(rep(estsMacNsNvt2[2,1],11),rev(rep(estsMacNsNvt2[3,1], 11))),
        x=c(t_star, rev(t_star)),col=rgb(0,0.1,1,0.45),lty=0)
lines(y=(rep(estsMacNsNvt2[1,1],11)),x=t_star,col='darkblue',lwd=0.5)
lines(y=(rep(estsMacNsNvt2[2,1],11)), x=t_star,col='darkblue',lwd=0.25)
lines(y=(rep(estsMacNsNvt2[3,1],11)), x=t_star,col='darkblue',lwd=0.25)
###

abline(v=0,lty='dotted',col='grey')

box(bty='l',lwd=0.5)
par(mgp=c(3,0.25,0))
axis(side=2,las=1,cex.axis=0.65,lwd=0,lwd.ticks=0.5, #NEW - this adds labels too far away from the axis 
     at=seq(0, 1, .25),labels=seq(0,100,25))


axis(side=1,at = seq(0,10,1),lwd=0,lwd.ticks=0.5, labels = NA, xpd = T) 
text(x=seq(0,10,2),y= -0.1,adj=1,seq(0,10,2),xpd=T,cex=0.65,srt=45) #adds  labels to the plots 
mtext(side=2, line = 1.25, 'Non-susceptible (%)', cex = 0.48, font =1)
text('10 year difference',x=10,adj=1,cex=0.65,y=1,xpd=T)
text(paste("Without secular trend:\n",(round.fn(outMacNs[,121,2]-outMacNs[,1,2]))),x=10,adj=1,cex=0.65,y=0.85,xpd=T) #NVT MAC WITHOUT SECULAR TREND?
text(paste("With secular trend:\n",(round.fn(outMacNs2[,121,2]-outMacNs2[,1,2]))),x=10,adj=1,cex=0.65,y=0.10,xpd=T) #NVT MAC WITH SECULAR TREND
mtext(side=1, line = 1.25, 'Years of PCV use \n without serotype', cex = 0.46, font =1)

####################
## ADD AXIS LABELS ##
####################
# par(mar=c(0,0,0,0))
# plot(1,axes=F,ann=F,type='n',ylim=c(0,1),xlim=c(0,1))
# 
# polygon(x=c(0.3,0.45,0.45,0.3),y=c(0.2,0.2,0.6,0.6),col=rgb(0,0.1,1,0.15),lty=0) #pre pcv
# polygon(x=c(0.55,0.7,0.7,0.55),y=c(0.2,0.2,0.6,0.6),col=rgb(0,0.1,1,0.45),lty=0) #Post PCV
# 
# lines(x=c(0.3,0.45,NA,0.55,0.7),y=rep(0.6,5),col='dark blue',lwd=0.25) #0.9 is upper bound
# lines(x=c(0.3,0.45,NA,0.55,0.7),y=rep(0.4,5),col='dark blue',lwd=0.5) #median
# lines(x=c(0.3,0.45,NA,0.55,0.7),y=rep(0.2,5),col='dark blue',lwd=0.25) #0.5 is lower bound
# text(x=0.275,y=0.65,adj=1,'Prevalence\n(non-susceptible)',cex=0.65)
# #text(x=c(0.425,0.725),y=rep(0.65,2),adj=0,c('Without secular \n effect','With secular\n effect'),cex=0.65)
# text(x=0.3,y=0.9,adj=0,'Without secular effect',cex=0.65)
# text(x=0.55,y=0.9,adj=0,'With secular effect',cex=0.65)

dev.off() 

