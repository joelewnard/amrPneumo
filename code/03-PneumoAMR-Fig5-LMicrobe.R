#Plotting code for AMR VT/NVT Serotype Plots #
# Updated 12-2-20
#This file uses as input the data frames derived from 00-PneumoAMR-Fig5Data-LMicrobe
library(MASS)
library(dplyr)
library(lme4)

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
nvt_diff <- (outPenNs2[,121,2]-outPenNs2[,1,2]); q95fn(nvt_diff) #NVT
vt_diff <- (outPenNs2[,121,1]-outPenNs2[,1,1]); q95fn(vt_diff) #VT 

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
pdf("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/fig5_120820.pdf", width=5, height=1.75) #NEW

mat <- matrix(c(1,1,   12, 6,6,
                2,3,   12, 7,8,
                4,5,   12, 9,10,
                11,11, 12, 11, 11), nrow = 4, byrow = T)
#layout(mat,heights=c(rep(c(0.125,0.1,1),2),0.25),widths=c(1,1,0.1,1,1))
layout(mat,heights=c(0.125,0.1,1,0.175),widths=c(1,1,0.1,1,1))
#layout.show(n=11)
par(tck=-0.025)


par(mar=c(0, 0, 0, 0)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0, y =0.5, 'Penicillin',cex=0.85,font=2,adj=0) 

par(mar=c(0, 0, 0, 0)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0.5, y =0.5, 'Vaccine Type',cex=0.75,font=3,adj=0.5) 

par(mar=c(0, 0, 0, 0)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0.5, y =0.5, 'Non-Vaccine Type',cex=0.75,font=3,adj=0.5) 

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
mtext(side=2, line = 1.25, 'Non-susceptible (%)', cex = 0.4, font =1) #change cex to 0.4 from 0.5
text('10 year difference',x=10,adj=1,cex=0.65,y=0.95,xpd=T)
text(round.fn(outPenNs[,121,1]-outPenNs[,1,1]),x=10,adj=1,cex=0.65,y=0.85,xpd=T)
mtext(side=1, line = 1, 'Years of PCV use \n with serotype', cex = 0.4, font =1)

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
mtext(side=2, line = 1.25, 'Non-susceptible (%)', cex = 0.4, font =1)
text('10 year difference',x=10,adj=1,cex=0.65,y=0.95,xpd=T)
text(round.fn(outPenNs[,121,2]-outPenNs[,1,2]),x=10,adj=1,cex=0.65,y=0.85,xpd=T)
mtext(side=1, line = 1, 'Years of PCV use \n without serotype', cex = 0.4, font =1)



######################################################
## Plot 2: Mac
#######################################################

par(mar=c(0, 0, 0, 0)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0, y =0.5, 'Macrolides',cex=0.85,font=2,adj=0) 

par(mar=c(0,2.5,0,0.5)) ### b, l, t, r
plot(0, type='n',axes=F,ann=F,ylim=c(0,1),xlim=c(0,1)) #changed 12 in xlmin to 10 and old ylim=c(-4.5,0)
text(x =0.5, y =0.5, 'Vaccine Type',cex=0.75,font=3,adj=0.5) 

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
mtext(side=2, line = 1.25, 'Non-susceptible (%)', cex = 0.4, font =1) #change cex to 0.4 from 0.5
text('10 year difference',x=10,adj=1,cex=0.65,y=0.25,xpd=T)
text(round.fn(outMacNs[,121,1]-outMacNs[,1,1]),x=10,adj=1,cex=0.65,y=0.15,xpd=T)
mtext(side=1, line = 1, 'Years of PCV use \n with serotype', cex = 0.4, font =1)

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
mtext(side=2, line = 1.25, 'Non-susceptible (%)', cex = 0.4, font =1)
text('10 year difference',x=10,adj=1,cex=0.65,y=0.95,xpd=T)
text(round.fn(outMacNs[,121,2]-outMacNs[,1,2]),x=10,adj=1,cex=0.65,y=0.85,xpd=T)
mtext(side=1, line = 1, 'Years of PCV use \n without serotype', cex = 0.4, font =1)

####################
## ADD AXIS LABELS ##
####################
par(mar=c(0,0,0,0))
plot(1,axes=F,ann=F,type='n',ylim=c(0,1),xlim=c(0,1))

polygon(x=c(0.3,0.45,0.45,0.3),y=c(0.2,0.2,0.6,0.6),col=rgb(0,0.1,1,0.15),lty=0) #pre pcv
polygon(x=c(0.55,0.7,0.7,0.55),y=c(0.2,0.2,0.6,0.6),col=rgb(0,0.1,1,0.45),lty=0) #Post PCV

lines(x=c(0.3,0.45,NA,0.55,0.7),y=rep(0.6,5),col='dark blue',lwd=0.25) #0.9 is upper bound
lines(x=c(0.3,0.45,NA,0.55,0.7),y=rep(0.4,5),col='dark blue',lwd=0.5) #median
lines(x=c(0.3,0.45,NA,0.55,0.7),y=rep(0.2,5),col='dark blue',lwd=0.25) #0.5 is lower bound
text(x=0.275,y=0.65,adj=1,'Prevalence\n(non-susceptible)',cex=0.65)
#text(x=c(0.425,0.725),y=rep(0.65,2),adj=0,c('Without secular \n effect','With secular\n effect'),cex=0.65)
text(x=0.3,y=0.9,adj=0,'Without secular effect',cex=0.65)
text(x=0.55,y=0.9,adj=0,'With secular effect',cex=0.65)

dev.off() 

