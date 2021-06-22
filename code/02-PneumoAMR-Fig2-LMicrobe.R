# Forest Plots for Figure 2 and Figure 3
# Create Table S6 and Table S7 #
# Kristin Andrejko
# Updated 12-01-20


cbind(regs,ests_3[,,1])

#setwd('~/Google drive (jlewnard@berkeley.edu)/pneumo AMR sys rev/02 AMR Pneumo Isolates /joe playing')

load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_pen_inv.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_pen_ninv.Rdata")
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_mac_inv.Rdata") #dat3
load("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/11-30-20/forest_mac_ninv.Rdata") #dat4


# load("forest_pen_inv.rda") #should be set to dat1
# load("forest_pen_ninv.rda") #should be set to dat2
# load("forest_mac_inv.rda") #should be set to dat3
# load("forest_mac_ninv.rda") #should be set to dat4

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

# Plot 1a - Penicillin Invasive Nonsusceptible ----

pdf("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/prepost_plots_3yr_pen.pdf", width=6, height=7.5) 

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

ys = c(1:6,
       (8:12)+0.5,
       (14:18)+1,
       20+1.5,
       22+2,
       (24:27)+2.5,
       (29:31)+3) 

regs_clean[regs_clean=='Southeast Asia, East Asia, & Oceania'] = 'Southeast/East Asia & Oceania'
regs_clean[regs_clean=='Central Europe, Eastern Europe'] = 'Central & Eastern Europe'
pchs = rep(16,length(ys)); pchs[c(6,11,16,17,18,22,25)] = 22 #type of point, old: 6,10,15,16,17,21,24
cexs = rep(0.75,length(ys)); cexs[c(6,11,16,17,18,22,25)] = 1.25 #changing the size of the points 
fonts = rep(1,length(ys)); fonts[c(6,11,16,17,18,22,25)] = 2 #normal vs. bold 
cols = c('cadetblue3','darkorchid3')
colsoutline = c('cadetblue4','darkorchid4')
mat = matrix(1:7,nrow=1,ncol=7,byrow=T)



layout(mat,widths=c(1.7,2,0.3,0.3,2,0.3,0.3)) #creates a layout with 2 rows (pen vs. mac, 7 columns (inv vs. ninv for nonsusceptible isolates))


grays = ys[c(1,3,5,7,9,11,12,14,16,17,18,19,21,23,25)]
hlines = c(0.5,6.5,
           8,13,
           14.5,19.5,
           21,22,
           23.5,24.5,
           26,30,
           31.5,34.5)

par(tck=-0.02)
par(mar=c(2.7,0.25,1,0.25)) # bltr
plot(1,ylim=c(-33,-1),type='n',xlim=c(0,1),axes=F,ann=F) #change -29 to -30, changed -30 to -31
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs_clean)){
  text(y=-ys[i],x=1,adj=1,cex=0.75,regs_clean[i],font=fonts[i]) #adds regions w/ appropriate spacing 
}
abline(h=-hlines,lwd=0.5,xpd=T)

legend(x = 0.01, y = -34.5, 
       legend = c("pre-PCV Introduction", "3+ years post-PCV Introduction"), 
       col = c('cadetblue3','darkorchid3'),
       pch = c(16,16), 
       xpd = TRUE, 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.75, 
       text.col = "black", 
       horiz = F, 
       inset = c(0.01, 0.01)) 

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #initalizes a plot 
erapos = c(0.15,-0.15) #sets up some space between estimates, changed from 0.15
erapos_2 = c(0.23, -0.23)

for (i in grays){polygon(x=c(0,1,1,0),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}

for (i in 1:length(regs)) for (j in 1:2){ #adds CI and point estimates to the plot with colors, and sizes 
  if (sum(ests[i,2:3,j]==c(0,0))!=2){
    lines(y=rep(-ys[i],2)+erapos[j],x=ests[i,2:3,j],col=colsoutline[j],lwd=0.5)
    points(y=-ys[i]+erapos[j],x=ests[i,1,j],pch=pchs[i],cex=cexs[i],col=colsoutline[j],bg=cols[j],lwd=0.5) 
  }
}
abline(h=-hlines,lwd=0.5,xpd=T)

axispos = hlines[c(2,4,6,8,10,12,14)]
for (i in 1:length(axispos)){
  axis(side=1,pos=-axispos[i],at=seq(0,1,0.25),labels=NA,lwd=0,lwd.ticks=0.5,xpd=T)
  text(x=seq(0,1,0.25),y=-axispos[i]-0.35,adj=1,seq(0,100,25),xpd=T,cex=0.65,srt=45) #adds percentage labels to the plots 
}

mtext(side=1,'Prop. (%) nonsusceptible to penicillin',cex=0.55,font=1,line=1.5,xpd=T) #adds axis label 
text(y=0,x=0,adj=0,'A. Invasive isolates',cex=1,font=2) #Adds "Invasive isolates" to the top of the column rows

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #moves to next layout region to add number of studies 
polygon(x=c(-1e3,1e3,1e3,-1e3),y=c(-1e3,-1e3,1e3,1e3),col='white',xpd=T,lty=0)
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){
  text(x=0.5,adj=0.5,studynums[i,j],y=-ys[i]+erapos_2[j],cex=0.675,font=fonts[i],xpd=T)
}
text(y=-0.25,x=-0.15,'Studies',cex=0.75,adj=0,srt=35,xpd=T)
abline(h=-hlines,lwd=0.5,xpd=T)

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #moves to next layout region and adds number of isolates 
polygon(x=c(-1e3,1e3,1e3,-1e3),y=c(-1e3,-1e3,1e3,1e3),col='white',xpd=T,lty=0)
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){
  text(x=0.5,adj=0.5,sampsizes[i,j],y=-ys[i]+erapos_2[j],cex=0.675,font=fonts[i],xpd=T)
}
text(y=-0.25,x=-0.15,'Isolates',cex=0.75,adj=0,srt=35,xpd=T)
abline(h=-hlines,lwd=0.5,xpd=T)

# Plot 1b- Penicillin Noninvasive Nonsusceptible ----

#Create new ests_2 vector for *noninvasive* isolates nonsusceptible 
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

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #initalizes a plot 
polygon(x=c(-1e3,1e3,1e3,-1e3),y=c(-1e3,-1e3,1e3,1e3),col='white',xpd=T,lty=0)
for (i in grays){polygon(x=c(0,1,1,0),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){ #adds CI and point estimates to the plot with colors, and sizes
  if (sum(ests_2[i,2:3,j]==c(0,0))!=2){
    lines(y=rep(-ys[i],2)+erapos[j],x=ests_2[i,2:3,j],col=colsoutline[j],lwd=0.5)
    points(y=-ys[i]+erapos[j],x=ests_2[i,1,j],pch=pchs[i],cex=cexs[i],col=colsoutline[j],bg=cols[j],lwd=0.5)  
  }
}
for (i in 1:length(axispos)){
  axis(side=1,pos=-axispos[i],at=seq(0,1,0.25),labels=NA,lwd=0,lwd.ticks=0.5,xpd=T)
  text(x=seq(0,1,0.25),y=-axispos[i]-0.35,adj=1,seq(0,100,25),xpd=T,cex=0.65,srt=45,xpd=T) #adds percentage labels to the plots 
}

mtext(side=1,'Prop. (%) nonsusceptible to penicillin',cex=0.55,font=1,line=1.5,xpd=T) #adds axis label 
abline(h=-hlines,lwd=0.5,xpd=T)
text(y=0,x=0,adj=0,'B. Non-invasive isolates',cex=1,font=2) #Adds "Invasive isolates" to the top of the column rows

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #moves to next layout region to add number of studies 
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){
  text(x=0.5,adj=0.5,studynums_2[i,j],y=-ys[i]+erapos_2[j],cex=0.65,font=fonts[i],xpd=T)
}
text(y=-0.25,x=-0.15,'Studies',cex=0.75,adj=0,srt=35,xpd=T)
abline(h=-hlines,lwd=0.5,xpd=T)

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #moves to next layout region and adds number of isolates 
polygon(x=c(-1e3,1e3,1e3,-1e3),y=c(-1e3,-1e3,1e3,1e3),col='white',xpd=T,lty=0)
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){
  text(x=0.5,adj=0.5,sampsizes_2[i,j],y=-ys[i]+erapos_2[j],cex=0.65,font=fonts[i],xpd=T)
}
text(y=-0.25,x=-0.15,'Isolates',cex=0.75,adj=0,srt=35,xpd=T)
abline(h=-hlines,lwd=0.5,xpd=T)

dev.off()


for (i in 1:dim(ests)[1]){
  print(c(round(as.numeric(ests_4[i,,2])*100,1)))
}


# Plot 2a- Macrolide Invasive Nonsusceptible ---- 

pdf("/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/plots/prepost_plots_3yr_mac.pdf",width=6, height=7.5) #height was 8

mat = matrix(1:7,nrow=1,ncol=7,byrow=T)
layout(mat,widths=c(1.7,2,0.3,0.3,2,0.3,0.3)) #creates a layout with 2 rows (pen vs. mac, 7 columns (inv vs. ninv for nonsusceptible isolates))
par(tck=-0.02)
par(mar=c(2.7,0.25,1,0.25)) ### b, l=0, t, r=0 originally

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

ys = c(1:6,
       (8:12)+0.5,
       (14:18)+1,
       20+1.5,
       22+2,
       (24:27)+2.5,
       (29:31)+3) 
grays = ys[c(1,3,5,7,9,11,12,14,16,17,18,19,21,23,25)]
hlines = c(0.5,6.5,
           8,13,
           14.5,19.5,
           21,22,
           23.5,24.5,
           26,30,
           31.5,34.5)


plot(1,ylim=c(-33,-1),type='n',xlim=c(0,1),axes=F,ann=F)
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs_clean)){
  text(y=-ys[i],x=1,adj=1,cex=0.75,regs_clean[i],font=fonts[i]) #adds regions w/ appropriate spacing 
}
abline(h=-hlines,lwd=0.5,xpd=T)

legend(x = 0.01, y = -34.5, 
       legend = c("pre-PCV Introduction", "3+ years post-PCV Introduction"), 
       col = c('cadetblue3','darkorchid3'),
       pch = c(16,16), 
       xpd = TRUE, 
       bty = "n", 
       pt.cex = 1, 
       cex = 0.75, 
       text.col = "black", 
       horiz = F, 
       inset = c(0.01, 0.01)) 

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #initalizes a plot 
for (i in grays){polygon(x=c(0,1,1,0),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){ #adds CI and point estimates to the plot with colors, and sizes
  if (sum(ests_3[i,2:3,j]==c(0,0))!=2){
    lines(y=rep(-ys[i],2)+erapos[j],x=ests_3[i,2:3,j],col=colsoutline[j],lwd=0.5)
    points(y=-ys[i]+erapos[j],x=ests_3[i,1,j],pch=pchs[i],cex=cexs[i],col=colsoutline[j],bg=cols[j],lwd=0.5)     
  }
}
abline(h=-hlines,lwd=0.5,xpd=T)

axispos = hlines[c(2,4,6,8,10,12,14)]
for (i in 1:length(axispos)){
  axis(side=1,pos=-axispos[i],at=seq(0,1,0.25),labels=NA,lwd=0,lwd.ticks=0.5,xpd=T)
  text(x=seq(0,1,0.25),y=-axispos[i]-0.35,adj=1,seq(0,100,25),xpd=T,cex=0.65,srt=45) #adds percentage labels to the plots 
}
mtext(side=1,'Prop. (%) nonsusceptible to macrolides',cex=0.55,font=1,line=1.5) #adds axis label 

text(y=0,x=0,adj=0,'A. Invasive isolates',cex=1,font=2) #Adds "Invasive isolates" to the top of the column rows


plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #moves to next layout region to add number of studies 
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){
  text(x=0.5,adj=0.5,studynums_3[i,j],y=-ys[i]+erapos_2[j],cex=0.65,font=fonts[i],xpd=T)
}
abline(h=-hlines,lwd=0.5,xpd=T)
text(y=-0.25,x=-0.15,'Studies',cex=0.75,adj=0,srt=35,xpd=T)

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #moves to next layout region and adds number of isolates 
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){
  text(x=0.5,adj=0.5,sampsizes_3[i,j],y=-ys[i]+erapos_2[j],cex=0.65,font=fonts[i],xpd=T)
}
abline(h=-hlines,lwd=0.5,xpd=T)
text(y=-0.25,x=-0.15,'Isolates',cex=0.75,adj=0,srt=35,xpd=T)


# Plot 2b Macrolide Noninvasive Nonsusceptible ---- 

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

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #initalizes a plot 
for (i in grays){polygon(x=c(0,1,1,0),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){ #adds CI and point estimates to the plot with colors, and sizes 
  if (sum(ests_4[i,2:3,j]==c(0,0))!=2){
    lines(y=rep(-ys[i],2)+erapos[j],x=ests_4[i,2:3,j],col=colsoutline[j],lwd=0.5)
    points(y=-ys[i]+erapos[j],x=ests_4[i,1,j],pch=pchs[i],cex=cexs[i],col=colsoutline[j],bg=cols[j],lwd=0.5)
  }
}

axispos = hlines[c(2,4,6,8,10,12,14)]
for (i in 1:length(axispos)){
  axis(side=1,pos=-axispos[i],at=seq(0,1,0.25),labels=NA,lwd=0,lwd.ticks=0.5,xpd=T)
  text(x=seq(0,1,0.25),y=-axispos[i]-0.35,adj=1,seq(0,100,25),xpd=T,cex=0.65,srt=45) #adds percentage labels to the plots 
}
mtext(side=1,'Prop. (%) nonsusceptible to macrolides',cex=0.55,font=1,line=1.5) #adds axis label 
abline(h=-hlines,lwd=0.5,xpd=T)

text(y=0,x=0,adj=0,'B. Non-invasive isolates',cex=1,font=2) #Adds "Invasive isolates" to the top of the column rows


plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #moves to next layout region to add number of studies 
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){
  text(x=0.5,adj=0.5,studynums_4[i,j],y=-ys[i]+erapos_2[j],cex=0.65,font=fonts[i],xpd=T)
}
text(y=-0.25,x=-0.15,'Studies',cex=0.75,adj=0,srt=35,xpd=T)
abline(h=-hlines,lwd=0.5,xpd=T)

plot(1,type='n',xlim=c(0,1),ylim=c(-33,-1),axes=F,ann=F) #moves to next layout region and adds number of isolates 
for (i in grays){polygon(x=c(-100,100,100,-100),y=-i+rep(c(-0.5,0.5),each=2),col='gray90',lty=0,xpd=T)}
for (i in 1:length(regs)) for (j in 1:2){
  text(x=0.5,adj=0.5,sampsizes_4[i,j],y=-ys[i]+erapos_2[j],cex=0.65,font=fonts[i],xpd=T)
}
abline(h=-hlines,lwd=0.5,xpd=T)
text(y=-0.25,x=-0.15,'Isolates',cex=0.75,adj=0,srt=35,xpd=T)

dev.off()

#### Create Table S6 and Table S7 #
#ests = penicillin invasive
#ests_2 = penicillin noninvasive
#ests_3 = macrolide invasive
#ests_4 = macrolide noninvasive 
pen_inv_pre <- data.frame(cbind(regs,ests[,,1])) %>% mutate(EE = paste0(round(as.numeric(as.character(V2)), 3)*100, " (", 
                                                                        round(as.numeric(as.character(V3)), 3)*100, ", ", 
                                                                        round(as.numeric(as.character(V4)), 3)*100, ")" )) %>% 
  dplyr::select(regs, EE) %>% mutate(era = "pre")   #For Pen-Inv-Pre
  
pen_inv_post <- data.frame(cbind(regs,ests[,,2])) %>% mutate(EE = paste0(round(as.numeric(as.character(V2)), 3)*100, " (", 
                                                                         round(as.numeric(as.character(V3)), 3)*100, ", ", 
                                                                         round(as.numeric(as.character(V4)), 3)*100, ")" )) %>% 
  dplyr::select(regs, EE) %>% mutate(era = "post")   #For Pen-Inv-Post

pen_ninv_pre <- data.frame(cbind(regs,ests_2[,,1])) %>% mutate(EE = paste0(round(as.numeric(as.character(V2)), 3)*100, " (", 
                                                                           round(as.numeric(as.character(V3)), 3)*100, ", ", 
                                                                           round(as.numeric(as.character(V4)), 3)*100, ")" )) %>% 
  dplyr::select(regs, EE) %>% mutate(era = "pre")   #For Pen-NInv-Pre

pen_ninv_post <- data.frame(cbind(regs,ests_2[,,2])) %>% mutate(EE = paste0(round(as.numeric(as.character(V2)), 3)*100, " (", 
                                                                            round(as.numeric(as.character(V3)), 3)*100, ", ", 
                                                                            round(as.numeric(as.character(V4)), 3)*100, ")" )) %>% 
  dplyr::select(regs, EE) %>% mutate(era = "post")   #For Pen-NInv-Post

mac_inv_pre <- data.frame(cbind(regs,ests_3[,,1]))%>% mutate(EE = paste0(round(as.numeric(as.character(V2)), 3)*100, " (", 
                                                                         round(as.numeric(as.character(V3)), 3)*100, ", ", 
                                                                         round(as.numeric(as.character(V4)), 3)*100, ")" )) %>% 
  dplyr::select(regs, EE) %>% mutate(era = "pre")   #For Mac-Inv-Pre

mac_inv_post <- data.frame(cbind(regs,ests_3[,,2])) %>% mutate(EE = paste0(round(as.numeric(as.character(V2)), 3)*100, " (", 
                                                                           round(as.numeric(as.character(V3)), 3)*100, ", ", 
                                                                           round(as.numeric(as.character(V4)), 3)*100, ")" )) %>% 
  dplyr::select(regs, EE) %>% mutate(era = "post")   #For Mac-Inv-Post

mac_ninv_pre <- data.frame(cbind(regs,ests_4[,,1])) %>% mutate(EE = paste0(round(as.numeric(as.character(V2)), 3)*100, " (", 
                                                                           round(as.numeric(as.character(V3)), 3)*100, ", ", 
                                                                           round(as.numeric(as.character(V4)), 3)*100, ")" )) %>% 
  dplyr::select(regs, EE) %>% mutate(era = "pre")   #For Mac-NInv-Pre

mac_ninv_post <- data.frame(cbind(regs,ests_4[,,2]))%>% mutate(EE = paste0(round(as.numeric(as.character(V2)), 3)*100, " (", 
                                                                           round(as.numeric(as.character(V3)), 3)*100, ", ", 
                                                                           round(as.numeric(as.character(V4)), 3)*100, ")" )) %>% 
  dplyr::select(regs, EE) %>% mutate(era = "post")   #For Mac-NInv-Post

order_region_s <- c("southern_latin_america", "western_europe", "hi_north_america", "australasia", 
                    "hi_asia_pacific","high_income","caribbean", "central_latin_america", "tropical_latin_america", 
                    "andean_latin_america", "latin_america_caribbean", "southern_subsaharan_africa", "western_subsaharan_africa", 
                    "central_subsaharan_africa", "eastern_subsaharan_africa", "sub_saharan_africa", "north_africa_middle_east", 
                    "south_asia","east_asia","southeast_asia",  "oceania","seasia_easia_oceania", "eastern_europe", "central_europe", "ceurope_eeurope_casia")

pen_inv <- rbind(pen_inv_pre, pen_inv_post)
pen_inv <- pen_inv %>% arrange(match(regs, order_region_s))

pen_ninv <- rbind(pen_ninv_pre, pen_ninv_post)
pen_ninv <- pen_ninv %>% arrange(match(regs, order_region_s))

mac_inv <- rbind(mac_inv_pre, mac_inv_post)
mac_inv <- mac_inv %>% arrange(match(regs, order_region_s))

mac_ninv <- rbind(mac_ninv_pre, mac_ninv_post)
mac_ninv <- mac_ninv %>% arrange(match(regs, order_region_s))

#FORMAT
df1 <- pen_inv
df2 <- pen_ninv
df3 <- mac_inv
df4 <- mac_ninv
row.names(df1) = NULL

#Number of empty rows to insert
N = 1

#Every N rows after which empty rows should be inserted
after_rows = 2

pen_inv <- do.call(rbind, lapply(split(df1, ceiling(1:NROW(df1)/after_rows)),
                      function(a) rbind(a, replace(a[1:N,], TRUE, ""))))
pen_ninv <- do.call(rbind, lapply(split(df2, ceiling(1:NROW(df2)/after_rows)),
                                 function(a) rbind(a, replace(a[1:N,], TRUE, ""))))
mac_inv <- do.call(rbind, lapply(split(df3, ceiling(1:NROW(df3)/after_rows)),
                                 function(a) rbind(a, replace(a[1:N,], TRUE, ""))))
mac_ninv <- do.call(rbind, lapply(split(df4, ceiling(1:NROW(df4)/after_rows)),
                                 function(a) rbind(a, replace(a[1:N,], TRUE, ""))))

write.csv(pen_inv, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/tables/forestPlots/pen_inv.csv")
write.csv(pen_ninv, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/tables/forestPlots/pen_ninv.csv")
write.csv(mac_inv, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/tables/forestPlots/mac_inv.csv")
write.csv(mac_ninv, "/Users/kristinandrejko/Box/01-Research/02-SP-AMR/LancetMicrobe-Revision/PneumoAMR-LMicrobe/output/tables/forestPlots/mac_ninv.csv")

cbind(regs,ests[,,2]) #For Pen-Inv-Post  