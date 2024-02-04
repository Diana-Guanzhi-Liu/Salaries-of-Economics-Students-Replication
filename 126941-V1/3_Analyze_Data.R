
load(paste0(secure,"UCSC_Econ.Rda"))

#Make plots

###
#  Balance Tests (Figure A-4)
###
#Figure A-4(c-d): Histograms
png("Figures/DataHist.png",width=500,height=500)
  hist(c$GPA[c$Year_Applied%in%2008:2012&c$GPA>1.8],breaks = seq(1.8,4,.02),xlab="Average GPA in Economics 1 & 2",main=NA)
  abline(v=2.75,col="blue",lty=2)
  dev.off()
  png("Figures/DataHist_0407.png",width=500,height=500)
  hist(c$GPA[c$Year_Applied%in%2003:2007&c$GPA>1.8],breaks = seq(1.8,4,.02),xlab="Average GPA in Economics 1 & 2",main=NA)
  abline(v=2.75,col="blue",lty=2)
dev.off()

#Figure A-4(a): Empirical CDF
c$Count<-1
hi0<-aggregate(Count~GPA,c[c$Year_Applied%in%2000:2002&c$GPA>1.8,],sum) ; hi0$Count<-hi0$Count/sum(hi0$Count) ; for(i in 2:nrow(hi0)) hi0$Count[i]<-hi0$Count[i]+hi0$Count[i-1] ; hi0<-rbind(data.frame(GPA=c(hi0$GPA[-1],NA),Count=hi0$Count),hi0,data.frame(GPA=c(1.75,1.8,4.05),Count=c(0,0,1))) ; hi0<-hi0[order(hi0$GPA),]
hi<-aggregate(Count~GPA,c[c$Year_Applied%in%2003:2007&c$GPA>1.8,],sum) ; hi$Count<-hi$Count/sum(hi$Count) ; for(i in 2:nrow(hi)) hi$Count[i]<-hi$Count[i]+hi$Count[i-1] ; hi<-rbind(data.frame(GPA=c(hi$GPA[-1],NA),Count=hi$Count),hi,data.frame(GPA=c(1.75,1.8,4.05),Count=c(0,0,1))) ; hi<-hi[order(hi$GPA),]
hi1<-aggregate(Count~GPA,c[c$Year_Applied%in%2008:2012&c$GPA>1.8,],sum) ; hi1$Count<-hi1$Count/sum(hi1$Count) ; for(i in 2:nrow(hi1)) hi1$Count[i]<-hi1$Count[i]+hi1$Count[i-1] ; hi1<-rbind(data.frame(GPA=c(hi1$GPA[-1],NA),Count=hi1$Count),hi1,data.frame(GPA=c(1.75,1.8,4.05),Count=c(0,0,1))) ; hi1<-hi1[order(hi1$GPA),]
png("Figures/DataCDF.png",width=500,height=500)
  plot(hi$GPA,hi$Count,type="l",col="gray40",xlab="Average GPA in Economics 1 & 2",ylab="Probability",cex.axis=2,cex.lab=1.5,xlim=c(1.8,4))
  lines(hi1$GPA,hi1$Count,lwd=2)
  lines(hi0$GPA,hi0$Count,lty=3,col="gray40")
  abline(v=2.75,col="blue",lty=2)
  legend(3,.25,legend=c("2000-2002","2003-2007","2008-2012"),lty=c(3,1,1),lwd=c(1,1,2),col=c("gray40","gray40","black"),cex=1.6)
dev.off()


###
#  Table 1: Descriptives
###
D<-data.frame(matrix("",12,5),stringsAsFactors = F)
count<-1 ; for(v in c("Female","White","Asian","Hispanic","Black","Residency","SAT","mean_agi")){
  n<-1 ; if(v%in%c("SAT","mean_agi")) n<-0
  D[count,1]<-ex(mean(S[S$Year_Applied%in%2008:2012&NA_to_F(S$Adm_Cat=="Freshman"),v],na.rm=T),n,comma=(!v%in%c("SAT","BYear")))
  D[count,2]<-ex(mean(c[c$Year_Applied%in%2008:2012,v],na.rm=T),n,comma=(!v%in%c("SAT","BYear")))
  D[count,3]<-ex(mean(c[c$Year_Applied%in%2008:2012&c$Econ==1,v],na.rm=T),n,comma=(!v%in%c("SAT","BYear")))
  c$DepVar<-c[,v]*c$Econ
  regiv<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients
  D[count,4]<-ex(regiv[4,1],n,comma=(!v%in%c("SAT","BYear")))
  D[count,5]<-ex(regiv[4,2],n,comma=(!v%in%c("SAT","BYear")),par=T)
  count<-count+1
}
D[count,1]<-ex(sum(S$Year_Applied%in%2008:2012&NA_to_F(S$Adm_Cat=="Freshman")),0,comma=T)
D[count,2]<-ex(sum(c$Year_Applied%in%2008:2012),0,comma=T)
D[count,3]<-ex(sum(c$Year_Applied%in%2008:2012&c$Econ==1),0,comma=T)
write.csv(D,file="Figures/SummaryStats.csv")


###
#  Annual thresholds; Figure A-1
###
for(y in 2000:2014){
  reg<-summary(felm(Economics~Above+GPA_Abo1+GPA_Bel1|0|0|GPA,data=c[c$Year_Applied%in%y&c$GPA>1.8,]))$coefficients
  hi<-aggregate(Economics~GPA,c[c$Year_Applied%in%y&c$GPA>1.7,],mean)
  hi1<-aggregate(Count~GPA,c[c$Year_Applied%in%y&c$GPA>1.7,],sum)
  hi1$Count<-hi1$Count/sum(hi1$Count)
  png(paste0("Figures/EconChoice",y,".png"),width=500,height=500)
    plot(x=1000,y=1000,xlim=c(1.7,4.2),ylim=c(0,100),xlab="Average GPA in Economics 1 & 2",ylab="Percent in Major",cex.axis=2,cex.lab=1.5)
    for(i in 1:nrow(hi)) draw.circle(x=hi$GPA[i],y=hi$Economics[i],radius=hi1$Count[i])
    abline(v=2.75,col="blue",lty=2)
    lines(x=c(1.7,2.75),y=c(reg[1,1]+(1.7-2.75)*reg[4,1],reg[1,1]))
    lines(x=c(2.75,4),y=c(reg[1,1]+reg[2,1],reg[1,1]+reg[2,1]+(4-2.75)*reg[3,1]))
    text(3.54,.05,labels=paste0("β = ",ex(reg[2,1],2,comma=T,justnum = T)," (",ex(reg[2,2],2,comma=T,justnum = T),")"),pos=3,cex=2)
  dev.off()
}




###
#  Now produce the main set of figures for the paper (including Figures 1, 2, 4, and 5; Appendix Tables A-1, A-2, A-3, and A-4; and Appendix Figures A-3, A-4(b), A-5. A-6, A-7, A-9, A-10, A-11, A-12, A-14, A-15, A-16, A-17, and A-18(b))
###

vars<-list(list("Economics",c(0,100),"Percent in Major",dec=1,top=F,IV=F),
           list("wage_sum_2018",c(40000,80000),"Dollars",dec=0,top=F,IV=T),
           list("wage_sum_2017",c(40000,80000),"Dollars",dec=0,top=F,IV=T),
           list("wage_sum_1718",c(40000,70000),"Dollars",dec=0,top=F,IV=T),
           list("wage_sum_1718_Early",c(40000,70000),"Dollars",dec=0,top=F,IV=T),
           list("wage_sum_1718_Late",c(30000,70000),"Dollars",dec=0,top=F,IV=T),
           list("wage_sum_1718_URM",c(30000,70000),"Dollars",dec=0,top=F,IV=T),
           list("wage_sum_1718_NonURM",c(30000,70000),"Dollars",dec=0,top=F,IV=T),
           list("wage_sum_1213",c(40000,70000),"Dollars",dec=0,top=F,IV=T),
           list("wage_sum_0809",c(40000,70000),"Dollars",dec=0,top=F,IV=T),
           list("logwage_sum_1718",c(10.2,10.8),"Log Dollars",dec=2,top=F,IV=T),
           list("Employ_1718",c(60,100),"% Employed",dec=1,top=F,IV=T),
           list("Grad_IncNSC",c(75,100),"Percent Grad.",dec=1,top=F,IV=T),
           list("NSC_NumWholeYearsEnrolled7",c(3.8,4.6),"Years",dec=2,top=F,IV=T),
           list("GradSch",c(0,25),"Percent",dec=1,top=F,IV=T),
           list("Humanities",c(0,50),"Percent in Major",dec=1,top=T,IV=T),
           list("SocSci",c(0,80),"Percent in Major",dec=1,top=T,IV=T),
           list("NatSci",c(0,50),"Percent in Major",dec=1,top=T,IV=T),
           list("Engineering",c(0,50),"Percent in Major",dec=1,top=T,IV=T),
           list("ShareBusEcon",c(0,100),"Percent",dec=1,top=F,IV=F),
           list("Major_Median0910",c(25000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Major_Median1718",c(25000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Major_MedianEconOnly0910",c(25000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Major_MedianEconOnly1718",c(25000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Major_ACSMedian2010",c(25000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Major_ACSMedian2018",c(25000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Major_ACSMedian_CA2010",c(25000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Major_ACSMedian_CA2018",c(25000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Major_ACSMedian_NotCA2018",c(25000,65000),"Dollars",dec=0,top=T,IV=T),
           list("Predicted_Wage_Employed",c(40000,70000),"Dollars",dec=1,top=T,IV=T),
           list("Predicted_Wage_UCUES",c(30000,50000),"Dollars",dec=1,top=T,IV=T),
           list("Predicted_Wage_UCUES_SJ",c(30000,50000),"Dollars",dec=1,top=T,IV=T),
           list("Female",c(30,70),"Percent",dec=1,top=F,IV=F),
           list("URM",c(0,60),"Percent",dec=1,top=F,IV=F),
           list("Residency",c(80,100),"Percent",dec=1,top=F,IV=F),
           list("SAT",c(1500,1900),"SAT Score",dec=0,top=F,IV=F),
           list("mean_agi",c(70000,110000),"Dollars",dec=0,top=F,IV=F),
           list("GPA_Econ2",c(1.8,4.2),"GPA",dec=2,top=F,IV=F),
           list("GPA_Overall",c(2.6,3.7),"GPA",dec=2,top=F,IV=T),
           list("GPA_Overall_Rel",c(-.5,.8),"Normed GPA",dec=2,top=F,IV=T),
           list("GPA_Overall_FE",c(2.8,4),"Normed GPA",dec=2,top=F,IV=T),
           list("C_Humanities",c(0,14),"# Classes",dec=1,top=F,IV=T),
           list("C_Economics",c(0,14),"# Classes",dec=1,top=F,IV=T),
           list("C_Mathematics",c(0,14),"# Classes",dec=1,top=F,IV=T),
           list("C_SocialSciences",c(0,14),"# Classes",dec=1,top=F,IV=T),
           list("C_NaturalSciences",c(0,14),"# Classes",dec=1,top=F,IV=T),
           list("C_EconType_Econ",c(0,10),"# Classes",dec=1,top=F,IV=T),
           list("C_EconType_Bus",c(0,10),"# Classes",dec=1,top=F,IV=T),
           list("C_QuantMethod",c(0,5),"# Classes",dec=1,top=F,IV=T),
           list("Class_Size_Median",c(130,155),"Number of Students",dec=1,top=F,IV=T),
           list("Accountant",c(0,20),"Percent",dec=1,top=F,IV=T),
           list("EdHealth",c(0,30),"Percent",dec=1,top=F,IV=T),
           list("FIRE",c(0,20),"Percent",dec=1,top=F,IV=T),
           list("FIREAcc",c(0,25),"Percent",dec=1,top=F,IV=T),
           list("Industry_Mean1718",c(30000,65000),"Dollars",dec=0,top=F,IV=T),
           list("Intend_Bus_SJ",c(0,100),"Percent",dec=1,top=F,IV=T),
           list("Intend_Bus_SJ_Outliers",c(0,100),"Percent",dec=1,top=F,IV=T),
           list("Intend_Bus_SJ_No2012",c(0,100),"Percent",dec=1,top=F,IV=T),
           list("Intend_Bus_Fr",c(0,60),"Percent",dec=1,top=F,IV=T),
           list("UCUES",c(0,40),"Percent",dec=1,top=F,IV=T),
           list("UCUES_Fr",c(0,40),"Percent",dec=1,top=F,IV=T),
           list("UCUES_SJ",c(0,40),"Percent",dec=1,top=F,IV=T),
           list("Hours_Study_SJ",c(6,18),"Hours",dec=1,top=F,IV=T))
Ests1<-data.frame(matrix("",15,1),stringsAsFactors = F)
for(v in vars){
  if(!v[[1]]%in%names(c)) next
  c$DepVar<-c[,v[[1]]]
  if(length(table(c$DepVar))>2) c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02)
  reg<-summary(felm(DepVar~Above+GPA_Abo1+GPA_Bel1|0|0|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients
  if(v[[6]]) regiv<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients
  hi<-aggregate(DepVar~GPA,c[c$Year_Applied%in%2008:2012&c$GPA>1.7,],mean,na.rm=T)
  c$Count<-!is.na(c$DepVar)
  hi1<-aggregate(Count~GPA,c[c$Year_Applied%in%2008:2012&c$GPA>1.7,],sum,na.rm=T)
  hi1$Count<-hi1$Count/sum(hi1$Count)
  png(paste0("Figures/RD_",v[[1]],".png"),width=500,height=500)
  plot(x=1000,y=1000,xlim=c(1.7,4.2),ylim=v[[2]],xlab="Average GPA in Economics 1 & 2",ylab=v[[3]],cex.axis=2,cex.lab=1.5)
  for(i in 1:nrow(hi)) draw.circle(x=hi$GPA[i],y=hi$DepVar[i],radius=hi1$Count[i])
  abline(v=2.75,col="blue",lty=2)
  lines(x=c(1.7,2.75),y=c(reg[1,1]+(1.7-2.75)*reg[4,1],reg[1,1]))
  lines(x=c(2.75,4),y=c(reg[1,1]+reg[2,1],reg[1,1]+reg[2,1]+(4-2.75)*reg[3,1]))
  if(!v[[6]]){
    text(3.54,v[[2]][1]-(v[[2]][2]-v[[2]][1])*(.035-.9*(v[[5]])),labels=paste0("β = ",ex(reg[2,1],v[[4]],comma=T,justnum = T)," (",ex(reg[2,2],v[[4]],comma=T,justnum = T),")"),pos=3,cex=2)
  } else text(3.54,v[[2]][1]-(v[[2]][2]-v[[2]][1])*(.035-.9*(v[[5]])),labels=paste0("β = ",ex(reg[2,1],v[[4]],comma=T,justnum = T)," (",ex(reg[2,2],v[[4]],comma=T,justnum = T),")\nIV = ",ex(regiv[4,1],v[[4]],comma=T,justnum = T)," (",ex(regiv[4,2],v[[4]],comma=T,justnum = T),")"),pos=3,cex=2)
  dev.off()
  
  if(v[[1]]%in%c("Economics","wage_sum_1213")){ #Replicate for 2004-2007, to show low estimates
    c$DepVar1<-c[,v[[1]]]
    if(length(table(c$DepVar1))>2) c$DepVar1[c$Year_Applied%in%2003:2007&c$GPA>1.8]<-winsor(c$DepVar1[c$Year_Applied%in%2003:2007&c$GPA>1.8],.02)
    ylim<-c(0,100) ; if(v[[1]]=="wage_sum_1718") ylim<-c(60000,110000) ; if(v[[1]]=="logwage_sum_1718") ylim<-c(10.7,11.3) ; if(v[[1]]=="wage_sum_1213") ylim<-c(30000,60000)
    reg<-summary(felm(DepVar1~Above+GPA_Abo1+GPA_Bel1|0|0|GPA,data=c[c$Year_Applied%in%2003:2007&c$GPA>1.8,]))$coefficients
    regiv<-summary(felm(DepVar1~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2003:2007&c$GPA>1.8,]))$coefficients
    hi<-aggregate(DepVar1~GPA,c[c$Year_Applied%in%2003:2007&c$GPA>1.7,],mean,na.rm=T)
    c$Count<-!is.na(c$DepVar1)
    hi1<-aggregate(Count~GPA,c[c$Year_Applied%in%2003:2007&c$GPA>1.7,],sum,na.rm=T)
    hi1$Count<-hi1$Count/sum(hi1$Count)
    png(paste0("Figures/RD_",v[[1]],"_0307.png"),width=500,height=500)
    plot(x=1000,y=1000,xlim=c(1.7,4.2),ylim=ylim,xlab="Average GPA in Economics 1 & 2",ylab=v[[3]],cex.axis=2,cex.lab=1.5)
    for(i in 1:nrow(hi)) draw.circle(x=hi$GPA[i],y=hi$DepVar1[i],radius=hi1$Count[i])
    abline(v=2.75,col="blue",lty=2)
    lines(x=c(1.7,2.75),y=c(reg[1,1]+(1.7-2.75)*reg[4,1],reg[1,1]))
    lines(x=c(2.75,4),y=c(reg[1,1]+reg[2,1],reg[1,1]+reg[2,1]+(4-2.75)*reg[3,1]))
    if(!v[[6]]){
      text(3.54,ylim[1]-(ylim[2]-ylim[1])*(.035-.9*(v[[5]])),labels=paste0("β = ",ex(reg[2,1],v[[4]],comma=T,justnum = T)," (",ex(reg[2,2],v[[4]],comma=T,justnum = T),")"),pos=3,cex=2)
    } else text(3.54,ylim[1]-(ylim[2]-ylim[1])*(.035-.9*(v[[5]])),labels=paste0("β = ",ex(reg[2,1],v[[4]],comma=T,justnum = T)," (",ex(reg[2,2],v[[4]],comma=T,justnum = T),")\nIV = ",ex(regiv[4,1],v[[4]],comma=T,justnum = T)," (",ex(regiv[4,2],v[[4]],comma=T,justnum = T),")"),pos=3,cex=2)
    dev.off()
  }
  if(v[[1]]%in%c("Economics","wage_sum_0809")){ #Replicate for 2000-2002, to show 0's
    c$DepVar1<-c[,v[[1]]]
    if(length(table(c$DepVar1))>2) c$DepVar1[c$Year_Applied%in%2000:2002&c$GPA>1.8]<-winsor(c$DepVar1[c$Year_Applied%in%2000:2002&c$GPA>1.8],.02)
    ylim<-c(0,100) ; if(v[[1]]=="wage_sum_1718") ylim<-c(60000,120000) ; if(v[[1]]=="logwage_sum_1718") ylim<-c(10.7,11.3) ; if(v[[1]]=="wage_sum_0809") ylim<-c(30000,60000)
    reg<-summary(felm(DepVar1~Above+GPA_Abo1+GPA_Bel1|0|0|GPA,data=c[c$Year_Applied%in%2000:2002&c$GPA>1.8,]))$coefficients
    regiv<-summary(felm(DepVar1~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2000:2002&c$GPA>1.8,]))$coefficients
    hi<-aggregate(DepVar1~GPA,c[c$Year_Applied%in%2000:2002&c$GPA>1.7,],mean,na.rm=T)
    c$Count<-!is.na(c$DepVar1)
    hi1<-aggregate(Count~GPA,c[c$Year_Applied%in%2000:2002&c$GPA>1.7,],sum,na.rm=T)
    hi1$Count<-hi1$Count/sum(hi1$Count)
    png(paste0("Figures/RD_",v[[1]],"_0002.png"),width=500,height=500)
    plot(x=1000,y=1000,xlim=c(1.7,4.2),ylim=ylim,xlab="Average GPA in Economics 1 & 2",ylab=v[[3]],cex.axis=2,cex.lab=1.5)
    for(i in 1:nrow(hi)) draw.circle(x=hi$GPA[i],y=hi$DepVar1[i],radius=hi1$Count[i])
    abline(v=2.75,col="blue",lty=2)
    lines(x=c(1.7,2.75),y=c(reg[1,1]+(1.7-2.75)*reg[4,1],reg[1,1]))
    lines(x=c(2.75,4),y=c(reg[1,1]+reg[2,1],reg[1,1]+reg[2,1]+(4-2.75)*reg[3,1]))
    if(!v[[6]]){
      text(3.54,ylim[1]-(ylim[2]-ylim[1])*(.035-.9*(v[[5]])),labels=paste0("β = ",ex(reg[2,1],v[[4]],comma=T,justnum = T)," (",ex(reg[2,2],v[[4]],comma=T,justnum = T),")"),pos=3,cex=2)
    } else text(3.54,ylim[1]-(ylim[2]-ylim[1])*(.035-.9*(v[[5]])),labels=paste0("β = ",ex(reg[2,1],v[[4]],comma=T,justnum = T)," (",ex(reg[2,2],v[[4]],comma=T,justnum = T),")\nIV = ",ex(regiv[4,1],v[[4]],comma=T,justnum = T)," (",ex(regiv[4,2],v[[4]],comma=T,justnum = T),")"),pos=3,cex=2)
    dev.off()
  }
  
  if(v[[1]]%in%c("wage_sum_1718")) for(f in c("F","M")){
    reg<-summary(felm(DepVar~Above+GPA_Abo1+GPA_Bel1|0|0|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8&c$Gender==f,]))$coefficients
    if(v[[6]]) regiv<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8&c$Gender==f,]))$coefficients
    hi<-aggregate(DepVar~GPA,c[c$Year_Applied%in%2008:2012&c$GPA>1.7&c$Gender==f,],mean,na.rm=T)
    c$Count<-!is.na(c$DepVar)
    hi1<-aggregate(Count~GPA,c[c$Year_Applied%in%2008:2012&c$GPA>1.7&c$Gender==f,],sum,na.rm=T)
    hi1$Count<-hi1$Count/sum(hi1$Count)
    png(paste0("Figures/RD_",v[[1]],"_",f,".png"),width=500,height=500)
    plot(x=1000,y=1000,xlim=c(1.7,4.2),ylim=v[[2]],xlab="Average GPA in Economics 1 & 2",ylab=v[[3]],cex.axis=2,cex.lab=1.5)
    for(i in 1:nrow(hi)) draw.circle(x=hi$GPA[i],y=hi$DepVar[i],radius=hi1$Count[i])
    abline(v=2.75,col="blue",lty=2)
    lines(x=c(1.7,2.75),y=c(reg[1,1]+(1.7-2.75)*reg[4,1],reg[1,1]))
    lines(x=c(2.75,4),y=c(reg[1,1]+reg[2,1],reg[1,1]+reg[2,1]+(4-2.75)*reg[3,1]))
    if(!v[[6]]){
      text(3.54,v[[2]][1]-(v[[2]][2]-v[[2]][1])*(.035-.9*(v[[5]])),labels=paste0("β = ",ex(reg[2,1],v[[4]],comma=T,justnum = T)," (",ex(reg[2,2],v[[4]],comma=T,justnum = T),")"),pos=3,cex=2)
    } else text(3.54,v[[2]][1]-(v[[2]][2]-v[[2]][1])*(.035-.9*(v[[5]])),labels=paste0("β = ",ex(reg[2,1],v[[4]],comma=T,justnum = T)," (",ex(reg[2,2],v[[4]],comma=T,justnum = T),")\nIV = ",ex(regiv[4,1],v[[4]],comma=T,justnum = T)," (",ex(regiv[4,2],v[[4]],comma=T,justnum = T),")"),pos=3,cex=2)
    dev.off()
  }
  
  e1<-data.frame(matrix("",15,1),stringsAsFactors = F)
  count1<-2 ; for(j in c("GPA_Abo1+GPA_Bel1|0","GPA_Abo1+GPA_Bel1+GPA_Abo2+GPA_Bel2|0",
                                    "GPA_Abo1+GPA_Bel1+Female|HS_ATP+Year_Applied+Eth",
                                    "GPA_Abo1+GPA_Bel1|0")){
    if(v[[1]]=="Female") j<-gsub("[+]Female","",j)
    if(v[[1]]%in%c("White","Asian","Hispanic","Black","URM")) j<-gsub("[+]Eth","",j)
    if(v[[1]]%in%c("International")|grepl("Hours_[SC]|Intend_|RUC",v[[1]])) j<-gsub("HS_ATP[+]","",j)
    condit<-c$Year_Applied%in%2008:2012&c$GPA>1.8
    if(count==5) condit<-c$Year_Applied%in%2008:2012&inrange(c$GPA,2.3,3.2)
    
    reg<-summary(felm(as.formula(paste0("DepVar~Above+",j,"|0|GPA")),data=c[condit,],exactDOF=T)) ; reg_c<-reg$coefficients
    
    e1[1,1]<-v[[1]]
    e1[count1,1]<-ex(reg_c[row.names(reg_c)=="Above",1],v[[4]],comma=T)
    e1[count1+1,1]<-ex(reg_c[row.names(reg_c)=="Above",2],v[[4]],comma=T,par=T)
    count1<-count1+3
  }
  regHON<-RDHonest(DepVar~GPA,data=c[c$Year_Applied%in%2008:2012,],cutoff=2.8,opt.criterion="MSE",M=NPR_MROT.fit(RDData(c[c$Year_Applied%in%2008:2012,c("DepVar","GPA")], cutoff=2.8)))
  e1[count1,1]<-ex(regHON$estimate,v[[4]],comma=T)
  e1[count1+1,1]<-ex(regHON$sd,v[[4]],comma=T,par=T)
  
  Ests1<-cbind(Ests1,e1)
}
write.csv(Ests1,file="Figures/RD_Robustness.csv")



###
#  Construct Figure 6 and Table A-6 (OLS vs. IV)
###
c$Major_Larger_NoSpace<-gsub("[^A-z]","",c$Major_Larger)
ms<-unique(c$Major1) ; ms<-ms[!grepl("Environmental Sciences",ms)]
R<-data.frame(matrix("",length(ms),5),stringsAsFactors = F)
r<-1 ; for(m in ms){
  R[r,1]<-ms[r]
  c$DepVar<-(c$Major1%in%m|c$Major2%in%m|c$Major3%in%m|c$Major4%in%m)*100
  regiv<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients
  R[r,c(2)]<-ex(regiv[4,c(1)],1)
  R[r,c(3)]<-ex(regiv[4,c(2)],1,par=T)
  R[r,c(4,5)]<-regiv[4,c(1,4)]
  r<-r+1
}
R[,4]<-as.numeric(R[,4])
R[,5]<-as.numeric(gsub("[^0-9.-]","",R$X2))
R<-R[order(R$X5),]
odd<-seq(1,nrow(R)*3,3) ; even<-seq(2,nrow(R)*3,3)
r<-data.frame(rep("",nrow(R)*3),rep("",nrow(R)*3),rep(NA,nrow(R)*3),rep("",nrow(R)*3),rep("",nrow(R)*3),stringsAsFactors = F) ; r[odd,1]<-R[,1]
r[odd,2]<-R[,2] ; r[odd,3]<-R[,4]
r[even,2]<-R[,3] ; names(r)<-c("Major","Marginal Share","Shares") ; R<-r
R$Shares[odd][1:(length(odd)-3)]<-R$Shares[odd][1:(length(odd)-3)]/abs(sum(R$Shares[odd][1:(length(odd)-3)])/100) #Adjust shares for changes in double-majoring
for(m in unique(S$Major_Larger)[unique(S$Major_Larger)!="Business Mgmt Economics"]) S[,paste0("MAJ_",gsub("[^A-z]","",m))]<-(S$Major_Larger%in%m)

R$UCSC_Share<-"" ; for(m in R$Major[odd]) R$UCSC_Share[R$Major==m]<-ex(mean((S$Major1%in%m|S$Major2%in%m|S$Major3%in%m|S$Major4%in%m)[S$Year_Applied==2008:2012],na.rm=T)*100,1)
R$UCSC_Share_cont<-NA ; for(m in R$Major[odd]) R$UCSC_Share_cont[R$Major==m]<-mean((S$Major_Larger%in%m)[S$Year_Applied==2008:2012],na.rm=T) ; R$UCSC_Share_cont[odd[1:(length(odd)-3)]]<- -R$UCSC_Share_cont[odd[1:(length(odd)-3)]]/sum(R$UCSC_Share_cont[odd[1:(length(odd)-3)]],na.rm=T) ; R$UCSC_Share_cont[odd[(length(odd)-2):length(odd)]]<-R$UCSC_Share_cont[odd[(length(odd)-2):length(odd)]]/sum(R$UCSC_Share_cont[odd[(length(odd)-2):length(odd)]],na.rm=T)
R$ACS_Share<-"" ; for(m in temp$ACS_Field) R$ACS_Share[odd][temp$ACS_Field==m]<-ex(mean((acs$DEGFIELDD%in%m|acs$DEGFIELD2D%in%m)[acs$YEAR%in%2017:2018],na.rm=T)*100,1)


i<-"OLS_UCSC" ; R[,i]<-"" ; hi<-data.frame(summary(felm(as.formula(paste0("wage_sum_1718~",paste0(names(S)[grepl("MAJ_",names(S))],collapse="+"),"")),S[S$Year_Applied%in%2008:2012&!is.na(S$Major1),]))$coefficients)[,1:2] ; hi<-hi[grepl("MAJ",row.names(hi)),] ; row.names(hi)<-gsub("MAJ_|TRUE","",row.names(hi)) ; h<-data.frame(Estimate=0,Std..Error=0) ; row.names(h)<-"BusinessMgmtEconomics" ; hi<-rbind(hi,h) ; hi<-hi[gsub("[^A-z]","",R$Major[odd]),] ; hi$Major_Larger_NoSpace<-row.names(hi) ; c<-merge(c[,!names(c)=="Estimate"],hi[,c("Major_Larger_NoSpace","Estimate")],all.x=T) ; R[odd,i]<-ex(hi[,1],0,comma=T) ; R[even,i]<-ex(hi[,2],0,par=T,comma=T)
R[150,i]<-ex(sum(R$UCSC_Share_cont[odd]*hi[,1],na.rm=T),0,comma=T)
R[151,i]<-ex(sum(R$Shares[odd]*hi[,1]/100,na.rm=T),0,comma=T)
#Replace Estimate with Leave-One-Out
S$Count<-1 ; t<-aggregate(.~Major_Larger,S[S$Year_Applied%in%2008:2012&!is.na(S$Major1),c("Major_Larger","Count","wage_sum_1718")],sum,na.rm=T) ; names(t)[2:3]<-c("Count_wage","Estimate") ; c<-merge(c[,!names(c)%in%c("Count_wage","Estimate")],t,all.x=T) ; c$Estimate<-(c$Estimate-c$wage_sum_1718)/(c$Count_wage-1)
regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R[152,i]<-ex(regiv[4,1],0,comma=T) ; R[153,i]<-ex(regiv[4,2],0,par=T,comma=T)
#Now include double-majors
S$Count<-1 ; t<-aggregate(.~AllMajors,S[S$Year_Applied%in%2008:2012&!is.na(S$Major1),c("AllMajors","Count","wage_sum_1718")],sum,na.rm=T) ; names(t)[2:3]<-c("Count_wage","Estimate") ; t<-t[t$Count_wage>=5,] ; c<-merge(c[,!names(c)%in%c("Count_wage","Estimate")],t,all.x=T) ; c$Estimate[!is.na(c$wage_sum_1718)]<-((c$Estimate-c$wage_sum_1718)/(c$Count_wage-1))[!is.na(c$wage_sum_1718)] ; c$Estimate[is.na(c$wage_sum_1718)]<-((c$Estimate)/(c$Count_wage))[is.na(c$wage_sum_1718)]  ; c$Major_OLS<-c$Estimate
regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R[154,i]<-ex(regiv[4,1],0,comma=T) ; R[155,i]<-ex(regiv[4,2],0,par=T,comma=T)

#Now the same, but with covariates
i<-"OLS_UCSC_Cov" ; R[,i]<-"" ; hi<-data.frame(summary(felm(as.formula(paste0("wage_sum_1718~",paste0(names(S)[grepl("MAJ_",names(S))],collapse="+"),"+Female*factor(Eth)+SAT+logmean_agi|Year_Applied+HS_ATP")),S[S$Year_Applied%in%2008:2012&!is.na(S$Major1),]))$coefficients)[,1:2] ; hi<-hi[grepl("MAJ",row.names(hi)),] ; row.names(hi)<-gsub("MAJ_|TRUE","",row.names(hi)) ; h<-data.frame(Estimate=0,Std..Error=0) ; row.names(h)<-"BusinessMgmtEconomics" ; hi<-rbind(hi,h) ; hi<-hi[gsub("[^A-z]","",R$Major[odd]),] ; hi$Major_Larger_NoSpace<-row.names(hi) ; c<-merge(c[,!names(c)=="Estimate"],hi[,c("Major_Larger_NoSpace","Estimate")],all.x=T) ; R[odd,i]<-ex(hi[,1],0,comma=T) ; R[even,i]<-ex(hi[,2],0,par=T,comma=T)
R[150,i]<-ex(sum(R$UCSC_Share_cont[odd]*hi[,1],na.rm=T),0,comma=T)
R[151,i]<-ex(sum(R$Shares[odd]*hi[,1]/100,na.rm=T),0,comma=T)
#Not doing leave-one-out
for(m in unique(S$AllMajors)[unique(S$AllMajors)!="Business Mgmt Economics"]) S[,paste0("MAD_",gsub("[^A-z]","",m))]<-(S$Major_Larger%in%m)
hi<-data.frame(summary(felm(as.formula(paste0("wage_sum_1718~",paste0(names(S)[grepl("MAD_",names(S))],collapse="+"),"+Female*factor(Eth)+SAT+logmean_agi|Year_Applied+HS_ATP")),S[S$Year_Applied%in%2008:2012&!is.na(S$Major1),]))$coefficients)[,1:2] ; hi<-hi[grepl("MAD",row.names(hi)),] ; row.names(hi)<-gsub("MAD_|TRUE","",row.names(hi)) ; h<-data.frame(Estimate=0,Std..Error=0) ; row.names(h)<-"BusinessMgmtEconomics" ; hi<-rbind(hi,h) ; hi<-hi[gsub("[^A-z]","",R$Major[odd]),] ; hi$AllMajors<-row.names(hi) ; hi$temp<-hi$AllMajors ; c$temp<-gsub("[^A-z]","",c$AllMajors) ; c<-merge(c[,!names(c)=="Estimate"],hi[,c("temp","Estimate")],all.x=T) ; regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; c$Major_OLS_Covariates<-c$Estimate ; R[154,i]<-ex(regiv[4,1],0,comma=T) ; R[155,i]<-ex(regiv[4,2],0,par=T,comma=T)

temp<-read.csv("Data/Raw/UCSC_Majors_to_ACS.csv")[,c(1,3,4)] ; row.names(temp)<-temp$Major ; temp<-temp[r$Major[odd],] #Add ACS matches
load("Data/Derived/ACS_Data_UCSCEcon.Rda")
ams<-unique(acs$DEGFIELDD)
#Choose the higher-earning major
t<-c() ; for(m in unique(acs$DEGFIELDD[acs$YEAR==2018])) t<-c(t,weighted.median(acs$INCWAGE[acs$YEAR%in%2017:2018&acs$DEGFIELDD==m&acs$DEGFIELD2D==0],acs$PERWT[acs$YEAR%in%2017:2018&acs$DEGFIELDD==m&acs$DEGFIELD2D==0],na.rm=T)) ; t<-unique(acs$DEGFIELDD[acs$YEAR==2018])[order(t,decreasing = T)] #Order the majors by higher-earning, among single majors
acs$Major_Larger<-acs$DEGFIELDD ; for(i in 1:(length(t)-1)){ ; check<-acs$DEGFIELD2D==t[i]&acs$DEGFIELDD%in%t[(i+1):length(t)] ; acs$Major_Larger[check]<-acs$DEGFIELD2D[check] ; }
for(m in ams[ams!=6205]) acs[,paste0("MAJ_",m)]<-(acs$Major_Larger%in%m)
i<-"OLS_ACS" ; R[,i]<-"" ; hi<-data.frame(summary(felm(as.formula(paste0("INCWAGE~",paste0(names(acs)[grepl("MAJ_",names(acs))],collapse="+"),"")),acs[acs$YEAR%in%2017:2018,],weights=acs$PERWT[acs$YEAR%in%2017:2018]))$coefficients)[,1:2] ; hi<-hi[grepl("MAJ",row.names(hi)),] ; row.names(hi)<-gsub("MAJ_|TRUE","",row.names(hi)) ; h<-data.frame(Estimate=0,Std..Error=0) ; row.names(h)<-"6205" ; hi<-rbind(hi,h) ; hi<-hi[as.character(temp$ACS_Field),] ; hi$Major_Larger<-as.character(temp$Major) ; c<-merge(c[,!names(c)=="Estimate"],hi[,c("Major_Larger","Estimate")],all.x=T) ; R[odd,i]<-ex(hi[,1],0,comma=T) ; R[even,i]<-ex(hi[,2],0,par=T,comma=T)
R[151,i]<-ex(sum(R$Shares[odd]*hi[,1]/100,na.rm=T),0,comma=T)
regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R[154,i]<-ex(regiv[4,1],0,comma=T) ; R[155,i]<-ex(regiv[4,2],0,par=T,comma=T)

#Medians by group
v<-"UCSC_Median" ; R$UCSC_Median<-"" ; t<-c() ; for(m in R$Major[odd]) t<-c(t,median(S$wage_sum_1718[S$Year_Applied%in%2008:2012&(S$Major1%in%m|S$Major2%in%m|S$Major3%in%m|S$Major4%in%m)],na.rm=T)) ; R$UCSC_Median[150]<-ex(sum(R$UCSC_Share_cont[odd]*t,na.rm=T),0,comma=T) ; R$UCSC_Median[151]<-ex(sum(R$Shares[odd]*t/100,na.rm=T),0,comma=T) ; R$UCSC_Median[odd]<-ex(t,0,comma=T) ; t<-data.frame(Major_Larger=R$Major[odd],Estimate=t) ; c<-merge(c[,!names(c)=="Estimate"],t[,c("Major_Larger","Estimate")],all.x=T)
regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R[152,v]<-ex(regiv[4,1],0,comma=T) ; R[153,v]<-ex(regiv[4,2],0,par=T,comma=T)
regiv<-summary(felm(winsor(Major_Median1718,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R[154,v]<-ex(regiv[4,1],0,comma=T) ; R[155,v]<-ex(regiv[4,2],0,par=T,comma=T)
v<-"UCSC0910_Median" ; R$UCSC0910_Median<-"" ; t<-c() ; for(m in R$Major[odd]) t<-c(t,median(S$wage_sum_0910[S$Year_Applied%in%2000:2004&(S$Major1%in%m|S$Major2%in%m|S$Major3%in%m|S$Major4%in%m)],na.rm=T)) ; R$UCSC0910_Median[150]<-ex(sum(R$UCSC_Share_cont[odd]*t,na.rm=T),0,comma=T) ; R$UCSC0910_Median[151]<-ex(sum(R$Shares[odd]*t/100,na.rm=T),0,comma=T) ; R$UCSC0910_Median[odd]<-ex(t,0,comma=T) ; t<-data.frame(Major_Larger=R$Major[odd],Estimate=t) ; c<-merge(c[,!names(c)=="Estimate"],t[,c("Major_Larger","Estimate")],all.x=T)
regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R[152,v]<-ex(regiv[4,1],0,comma=T) ; R[153,v]<-ex(regiv[4,2],0,par=T,comma=T)
regiv<-summary(felm(winsor(Major_Median0910,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R[154,v]<-ex(regiv[4,1],0,comma=T) ; R[155,v]<-ex(regiv[4,2],0,par=T,comma=T)
regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8&!is.na(c$Major_Median0910),]))$coefficients ; R[156,v]<-ex(regiv[4,1],0,comma=T) ; R[157,v]<-ex(regiv[4,2],0,par=T,comma=T)
R$ACS_Median<-"" ; t<-c() ; for(m in temp$ACS_Field) t<-c(t,weighted.median(acs$INCWAGE[acs$YEAR%in%2017:2018&(acs$Major_Larger%in%m)],acs$PERWT[acs$YEAR%in%2017:2018&(acs$Major_Larger%in%m)],na.rm=T)) ; R$ACS_Median[151]<-ex(sum(R$Shares[odd]*t/100,na.rm=T),0,comma=T) ; R$ACS_Median[odd]<-ex(t,0,comma=T) ; t<-data.frame(Major_Larger=temp$Major,Estimate=t) ; c<-merge(c[,!names(c)=="Estimate"],t[,c("Major_Larger","Estimate")],all.x=T)
R$ACS_Median[150]<-ex(sum(R$UCSC_Share_cont[odd]*t$Estimate,na.rm=T),0,comma=T)

regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R$ACS_Median[152]<-ex(regiv[4,1],0,comma=T) ; R$ACS_Median[153]<-ex(regiv[4,2],0,par=T,comma=T)
regiv<-summary(felm(winsor(Major_ACSMedian2018,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R$ACS_Median[154]<-ex(regiv[4,1],0,comma=T) ; R$ACS_Median[155]<-ex(regiv[4,2],0,par=T,comma=T)
#Now recalculate for CA
acsn<-acs[acs$STATEFIP%in%c(6),]
t<-c() ; for(m in unique(acsn$DEGFIELDD[acsn$YEAR==2018])) t<-c(t,weighted.median(acsn$INCWAGE[acsn$YEAR%in%2017:2018&acsn$DEGFIELDD==m&acsn$DEGFIELD2D==0],acsn$PERWT[acsn$YEAR%in%2017:2018&acsn$DEGFIELDD==m&acsn$DEGFIELD2D==0],na.rm=T)) ; t<-unique(acsn$DEGFIELDD[acsn$YEAR==2018])[order(t,decreasing = T)] #Order the majors by higher-earning, among single majors
acsn$Major_Larger<-acsn$DEGFIELDD ; for(i in 1:(length(t)-1)){ ; check<-acsn$DEGFIELD2D==t[i]&acsn$DEGFIELDD%in%t[(i+1):length(t)] ; acsn$Major_Larger[check]<-acsn$DEGFIELD2D[check] ; }
R$ACS_CA_Median<-"" ; t<-c() ; for(m in temp$ACS_Field) t<-c(t,weighted.median(acsn$INCWAGE[acsn$YEAR%in%2017:2018&(acsn$DEGFIELDD%in%m)],acsn$PERWT[acsn$YEAR%in%2017:2018&(acsn$DEGFIELDD%in%m)],na.rm=T)) ; R$ACS_CA_Median[151]<-ex(sum(R$Shares[odd]*t/100,na.rm=T),0,comma=T) ; R$ACS_CA_Median[odd]<-ex(t,0,comma=T) ; t<-data.frame(Major_Larger=temp$Major,Estimate=t) ; c<-merge(c[,!names(c)=="Estimate"],t[,c("Major_Larger","Estimate")],all.x=T)
R$ACS_CA_Median[150]<-ex(sum(R$UCSC_Share_cont[odd]*t$Estimate,na.rm=T),0,comma=T)
regiv<-summary(felm(winsor(Major_ACSMedian_CA2018,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R$ACS_CA_Median[154]<-ex(regiv[4,1],0,comma=T) ; R$ACS_CA_Median[155]<-ex(regiv[4,2],0,par=T,comma=T)
#Now re-calculate everything for anti-CA states
acsn<-acs[acs$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18),]
for(v in c("DEGFIELDD","DEGFIELD2D")) acsn[acsn[,v]==6205,v]<-5501
t<-c() ; for(m in unique(acsn$DEGFIELDD[acsn$YEAR==2018])) t<-c(t,weighted.median(acsn$INCWAGE[acsn$YEAR%in%2017:2018&acsn$DEGFIELDD==m&acsn$DEGFIELD2D==0],acsn$PERWT[acsn$YEAR%in%2017:2018&acsn$DEGFIELDD==m&acsn$DEGFIELD2D==0],na.rm=T)) ; t<-unique(acsn$DEGFIELDD[acsn$YEAR==2018])[order(t,decreasing = T)] #Order the majors by higher-earning, among single majors
acsn$Major_Larger<-acsn$DEGFIELDD ; for(i in 1:(length(t)-1)){ ; check<-acsn$DEGFIELD2D==t[i]&acsn$DEGFIELDD%in%t[(i+1):length(t)] ; acsn$Major_Larger[check]<-acsn$DEGFIELD2D[check] ; }
R$ACS_NotCA_Median<-"" ; t<-c() ; for(m in temp$ACS_Field_NotCA) t<-c(t,weighted.median(acsn$INCWAGE[acsn$YEAR%in%2017:2018&(acsn$Major_Larger%in%m)],acsn$PERWT[acsn$YEAR%in%2017:2018&(acsn$Major_Larger%in%m)],na.rm=T)) ; R$ACS_NotCA_Median[151]<-ex(sum(R$Shares[odd]*t/100,na.rm=T),0,comma=T) ; R$ACS_NotCA_Median[odd]<-ex(t,0,comma=T) ; t<-data.frame(Major_Larger=temp$Major,Estimate=t) ; c<-merge(c[,!names(c)=="Estimate"],t[,c("Major_Larger","Estimate")],all.x=T)
acsm<-data.frame() ; for(m in unique(acsn$Major_Larger[acsn$YEAR%in%2017:2018])) acsm<-rbind(acsm,data.frame(Major=m,Share=weighted.mean(acsn$Major_Larger[acsn$YEAR%in%2017:2018]==m,acsn$PERWT[acsn$YEAR%in%2017:2018]),Median=weighted.median(acsn$INCWAGE[acsn$YEAR%in%2017:2018&acsn$Major_Larger==m],acsn$PERWT[acsn$YEAR%in%2017:2018&acsn$Major_Larger==m]))) ; acsm$Econ<-acsm$Major%in%c(5501,6205) ; acsm$Share[acsm$Econ==0]<- -acsm$Share[acsm$Econ==0]/sum(acsm$Share[acsm$Econ==0]) ; acsm$Share[acsm$Econ==1]<- acsm$Share[acsm$Econ==1]/sum(acsm$Share[acsm$Econ==1])
R$ACS_NotCA_Median[150]<-ex(sum(acsm$Share*acsm$Median,na.rm=T),0,comma=T)
regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R$ACS_NotCA_Median[152]<-ex(regiv[4,1],0,comma=T) ; R$ACS_NotCA_Median[153]<-ex(regiv[4,2],0,par=T,comma=T)
regiv<-summary(felm(winsor(Major_ACSMedian_NotCA2018,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients ; R$ACS_NotCA_Median[154]<-ex(regiv[4,1],0,comma=T) ; R$ACS_NotCA_Median[155]<-ex(regiv[4,2],0,par=T,comma=T)
regiv<-summary(felm(winsor(Estimate,0.02)~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA+AllMajors_ACS,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8&!is.na(c$Major_ACSMedian_NotCA2018),]))$coefficients ; R$ACS_NotCA_Median[156]<-ex(regiv[4,1],0,comma=T) ; R$ACS_NotCA_Median[157]<-ex(regiv[4,2],0,par=T,comma=T)
write.csv(R,file="Figures/MajorChanges.csv")

###
#  Produce Figure 6
###
png(paste0("Figures/CounterfactualMajorFigure.png"),width=3200,height=2600,res=300)
  plot(-100,-100,type="l",ylim=c(-50000,70000),yaxp=c(0,60000,4),xlim=c(0,10.03),xlab="",ylab="",cex.axis=2,cex.lab=1.5,bty="l",axes=F)
  axis(side=2,at=seq(0,60000,20000),labels=c("0","20,000","40,000","60,000"),cex.axis=1.5)
  mean_wage<-mean(c$Major_OLS[c$BusEcon==100&c$Year_Applied%in%2008:2012&c$GPA>1.8&is.na(c$Major2)],na.rm=T) #Using different mean_wages for OLS and OLS_Cov, from each regression.
  #OLS Without Controls
  start<-0 ; xv<-start ; for(i in 1:10){
    xv1<- -R$Shares[3*i-2]/100 ; med<-as.integer(gsub("[^0-9-]","",R$OLS_UCSC[3*i-2]))+mean_wage
    polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=paste0("gray",8+(i*7)),lty = 0)
    xv<-xv+xv1
  }
  xv1<- -sum(R$Shares[seq(1,139,3)[-(1:10)]]/100) ; med<-sum(as.integer(gsub("[^0-9-]","",R$OLS_UCSC[seq(1,139,3)[-(1:10)]]))*-R$Shares[seq(1,139,3)[-(1:10)]]/100,na.rm=T)/xv1+mean_wage
  polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=paste0("gray",8+(11*7)),lty = 0)
  c$DepVar<-c$Major_OLS-mean_wage-700 ; c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02) ; c$DepVar<-c$DepVar*!c$Econ
  avg<- -summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients[4,1]+mean_wage
  lines(c(start,xv),c(avg,avg),lty=2,lwd=2) ; diff1<-avg
  start<-start+1 ; xv<-start ; for(i in 3:1){
    xv1<- R$Shares[3*i+139]/100 ; med<-as.integer(gsub("[^0-9-]","",R$OLS_UCSC[3*i+139]))+mean_wage
    polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=lighten("blue",.75-.15*i),lty = 0)
    xv<-xv+xv1
  }
  c$DepVar<-c$Major_OLS-mean_wage-700 ; c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02) ; c$DepVar<-c$DepVar*c$Econ 
  avg<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients[4,1]+mean_wage
  lines(c(start,xv),c(avg,avg),lty=2,lwd=2) ; y1<-diff1+(avg-diff1)/2 ; diff1<-paste0("$",ex(avg-diff1,0,comma=T,justnum = T))
  #OLS With Controls
  start<-3 ; xv<-start ; for(i in 1:10){
    xv1<- -R$Shares[3*i-2]/100 ; med<-as.integer(gsub("[^0-9-]","",R$OLS_UCSC_Cov[3*i-2]))+mean_wage
    polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=paste0("gray",8+(i*7)),lty = 0)
    xv<-xv+xv1
  }
  xv1<- -sum(R$Shares[seq(1,139,3)[-(1:10)]]/100) ; med<-sum(as.integer(gsub("[^0-9-]","",R$OLS_UCSC_Cov[seq(1,139,3)[-(1:10)]]))*-R$Shares[seq(1,139,3)[-(1:10)]]/100,na.rm=T)/xv1+mean_wage
  polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=paste0("gray",8+(11*7)),lty = 0)
  c$DepVar<-c$Major_OLS_Covariates ; c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02) ; c$DepVar<-c$DepVar*!c$Econ
  avg<- -summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients[4,1]+mean_wage
  lines(c(start,xv),c(avg,avg),lty=2,lwd=2) ; diff2<-avg
  start<-start+1 ; xv<-start ; for(i in 3:1){
    xv1<- R$Shares[3*i+139]/100 ; med<-as.integer(gsub("[^0-9-]","",R$OLS_UCSC_Cov[3*i+139]))+mean_wage
    polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=lighten("blue",.75-.15*i),lty = 0)
    xv<-xv+xv1
  }
  c$DepVar<-c$Major_OLS_Covariates ; c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02) ; c$DepVar<-c$DepVar*c$Econ
  avg<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients[4,1]+mean_wage
  lines(c(start,xv),c(avg,avg),lty=2,lwd=2) ; y2<-diff2+(avg-diff2)/2 ; diff2<-paste0("$",ex(avg-diff2,0,comma=T,justnum = T))
  #ACS CA
  start<-6 ; xv<-start ; for(i in 1:10){
    xv1<- -R$Shares[3*i-2]/100 ; med<-as.integer(gsub("[^0-9]","",R$ACS_CA_Median[3*i-2]))
    polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=paste0("gray",8+(i*7)),lty = 0)
    xv<-xv+xv1
  }
  xv1<- -sum(R$Shares[seq(1,139,3)[-(1:10)]]/100) ; med<-sum(as.integer(gsub("[^0-9-]","",R$ACS_CA_Median[seq(1,139,3)[-(1:10)]]))*-R$Shares[seq(1,139,3)[-(1:10)]]/100,na.rm=T)/xv1
  polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=paste0("gray",8+(11*7)),lty = 0)
  c$DepVar<-c$Major_ACSMedian_CA2018 ; c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02) ; c$DepVar<-c$DepVar*!c$Econ
  avg<- -summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients[4,1]
  lines(c(start,xv),c(avg,avg),lty=2,lwd=2) ; diff3<-avg
  start<-start+1 ; xv<-start ; for(i in 3:1){
    xv1<- R$Shares[3*i+139]/100 ; med<-as.integer(gsub("[^0-9]","",R$ACS_CA_Median[3*i+139]))
    polygon(c(xv,xv,xv+xv1,xv+xv1),c(0,med,med,0),col=lighten("blue",.75-.15*i),lty = 0)
    xv<-xv+xv1
  }
  c$DepVar<-c$Major_ACSMedian_CA2018 ; c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02) ; c$DepVar<-c$DepVar*c$Econ
  avg<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients[4,1]
  lines(c(start,xv),c(avg,avg),lty=2,lwd=2) ; y3<-diff3+(avg-diff3)/2 ; diff3<-paste0("$",ex(avg-diff3,0,comma=T,justnum = T))
  #Now the final bars
  c$DepVar<-c$wage_sum_1718 ; c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02) ; c$DepVar<-c$DepVar*!c$Econ
  avg<- -summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients[4,1]
  polygon(c(9.125,9.125,9.375,9.375),c(0,avg,avg,0),col="black",lty = 0)
  lines(c(-.6,9.25),c(avg,avg),lty=3,col=lighten("forestgreen",0.1),lwd=2) ; diff4<-avg
  c$DepVar<-c$wage_sum_1718 ; c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%2008:2012&c$GPA>1.8],.02) ; c$DepVar<-c$DepVar*c$Econ
  avg<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients[4,1]
  polygon(c(9.625,9.625,9.875,9.875),c(0,avg,avg,0),col="blue",lty = 0)
  lines(c(-.6,9.75),c(avg,avg),lty=3,col=lighten("forestgreen",0.1),lwd=2) ; y4<-diff4+(avg-diff4)/2 ; diff4<-paste0("$",ex(avg-diff4,0,comma=T,justnum = T))
  #And final info
  title(ylab="                                 Est. Average Wages ($)",line=2.7,cex.lab=1.4)
  text(.88,y1,diff1,adj = .5, cex = 1.32, srt = 270)
  arrows(.89, y1+as.integer(gsub("[^0-9]","",diff1))/2-2200, .89, y1+as.integer(gsub("[^0-9]","",diff1))/2, length=0.05, angle=90, code=2)
  arrows(.89, y1-as.integer(gsub("[^0-9]","",diff1))/2+2200, .89, y1-as.integer(gsub("[^0-9]","",diff1))/2, length=0.05, angle=90, code=2)
  text(3.88,y2,diff2,adj = .5, cex = 1.32, srt = 270)
  arrows(3.89, y2+as.integer(gsub("[^0-9]","",diff2))/2-1000, 3.89, y2+as.integer(gsub("[^0-9]","",diff2))/2, length=0.05, angle=90, code=2)
  arrows(3.89, y2-as.integer(gsub("[^0-9]","",diff2))/2+1000, 3.89, y2-as.integer(gsub("[^0-9]","",diff2))/2, length=0.05, angle=90, code=2)
  text(6.88,y3,diff3,adj = .5, cex = 1.32, srt = 270)
  arrows(6.89, y3+as.integer(gsub("[^0-9]","",diff3))/2-1700, 6.89, y3+as.integer(gsub("[^0-9]","",diff3))/2, length=0.05, angle=90, code=2)
  arrows(6.89, y3-as.integer(gsub("[^0-9]","",diff3))/2+1700, 6.89, y3-as.integer(gsub("[^0-9]","",diff3))/2, length=0.05, angle=90, code=2)
  text(9.5,y4,diff4,adj = .5, cex = 1.42, srt = 270,col="forestgreen")
  arrows(9.5, y4+as.integer(gsub("[^0-9]","",diff4))/2-2900, 9.5, y4+as.integer(gsub("[^0-9]","",diff4))/2, length=0.05, angle=90, code=2,col="forestgreen")
  arrows(9.5, y4-as.integer(gsub("[^0-9]","",diff4))/2+2900, 9.5, y4-as.integer(gsub("[^0-9]","",diff4))/2, length=0.05, angle=90, code=2,col="forestgreen")
  text(1,-3000,"UCSC OLS,",adj=.5,cex=1.4)
  text(1,-8500,"No Controls",adj=.5,cex=1.4)
  text(4,-3000,"UCSC OLS,",adj=.5,cex=1.4)
  text(4,-8500,"With Controls",adj=.5,cex=1.4)
  text(7,-3000,"CA Median",adj=.5,cex=1.4)
  text(7,-8500,"Wages (ACS)",adj=.5,cex=1.4)
  text(9.5,-3000,"Local Average",adj=.5,cex=1.4)
  text(9.5,-8500,"Treat. Effect",adj=.5,cex=1.4)
  cols<-c() ; for(i in 1:11) cols<-c(cols,paste0("gray",8+(i*7))) ; cols<-c(cols,"white") ; for(i in 3:1) cols<-c(cols,lighten("blue",.75-.15*i))
  text(3.45,-20000,bquote(underline("Counterfactual Majors and Shares")),cex=1.5)
  text(9.05,-20000,bquote(underline("Econ. Majors")),cex=1.5)
  legend(-.6,-22500,legend = c("Psychology: 20%","Env. Studies: 14%","Tech/Info. Mgmt: 12%","Sociology: 10%","Film and Dg. Med: 8%","Legal Studies: 8%","Mathematics: 7%","Lat. Amer. Stud: 5%","Art: 4%","Anthropology: 4%","Other Majors: 19%","","Bus. Mgmt. Econ: 90%","Global Econ: 6%","Economics: 4%"),fill=cols,border = F,bty = "n",ncol = 4,cex=1.21,x.intersp =.5,text.width = 2.2)
  lines(c(-.6,1000),c(0,0))
dev.off()


###
#Now show how average wages by the weighted majors evolve over time (Figure A-8)
###
ddi <- read_ipums_ddi("Data/Raw/usa_00075.xml")
  acs <- read_ipums_micro(ddi)
  acs <- acs[acs$EDUCD>100&acs$INCWAGE>0&acs$INCWAGE!=999999,] ; gc() #Just combine across years
cpi<-read.csv("Data/Raw/CPI.csv") ; names(cpi)<-c("YEAR","CPI")
acs<-merge(acs,cpi) ; acs$INCWAGE<-acs$INCWAGE/(acs$CPI/cpi$CPI[cpi$YEAR==2018])
acs$DEGFIELDD[acs$DEGFIELDD==5501]<-6205
#For now, just using first major
hi<-ddply(acs[acs$AGE%in%22:62&acs$DEGFIELDD%in%temp$ACS_Field,],.(AGE,DEGFIELDD),function(x) weighted.median(x$INCWAGE,x$PERWT))
hi1<-data.frame(Share=-R$Shares[odd]/100,DEGFIELDD=temp$ACS_Field,Econ=c(rep(0,47),1/3,1/3,1/3)) ; hi1$DEGFIELDD[hi1$DEGFIELDD==5501]<-6205 ; hi1<-aggregate(.~DEGFIELDD,hi1,sum) ; hi<-merge(hi,hi1)
hi<-ddply(hi,.(AGE,Econ),function(x) sum(x$V1*x$Share))
options(scipen=999)
png(paste0("Figures/EconWageTrend.png"),width=800,height=500)
  plot(-100,-100,type="l",ylim=c(0,110000),yaxp=c(0,100000,2),xlim=c(20,62),xlab="Age",ylab="Median Wages",cex.axis=2,cex.lab=1.5)
  polygon(c(23,23,28,28),c(-100000,1000000,1000000,-100000),col='gray80',lty = 0)
  par(new = T)
  plot(-100,-100,type="l",ylim=c(0,110000),yaxp=c(0,100000,2),xlim=c(20,62),xlab="Age",ylab="Median Wages",cex.axis=2,cex.lab=1.5)
  lines(hi$AGE[hi$Econ==1],-hi$V1[hi$Econ==1],lwd=2)
  lines(hi$AGE[hi$Econ==0],hi$V1[hi$Econ==0],lty=2,lwd=2)
  text(46,82000,paste0("Age 22-62 NPV: $",ex(-sum((hi$V1[hi$Econ==1]+hi$V1[hi$Econ==0])*(0.97^(0:(sum(hi$Econ)-1)))),0,comma=T,justnum=T)),cex=2)
  legend(29,20000,legend=c("Median Wages of Economics Majors","Median Wages in Second-Choice Majors (Weighted)"),lwd=c(2,2),lty=c(1,2),cex=1.55)
dev.off()


###
#  Show growth over age instead of time (Figure 3)
###
for(y in 2008:2012) for(i in 4:(2018-y)) c[c$Year_Applied==y,paste0("wage_",i)]<-c[c$Year_Applied==y,paste0("wage_sum_",y+i)]
odds<-c(1,4,7,10,13,16,19) ; evens<-odds+1
W<-data.frame(Num=rep("",20),stringsAsFactors = F) ; W$Num[odds]<-4:10
for(i in 4:10){
  for(y in list(2008:2009,2010,2011:2012)){
    if(max(y)+i>2018) next
    c$DepVar<-c[,paste0("wage_",i)]
    if(length(table(c$DepVar))>2) c$DepVar[c$Year_Applied%in%y&c$GPA>1.8]<-winsor(c$DepVar[c$Year_Applied%in%y&c$GPA>1.8],.02)
    regiv<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%y&c$GPA>1.8,]))$coefficients
    W[(i-3)*3-2,paste0("Y",y[1])]<-round(regiv[row.names(regiv)=="`Econ(fit)`",1]) ; W[(i-3)*3-1,paste0("Y",y[1])]<-round(regiv[row.names(regiv)=="`Econ(fit)`",2])
  }
}
png("Figures/RDIV_TimeTrend.png",width=800,height=500)
  plot(1000000,1000000,xlim=c(3.5,15.5),ylim=c(-20000,70000),xlab="Years since HS Graduation",ylab="Dollars",cex.axis=2,cex.lab=1.5)
  abline(h=0)
  lines(4:15,-(hi$V1[hi$Econ==0]+hi$V1[hi$Econ==1])[1:12])
  points(4:10-.15,W$Y2008[odds],pch=15,col="firebrick1",cex=2)
  arrows(4:10-.15, W$Y2008[odds]-W$Y2008[evens]*1.96, 4:10-.15, W$Y2008[odds]+W$Y2008[evens]*1.96, length=0.05, angle=90, code=3,col="firebrick1")
  points(4:10,W$Y2010[odds],pch=16,col="blue",cex=2)
  arrows(4:10, W$Y2010[odds]-W$Y2010[evens]*1.96, 4:10, W$Y2010[odds]+W$Y2010[evens]*1.96, length=0.05, angle=90, code=3,col="blue")
  points(4:10+.15,W$Y2011[odds],pch=17,col="chartreuse4",cex=2)
  arrows(4:10+.15, W$Y2011[odds]-W$Y2011[evens]*1.96, 4:10+.15, W$Y2011[odds]+W$Y2011[evens]*1.96, length=0.05, angle=90, code=3,col="chartreuse4")
  legend(9.8,70000,lty=c(NA,NA,NA,1),pch=c(15,16,17,26),cex=1.6,col=c("firebrick1","blue","chartreuse4","black"),legend=c("RDIV Est., 2008-2009 Cohorts","RDIV Est., 2010 Cohort","RDIV Est., 2011-2012 Cohorts","Diff. in ACS Medians"))
dev.off()







###
#  The rest of the code produces extra statistics for appendix tables
###

#Find which industries have biggest shifts (Appendix Table A-5)
for(d in c("c","S")){
  D<-get(d)
  D$I1<-D$Ind2_2017 ; D$I1[D$NAICS4_2017==541211]<-541211
  D$I2<-D$Ind2_2018 ; D$I2[D$NAICS4_2018==541211]<-541211
  assign(d,D)
}
ns<-unique(c$I1[!is.na(c$I1)])
R1<-data.frame(matrix("",length(ns),6),stringsAsFactors = F)
r<-1 ; for(n in ns){
  R1[r,1]<-ns[r]
  c$DepVar<-(c$I1%in%n|c$I2%in%n)*100 ; c$DepVar[is.na(c$I1)&is.na(c$I2)]<-NA
  regiv<-summary(felm(DepVar~GPA_Abo1+GPA_Bel1|0|(Econ~Above)|GPA,data=c[c$Year_Applied%in%2008:2012&c$GPA>1.8,]))$coefficients
  R1[r,c(2)]<-ex(regiv[4,c(1)],1)
  R1[r,c(3)]<-ex(regiv[4,c(2)],1,par=T)
  R1[r,c(4,5)]<-regiv[4,c(3,4)]
  R1$UCSCEcon[r]<-ex(mean(c$DepVar[c$Econ&c$Year_Applied%in%2008:2012],na.rm=T),1)
  S$DepVar<-(S$I1%in%n|S$I2%in%n)*100 ; S$DepVar[is.na(S$I1)&is.na(S$I2)]<-NA
  R1$UCSC[r]<-ex(mean(S$DepVar[S$Year_Applied%in%2008:2012&S$Adm_Cat=="Freshman"],na.rm=T),1)
  r<-r+1
}
hi<-read.csv("Data/Raw/NAICS2_to_Names.csv") ; names(hi)<-c("X1","Industry") ; R1<-merge(R1,hi,all.x=T)
R1[,4]<-as.numeric(R1[,4])
R1[,5]<-as.numeric(gsub("[^0-9.-]","",R1$X2))
R1<-R1[order(-R1$X5),]
#Now add industry shares from ACS
load("Data/Derived/ACS_Data_UCSCEcon_NAICS.Rda")
acs<-acs[acs$YEAR%in%c(2016,2017),]
load("Data/Derived/ACS_Data_UCSCEcon_NAICS_AllYears.Rda")
acs1<-acs1[acs1$YEAR!=2018,]
for(a in c("acs","acs1")){
  temp<-get(a)
  temp$NAICS2<-substr(temp$INDNAICS,1,2)
  temp$NAICS2[NA_to_F(grepl("^531",temp$INDNAICS))]<-"52" #Combine FIRE, but not "Rental/Leasing"
  temp$NAICS2[NA_to_F(temp$NAICS2%in%c("32","33"))]<-"31"
  temp$NAICS2[NA_to_F(temp$NAICS2%in%c("45"))]<-"44"
  temp$NAICS2[NA_to_F(temp$NAICS2%in%c("49"))]<-"48"
  temp$NAICS2[NA_to_F(temp$INDNAICS%in%c("5412"))]<-"541211"
  temp$Econ<-temp$DEGFIELDD%in%c(5501,6205)|temp$DEGFIELD2D%in%c(5501,6205)
  assign(a,temp)
}
for(i in 1:nrow(R1)){
  R1$USEcon[i]<-ex(mean(acs$NAICS2[acs$DEGFIELDD%in%c(5501,6205)|acs$DEGFIELD2D%in%c(5501,6205)]==R1$X1[i])*100,1)
  R1$US[i]<-ex(mean(acs$NAICS2==R1$X1[i])*100,1)
  R1$CA[i]<-ex(mean(acs$NAICS2[acs$STATEFIP%in%c(6)]==R1$X1[i])*100,1)
  R1$NonCA[i]<-ex(mean(acs$NAICS2[acs$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)]==R1$X1[i])*100,1)
  R1$CA_wages[i]<-ex(weighted.median(acs$INCWAGE[acs$STATEFIP%in%c(6)&acs$NAICS2==R1$X1[i]],acs$PERWT[acs$STATEFIP%in%c(6)&acs$NAICS2==R1$X1[i]]),1)
  R1$NonCA_wages[i]<-ex(weighted.median(acs$INCWAGE[acs$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)&acs$NAICS2==R1$X1[i]],acs$PERWT[acs$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)&acs$NAICS2==R1$X1[i]]),1)
  R1$CA_Econ[i]<-ex(weighted.mean((acs1$NAICS2[(acs1$STATEFIP%in%c(6))&acs1$Econ]==R1$X1[i])*100,acs1$PERWT[(acs1$STATEFIP%in%c(6))&acs1$Econ]),1)
  R1$NonCA_Econ[i]<-ex(weighted.mean((acs1$NAICS2[(acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18))&acs1$Econ]==R1$X1[i])*100,acs1$PERWT[(acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18))&acs1$Econ]),1)
  R1$CA_wages_Econ[i]<-ex(weighted.median(acs1$INCWAGE[acs1$STATEFIP%in%c(6)&acs1$NAICS2==R1$X1[i]&acs1$Econ],acs1$PERWT[acs1$STATEFIP%in%c(6)&acs1$NAICS2==R1$X1[i]&acs1$Econ]),1)
  R1$NonCA_wages_Econ[i]<-ex(weighted.median(acs1$INCWAGE[acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)&acs1$NAICS2==R1$X1[i]&acs1$Econ],acs1$PERWT[acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)&acs1$NAICS2==R1$X1[i]&acs1$Econ]),1)
  R1$CA_Num_Econ[i]<-ex(sum(acs1$STATEFIP%in%c(6)&acs1$NAICS2==R1$X1[i]&acs1$Econ),0)
  R1$NonCA_Num_Econ[i]<-ex(sum(acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)&acs1$NAICS2==R1$X1[i]&acs1$Econ),0)
  R1$CA_NonEcon[i]<-ex(weighted.mean((acs1$NAICS2[(acs1$STATEFIP%in%c(6))&!acs1$Econ]==R1$X1[i])*100,acs1$PERWT[(acs1$STATEFIP%in%c(6))&!acs1$Econ]),1)
  R1$NonCA_NonEcon[i]<-ex(weighted.mean((acs1$NAICS2[(acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18))&!acs1$Econ]==R1$X1[i])*100,acs1$PERWT[(acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18))&!acs1$Econ]),1)
  R1$CA_wages_NonEcon[i]<-ex(weighted.median(acs1$INCWAGE[acs1$STATEFIP%in%c(6)&acs1$NAICS2==R1$X1[i]&!acs1$Econ],acs1$PERWT[acs1$STATEFIP%in%c(6)&acs1$NAICS2==R1$X1[i]&!acs1$Econ]),1)
  R1$NonCA_wages_NonEcon[i]<-ex(weighted.median(acs1$INCWAGE[acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)&acs1$NAICS2==R1$X1[i]&!acs1$Econ],acs1$PERWT[acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)&acs1$NAICS2==R1$X1[i]&!acs1$Econ]),1)
  R1$CA_Num_NonEcon[i]<-ex(sum(acs1$STATEFIP%in%c(6)&acs1$NAICS2==R1$X1[i]&!acs1$Econ),0)
  R1$NonCA_Num_NonEcon[i]<-ex(sum(acs1$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18)&acs1$NAICS2==R1$X1[i]&!acs1$Econ),0)
}
write.csv(R1,file="Figures/IndustryChanges.csv")



#Build a table showing UCSC-ACS major link (Table A-7)
c$Count<-1 ; hi<-aggregate(Count~Major1,c,sum)
hi1<-aggregate(Count~Major2,c,sum) ; names(hi1)<-c("Major1","Count2") ; hi<-merge(hi,hi1,all=T)
hi1<-aggregate(Count~Major3,c,sum) ; names(hi1)<-c("Major1","Count3") ; hi<-merge(hi,hi1,all=T)
hi1<-aggregate(Count~Major4,c,sum) ; names(hi1)<-c("Major1","Count4") ; hi<-merge(hi,hi1,all=T)
hi$Prop<-ex((NA_to_F(hi$Count)+NA_to_F(hi$Count2)+NA_to_F(hi$Count3)+NA_to_F(hi$Count4))/nrow(c)*100,1)
temp<-read.csv("Data/Raw/UCSC_Majors_to_ACS.csv")[,c(1,3,4)] ; names(temp)[1]<-"Major1"
hi<-merge(hi,temp)
hi<-hi[,c(1,7,8,6)]
write.csv(hi,file="Figures/Major_to_ACS_table.csv")


#Histograms of economics course-taking at UCSB (Figure A-13)
tim<-"Technology&Info Management" ; S$TIM<-S$Major1%in%tim|S$Major2%in%tim|S$Major3%in%tim|S$Major4%in%tim
quantile(S$C_Economics[S$Econ&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"],.05) ; sum(!S$Econ[S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"])
sum(S$C_Economics_Upper[S$Econ&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"]>24)
sum(S$C_Economics[!S$Econ&S$TIM&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"]>8)
png("Figures/Hist_EconCourses_All_NoTIM.png",width=500,height=300)
  hist(S$C_Economics[S$Econ&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"], col=rgb(0.1,0.1,0.1,0.5),xlim=c(0,25), ylim=c(0,1000), main="",breaks=seq(-0.5,25.5,1),xlab="Number of Economics Courses",ylab="Number of Students",cex.axis=1.5,cex.lab=1.5,mgp=c(2.4,1,0))
  hist(S$C_Economics[!S$Econ&!S$TIM&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"], col=rgb(0.8,0.8,0.8,0.5),breaks=seq(-0.5,25.5,1), add=T)
  box()
dev.off()
png("Figures/Hist_EconCourses_All_WithTIM.png",width=500,height=300)
  hist(S$C_Economics[S$Econ&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"], col=rgb(0.1,0.1,0.1,0.5),xlim=c(0,25), ylim=c(0,1000), main="",breaks=seq(-0.5,25.5,1),xlab="Number of Economics Courses",ylab="Number of Students",cex.axis=1.5,cex.lab=1.5,mgp=c(2.4,1,0))
  hist(S$C_Economics[!S$Econ&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"], col=rgb(0.8,0.8,0.8,0.5),breaks=seq(-0.5,25.5,1), add=T)
  box()
dev.off()
png("Figures/Hist_EconCourses_UppDiv_NoTIM.png",width=500,height=300)
  hist(S$C_Economics_Upper[S$Econ&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"], col=rgb(0.1,0.1,0.1,0.5),xlim=c(0,25), ylim=c(0,1000), main="",breaks=seq(-0.5,25.5,1),xlab="Number of Economics Courses",ylab="Number of Students",cex.axis=1.5,cex.lab=1.5,mgp=c(2.4,1,0))
  hist(S$C_Economics_Upper[!S$Econ&!S$TIM&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"], col=rgb(0.8,0.8,0.8,0.5),breaks=seq(-0.5,25.5,1), add=T)
  box()
dev.off()
png("Figures/Hist_EconCourses_UppDiv_WithTIM.png",width=500,height=300)
  hist(S$C_Economics_Upper[S$Econ&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"], col=rgb(0.1,0.1,0.1,0.5),xlim=c(0,25), ylim=c(0,1000), main="",breaks=seq(-0.5,25.5,1),xlab="Number of Economics Courses",ylab="Number of Students",cex.axis=1.5,cex.lab=1.5,mgp=c(2.4,1,0))
  hist(S$C_Economics_Upper[!S$Econ&S$Year_Applied%in%2008:2012&!is.na(S$Grad_Year)&S$Adm_Cat=="Freshman"], col=rgb(0.8,0.8,0.8,0.5),breaks=seq(-0.5,25.5,1), add=T)
  box()
dev.off()
png(paste0("Figures/Hist_EconCourses_Legend.png"),width=8,height=3,units="in",res=500)
  plot(100000,100000,bty="n",box=F,yaxt="n",xaxt="n",xlim=c(0,50),ylim=c(0,10),xlab="",ylab="")
  par( lend = 1 )
  legend(1,8,legend=c("Non-Majors","Economics Majors"),lty=c(1,1),col=c(rgb(0.8,0.8,0.8,0.5),rgb(0.1,0.1,0.1,0.5)),cex=.6,ncol = 2, lwd = 10)
dev.off()

