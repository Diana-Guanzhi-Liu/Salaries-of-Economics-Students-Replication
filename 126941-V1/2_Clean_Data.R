###
#  Load UCSC student and course data
###

load(paste0(secure,"UCSC_Digital_Data.Rda"))

###
#  Clean Student Data
###

S<-S[NA_to_F(S$UG)&S$Admit_Year%in%1986:2017&!is.na(S$SID)&NA_to_F(S$Major1!="Limited Status"),]
S$URM<-grepl("Alaska|Black|Chicano|Latino|Hispanic|Puerto",S$Ethnicity)*100
S$Female<-S$Sex=="F" ; S$Female[S$Gender=="U"]<-NA ; S$Gender<-S$Sex
for(m in 1:4) S[NA_to_F(S[,paste0("Major",m)]=="Information Systems Management"),paste0("Major",m)]<-"Technology&Info Management" #Major was renamed over time
S$Grad_Year<-as.integer(S$Grad_Year)

#Add NSC data
load(paste0(secure,"UCSC_NSC_Data.Rda"))
  S<-merge(S,NSC,all.x=T) ; rm(NSC)
  
#Add EDD data
load(paste0(secure,"UCSC_EDD_Data.Rda"))
  S<-merge(S,EDD,all.x=T) ; rm(EDD)

#Categorize majors
S$Count<-1
c<-aggregate(Count~Major1,S,sum)
names(c)[1]<-"Field"
c$Area<-"" ; c$GenArea<-""
for(i1 in 1:7){
  for(i2 in 1:length(Fields_all[[i1]])){
    for(i3 in 1:length(Fields_all[[i1]][[i2]])){
      check<-NA_to_F(tolower(Fields_all[[i1]][[i2]][[i3]])==tolower(c$Field))
      c$Area[check]<-Fields_all[[i1]][[i2]][[1]]
      c$GenArea[check]<-Fields_all[[i1]][[1]][[1]]
    }
  }
}
c<-c[,-2]
for(i in 1:4){
  names(c)<-paste0(c("Major","Area","GenArea"),i)
  S<-merge(S,c,all.x=T)
}

for(v in names(S)[grepl("wage",names(S))]){
  S[NA_to_F(S[,v]>quantile(S[,v],.98,na.rm=T)),v]<-quantile(S[,v],.98,na.rm=T)
  S[NA_to_F(S[,v]<quantile(S[,v],.02,na.rm=T)),v]<-quantile(S[,v],.02,na.rm=T)
  S[,paste0("log",v)]<-log(1+S[,v])
}
S$AllMajors<-apply(S[,c("Major1","Major2","Major3","Major4")],1,function(x){ #Combine majors into string
  x<-x[!is.na(x)&!duplicated(x)]
  return(paste(x[order(x)],collapse=","))
})
S$wage_sum_1314<-apply(S[,c("wage_sum_2013","wage_sum_2014")],1,mean,na.rm=T)
S$wage_sum_0910<-apply(S[,c("wage_sum_2009","wage_sum_2010")],1,mean,na.rm=T)
S$wage_sum_1718<-apply(S[,c("wage_sum_2017","wage_sum_2018")],1,mean,na.rm=T)
#Choose a random major
set.seed(1) ; S$Random<-runif(nrow(S)) ; S$Major_Random<-NA
  check<-is.na(S$Major2)|(is.na(S$Major3)&S$Random<.5)|(is.na(S$Major4)&S$Random<(1/3))|(!is.na(S$Major4)&S$Random<.25) ; S$Major_Random[check]<-S$Major1[check]
  check<-is.na(S$Major_Random)&(is.na(S$Major3)|(is.na(S$Major4)&inrange(S$Random,(1/3),(2/3)))|(!is.na(S$Major4)&inrange(S$Random,(1/4),(1/2)))) ; S$Major_Random[check]<-S$Major2[check]
  check<-is.na(S$Major_Random)&(is.na(S$Major4)|inrange(S$Random,(1/2),(3/4))) ; S$Major_Random[check]<-S$Major3[check]
  check<-is.na(S$Major_Random) ; S$Major_Random[check]<-S$Major4[check]
#Choose the larger of the two majors, by 2008-2012 earnings
t<-c() ; for(m in unique(S$Major1[S$Year_Applied%in%2008:2012&!is.na(S$Major1)])) t<-c(t,median(S$wage_sum_1718[S$Year_Applied%in%2008:2012&S$Major1==m&!(NA_to_F(S$Major2!=m)|NA_to_F(S$Major3!=m)|NA_to_F(S$Major4!=m))],na.rm=T)) ; t<-unique(S$Major1[S$Year_Applied%in%2008:2012&!is.na(S$Major1)])[order(t,decreasing = T)] #Order the majors by higher-earning, among single majors
  S$Major_Larger<-S$Major1 ; for(i in 1:(length(t)-1)) for(j in 2:4){ ; check<-NA_to_F(S[,paste0("Major",j)]==t[i]&S$Major1%in%t[(i+1):length(t)]) ; S$Major_Larger[check]<-S[check,paste0("Major",j)] ; }


#Add information about NAICS
#Add NAICS information
n<-read_excel("Data/Raw/NAICS_Codes.xlsx")
n<-n[-(1:2),2:3] ; names(n)<-c("NAICS","NAICS_Text") ; n$NAICS_Text<-gsub("T$","",n$NAICS_Text)
n$NAICS<-as.integer(n$NAICS) ; n<-n[!duplicated(n$NAICS)&!is.na(n$NAICS),]
S<-S[,!grepl("NAICS_Text",names(S))]
for(i in c(2017,2018)){
  S[,paste0("NAICS4_",i,"_2")]<-as.integer(floor(S[,paste0("NAICS4_",i)]/(10^(nchar(S[,paste0("NAICS4_",i)])-2))))
  S<-merge(S,n,by.x=paste0("NAICS4_",i,"_2"),by.y="NAICS",all.x=T)
  
  S$NAICS_Text[S[,paste0("NAICS4_",i,"_2")]%in%c(31,32,33)]<-"Manufacturing"
  S$NAICS_Text[S[,paste0("NAICS4_",i,"_2")]%in%c(44,45)]<-"Retail"
  S$NAICS_Text[S[,paste0("NAICS4_",i,"_2")]%in%c(48,49)]<-"Transportation and Shipping"
  
  #Edit NAICS codes
  S$NAICS_Text[S$NAICS_Text=="Elementary and Secondary Schools"]<-"K-12 Education"
  S$NAICS_Text[S$NAICS_Text%in%c("Colleges, Universities, and Professional Schools","Junior Colleges")]<-"Higher Education"
  S$NAICS_Text[S$NAICS_Text%in%c("Offices of Lawyers","All Other Legal Services")]<-"Law"
  S$NAICS_Text[S$NAICS_Text%in%c("Custom Computer Programming Services","Computer Systems Design Services","Other Computer Related Services","Computer Facilities Management Services","Data Processing, Hosting, and Related Services")]<-"Internet and Technology"
  S$NAICS_Text[S$NAICS_Text%in%c("Other Scientific and Technical Consulting Services","Environmental Consulting Services","Administrative Management and General Management Consulting Services","Other Management Consulting Services","Process, Physical Distribution, and Logistics Consulting Services")]<-"Business Services"
  S$NAICS_Text[S$NAICS_Text%in%c("Veterinary Services","Voluntary Health Organizations")]<-"Health Care"
  S$NAICS_Text[S$NAICS_Text%in%c("Temporary Help Services","Professional Employer Organizations","Human Resources Consulting Services","Employment Placement Agencies","Executive Search Services")]<-"Human Resources and Temporary Help"
  S$NAICS_Text[S$NAICS_Text%in%c("Advertising Agencies","Marketing Consulting Services","Public Relations Agencies","Marketing Research and Public Opinion Polling","Telemarketing Bureaus and Other Contact Centers","Media Representatives","Other Services Related to Advertising","Media Buying Agencies","Direct Mail Advertising","Outdoor Advertising","Commercial Photography","Advertising Material Distribution Services")]<-"Business Services"
  S$NAICS_Text[S$NAICS_Text%in%c("Grantmaking Foundations","Environment, Conservation and Wildlife Organizations","Civic and Social Organizations","Religious Organizations","Other Social Advocacy Organizations","Labor Unions and Similar Labor Organizations","Human Rights Organizations","Business Associations","Professional Organizations","Other Grantmaking and Giving Services","Other Similar Organizations (except Business, Professional, Labor, and Political Organizations)","Political Organizations")]<-"Community and Advocacy"
  S$NAICS_Text[S$NAICS_Text%in%c("Engineering Services")]<-"Manufacturing"
  
  S$NAICS_Text[S$NAICS_Text=="Educational Services"]<-"Education"
  S$NAICS_Text[S$NAICS_Text=="Professional, Scientific, and Technical Services"]<-"Business Services"
  S$NAICS_Text[S$NAICS_Text=="Administrative and Support and Waste Management and Remediation Services"]<-"Business Services"
  S$NAICS_Text[S$NAICS_Text=="Management of Companies and Enterprises"]<-"Business Services"
  S$NAICS_Text[S$NAICS_Text=="Other Services (except Public Administration)"]<-"Other Services"
  S$NAICS_Text[S$NAICS_Text=="Utilities"]<-"Other Services"
  S$NAICS_Text[S$NAICS_Text=="Health Care and Social Assistance"]<-"Health Care"
  S$NAICS_Text[S$NAICS_Text=="Information"]<-"Media and Film"
  S$NAICS_Text[S$NAICS_Text=="Agriculture, Forestry, Fishing and Hunting"]<-"Agriculture and Mining"
  S$NAICS_Text[S$NAICS_Text=="Mining, Quarrying, and Oil and Gas Extraction"]<-"Agriculture and Mining"
  S$NAICS_Text[S$NAICS_Text=="Wholesale Trade"]<-"Wholesale and Retail"
  S$NAICS_Text[S$NAICS_Text=="Retail"]<-"Wholesale and Retail"
  S$NAICS_Text[S$NAICS_Text=="Transportation and Shipping"]<-"Wholesale and Retail"
  
  names(S)[names(S)=="NAICS_Text"]<-paste0("NAICS_Text_",i)
}

#Pull in zipcode incomes
S$Zipcode<-gsub("\\s|[-].*$","",S$Zipcode) ; S$Zipcode[NA_to_F(S$Country!="United States")]<-NA
S$Zipcode<-substr(S$Zipcode,1,5)
zip<-data.frame()
#CPI-adjust to 2018
CPI<-read.csv("Data/Raw/CPI_California.csv")
for(i in 1991:2017){
  if(i%in%1988:2000) hi<-read.dta13("Data/Derived/Zipcode_Income/Zip_Code_1998.dta")
  if(i%in%2001:2003) hi<-read.dta13("Data/Derived/Zipcode_Income/Zip_Code_2001.dta")
  if(i%in%c(2004:2007,2009:2017)) hi<-read.dta13(paste0("Data/Derived/Zipcode_Income/Zip_Code_",i,".dta"))
  if(i%in%2008) hi<-read.dta13("Data/Derived/Zipcode_Income/Zip_Code_2007.dta")
  hi$mean_agi<-hi$mean_agi/(CPI$CPI[CPI$Year==hi$t[1]]/CPI$CPI[1])
  hi$mean_agi[NA_to_F(hi$mean_agi>quantile(hi$mean_agi,.98,na.rm=T))]<-quantile(hi$mean_agi,.98,na.rm=T)
  hi$mean_agi[NA_to_F(hi$mean_agi<quantile(hi$mean_agi,.02,na.rm=T))]<-quantile(hi$mean_agi,.02,na.rm=T)
  
  hi$Admit_Year<-i
  hi$Zipcode<-hi$zipcode
  zip<-rbind(zip,hi[,c("Zipcode","Admit_Year","mean_agi")])
}
zip<-zip[!duplicated(zip[,c("Zipcode","Admit_Year")]),]
zip$Zipcode<-as.character(zip$Zipcode)
S<-merge(S,zip,all.x=T)
S$mean_agi[NA_to_F(S$mean_agi<0)]<-0 ; S$logmean_agi<-log(1+S$mean_agi)

S$Asian<-(S$Eth%in%c("Chinese/Chinese-American","East Indian/Pakistani","Filipino","Japanese/Japanese-American","Korean","Other Asian","Vietnamese"))*100
S$Hispanic<-(S$Eth%in%c("Hispanic or Latino"))*100
S$Black<-(S$Eth%in%c("African American"))*100
S$White<-(S$Eth=="White")*100
S$Female<-S$Female*100
S$Residency<-S$Residency*100
S$International<-(S$PrevSchCat%in%c("Foreign Institution"))*100
S$logmean_agi<-log(1+S$mean_agi)
for(y in c(2017,2018)){
  S[,paste0("Ind2_",y)]<-substr(S[,paste0("NAICS4_",y)],1,2)
  S[NA_to_F(grepl("^531",S[,paste0("NAICS4_",y)])),paste0("Ind2_",y)]<-"52" #Combine FIRE, but not "Rental/Leasing"
  S[NA_to_F(S[,paste0("Ind2_",y)]%in%c("32","33")),paste0("Ind2_",y)]<-"31"
  S[NA_to_F(S[,paste0("Ind2_",y)]%in%c("45")),paste0("Ind2_",y)]<-"44"
  S[NA_to_F(S[,paste0("Ind2_",y)]%in%c("49")),paste0("Ind2_",y)]<-"48"
}

###
#  Clean course data
###

C<-merge(C,S[,c("SID","Year_Applied")])
C$Year<-as.integer(C$Year)

#Categorize majors into disciplines
C$Count<-1
c<-aggregate(Count~Department,C[C$SID%in%S$SID,],sum)
c$School<-"UCSC"
c$Dept<-apply(c[,c("Department","School")],1,major_clean,single=T,norelax=T)
c$Area<-"" ; c$GenArea<-""
for(i1 in 1:7){
  for(i2 in 1:length(Fields_all[[i1]])){
    for(i3 in 1:length(Fields_all[[i1]][[i2]])){
      check<-NA_to_F(tolower(Fields_all[[i1]][[i2]][[i3]])==tolower(c$Dept))
      c$Area[check]<-Fields_all[[i1]][[i2]][[1]]
      c$GenArea[check]<-Fields_all[[i1]][[1]][[1]]
    }
  }
}
C<-merge(C,c[,c("Department","Dept","Area","GenArea")],all.x=T)

#Grades->GPAs
C$Grade<-gsub("GC|Z","-",C$Grade)
C$Grade[C$Grade%in%c("H","HH","HP","S","IP","PC","WS")]<-"P"
C$Grade[C$Grade%in%c("N","NC","NR","NP","NS","W","WNC","WW","I","U","-","","AC")]<-"NP"
gpa<-0 ; gpa[grepl("A",C$Grade)]<-4 ; gpa[grepl("B",C$Grade)]<-3 ; gpa[grepl("C",C$Grade)]<-2 ; gpa[grepl("D",C$Grade)]<-1 ; gpa[grepl("[+]",C$Grade)]<-gpa[grepl("[+]",C$Grade)]+0.3 ; gpa[grepl("[-]",C$Grade)]<-gpa[grepl("[-]",C$Grade)]-0.3
C$GPA<-gpa ; C$GPA[C$Grade%in%c("NP","P")]<-NA ; C$GPA[C$Grade=="F"]<-0
C$GPA[NA_to_F(C$GPA==4.3)]<-4

cm<-aggregate(GPA~Year+Term+Department+Course_Number,C,mean,na.rm=T) ; names(cm)[5]<-"GPA_mean" ; C<-merge(C,cm,all.x=T)
cm<-aggregate(GPA~Year+Term+Department+Course_Number,C,sd,na.rm=T) ; names(cm)[5]<-"GPA_sd" ; C<-merge(C,cm,all.x=T)
C$GPA_normed<-(C$GPA-C$GPA_mean)/C$GPA_sd
C<-C[C$SID%in%S$SID,]

C$t<-C$Year+.25*(C$Term=="Spring")+.5*(C$Term=="Summer")+.75*(C$Term=="Fall")
class<-aggregate(Count~Department+Course_Number+Year+Term,C,sum) ; names(class)[5]<-"Class_Size" ; C<-merge(C,class) ; gc()
#Course-corrected GPAs
C$Course_ID<-paste(C$Department,C$Course_Number,C$Year,C$Term)
Sys.time() ; gpa<-felm(GPA~1|SID+Course_ID,C[C$Year%in%2000:2019,]) ; Sys.time()
Sys.time() ; dept<-getfe(gpa) ; Sys.time()
dept$Course_ID<-row.names(dept) ; dept<-dept[grepl("Course_ID",dept$Course_ID),] ; dept$Course_ID<-gsub("Course_ID[.]","",dept$Course_ID) ; dept$Course_FE<-dept$effect ; C<-merge(C,dept[,c("Course_ID","Course_FE")],all.x=T) ; rm(gpa) ; gc()
C$GPA_FE<-C$GPA-C$Course_FE

C$Course_Dept<-C$Department ; C$Department<-C$Dept

#Add some course counts to S: how many economics courses and upper-div economics courses
C$temp<-C$Department=="Economics"
  temp<-aggregate(temp~SID,C,sum)
  names(temp)[2]<-"C_Economics"
  S<-merge(S,temp,all.x=T)
  S$C_Economics[is.na(S$C_Economics)]<-0
  S$C_Economics[S$C_Economics>25]<-25
cc<-C[C$temp,]
temp<-aggregate(temp~SID,cc[as.integer(gsub("[^0-9]","",cc$Course_Number))>99,],sum)
  names(temp)[2]<-"C_Economics_Upper"
  S<-merge(S,temp,all.x=T)
  S$C_Economics_Upper[is.na(S$C_Economics_Upper)]<-0
  S$C_Economics_Upper[S$C_Economics_Upper>21]<-21
  
  
###
#  Add in SERU survey information
###
ids<-read.csv(paste0(secure,"SERU_ID_UCSC.csv"))
  S<-merge(S,ids,all.x=T)
A<-data.frame(read_csv(paste0(secure,"SERU_Data_UCSC.csv")),stringsAsFactors = F)
  A<-A[A$CAMPUS=="SC",c("YEAR","SERU_ID","RUCTIMESTUDY","RUCASPIRJOB")] ; gc()
U<-data.frame() ; for(y in 2006:2016) U<-rbind(U,A[A$YEAR==y&A$SERU_ID%in%S$SERU_ID[S$Year_Applied%in%c(y-4,y-3)],])
S<-merge(S,U,all.x=T)

###
#  Construct the estimation data
###

c<-C[C$Department=="Economics"&C$Course_Number%in%c("1","2")&C$Year>1999&!C$Grade%in%c("P","NP"),]
c<-c[order(c$SID,c$Course_Number,c$t),] ; c<-c[!duplicated(c[,c("SID","Course_Number")]),]
c<-c[c$SID%in%c$SID[c$Course_Number=="1"]&c$SID%in%c$SID[c$Course_Number=="2"],]
c$GPA_Econ1[c$Course_Number=="1"]<-c$GPA[c$Course_Number=="1"] ; c$GPA_Econ2[c$Course_Number=="2"]<-c$GPA[c$Course_Number=="2"]
econ<-c("Economics","Global Economics","Business Mgmt Economics") ; S$Econ<-S$Major1%in%econ|S$Major2%in%econ|S$Major3%in%econ|S$Major4%in%econ

c<-merge(merge(aggregate(GPA~SID,c,mean),c[c$Course_Number==1,c("SID","GPA_Econ1")]),c[c$Course_Number==2,c("SID","GPA_Econ2")])
c<-merge(c,S[,c("SID","SERU_ID","Admit_Year","Female","Gender","Eth","HS_ATP","PrevSchCat","Econ","Major1","Major2","Major3","Major4","Major_Random","Major_Larger","wage_sum_2018","logwage_sum_2018","wage_sum_2017","logwage_sum_2017","wage_sum_2012","wage_sum_2013","wage_sum_2011","wage_sum_2015","wage_sum_2016","wage_sum_2014","wage_sum_2008","wage_sum_2009","wage_sum_2010","Grad_Year","GenArea1","GenArea2","GenArea3","GenArea4","AllMajors","NAICS_Text_2017","NAICS_Text_2018","NAICS4_2009","NAICS4_2010","NAICS4_2013","NAICS4_2014","NAICS4_2017","NAICS4_2018","Adm_Cat","SAT","Year_Applied","mean_agi","BYear","Hispanic","Asian","Black","White","URM","Residency","International","logmean_agi","Ind2_2017","Ind2_2018",names(S)[grepl("NSC",names(S))])])
c<-c[NA_to_F(c$Adm_Cat=="Freshman"),] #GPA cutoff doesn't apply to transfer students


c$Economics<-c$Econ*100
c$Count<-1
c$RunVar<-c$GPA-2.75
c$Above<-(c$GPA>2.75)*1
for(i in 1:2){
  c[,paste0("GPA_Abo",i)]<-c$RunVar^i*c$Above
  c[,paste0("GPA_Bel",i)]<-c$RunVar^i*(1-c$Above)
}
c$Eth_Cat[c$Eth%in%c("P")]<-"White"
c$Eth_Cat[c$Eth%in%c("B")]<-"Black"
c$Eth_Cat[c$Eth%in%c("A","K","M")]<-"Other"
c$Eth_Cat[c$Eth%in%c("C","J")]<-"Hispanic"
c$Eth_Cat[c$Eth%in%c("D","F","G","H","L","N","V")]<-"Asian"
c$Eth_Cat[c$Eth%in%c("E","")]<-"Decline"

c$logwage_sum_1718<-apply(c[,c("logwage_sum_2018","logwage_sum_2017")],1,mean,na.rm=T)
c$Employ_2018<-(!is.na(c$wage_sum_2018))*100
c$Employ_2017<-(!is.na(c$wage_sum_2017))*100
c$wage_sum_1718<-apply(c[,c("wage_sum_2018","wage_sum_2017")],1,mean,na.rm=T)
e<-c$Year_Applied%in%2008:2010 ; c$wage_sum_1718_Early[e]<-c$wage_sum_1718[e] ; c$logwage_sum_1718_Early[e]<-c$logwage_sum_1718[e]
l<-c$Year_Applied%in%2010:2012 ; c$wage_sum_1718_Late[l]<-c$wage_sum_1718[l] ; c$logwage_sum_1718_Late[l]<-c$logwage_sum_1718[l]
c$wage_sum_1718_URM[c$URM==100]<-c$wage_sum_1718[c$URM==100]
c$wage_sum_1718_NonURM[c$URM==0]<-c$wage_sum_1718[c$URM==0]
c$wage_sum_1213<-apply(c[,c("wage_sum_2013","wage_sum_2012")],1,mean,na.rm=T)
c$wage_sum_1314<-apply(c[,c("wage_sum_2013","wage_sum_2012")],1,mean,na.rm=T)
c$wage_sum_0910<-apply(c[,c("wage_sum_2009","wage_sum_2010")],1,mean,na.rm=T)
c$wage_sum_0809<-apply(c[,c("wage_sum_2008","wage_sum_2009")],1,mean,na.rm=T)
c$Employ_1718<-(!is.na(c$wage_sum_1718))*100
c$NAICS_Text_1718<-c$NAICS_Text_2018 ; c$NAICS_Text_1718[is.na(c$NAICS_Text_1718)]<-c$NAICS_Text_2017[is.na(c$NAICS_Text_1718)]
c$NAICS4_1718<-c$NAICS4_2018 ; c$NAICS4_1718[is.na(c$NAICS4_1718)]<-c$NAICS4_2017[is.na(c$NAICS4_1718)]
check<-c$NSC_Grad_Degree%in%c("AA","MA") ; for(v in names(c)[grepl("NSC_Grad_",names(c))]) c[check,v]<-NA
c$Grad<-(!is.na(c$Grad_Year))*100
c$Grad_IncNSC<-c$Grad ; c$Grad_IncNSC[!is.na(c$NSC_Grad_Year)]<-100
c$GradSch<-c$NSC_GradSchool7*100
temp<-aggregate(Year~SID,C[C$Level!="G"&C$SID%in%c$SID,],max) ; names(temp)[2]<-"MaxYear" ; c<-merge(c,temp)
a<-c("541211") ; c$Accountant<-(NA_to_F(c$NAICS4_2017%in%a)|NA_to_F(c$NAICS4_2018%in%a))*100 ; c$Accountant[is.na(c$wage_sum_1718)]<-NA 
for(v in unique(c$NAICS_Text_2017)[!is.na(unique(c$NAICS_Text_2017))]){
  c[,gsub("[ ,]","",v)]<-(NA_to_F(c$NAICS_Text_2017==v)|NA_to_F(c$NAICS_Text_2018==v))*100
  c[is.na(c$NAICS_Text_2017)&is.na(c$NAICS_Text_2018),gsub("[ ,]","",v)]<-NA
}
c$Industry<-as.character(c$NAICS4_1718) ; c$Industry[NA_to_F(nchar(c$Industry)<6)]<-paste0("0",c$Industry[NA_to_F(nchar(c$Industry)<6)]) ; c$Industry<-substr(c$Industry,1,4)
temp<-c$Industry
for(v in 3:2){
  check<-!c$Industry%in%names(table(c$Industry[c$Year_Applied%in%2008:2012])[table(c$Industry[c$Year_Applied%in%2008:2012])>100])
  c$Industry[check]<-substr(temp[check],1,v)
}
c$Industry[c$Industry%in%c(32,33)]<-31 #Combine manufacturing.
c$Industry[c$Industry%in%c(49)]<-48 #Combine transportation/warehousing
c$Industry[c$Industry%in%c(6111)]<-611
for(v in as.integer(names(table(c$Industry)[table(c$Industry)>100]))) c[,paste0("Ind",v)]<-(c$Industry==v)*100
c$FIRE<-(c$Ind2_2017%in%c(52)|c$Ind2_2018%in%c(52))*100
c$Healthcare<-(c$Ind2_2017%in%c(62)|c$Ind2_2018%in%c(62))*100
c$Education<-(c$Ind2_2017%in%c(61)|c$Ind2_2018%in%c(61))*100
c$FIREAcc<-(NA_to_F(c$FIRE==100)|NA_to_F(c$Accountant==100))*100
c$EdHealth<-(NA_to_F(c$Healthcare==100)|NA_to_F(c$Education==100))*100

for(v in c("FIRE","Healthcare","Education","FIREAcc","EdHealth")){
  c[is.na(c$Ind2_2017)&is.na(c$Ind2_2018),v]<-NA
}

#Reorganize some majors into the correct UCSC discipline
for(i in 1:4){
  c[c[,paste0("Major",i)]%in%"Community Studies",paste0("GenArea",i)]<-"Social Sciences"
  c[grepl("Economics",c[,paste0("Major",i)]),paste0("GenArea",i)]<-"Economics"
  c[c[,paste0("Major",i)]%in%c("Plant Sciences","Health Sciences"),paste0("GenArea",i)]<-"Natural Sciences"
  c[c[,paste0("Major",i)]%in%c("Computer & Info Science","Network & Digital Technology"),paste0("GenArea",i)]<-"Engineering"
  c[c[,paste0("Major",i)]%in%"Network & Digital Technology",paste0("Major",i)]<-"Technology&Info Management"
}
c$Humanities<-(c$GenArea1%in%"Humanities"|c$GenArea2%in%"Humanities"|c$GenArea3%in%"Humanities"|c$GenArea4%in%"Humanities")*100
c$SocSci<-(c$GenArea1%in%"Social Sciences"|c$GenArea2%in%"Social Sciences"|c$GenArea3%in%"Social Sciences"|c$GenArea4%in%"Social Sciences")*100
c$NatSci<-(c$GenArea1%in%"Natural Sciences"|c$GenArea2%in%"Natural Sciences"|c$GenArea3%in%"Natural Sciences"|c$GenArea4%in%"Natural Sciences")*100
c$Engineering<-(c$GenArea1%in%"Engineering"|c$GenArea2%in%"Engineering"|c$GenArea3%in%"Engineering"|c$GenArea4%in%"Engineering")*100
econ<-c("Business Mgmt Economics") ; c$BusEcon<-(c$Major1%in%econ|c$Major2%in%econ|c$Major3%in%econ|c$Major4%in%econ)*100
c$ShareBusEcon<-c$BusEcon ; c$ShareBusEcon[c$Economics==0]<-NA

#Number of courses by discipline
CC<-C[C$SID%in%c$SID,]
CC$GenArea[CC$Department%in%c("Mathematics","Applied Mathematics and Statistics")]<-"Mathematics"
CC$GenArea[CC$Department=="Economics"]<-"Economics"
for(g in c("Humanities","Natural Sciences","Social Sciences","Engineering","Mathematics","Economics")){
  CC$temp<-CC$GenArea==g
  temp<-aggregate(temp~SID,CC,sum)
  names(temp)[2]<-paste0("C_",gsub(" ","",g))
  c<-merge(c,temp,all.x=T)
}
for(v in names(c)[grepl("^C_",names(c))]) c[,v]<-winsor(c[,v],.02)
#Now break up the Econ courses
CC$EconType[CC$Department=="Economics"&(CC$Course_Number%in%c("106","130","159","160A","160B","195","199","199F","105","107","114","114L","120","121","125","126","128","130","137","140","141","142","143","148","149","150","159","160A","160B","165","166A","166B","169","170","171","175","180","183","184","189","190","1","2","100A","100B","113","197","104","11A","11B","80A","200","100M","100N","294A","294B","217","291","185","20","157","186","153"))]<-"Econ"
CC$EconType[CC$Department=="Economics"&(CC$Course_Number%in%c("108","109","110","111A","111B","111C","112","115","116","117","117A","117B","118","119","131","136","136L","138","139A","139B","161","161A","161B","162","164","188","194","101","133","135","10A","10B","80H","80G","30","233","194B","236","235","194F","181","234"))]<-"Bus"
CC$QuantMethod<-CC$Department=="Applied Mathematics and Statistics"|paste(CC$Department,"-",CC$Course_Name)%in%c("Economics - Math Methd For Econ","Economics - Math Methd for Econ","Psychology - Research Methods","Economics - Math Methds Econ II","Sociology - Statistical Methods","Economics - Adv Quant Methods","Engineering - Math Methd For Econ","Political Science - Research Methods","Psychology - Adv Reserch Methods","Economics - AdvQuantMethodLab","Sociology - Information Methods","Economics - Math Econ Methods","Political Science - Research Methods Seminar","Economics - Numbr Truth: Statis","Psychology - Intro Psyc Stats","Engineering - Statistics","Computer Engineering - Probability/Stats","Mathematics - Elem Basic Statist","Engineering - Manageral Statistic","Engineering - Intro Biostatistics","Engineering - Biostatistics","Mathematics - Intro Biostatistics","Engineering - Prob & Statistics","Economics - Intro Econometrics","Economics - Adv Econometrics","Economics - AdvEconometricsIII","Economics - AdvEconometrics II","Economics - Econometrics")
for(v in c("Econ","Bus")){
  CC$temp<-NA_to_F(CC$EconType==v)
  temp<-aggregate(temp~SID,CC,sum)
  names(temp)[2]<-paste0("C_EconType_",v)
  c<-merge(c,temp,all.x=T)
}
temp<-aggregate(QuantMethod~SID,CC,sum)
names(temp)[2]<-"C_QuantMethod"
c<-merge(c,temp,all.x=T) ; c$QuantMethod<-winsor(c$C_QuantMethod,0.02)

#Average class size
class<-aggregate(Class_Size~SID,C[C$SID%in%c$SID,],median) ; names(class)[2]<-"Class_Size_Median" ; c<-merge(c,class)

gpa<-ddply(C[C$SID%in%c$SID,],.(SID),function(x) weighted.mean(x$GPA,x$Units,na.rm=T)) ; names(gpa)[2]<-"GPA_Overall" ; c<-merge(c,gpa)
gpa<-ddply(C[C$SID%in%c$SID,],.(SID),function(x) weighted.mean(x$GPA_normed,x$Units,na.rm=T)) ; names(gpa)[2]<-"GPA_Overall_Rel" ; c<-merge(c,gpa)
gpa<-ddply(C[C$SID%in%c$SID,],.(SID),function(x) weighted.mean(x$GPA_FE,x$Units,na.rm=T)) ; names(gpa)[2]<-"GPA_Overall_FE" ; c<-merge(c,gpa)

#Merge in major averages
temp<-aggregate(wage_sum_1718~AllMajors,S[S$Year_Applied%in%2008:2012&!is.na(S$Major1)&S$AllMajors%in%names(table(S$AllMajors)[table(S$AllMajors)>9]),],median,na.rm=T) ; names(temp)[2]<-"Major_Median1718" ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_1314~AllMajors,S[S$Year_Applied%in%2004:2008&!is.na(S$Major1)&S$AllMajors%in%names(table(S$AllMajors)[table(S$AllMajors)>9]),],median,na.rm=T) ; names(temp)[2]<-"Major_Median1314" ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_0910~AllMajors,S[S$Year_Applied%in%2000:2004&!is.na(S$Major1)&S$AllMajors%in%names(table(S$AllMajors)[table(S$AllMajors)>9]),],median,na.rm=T) ; names(temp)[2]<-"Major_Median0910" ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_1718~AllMajors,c[c$Year_Applied%in%2008:2012&!is.na(c$Major1)&c$AllMajors%in%names(table(c$AllMajors)[table(c$AllMajors)>9]),],median,na.rm=T) ; names(temp)[2]<-"Major_MedianEconOnly1718" ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_1314~AllMajors,c[c$Year_Applied%in%2004:2008&!is.na(c$Major1)&c$AllMajors%in%names(table(c$AllMajors)[table(c$AllMajors)>9]),],median,na.rm=T) ; names(temp)[2]<-"Major_MedianEconOnly1314" ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_0910~AllMajors,c[c$Year_Applied%in%2000:2004&!is.na(c$Major1)&c$AllMajors%in%names(table(c$AllMajors)[table(c$AllMajors)>9]),],median,na.rm=T) ; names(temp)[2]<-"Major_MedianEconOnly0910" ; c<-merge(c,temp,all.x=T)
temp<-read.csv("Data/Raw/UCSC_Majors_to_ACS.csv")[,c(1,3,4)] ; for(v in 1:2){
  names(temp)<-c(paste0("Major",v),paste0("ACSMajor",v),paste0("ACSMajor_NotCA",v))
  c<-merge(c,temp,all.x=T)
}
c$AllMajors_ACS<-apply(c[,c("ACSMajor1","ACSMajor2")],1,function(x){
  x<-x[!is.na(x)&!duplicated(x)]
  return(paste(x[order(x)],collapse=","))
})
c$AllMajors_ACS_NotCA<-apply(c[,c("ACSMajor_NotCA1","ACSMajor_NotCA2")],1,function(x){
  x<-x[!is.na(x)&!duplicated(x)]
  return(paste(x[order(x)],collapse=","))
})
load("Data/Derived/ACS_Majors_INCWAGE.Rda")
for(y in c(2010,2014,2018)){
  temp<-acsmaj[acsmaj$Y==y,c(1,3)]
  names(temp)<-c("AllMajors_ACS",paste0("Major_ACSMedian",y))
  c<-merge(c,temp,all.x=T)
  temp<-acsmaj_CA[acsmaj_CA$Y==y,c(1,3)]
  names(temp)<-c("AllMajors_ACS",paste0("Major_ACSMedian_CA",y))
  c<-merge(c,temp,all.x=T)
  temp<-acsmaj_NotCA[acsmaj_NotCA$Y==y,c(1,3)]
  names(temp)<-c("AllMajors_ACS",paste0("Major_ACSMedian_NotCA_OrigMaj",y))
  c<-merge(c,temp,all.x=T)
  names(temp)<-c("AllMajors_ACS_NotCA",paste0("Major_ACSMedian_NotCA",y))
  c<-merge(c,temp,all.x=T)
}
load("Data/Derived/ACS_Industries_INCWAGE.Rda")
#Wrap up NAICS to ACS industries
c$NAICS4_1718_ACS<-c$NAICS4_1718
for(v in 1:5){
  check<-!c$NAICS4_1718_ACS%in%acsind$INDNAICS
  c$NAICS4_1718_ACS[check]<-gsub(".$","",c$NAICS4_1718_ACS[check]) #Mostly 4-digit, then 3. Some 2 and 5.
}
for(y in c(2010,2014,2017)){
  temp<-acsind[acsind$Y==y,c(1,3)]
  names(temp)<-c("NAICS4_1718_ACS",paste0("Ind_ACSMedian",y))
  c<-merge(c,temp,all.x=T)
  temp<-acsind_CA[acsind_CA$Y==y,c(1,3)]
  names(temp)<-c("NAICS4_1718_ACS",paste0("Ind_ACSMedian_CA",y))
  c<-merge(c,temp,all.x=T)
  temp<-acsind_NotCA[acsind_NotCA$Y==y,c(1,3)]
  names(temp)<-c("NAICS4_1718_ACS",paste0("Ind_ACSMedian_NotCA",y))
  c<-merge(c,temp,all.x=T)
}

#Same thing by industry, for decomposition
S$NAICS4_1718<-S$NAICS4_2018 ; S$NAICS4_1718[is.na(S$NAICS4_1718)]<-S$NAICS4_2017[is.na(S$NAICS4_1718)]
S$NAICS4_1314<-S$NAICS4_2014 ; S$NAICS4_1314[is.na(S$NAICS4_1314)]<-S$NAICS4_2013[is.na(S$NAICS4_1314)]
S$NAICS4_0910<-S$NAICS4_2010 ; S$NAICS4_0910[is.na(S$NAICS4_0910)]<-S$NAICS4_2009[is.na(S$NAICS4_0910)]
temp<-aggregate(wage_sum_1718~NAICS4_1718,S[S$Year_Applied%in%2008:2012&!is.na(S$NAICS4_1718)&S$NAICS4_1718%in%names(table(S$NAICS4_1718)[table(S$NAICS4_1718)>9]),],median,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718","Industry_Median1718") ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_1314~NAICS4_1314,S[S$Year_Applied%in%2004:2008&!is.na(S$NAICS4_1314)&S$NAICS4_1314%in%names(table(S$NAICS4_1314)[table(S$NAICS4_1314)>9]),],median,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718","Industry_Median1314") ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_0910~NAICS4_0910,S[S$Year_Applied%in%2000:2004&!is.na(S$NAICS4_0910)&S$NAICS4_0910%in%names(table(S$NAICS4_0910)[table(S$NAICS4_0910)>9]),],median,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718","Industry_Median0910") ; c<-merge(c,temp,all.x=T)
c$NAICS4_1314<-c$NAICS4_2014 ; c$NAICS4_1314[is.na(c$NAICS4_1314)]<-c$NAICS4_2013[is.na(c$NAICS4_1314)]
c$NAICS4_0910<-c$NAICS4_2010 ; c$NAICS4_0910[is.na(c$NAICS4_0910)]<-c$NAICS4_2009[is.na(c$NAICS4_0910)]
temp<-aggregate(wage_sum_1718~NAICS4_1718,c[c$Year_Applied%in%2008:2012&!is.na(c$NAICS4_1718)&c$NAICS4_1718%in%names(table(c$NAICS4_1718)[table(c$NAICS4_1718)>9]),],median,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718","Industry_MedianEconOnly1718") ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_1314~NAICS4_1314,c[c$Year_Applied%in%2004:2008&!is.na(c$NAICS4_1314)&c$NAICS4_1314%in%names(table(c$NAICS4_1314)[table(c$NAICS4_1314)>9]),],median,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718","Industry_MedianEconOnly1314") ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_0910~NAICS4_0910,c[c$Year_Applied%in%2000:2004&!is.na(c$NAICS4_0910)&c$NAICS4_0910%in%names(table(c$NAICS4_0910)[table(c$NAICS4_0910)>9]),],median,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718","Industry_MedianEconOnly0910") ; c<-merge(c,temp,all.x=T)
#Now add means as well
temp<-aggregate(wage_sum_1718~NAICS4_1718,S[S$Year_Applied%in%2008:2012&!is.na(S$NAICS4_1718)&S$NAICS4_1718%in%names(table(S$NAICS4_1718)[table(S$NAICS4_1718)>9]),],mean,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718","Industry_Mean1718") ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_1718~NAICS4_1718,c[c$Year_Applied%in%2008:2012&!is.na(c$NAICS4_1718)&c$NAICS4_1718%in%names(table(c$NAICS4_1718)[table(c$NAICS4_1718)>9]),],mean,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718","Industry_MeanEconOnly1718") ; c<-merge(c,temp,all.x=T)
#And now do this for just FIRE and Accounting
S$NAICS4_1718_FireAcc<-0 ; S$NAICS4_1718_FireAcc[S$Ind2_2017%in%c(52)|S$Ind2_2018%in%c(52)]<-1 ; c$NAICS4_1718_FireAcc[c$NAICS4_2017%in%c("541211")|c$NAICS4_2018%in%c("541211")]<-2 ; c$NAICS4_1718_FireAcc[is.na(c$wage_sum_1718)]<-NA
c$NAICS4_1718_FireAcc<-0 ; c$NAICS4_1718_FireAcc[c$Ind2_2017%in%c(52)|c$Ind2_2018%in%c(52)]<-1 ; c$NAICS4_1718_FireAcc[c$NAICS4_2017%in%c("541211")|c$NAICS4_2018%in%c("541211")]<-2 ; c$NAICS4_1718_FireAcc[is.na(c$wage_sum_1718)]<-NA
temp<-aggregate(wage_sum_1718~NAICS4_1718_FireAcc,S[S$Year_Applied%in%2008:2012&!is.na(S$NAICS4_1718_FireAcc)&S$NAICS4_1718_FireAcc%in%names(table(S$NAICS4_1718_FireAcc)[table(S$NAICS4_1718_FireAcc)>9]),],mean,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718_FireAcc","IndustryFIREAcc_Mean1718") ; c<-merge(c,temp,all.x=T)
temp<-aggregate(wage_sum_1718~NAICS4_1718_FireAcc,c[c$Year_Applied%in%2008:2012&!is.na(c$NAICS4_1718_FireAcc)&c$NAICS4_1718_FireAcc%in%names(table(c$NAICS4_1718_FireAcc)[table(c$NAICS4_1718_FireAcc)>9]),],mean,na.rm=T) ; names(temp)[1:2]<-c("NAICS4_1718_FireAcc","IndustryFIREAcc_MeanEconOnly1718") ; c<-merge(c,temp,all.x=T)

#Control variables summary
hi<-lm(wage_sum_1718~Female*factor(Eth)+Residency+International+poly(SAT,3)+poly(mean_agi,3),S[!S$SID%in%c$SID&!is.na(S$SAT)&S$Year_Applied%in%2008:2012&!is.na(S$mean_agi),])
c$Predicted_Wage[c$Year_Applied%in%2008:2012]<-predict(hi,c[c$Year_Applied%in%2008:2012,])
c$Predicted_Wage_Employed<-c$Predicted_Wage ; c$Predicted_Wage_Employed[c$Employ_1718==0]<-NA

#Pull in additional SERU data
U<-data.frame() ; for(y in 2006:2016) U<-rbind(U,A[A$YEAR==y&A$SERU_ID%in%c$SERU_ID[c$Year_Applied%in%c(y-4,y-3)],])
Uf<-data.frame() ; for(y in 2006:2016) Uf<-rbind(Uf,A[A$YEAR==y&A$SERU_ID%in%c$SERU_ID[c$Year_Applied%in%c(y-1)],])
Ufs<-data.frame() ; for(y in 2006:2016) Ufs<-rbind(Ufs,A[A$YEAR==y&A$SERU_ID%in%c$SERU_ID[c$Year_Applied%in%c(y-1,y-2)],])
Usj<-data.frame() ; for(y in 2006:2016) Usj<-rbind(Usj,A[A$YEAR==y&A$SERU_ID%in%c$SERU_ID[c$Year_Applied%in%c(y-3,y-2)],])
c<-merge(c,U,all.x=T)
names(Uf)[-2]<-paste0(names(Uf)[-2],"_Fr") ; c<-merge(c,Uf,all.x=T)
names(Ufs)[-2]<-paste0(names(Ufs)[-2],"_FS") ; c<-merge(c,Ufs,all.x=T)
names(Usj)[-2]<-paste0(names(Usj)[-2],"_SJ") ; c<-merge(c,Usj,all.x=T)

c$Intend_Bus_SJ<-(c$RUCASPIRJOB_SJ==3)*100
  c$Intend_Bus_SJ_Outliers<-c$Intend_Bus_SJ ; c$Intend_Bus_SJ_Outliers[c$Year_Applied==2012&c$GPA==2.7]<-NA
  c$Intend_Bus_SJ_No2012<-c$Intend_Bus_SJ ; c$Intend_Bus_SJ_No2012[c$Year_Applied==2012]<-NA
  c$Intend_Bus_Fr<-(c$RUCASPIRJOB_Fr==3)*100
c$UCUES<-(!is.na(c$YEAR))*100 ; c$UCUES_Fr<-(!is.na(c$YEAR_Fr))*100 ; c$UCUES_SJ<-(!is.na(c$YEAR_SJ))*100
hrs<-c(0,3,8,13,18,23,28,35)
for(i in 1:8) c$Hours_Study[NA_to_F(c$RUCTIMESTUDY==i)]<-hrs[i]
for(i in 1:8) c$Hours_Study_SJ[NA_to_F(c$RUCTIMESTUDY_SJ==i)]<-hrs[i]
c$Predicted_Wage_UCUES<-c$Predicted_Wage ; c$Predicted_Wage_UCUES[c$UCUES==0]<-NA
c$Predicted_Wage_UCUES_SJ<-c$Predicted_Wage ; c$Predicted_Wage_UCUES_SJ[c$UCUES_SJ==0]<-NA
c$Predicted_Wage_UCUES_Fr<-c$Predicted_Wage ; c$Predicted_Wage_UCUES_Fr[c$UCUES_Fr==0]<-NA



###
#  Produce Figure A-2, Chart summarizing growth in Econ at UCSC, before dropping C
###
econ<-c("Economics","Global Economics","Business Mgmt Economics")
S$Economics<-(S$Major1%in%econ|S$Major2%in%econ|S$Major3%in%econ|S$Major4%in%econ)
C$Time<-C$Year-C$Year_Applied+.25*(C$Term=="Spring")+.5*(C$Term=="Summer")+.75*(C$Term=="Fall") 
S$Econ1<-S$SID%in%C$SID[C$Department=="Economics"&C$Course_Number=="1"&(C$Time<=2)]
S$Econ2<-S$SID%in%C$SID[C$Department=="Economics"&C$Course_Number=="2"&(C$Time<=2)]
S$Count<-1
hi<-aggregate(.~Year_Applied,S[NA_to_F(S$Adm_Cat=="Freshman")&S$Year_Applied>1994,c("Year_Applied","Economics","Econ1","Econ2","Count")],sum)
for(v in c("Economics","Econ1","Econ2")) hi[,paste0(v,"_P")]<-(hi[,v]/hi$Count)*100
png(paste0("Figures/EconMajorTrend.png"),width=500,height=500)
  plot(hi$Year_Applied,hi$Economics_P,ylim=c(0,50),xlim=c(1995,2016),type="l",xlab="Cohort Start Year",ylab="Percent of UCSC Students",lwd=2,cex.axis=2,cex.lab=1.5)
  polygon(c(2007.5,2007.5,2012.5,2012.5),c(-100,100,100,-100),col='gray80',lty = 0)
  polygon(c(2007.5,2007.5,2002.5,2002.5),c(-100,100,100,-100),col='gray90',lty = 0)
  polygon(c(2018.5,2018.5,2012.5,2012.5),c(-100,100,100,-100),col='gray90',lty = 0)
  par(new = T)
  plot(hi$Year_Applied,hi$Economics_P,ylim=c(0,50),xlim=c(1995,2016),type="l",xlab="Cohort Start Year",ylab="Percent of UCSC Students",lwd=2,cex.axis=2,cex.lab=1.5)
  lines(hi$Year_Applied,hi$Econ1_P,lty=2)
  lines(hi$Year_Applied,hi$Econ2_P)
  legend(1995,50,legend=c("% Students Take Econ. 1 in First 6 Quarters","% Students Take Econ. 2 in First 6 Quarters","% Students Declare Econ. Major"),lwd=c(1,1,2),lty=c(2,1,1),cex=1.3)
  text(2005.5,6,pos=3,"GPA Restrictions for Major:",cex=1.3)
  text(1997.9,0.1,pos=3,"None",cex=1.2)
  text(2005,-1,pos=3,"Less\nBinding",cex=1.2)
  text(2010,0.1,pos=3,"Binding",cex=1.2)
  text(2014.6,-1,pos=3,"Less\nBinding",cex=1.2)
dev.off()



c<-c[,!names(c)%in%c("SERU_ID","SID")]
S<-S[,!names(S)%in%c("SERU_ID","SID")]

save(c,S,file=paste0(secure,"UCSC_Econ.Rda"))
