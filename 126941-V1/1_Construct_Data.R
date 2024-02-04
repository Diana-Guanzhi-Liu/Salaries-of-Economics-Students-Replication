###
#  Toggles for data construction
###
UCSC_Records<-T
NSC_Records<-T
EDD_Records<-T
ACS_Records<-T



if(UCSC_Records){
  S<-rbind(data.frame(read_excel(paste0(secure,"UC-CHP File 2 - Educational Info F99-F11.xlsx")),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 2 - Educational Info prior to F99.xlsx")),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 2 - Educational Info W12-S19.xlsx")),stringsAsFactors = F))
  S<-rename(S,Grad=`Graduated.`,Last_Year=Last.Year,Admit_Date=Admit.Date,Deg=Degree.Name,Deg_Spec=Specialization,Deg_Major=Curriculum.Name)
  S$Grad<-NA_to_F(S$Grad=="Yes")
  S$Admit_Year<-as.integer(substr(S$Admit_Date,1,4))
  S$Admit_Term<-dplyr::recode(substr(S$Admit_Date,6,7),"01"="Winter","03"="Spring","04"="Spring","06"="Summer","09"="Fall")
  S$Deg<-dplyr::recode(S$Deg,"Bachelor of Arts"="BA","Bachelor of Music"="BM","Bachelor of Science"="BS","Certificate"="Cert","Doctor of Education"="Ed.D.","Doctor of Musical Arts"="D.M.A.","Doctor of Philosophy"="Ph.D.","Master of Arts"="MA","Master of Fine Arts"="MFA","Master of Science"="MS")
  S$UG<-S$Deg%in%c("BA","BM","BS","-")
  minor<-S[grepl("Minor\\s*$",S$Deg_Major),] ; S<-S[!grepl("Minor\\s*$",S$Deg_Major),]
  minor$Deg_Major<-gsub("\\s*Minor\\s*$","",minor$Deg_Major)
  S$Deg[grepl("Pre-",S$Deg_Major)]<-"BA"
  S$Deg_Major<-gsub("^\\s*(Pre[-](Major)?\\s*)|\\s*(BA|D?MA|PhD|BS|Cert(ificate)?|MS|[(]B[AS][)]|BM|MFA|EdD|Major)\\s*$","",S$Deg_Major)
  S$Deg_Spec[grepl("Indiv(idual)?",S$Deg_Major)]<-paste(S$Deg_Spec," ; ",gsub("\\s*Indiv(idual)?\\s*$","",S$Deg_Major))[grepl("Indiv(idual)?",S$Deg_Major)] ; S$Deg_Major[grepl("Indiv(idual)?",S$Deg_Major)]<-"Individual"
  #Split up 'combined' majors into double majors
  temp<-S[grepl("[/]",S$Deg_Major),] 
  temp$Deg_Major<-gsub("Comb(ined?)?\\s*$","",temp$Deg_Major)
  temp1<-temp
  temp$Deg_Major<-gsub("/.*$","",temp$Deg_Major) ; temp1$Deg_Major<-gsub("^.*/","",temp1$Deg_Major)
  temp<-rbind(temp,temp1)
  S<-rbind(S[!grepl("[/]",S$Deg_Major),],temp)
  #Fix up majors
  S$Deg_Major<-gsub("^\\s*|\\s*$","",S$Deg_Major)
  S$Deg_Major<-dplyr::recode(S$Deg_Major,"Lat Am Stu"="Latin Amer & Latino Studies","Latin Amer Studies"="Latin Amer & Latino Studies","Latin American Studies"="Latin Amer & Latino Studies","Lat Am Stu"="Latin Amer & Latino Studies","Latin American & Latino Study"="Latin Amer & Latino Studies","Legal Stu"="Legal Studies","Physics (Astrophysics)"="Physics","Applied Physics"="Physics","Anthro"="Anthropology","Technology & Info Management"="Technology&Info Management","Technology & Info Managemnt"="Technology&Info Management","Relig Thought"="Religious Studies","Neuroscience & Behavior"="Neuroscience","Music Composition"="Music","Molecular, Cellular & Devl Bio"="Molec Cell Develop Biology","Molecular Cell Dev Biology"="Molec Cell Develop Biology","Marine Sciences"="Marine Biology","Math"="Mathematics","Information Systems Mgt"="Information Systems Management","Hist. of Art & Visual Culture"="Hist of Art and Vis Culture","Global Econ"="Global Economics","Film & Digital Media"="Film and Digital Media","Environmental Study"="Environmental Studies","Env Studies"="Environmental Studies","Env Stu"="Environmental Studies","Econ"="Economics","Ecology & Evolutionary Bio"="Ecology and Evolution","Ecology Evolutionary Biol"="Ecology and Evolution","Eart Sci"="Earth Sciences","Earth Sci"="Earth Sciences","Computer & Information Science"="Computer Science","Computer & Information Sci"="Computer Science","Comp Sci: Comp Game Design"="Comp Sci Computer Game Des","Business Management Economics"="Business Mgmt Economics","Biol"="Biology","Biochem & Molecular Biology"="Biochemistry&Molecular Bio","Hist of Art and Vis Culture"="Art History","Aesthetic Studies"="Aesthetic Stu","Applied Linguistics"="Applied Ling and Multiling","Art & Des: Games & Playable"="Art","Computer & Info Sci"="Computer & Info Science","Environ Sci"="Environmental Studies","Envronmental Studies"="Environmental Studies","Lat Amer Study"="Latin Amer & Latino Studies","Latin Am Stu"="Latin Amer & Latino Studies","Latin Am Studies"="Latin Amer & Latino Studies","Latin Amer Stu"="Latin Amer & Latino Studies","Lit"="Literature","Mod Soc & Soc Thgt"="Modern Society & Soc Thght","Modern Society & Soc Thought"="Modern Society & Soc Thght") #Combine majors
  
  #Now consolidate by individual
  
  
  s<-S[S$UG,]
  s<-ddply(s,.(SID),function(x){
    if(sum(x$Grad)>0) x<-x[x$Grad,]
    y<-data.frame(SID=x$SID[1],UG=T)
    y$Admit_Year<-min(x$Admit_Year)
    y$Admit_Term<-x$Admit_Term[x$Admit_Year==y$Admit_Year][1]
    y$Last_Year<-max(x$Last_Year)
    if(sum(x$Grad)>0) y$Grad_Year<-max(x$Last_Year)
    if(nrow(x)>0) for(i in 1:nrow(x)){
      y[,paste0("Deg",i)]<-x$Deg[i]
      y[,paste0("Major",i)]<-x$Deg_Major[i]
      y[,paste0("Spec",i)]<-x$Deg_Spec[i]
      if(x$Grad[i]) y[,paste0("Grad_Year",i)]<-x$Last_Year[i]
    }
    return(y)
  })
  grad<-S[!S$UG,]
  grad<-ddply(grad,.(SID),function(x){
    y<-data.frame(SID=x$SID[1],UG=F)
    y$Admit_Year<-min(x$Admit_Year)
    y$Admit_Term<-x$Admit_Term[x$Admit_Year==y$Admit_Year][1]
    y$Last_Year<-max(x$Last_Year)
    if(sum(x$Grad)>0) y$Grad_Year<-max(x$Last_Year)
    if(nrow(x)>3&sum(x$Grad)>0) x<-x[x$Grad,]
    if(nrow(x)>0) for(i in 1:nrow(x)){
      y[,paste0("Deg",i)]<-x$Deg[i]
      y[,paste0("Major",i)]<-x$Deg_Major[i]
      y[,paste0("Spec",i)]<-x$Deg_Spec[i]
      if(x$Grad[i]) y[,paste0("Grad_Year",i)]<-x$Last_Year[i]
    }
    return(y)
  })
  S<-rbind.fill(s,grad)
  
  
  #Add individual info
  s<-rbind(data.frame(read_excel(paste0(secure,"UC-CHP File 1 - Student Info F99-F11.xlsx")),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 1 - Student Info prior to F99.xlsx")),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 1 - Student Info W12-S19.xlsx")),stringsAsFactors = F))
  s<-rename(s,FName=First.Name,MName=Middle.Name,LName=Last.Name,Address=First.Known.Home.Address.1,City=First.Known.Home.City,State=First.Known.Home.State,Zipcode=First.Known.Home.Zipcode,Country=First.Known.Home.Country,BCity=Birth.City,BState=Birth.State,BCountry=Birth.Country,BDate=Birth.Date,Residency=Residency.Status)
  s$Address<-paste(s$Address,";",s$First.Known.Home.Address.2)
  s$BYear<-as.integer(substr(s$BDate,1,4)) ; s$BMonth<-as.integer(substr(s$BDate,6,7)) ; s$BDay<-as.integer(substr(s$BDate,9,10))
  s<-s[,!names(s)%in%c("First.Known.Home.Address.2","First.Known.Home.Address.3","First.Known.Home.Address.4","BDate")]
  s$Residency<-s$Residency=="In State"
  s<-s[!duplicated(s$SID,fromLast = T),]
  S<-merge(S,s)
  
  #Additional student info
  s<-data.frame(read_excel(paste0(secure,"UC-CHP File 6 - Admissions Info F99-F12.xlsx")),stringsAsFactors = F)
  names(s)<-c("SID","HS_ATP","PrevSchCat","Adm_Cat","Eth","SAT","Year_Applied")
  s$SID[nchar(s$SID)==6]<-paste0(0,s$SID[nchar(s$SID)==6])
  S<-merge(S,s,all.x=T)
  
  #Courses
  C<-rbind(read.csv(paste0(secure,"UC-CHP File 4A - Courses_Primary Instuctors F99-F11.csv"),stringsAsFactors = F,colClasses = "character"),
           data.frame(read_excel(paste0(secure,"UC-CHP File 4 - Courses prior to F99 - Revised.xlsx"),sheet = "Report 1"),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 4 - Courses prior to F99 - Revised.xlsx"),sheet = "Report 2",col_names = c("SID","Year","Term","Department","Course.Number","Course.Name","Section.Number","Course.Level","Instructor.Full.Name","Grade","Units")),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 4 - Courses W00-F03 - Revised.xlsx")),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 4A - Courses_Primary Instructors W12-S19.xlsx"),sheet = "Report 1"),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 4A - Courses_Primary Instructors W12-S19.xlsx"),sheet = "Report 1(1)",col_names = c("SID","Year","Term","Department","Course.Number","Course.Name","Section.Number","Course.Level","Instructor.Full.Name","Grade","Units")),stringsAsFactors = F))
  C$Course.Number<-gsub("^\\s*|\\s*$","",C$Course.Number)
  C<-C[!duplicated(C),]
  c<-rbind(data.frame(read_excel(paste0(secure,"UC-CHP File 4B - Courses_Section Instructors F99-F11.xlsx")),stringsAsFactors = F),
           data.frame(read_excel(paste0(secure,"UC-CHP File 4B - Courses_Section Instructors W12-S19.xlsx")),stringsAsFactors = F))
  C<-merge(C,c,all.x=T)
  names(C)<-gsub("[.]","_",names(C))
  C<-rename(C,Level=Course_Level,Section=Section_Number,Inst1=Instructor_Full_Name,Section_Inst1=Section_Instructor_Name)
  C$Level<-dplyr::recode(C$Level,"Graduate"="G","Lower Division"="L","Upper Division"="U","Independent"="I","Other"="O")
  C$Units<-as.integer(C$Units)

  S$SID<-as.character(S$SID)
  S$UG[S$Major1=="Non-Degree"|S$Deg1%in%c("-","Cert")]<-NA #Instead, 'dropping' non-degree students
  S<-S[,!names(S)%in%c("FName","MName","LName","Address","SSN","BDay","BMonth")]
  
  S<-S[order(S$SID),]
  C<-C[order(C$Year,C$Term,C$SID,C$Department,C$Course_Number),]
  
  save(S,C,file=paste0(secure,"UCSC_Digital_Data.Rda"))
}




if(NSC_Records){
  NSC<-read.csv(paste0(secure,"NSC_Data_UCSC.csv"))
    names(NSC)[1]<-"SID"
    NSC$Year<-as.integer(substr(as.character(NSC$BeginDate),1,4))
  
  Data<-NSC[,c("SID","Your.Unique.Identifier")] ; names(Data)<-c("SID","X")
    Data<-Data[!duplicated(Data$SID),]
    
  
  #Find undergraduate degree and degree year
  graduate<-NSC[NA_to_F(NSC$Graduation.Date<((NSC$Year+9)*10000))&NA_to_F(NSC$Graduation.Date>((NSC$Year+1)*10000+200)),]
  graduate$NSC_Grad_Degree[grepl("BACHELOR|^B[. ]*[ASFMHBCON](?!SIC)|BACCALAUR|\\sBS",graduate$Degree.Title,perl=T)]<-"BA"
  graduate$NSC_Grad_Degree[grepl("ASSOC|^A[. ]*[AS]",graduate$Degree.Title)]<-"AA"
  graduate$NSC_Grad_Degree[grepl("MASTER|(^|/)M[. ]*[ASBEPFMH]|JURIS|JD|DOCTOR|^PHARM|^PD|^DPT|^EDS|^DPH",graduate$Degree.Title)]<-"MA"
  graduate$NSC_Grad_Degree[graduate$Degree.Title==""]<-"Unknown"
  graduate<-graduate[!is.na(graduate$NSC_Grad_Degree)&!grepl("CERT",graduate$Degree.Title),]
  graduate$num<-1:nrow(graduate) #Pick out a single degree
  temp<-ddply(graduate[,c("num","SID","NSC_Grad_Degree")],.(SID),function(x){
    for(d in c("BA","MA","AA","Unknown")){ #Care most about undergrad school; will take grad school if necessary
      if(sum(grepl(d,x$NSC_Grad_Degree))>0) return(x[which(grepl(d,x$NSC_Grad_Degree))[1],])
    }
  })
  graduate<-graduate[graduate$num%in%temp$num,!names(graduate)=="num"]
  graduate$NSC_Grad_Year<-round(graduate$Graduation.Date/10000)
  graduate<-graduate[,grepl("NSC",names(graduate))|names(graduate)%in%c("SID")]
  graduate<-graduate[!duplicated(graduate[,c("SID")]),]
  Data<-merge(Data,graduate,by=c("SID"),all.x=T)
  rm(graduate)
  
  
  #Find total number of years of enrollment in first seven years, excluding grad school
  graduate<-NSC[NA_to_F(NSC$Graduation.Date<((NSC$Year+6)*10000+701))&NA_to_F(NSC$Graduation.Date>((NSC$Year+1)*10000+200)),] #Before July 1, six years later, and after the following January (otherwise get AA's too early)
  graduate<-graduate[grepl("BACHELOR|^B[. ]*[ASFMHBCON]|BACCALAUR|\\sBS",graduate$Degree.Title),]
  graduate<-graduate[order(graduate$SID,graduate$Graduation.Date),]
  graduate<-graduate[!duplicated(graduate$SID),c("SID","Graduation.Date")] ; names(graduate)[2]<-"GradDate"
  enrollment<-NSC[!is.na(NSC$Enrollment.Begin),c("SID","Year","Enrollment.Begin","College.Name","College.State","College.Code.Branch")]
  enrollment<-enrollment[(enrollment$Enrollment.Begin>enrollment$Year*10000+701)&(enrollment$Enrollment.Begin<(enrollment$Year+7)*10000+701),]
  enrollment<-merge(enrollment,graduate,all.x=T)
  enrollment<-enrollment[NA_to_T(enrollment$Enrollment.Begin<enrollment$GradDate),] #Remove grad school or other post-grad enrollment
  enrollment<-enrollment[,c("Enrollment.Begin","SID","Year")]
  e<-enrollment[F,c("SID","Year")]
  for(i in 1:7){
    for(j in 0:1){
      if(j==0) enroll<-enrollment[(enrollment$Enrollment.Begin>=(enrollment$Year+i-1)*10000+701)&(enrollment$Enrollment.Begin<(enrollment$Year+i)*10000+101),!names(enrollment)=="Enrollment.Begin"]
      if(j==1) enroll<-enrollment[(enrollment$Enrollment.Begin>=(enrollment$Year+i)*10000+101)&(enrollment$Enrollment.Begin<(enrollment$Year+i)*10000+701),!names(enrollment)=="Enrollment.Begin"]
      enroll<-enroll[!duplicated(enroll),]
      enroll[,paste0("Enroll",i,j)]<-1
      e<-merge(e,enroll,all=T)
      print(paste("Year",i,"Merged"))
    }
  }
  for(i in 1:7){ ; for(j in 0:1){
    e[is.na(e[,paste0("Enroll",i,j)]),paste0("Enroll",i,j)]<-0
  } ; }
  e$NSC_NumWholeYearsEnrolled7<-(e$Enroll10==1|e$Enroll11==1)+(e$Enroll20==1|e$Enroll21==1)+(e$Enroll30==1|e$Enroll31==1)+(e$Enroll40==1|e$Enroll41==1)+(e$Enroll50==1|e$Enroll51==1)+(e$Enroll60==1|e$Enroll61==1)+(e$Enroll70==1|e$Enroll71==1)
  e<-e[,!grepl("^Enroll",names(e))]
  Data<-merge(Data,e[,names(e)!="Year"],all.x=T)
  rm(e,enroll,enrollment,graduate) ; gc()
    
  
  #Find first year of non-school employment and indicator for graduate school
  graduate<-NSC[NA_to_F(NSC$Graduation.Date<((NSC$Year+7)*10000+701))&NA_to_F(NSC$Graduation.Date>((NSC$Year+1)*10000+200)),]
  graduate<-graduate[NA_to_F(grepl("BACHELOR|^B[. ]*[ASFMHBCON]|BACCALAUR|\\sBS",graduate$Degree.Title))&NA_to_F(!grepl("CERT",graduate$Degree.Title)),]
  graduate$GradYear<-round(graduate$Graduation.Date/10000) ; graduate<-graduate[order(graduate$GradYear,decreasing=T),] ; graduate<-graduate[!duplicated(graduate$SID),]
  graduate$GradTime<-round(graduate$GradYear+(as.integer(substr(as.character(graduate$Graduation.Date),5,6))-1)/12,3)
  graduate<-graduate[,c("SID","Year","GradYear","GradTime")]
  
  enrollment<-NSC[!is.na(NSC$Enrollment.Begin),c("SID","Year","Enrollment.Begin","X2.year...4.year")]
  enrollment<-merge(enrollment,graduate[,c("SID","GradTime")],all.x=T)
  enrollment$EnrTime<-round(round(enrollment$Enrollment.Begin/10000)+(as.integer(substr(as.character(enrollment$Enrollment.Begin),5,6))-1)/12,3)
  enrollment<-enrollment[NA_to_T(enrollment$EnrTime>=(enrollment$GradTime+0.24)),] 
  enrollment<-enrollment[is.na(enrollment$GradTime)|NA_to_F(enrollment$X2.year...4.year==4),] 
  enrollment<-enrollment[round(enrollment$Enrollment.Begin/10000)<=(enrollment$Year+7),] 
  gradsch<-enrollment[NA_to_F(enrollment$X2.year...4.year==4)&!is.na(enrollment$GradTime),] ; gradsch<-gradsch[!duplicated(gradsch$SID),] ; gradsch$NSC_GradSchool7<-1 #List of students who enroll at graduate school
  enrollment1<-ddply(enrollment[,c("SID","EnrTime","GradTime")],c("SID"),function(x){
    x<-x[order(x$EnrTime),]
    attempts<-x$EnrTime ; if(!is.na(x$GradTime[1])) attempts<-c(x$GradTime[1],attempts)
    if(length(attempts)==1) return(attempts)
    i<-1 ; result<-NA ; for(i in 1:(length(attempts)-1)){
      if(attempts[i]<(attempts[i+1]-1.25)){
        result<-attempts[i] ; break
      }
    }
    if(is.na(result)) result<-attempts[length(attempts)]
    return(result)
  })
  names(enrollment1)[2]<-"Earliest_NonEnrollment"
  enrollment<-merge(enrollment,enrollment1,all.x=T) ; enrollment<-enrollment[!duplicated(enrollment$SID),]
  enrollment<-rbind.fill(enrollment,graduate) ; enrollment<-enrollment[!duplicated(enrollment$SID),] ; enrollment$Earliest_NonEnrollment[is.na(enrollment$Earliest_NonEnrollment)]<-enrollment$GradTime[is.na(enrollment$Earliest_NonEnrollment)] #Some people who graduated will have dropped out. Need to add them back in prior to merge
  enrollment$Earliest_NonEnrollment<-ceiling(enrollment$Earliest_NonEnrollment*4)/4+2 #Skip that quarter and the following quarter, but then start counting
  enrollment<-merge(enrollment,gradsch[,c("SID","NSC_GradSchool7")],all.x=T) #Merge in graduate school attendance
  enrollment$NSC_GradSchool7[enrollment$Year>2011]<-NA #Don't have data past 2011, since NSC not yet collected seven years
  enrollment<-enrollment[,c("SID","NSC_GradSchool7")]
  enrollment<-enrollment[!duplicated(enrollment[,c("SID")]),]
  Data<-merge(Data,enrollment,all.x=T)
  Data$NSC_GradSchool7[is.na(Data$NSC_GradSchool7)]<-0
    
  NSC<-Data[,c("SID","NSC_GradSchool7","NSC_Grad_Year","NSC_Grad_Degree","NSC_NumWholeYearsEnrolled7")]
  save(NSC,file=paste0(secure,"UCSC_NSC_Data.Rda"))
}




if(EDD){
  EDD<-read.csv(paste0(secure,"EDD_Data_UCSC.csv"))
  
  EDD<-rename(EDD,ID=AEO_UNIQUE_IDENTIFIER)
  EDD$Year<-round(EDD$QUARTER_PERIOD/10)
  EDD$Quarter<-(EDD$QUARTER_PERIOD-(EDD$Year*10))
  EDD<-EDD[,!names(EDD)%in%c("GROUP_NUMBER","YEAR_CODE","QUARTER_PERIOD","COUNTY_CODE")] ; gc()
  
  hi<-EDD[,c("ID","Year")]
  hi<-hi[!duplicated(hi),]
  hi$hi<-round(seq(1,nrow(hi))/10000)
  Sys.time() ; EDD<-merge(EDD,hi) ; gc()
  edd<-list()
  for(i in unique(hi$hi)){
    edd<-append(edd,list(EDD[EDD$hi==i,]))
    if(i%%10==0) print(i)
  } ; Sys.time()
  
  process_edd<-function(EDD){
    EDD<-ddply(EDD,.(ID,Year),function(x){
      y<-x[1,c("ID","Year")]
      y$wage_sum<-sum(x$QUARTELY_SUBJECT_WAGES)
      for(q in 1:4){
        y[,paste0("wage_sum_Q",q)]<-sum(x$QUARTELY_SUBJECT_WAGES[x$Quarter==q])
      }
      for(n in c("NACIS_CODE","STATE_CODE","CITY","ZIP_CODE")){
        for(q in 1:4){
          y[,paste0(n,q)]<-x[x$Quarter==q,n][1]
        }
      }
      return(y)
    })
    return(EDD)
  }
  
  rm(EDD) ; gc()
  
  library(doSNOW) ; cl<-makeCluster(3,type="SOCK", outfile="") ; registerDoSNOW(cl)
  Sys.time() ; EDD<-foreach(e=edd,.inorder=F,.packages = c("plyr"),.combine=rbind) %dopar% process_edd(e) ; Sys.time()
  rm(edd) ; stopCluster(cl) ; gc()
  
  EDD$SID<-substr(EDD$ID,3,9)
  names(EDD)<-gsub("N.*_CODE","NAICS",names(EDD))

  #CPI-adjust to 2018
  CPI<-read.csv("Data/Raw/CPI_California.csv")
  EDD<-merge(EDD,CPI[,c(1:2)])
  for(v in names(EDD)[grepl("^wage",names(EDD))]){
    EDD[,v]<-EDD[,v]/(EDD$CPI/CPI$CPI[CPI$Year==2018])
  }
  EDD<-EDD[,!names(EDD)=="CPI"]
  
  #Use earlier quarter's industry if fourth quarter unavailable
  EDD$NAICS4[is.na(EDD$NAICS4)]<-EDD$NAICS3[is.na(EDD$NAICS4)] 
  EDD$NAICS4[is.na(EDD$NAICS4)]<-EDD$NAICS2[is.na(EDD$NAICS4)]
  EDD$NAICS4[is.na(EDD$NAICS4)]<-EDD$NAICS1[is.na(EDD$NAICS4)]
  
  edd<-EDD[!duplicated(EDD$SID),c("SID","Year")]
  for(i in 2008:2018){
    EDD_merge<-EDD[EDD$Year==i,c("SID","wage_sum","NAICS4")]
    names(EDD_merge)<-gsub("([^A-Z])$",paste0("\\1_",i),names(EDD_merge))
    edd<-merge(edd,EDD_merge,all.x=T)
    print(i)
  }
  edd<-edd[,names(edd)!="Year"]
  
  EDD<-edd ; rm(edd) ; gc()
  
  save(EDD,file=paste(secure,"UCSC_EDD_Data.Rda"))
}




#Find median wages in ACS
if(ACS_Records){ 
  ddi <- read_ipums_ddi("Data/Raw/usa_00075.xml")
  acs <- read_ipums_micro(ddi)
  acs <- acs[acs$EDUCD>100&acs$YEAR%in%c(2009,2010,2013,2014,2017,2018)&acs$INCWAGE>0&acs$INCWAGE!=999999,] ; gc()
  acs <- acs[(acs$YEAR%in%c(2009,2013,2017)&acs$AGE%in%23:27)|(acs$YEAR%in%c(2010,2014,2018)&acs$AGE%in%24:28),]
  cpi<-read.csv("Data/Raw/CPI.csv") ; names(cpi)<-c("YEAR","CPI")
  acs<-merge(acs,cpi) ; acs$INCWAGE<-acs$INCWAGE/(acs$CPI/cpi$CPI[cpi$YEAR==2018])
  acs$AllMajors<-as.character(acs$DEGFIELDD)
  acs$AllMajors[acs$DEGFIELD2D!=0]<-apply(acs[acs$DEGFIELD2D!=0,c("DEGFIELDD","DEGFIELD2D")],1,function(x){
    x<-x[!is.na(x)&!duplicated(x)]
    return(paste(x[order(x)],collapse=","))
  })
  acs$Y[acs$YEAR%in%2009:2010]<-2010 ; acs$Y[acs$YEAR%in%2013:2014]<-2014 ; acs$Y[acs$YEAR%in%2017:2018]<-2018
  save(acs,file="Data/Derived/ACS_Data_UCSCEcon.Rda")
  acs$ID<-paste(acs$AllMajors,acs$YEAR)
  acs<-acs[acs$ID%in%names(table(acs$ID)[table(acs$ID)>9]),]
  acsmaj<-ddply(acs,.(AllMajors,Y),function(x) weighted.median(x$INCWAGE,x$PERWT,na.rm=T))
  acs_CA<-acs[acs$STATEFIP==6,]
  acs_CA<-acs_CA[acs_CA$ID%in%names(table(acs_CA$ID)[table(acs_CA$ID)>9]),]
  acsmaj_CA<-ddply(acs_CA,.(AllMajors,Y),function(x) weighted.median(x$INCWAGE,x$PERWT,na.rm=T))
  for(v in c("DEGFIELDD","DEGFIELD2D")) acs[acs[,v]==6205&acs$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18),v]<-5501 #Combine business and regular economics
  acs$AllMajors<-as.character(acs$DEGFIELDD)
  acs$AllMajors[acs$DEGFIELD2D!=0]<-apply(acs[acs$DEGFIELD2D!=0,c("DEGFIELDD","DEGFIELD2D")],1,function(x){
    x<-x[!is.na(x)&!duplicated(x)]
    return(paste(x[order(x)],collapse=","))
  })
  acs$ID<-paste(acs$AllMajors,acs$YEAR)
  acs_NotCA<-acs[acs$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18),]
  acs_NotCA<-acs_NotCA[acs_NotCA$ID%in%names(table(acs_NotCA$ID)[table(acs_NotCA$ID)>9]),]
  acsmaj_NotCA<-ddply(acs_NotCA,.(AllMajors,Y),function(x) weighted.median(x$INCWAGE,x$PERWT,na.rm=T))
  save(acsmaj,acsmaj_CA,acsmaj_NotCA,file="Data/Derived/ACS_Majors_INCWAGE.Rda")
  
  #Identify the states least similar to California (Figure A-18(a))
  ind<-unique(acs$IND)
  temp<-ddply(acs[acs$YEAR%in%2017:2018,],.(STATEFIP),function(x){
    y<-data.frame(State=x$STATEFIP[1],stringsAsFactors = F)
    for(i in ind) y[,paste0("Ind_",i)]<-weighted.mean(x$IND==i,x$PERWT,na.rm=T)
    return(y)
  })[,-1]
  pr<-cbind(temp$State,data.frame(prcomp(temp[,2:51])$x))
  pr$Dist<-sqrt((pr[,3]-pr[5,3])^2+(pr[,2]-pr[5,2])^2)
  names(pr)[1]<-"fips" ; fips<-read.csv("Data/Raw/State_to_FIPS.csv") ; pr<-merge(pr,fips)
  pr<-pr[order(pr$Dist,decreasing = T),]
  png("Figures/StateIndPC.png",width=500,height=500)
    plot(100,100,xlim=c(-0.09,0.08),ylim=c(-0.05,0.06),xlab="PC1",ylab="PC2",cex.axis=1.3,cex.lab=1.5)
    for(i in 1:nrow(pr)){
      col="black" ; if(i%in%16:50) col="gray50"
      text(pr[i,2],pr[i,3],labels=pr$state[i],adj=.5,cex=1.5,col=col)
      if(i==51) draw.circle(x=pr[i,2],y=pr[i,3],radius=.006)
    }
  dev.off()
  
  #Decomposition by industry as well
  ddi <- read_ipums_ddi("Data/Raw/usa_00075.xml")
  acs <- read_ipums_micro(ddi)
  acs$INDNAICS_Orig<-acs$INDNAICS ; acs$INDNAICS<-gsub("[A-Z].*","",acs$INDNAICS)
  acs <- acs[acs$EDUCD>100&acs$INCWAGE>0&acs$INCWAGE!=999999&acs$AGE%in%23:28,] ; gc()
  cpi<-read.csv("Data/CPI.csv") ; names(cpi)<-c("YEAR","CPI")
  acs<-merge(acs,cpi) ; acs$INCWAGE<-acs$INCWAGE/(acs$CPI/cpi$CPI[cpi$YEAR==2018])
  acs1<-acs ; save(acs1,file="Data/Derived/ACS_Data_UCSCEcon_NAICS_AllYears.Rda") ; rm(acs1)
  acs <- acs[(acs$YEAR%in%c(2009,2013,2016)&acs$AGE%in%23:27)|(acs$YEAR%in%c(2010,2014,2017)&acs$AGE%in%24:28),]
  acs$Y[acs$YEAR%in%2009:2010]<-2010 ; acs$Y[acs$YEAR%in%2013:2014]<-2014 ; acs$Y[acs$YEAR%in%2016:2017]<-2017
  save(acs,file="Data/Derived/ACS_Data_UCSCEcon_NAICS.Rda")
  #Wrap up industries that are too small
  for(i in 6:2){ #Wraps up about 25 industries
    check<-nchar(acs$INDNAICS)==i&acs$INDNAICS%in%names(table(acs$INDNAICS[acs$YEAR==2017])[table(acs$INDNAICS[acs$YEAR==2017])<9])
    acs$INDNAICS[check]<-gsub(".$","",acs$INDNAICS[check])
  } 
  acs$ID<-paste(acs$INDNAICS,acs$YEAR)
  acs<-acs[acs$ID%in%names(table(acs$ID)[table(acs$ID)>9]),]
  
  acsind<-ddply(acs,.(INDNAICS,Y),function(x) weighted.median(x$INCWAGE,x$PERWT,na.rm=T))
  acs_CA<-acs[acs$STATEFIP==6,]
  acs_CA<-acs_CA[acs_CA$ID%in%names(table(acs_CA$ID)[table(acs_CA$ID)>9]),]
  acsind_CA<-ddply(acs_CA,.(INDNAICS,Y),function(x) weighted.median(x$INCWAGE,x$PERWT,na.rm=T))
  acs_NotCA<-acs[acs$STATEFIP%in%c(56,54,2,5,28,50,46,20,31,15,38,19,16,22,18),]
  acs_NotCA<-acs_NotCA[acs_NotCA$ID%in%names(table(acs_NotCA$ID)[table(acs_NotCA$ID)>9]),]
  acsind_NotCA<-ddply(acs_NotCA,.(INDNAICS,Y),function(x) weighted.median(x$INCWAGE,x$PERWT,na.rm=T))
  save(acsind,acsind_CA,acsind_NotCA,file="Data/Derived/ACS_Industries_INCWAGE.Rda")
}
