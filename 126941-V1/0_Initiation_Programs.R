setwd("INSERT PATH TO MAIN DIRECTORY HERE")
secure<-"INSERT PATH TO SECURE DATA HERE"

install.packages(c("readstata13","rdd","lfe","glmnet","ipumsr","spatstat","colorspace","RDHonest","plotrix","plyr","dplyr","readxl","readr"))

library(readstata13)
library(rdd)
library(lfe)
library(glmnet)
library(ipumsr)
library(spatstat)
library(colorspace)
library(RDHonest)
library(plotrix)
library(plyr)
library(dplyr)
library(readxl)
library(readr)

NA_to_F<-function(x){
  x[is.na(x)]<-FALSE
  return(x)
}
NA_to_T<-function(x){
  x[is.na(x)]<-TRUE
  return(x)
}
inrange<-function(x,a=0,b=0){
  return(x>=a&x<=b)
}
winsor<-function(x,n){
  x[x>quantile(x,(1-n),na.rm=T)]<-quantile(x,(1-n),na.rm=T)
  x[x<quantile(x,n,na.rm=T)]<-quantile(x,n,na.rm=T)
  return(x)
}
ex<-function(x,i,par=F,brk=F,p=-1,comma=F,dagger=0.1,justnum=F){
  miss<-is.na(x) ; x[miss]<-0 ; if(length(p)==length(miss)) p[miss]<- -1
  if(par) hi<-paste0('="(',sprintf(paste0("%.",i,"f"), round(x,i)),')"')
  else if(brk) hi<-paste0('="[',sprintf(paste0("%.",i,"f"), round(x,i)),']"')
  else hi<-paste0('="',sprintf(paste0("%.",i,"f"), round(x,i)),'"')
  if(p[1]>=0){
    check<-inrange(p,0.05000000001,dagger) ; hi[check]<-gsub('(\\]|[)])?("$)',"†\\1\\2",hi[check])
    check<-inrange(p,0.01000000001,0.05) ; hi[check]<-gsub('(\\]|[)])?("$)',"*\\1\\2",hi[check])
    check<-p<0.01 ; hi[check]<-gsub('(\\]|[)])?("$)',"**\\1\\2",hi[check])
  }
  if(comma){
    for(zzz in 1:3) hi<-gsub('([0-9]{3}|(["(-]|\\[)[0-9]{1,2})([0-9]{3})([",.)†*]|\\])',"\\1,\\3\\4",hi)
  }
  if(justnum) hi<-gsub('["=]',"",hi)
  hi[miss]<-'=""'
  return(hi)
} 








###
#  College Major categorization
###
Fields_all<-list(
  list("Humanities",
       list("Art","Decorative Art","Practice of Art","Art Practice","Art and Architecture","Drawing and Painting","Fine Arts","Drawing and Art","Household Art","Art, Design and Art History","Art Studio","Studio Art","Visual Arts","Visual Studies","Vis & Perf Arts & Media","History Art & Visual Culture","Mechanic Arts"),
       list("Art History","History of Art","Art and Art History","Graphic Arts","Philosophy and Fine Arts","Aesthetic Studies","History of Art and Architecture"),
       list("Classics","Classical Literature","Classical Studies","Literature Classical Studies","Classical Civilization"),
       list("Comparative Literature","Critical Theory","Comparative Lit","History Of Consciousness","Literature World & Cultural Studies"),
       list("English","English Literature and Rhetoric","English Philology","Literature","Modern Thought and Literature","Literature English","English Literature","Literature Modern Literary Studies","Literature American","Literature Pre-& Early Mod Studies","Russian Literature"),
       list("English Composition","Creative Writing","Composition","Literature Creative Writing"),
       list("Film","Film and Television","Moving Image Archive Studies","New Media","Film and Television","Film and Media","Film and Media Studies","Film Studies","Design-Media Arts","Film and Digital Media","Film and Video","Social Documentation","Media Studies","Cinema & Technocultural Stud","Technocultural Studies","Cinema and Digital Media"),
       list("Folklore","Folklore and Mythology"),
       list("Humanities","Interdisciplinary Studies in Humanities"),
       list("Languages Ancient","Greek","Latin","Hebrew","Semitic Languages","Classical Languages","Egyptian","Cuneiform"),
       list("Langauges Arabic","Arabic","Near Eastern Languages","Near Eastern Languages and Culture","Near Eastern and African Languages"),
       list("Languages Asian","Oriental Languages","Asian Languages","Chinese Japanese Language","Chinese Japanese Language and Area Studies Faculty","Cantonese","East Asian Languages","East Asian Languages and Cultures","East Asian Languages and Cult","Sanskrit","Chinese","Asian Languages and Cultures","Chinese and Japanese","Hindi","Japanese","Turkish","Hindi/Urdu","Vietnamese","Thai","Tamil","Tagalog","Punjabi","Korean","Persian","Indonesian"),
       list("Languages Germanic","German","Germanic Languages","German Studies","Germanic Philology","Literature German","German and Russian","Yiddish","Dutch","Germanic Languages and Literatures"),
       list("Languages Romantic","French","Romanic Languages","Romance Philology","French and Italian","Italian","Literature Italian","Modern European Languages","Germanic and Romanic Languages","Portuguese","Romance Languages and Literatures","Literature French","French Literature"),
       list("Languages Slavic","Russian","Slavic Languages","Slavic Languages and Literatures?","Slavic"),
       list("Languages Other","Scandinavian Languages and Literature","Scandinavian","Scandinavian Languages","Foreign Languages","Hungarian","Celtic","Languages and Applied Linguistics","Swahili"),
       list("Music","Musicology","Ethnomusicology","Ethnomusicology and Systematic Musicology","Historical Musicology","Ethno-Jazz","Jazz","Music Studies","Music Composition"),
       list("Philosophy"),
       list("Religion","Religious Studies","Biblical History and Literature","Religious Thought"),
       list("Spanish","Spanish and Portuguese","Literature Spanish","Spanish and Classics","Spanish Studies"),
       list("Theater","Dramatic Art","Drama","Public Speaking and Dramatic Arts","Speech and Drama","Dance","Theater and Dance","Theater Arts","Theater, Dance and Performance Studies","Theater, Dance and Perf Studies","Dramatic Art and Dance","Theatre and Dance")
  ),
  list("Social Sciences",
       list("Anthropology","Anthropological Sciences","Cultural and Social Anthropology","Anthropology and Sociology"),
       list("Archaeology","Mediterranean Archaeology","Conservation of Archaeological and Ethnographic Materials"),
       list("Criminology","Law and Society-Criminal Justice"),
       list("Demography","Population Studies","Population Health and Reproduction"),
       list("Economics","Economics and Social Science","Economics and Sociology","Business Management Economics","Business Mgmt Economics","Global Economics","Applied Economics And Finance","Applied Economics","Business & Economics","Economics and Accounting","Business Economics","International Economics","Environmental Economics","Economics-Mathematics","Agricultural Economics","Agricultural and Resource Economics","Agricultural & Resource Econ","Agricultural Res Econ Pol","Agricultural & Resource Econ","Managerial Economics","Agri & Managerial Econ"),
       list("Environmental Studies","Environ Sci, Policy and Mgmt","Environmental Science, Policy and Management","Human Ecology","Environ Policy Analy & Plan"),
       list("Ethnic Studies","Ethno","Dutch Studies","Buddhist Studies","Black Studies","AfroAmerican Studies","African American Studies","African Am Studies","African American and African Studies","Chicano Studies","Cesar Chavez","Chicano Studies Program","Chicana and Chicano Studies","International Education","Near Eastern Studies","Asian Studies","South and Southeast Asian Studies","Italian Cultural Studies","East Asian Studies","Native American Studies","Group Major in Dutch Studies","Celtic Studies","Asian American Studies","Center for Russian and European Studies","Asiatic and Slavic Studies","Asian American and Asian Diaspora Studies","La Raza Studies","Slavic Studies","Asian American Studies Program","Italian Studies","Native American Studies Program","Peace and Conflict Studies","Human Rights","American Studies","European Studies","Indoeuropean Studies","Islamic Studies","Labor and Workplace Studies","African-American Studies","Latin American Studies","Latin American and Iberian Studies","Latin Amer & Latino Studies","Latin American and Hemispheric Studies","African Amer&African Studies","Latin Amer & Latino Stu","Chicana/O Studies","Latin American & Latino Studies","World Arts and Cultures","American Indian Studies","Jewish Studies","Civilization and Culture","Humn Psyc, Cultr & Soc","Contemporary Societies","Overseas Studies","East European Studies","International and Area Studies","Intl and Area Studies","Lib Arts & Area Studies","Middle Eastern Studies","Middle East/South Asian Std","Slavic and Eastern European Studies","Slavic and East Euro Studies","Near East Studies","Middle East Studies","Chinese Studies","African Studies","Hispanic Studies","World Arts and Cultures","Language And Culture","South Asian Studies","South-East Asian Studies","Eastern European Studies","Global Studies","African American & African Std","Middle East/South Asia Studies"),
       list("Geography","Physical Geography"),
       list("History","History and Political Science","Ancient History and Mediterranean","Ancient History and Mediterranean Archa?eology","Medieval Studies","Medieval and Early Modern Studies","American Civilization","History and Economics","Social Theory and Comparative History","World Civilizations"),
       list("Home Economics","Domestic Science","Child Development","Textiles & Clothing"),
       list("Legal Studies","Law and Society"),
       list("Linguistics","Human Language","Applied Linguistics","Linguistics and Philology","Language Studies","Lang. Lit & Commun"),
       list("Logic and the Methodology of Science","Program in the History of Sciene","Science and Society","History and Philosophy of Science","Science and Technology Studies","Logic and Methodology","Science & Technology Studies","History & Philosophy of Sci."),
       list("Nature and Culture"),
       list("Political Science","Political Economy","Political Economy of Industrial Societies","American Institutions","Political Behavior","World Affairs","Politics","Government","Political Science Publ Ser"),
       list("Psychology","Psych","Philosophy and Psychology","Behavioral Sciences","Applied Behavioral Science","Biobehavioral Sciences","Applied Behavioral Sciences","Psychobiology","Biopsychology","Educational Psychology","Psychological & Brain Sciences","Developmental Psychology","Experimental Psychology"),
       list("Public Policy","Goldman Public Policy","Policy Studies","International Relations","Public Affairs","History of Public Policy"),
       list("Sociology","Sociology and Social Institutions","Social Institutions","Sociology and Anthropology","Sociology and Demography","Social Thought","Modern Society And Social Thought","Rural Institutions","Social Studies","Language, Culture, and Society","Sociology-Organizational Stds","Modern Society & Soc Thght"),
       list("Women's Studies","Womens Studies","Gender and Womens Studies","Feminist Theory and Research","Women and Gender Studies","Feminist Studies","LGBT Studies","Gender, Sexuality & Women Stdy")
  ),
  list("Natural Sciences", #Hysical Sciences?
       list("Applied Mathematics","Biomathematics","Mathematics and Physics","Applied Mathematics and Statistics","Financial Mathematics and Statistics","Math & Physical Sci","Math Analytics & Ops Rsch","Math & Scientific Computation"),
       list("Astronomy","Astronomy Couse Program","Center for Space Science and Astrophysics","Atmospheric and Space Sciences","Atmos Sci","Atmospheric Sciences","Atmospheric and Oceanic Sciences","Astronomy & Astrophysics"),
       list("Biochemistry","Comparative Biochemistry","Biophysics","Molecular Pharmacology","Biophysics Program","Biophysics and Medical Physics","Biochemistry and Pharmacology","Biochemistry and Biophysics","Chem and Bioc","Biochemistry and Molecular Biology","Biochem & Molecular Biology","Plant Biochemistry","Biochemistry&Molecular Bio","Biochemistry-Molecular Biology","Med - Biological Chemistry","Chemistry and Biochemistry","Biochemistry & Biophysics"),
       list("Biology","Microbiology","Botany","Zoology","Animal Biology","Bacteriology","Physiology","Physiological Chemistry","Anatomy","Genetics","General Botany","Entomology and Parisitology","Cell Physiology","Anatomy and Physiology","Molecular Biology","Plant Physiology","Systematic Biology","Physiology and Histology","Entomology and Genetics","Histology","Plant Pathology","Plant Pathology and Microbiolo","Plant Biology","Plant Cell Biology","Botany and Plant Sciences","Integrative Biology","Entomological Studies","Endocrinology","Developmental Biology","Microbiology and Immunology","Molecular and Cellular","Physiology and Cell Biology","Cell Biology","Ecology","Pathology","Bacteriology and Experimental Pathology","Bacteriology and Immunity","Bacteriology and Immunology","Evolution, Ecol & Biodiversity","School of Biological Sciences","Economic Biology","Entomology","Human Biology","Environmental Biology","Molecular and Cellular Physiology","Program in Human Biology","Pathology and Bacteriology","Structural Biology","Systematic Botany","Systematic Botany and Ecology","Ecology, Evolution, and Marine Biology","MCD Biology","Entomological Sciences","Microbiology","Molecular and Cell Biology","Molecular and Physiological Plany Biology","Molecular Plant Biology","Molecular Toxicology","Parisitology","Physiology Anatomy","Plant and Microbial Biology","Plant and Soil Biology","Plant Nutrition","Genetics and Genomics","Zoology and Psychology","Insect Biology","Physiology and Physiological Chemistry","Biological Chemistry","Microbiology and Molecular Genetics","Human Genetics","Genetics Program","Molecular and Cellular Biology","Hematology","Evolution and Ecology","Ecology And Evolution","Molecular, Cellular & Develop Biology","Plant Hematology","Botany and Plant Sciences","Bioinformatics","Molec, Cell & Devel Biology","Vertebrate Zoology","Animal Genetics","Cell, Molecular, and Developme","Botany & Plant Sciences","Biological Sciences","Environmental Toxicology","Cell and Developmental Biology","Neurobiology,Physio & Behavior","Neurobiol, Physiol & Behavior","Neurobio,Physiology & Behavior","Molec Cell Develop Biology","Biology-Molec, Cell & Dev"),
       list("Chemistry","Chemistry and Chemical Engineering"),
       list("Cognitive Science","Cognitive Science Program","Neuroscience & Behavior"),
       list("Computer Science","Computer Science Division","Computer And Information Sciences","Computer & Info Sciences","Engineering Computer Science","Computer Science & Math","Comp Sci Computer Game Des"),
       list("Environmental Science","Environ Sci & Management","Environ Sciences","Environmental Toxicology","Environmental Horticulture","Environmental Science and Policy","Energy and Resources Group ERG","Environ Health Sci","Environmental Resource Science","Institute of the Environment","Climate","Soils & Environmental Sciences","Environmental Science & Policy","Environmental Sci & Management"),
       list("Geology","Geological Sciences","Geology and Geophysics","Geology and Mining","Applied Earth Sciences","Earth Science","Earth Sciences and Resources","Geological and Environmental Sciences","Geology and Mining Geology","Geology and Paleontology","Earth and Planetary Science","Earth and Space Science","Oceanography","Integrative Oceanography","Geology and Mineralogy","Water Science and Engineering","Hydrologic Sciences and Policy","Hydrology","Hydrologic Sciences","Hydrologic Science","Meteorology","Earth Sciences","Atmospheric Science","Earth and Planetary Sciences","Geophysics"),
       list("Mathematics","Pure Mathematics","Physical Sci And Math","Mathematical Sciences"),
       list("Marine Biology","Ocean Studies","Marine Sciences","Marine and Coastal Science","Marine Biological Laboratory","Division of Marine Biology Hopkins Marine Station","Hopkins Marine Station","The Marine Biological Laboratory","Aquatic Biology"),
       list("Neuroscience","Undergraduate Group Major in Neurobiology","Neurology and Neurological Sciences","Neurobiology","Neurobiological Group","Neurology","Helen Wills Neuroscience Inst","Neurobiology, Physiology and Behavior","Cell Biology and Neuroscience"),
       list("Physics","Applied Physics","Physics and Astronomy","Physics Astrophysics","Astrophysics","Chemical Physics"),
       list("Statistics","Biostatistics","Statistical Science"),
       list("Other Natural Sciences","Fiber And Polymer Science","Paleontology")
  ),
  list("Engineering",
       list("Aeronautical Engineering","Aeronautics and Astronautics","Aeronautics","Aeronautical Sciences","Engineering Aerospace Sci","Aeronautical Science & Engr","Aerospace Sci & Engineer"),
       list("Agricultural Engineering","Biological and Agricultural Engineering","Agricultural Engineering Technology"),
       list("Applied Science and Technology","Applied Science"),
       list("Biomedical Engineering","Bioengineering","Biomolecular Engineering","Engineering Biomedical","Engineering Biological Systems","Biological Systems Engineering","Biochemical Engineering","Chemical/Biochemical Engr"),
       list("Chemical Engineering","Chemical E","Chemical and Biomolecular Engineering","Chemical Engineering and Materials Science","Chemical and Environmental Eng","Engineering Chemical","Engineering Chemical-Materials","Chm/Matls Science & Engr"),
       list("Civil Engineering","Civil and Environmental Engineering","Civil and Environ Engineer","Civil and Env","Sanitary Engineering","Engineering Civil & Environ"),
       list("Electrical Engineering","Electrical Engineering and Computer Science","Cybernetics","Electrical and Computer Engineering","Elec Engineering and Computer Sc","Elect Engineering","Computer Engineering","Computer Science & Engineering","Electrical & Computer Engineer","Engineering Electrical & Compu","Electrical Engin, General","Elect Engin: Computers"),
       list("Engineering Design","Drawing"),
       list("Environmental Engineering","Environmental Science and Engineering"),
       list("Geological Engineering","Engineering Geology and Hydrogeology"),
       list("Industrial Engineering","Industrial Engineering and Operations Research","Engineering Economic Systems","Engineering Economic Systems and Operations Research","Industrial Engineering and Engineering Management","Industrial Engineering Program","Operations Research","Integrated Manufacturing Engineering","Industrial Engineering and Ops Res","Information Systems Management","Technology&Info Management"),
       list("Material Science","Materials Science","Material Science and Engineering","Material Sci and Engineeri","Materials Science and Mineral Engineering","Materials","Metallurgical Engineering","Materials Science & Engr","Nanoscale Science and Engineering","Nanoscience and Engineering","Materials Science and Enginee"),
       list("Mechanical Engineering","Engineering Materials Science","Mechanical and Electrical Engineering","Hydraulic and Sanitary Engineering","Structural Engineering and Structural Mechanics","Applied Mechanics","Mechanical Design","Structural Mechanics","Mechanical and Aerospace Engineering","Mechanical and Aeronautical Engineering","Mech/Materials Sci & Engr","Mechanics","Robotics Engineering","Engineering Mechanical","Engineering Mechanical & Aero"),
       list("Mining Engineering","Mineralogy","Mining and Metallurgy","Mineral Technology","Mineral Engineering","Mining","Metallurgy"),
       list("Nuclear Engineering"),
       list("Ocean Engineering","Nautical Engineering","Naval Architecture and Offshore Engineering","Engineering- Naval Architecture"),
       list("Petroleum Engineering","Petroleum"),
       list("Transportation Engineering","Transportation and Traffic Engineering","Transportation"),
       list("Other Engineering","Engineering","Engineering Interdisciplinary Studies","Engineering Science","College of Engineering","Engineering- Physics","Optical Science & Engineering","Engineering Mathematics")
  ),
  list("Professional",
       list("Agriculture","Agricultural Science","Irrigation","Agricultural Chemistry","Agricultural and Environmental Chemistry","Range Management","Wood Science and Technology","Animal Husbandry","Dairy Industry","Natural Resources","Land, Air and Water Resources","Poultry Husbandry","Resource Sciences","Wildland Resource Science","Animal Industry","Irrigation Investigations and Practice","Irrigation and Soil Science","Soils and Plant Nutrition","Soils","Soil Science","Soil Resources","Environmental & Resource Sci","Subtropical Horticulture","Vegetable Crops","Agronomy","Agronomy and Range Science","Viticulture and Enology","Viticulture","Viticulture and Fruit Products","Agricultural Systems & Envir","Soil and Water Science","Soil & Water Science","Plant Sciences","Plant Science","Crop Science and Management","Avian Sciences","Fermentation Science","Wildlife and Fisheries Biology","Wildlife, Fish & Conserv Biol","Agric Mngt & Range Resources","Wildlife, Fish, and Conservation Biology","Wildlife,Fish&Conservation Bio","Agricultural Practices","Environ Biology & Mangt","Biotechnology","Floriculture and Ornamental Horticulture","Agri Science & Mangt","Agriculture, Horticulture, and Entomology","Intern'l Agri Devlopmt","Wildlife & Fisheries Biology","Pomology","Citriculture","Olericulture","Soil Chemistry and Bacteriology","International Agricultural Dev","Sustainable Ag & Food Sys","Soil Survey","Nematology","Animal Science","Animal Science & Management","Viticulture & Enology"), #Nematology... At Davis, but messes up Hematology? At Riverside, too; so it goes?
       list("Architecture","Landscape Architecture","Landscape Horticulture","Landscape Architecture and Environmental Planning","Landscape Arch and Envir Plng","Landscape Management","Biological Control","Architecture and Urban Design","Landscape Design","Landscape Gardening and Floriculture"),
       list("Business","Business Administration","Commerce","Public and NonProfit Management","Management","Business Education","Accounting","Financial Engineering","Actuarial Science","Management Working Professionl","Food Service Management","Technology Management"),
       list("Communication","Communications","Journalism","Communication and Journalism","Institute for Journalistic Studies","Mass Communications","Journalistic Studies","Communication Studies","Rhetoric and Communication","Science Communication","Rhetoric & Communication"),
       list("Data Science"),
       list("Dentistry","Oral Biology"),
       list("Development","Development Studies","International Development Studies","Human and Community Development","Human Development","Community Studies","Global Poverty and Practice","Community & Regional Develpmnt","Community and Regional Develop"),
       list("Education","Science and Mathematics Education","Mathematics Education","Education and Psychology","Agric & Environ Education","Agricultural Education"),
       list("Energy and Resources","Conservation","Conservation and Resource Studies"),
       list("English as a Second Language","TESL and Applied Linguistics"),
       list("Environmental Design","Sustainable Environmental Design","Architecture- Environmental Design","Sustainable Environmntl Design"),
       list("Fashion","Textiles and Clothing","Textile Science"),
       list("Food Science","Food Research Institute","Food Research","Food Technology","Consumer Sciences","Food Science & Technology","Food Biochemistry"),
       list("Forestry","Forestry and Resource Management","Forestry and Conservation","Environ Hort & Urban Forestry"),
       list("Health Sciences","Social and Administrative Health Sciences","Biomedical and Environmental Health Sciences","Health Arts and Sciences","Biomedical and Environmental Health Sciences","Biomedical Sciences","Environmental Health Sciences","Health and Medical Sciences Program","Health Research and Policy","Health Education","Pharmaceutical Chemistry","Pharmacology","Pharmacology and Toxicology","Med - Medical Pharmacol &Toxic","Medicine and Epidemiology","Health Arts and Sciences","Program in Hearing and Speech Sciences","Speech and Hearing Sciences","Program in Speech and Hearing Sciences","Allied Medical Sciences","Physical Therapy","Physical Therapeutics","Preventative Medicine","Global Disease Biology","Epidemiology","Health and Medical Services","Health Services and Policy Analysis","Immunology","Infectious Diseases and Immunity","Infectious Diseases","Health and Medical Sciences","Hygiene and Health Education","Pharmacy","Health Services","Community Health Sciences","Ecology and Evolutionary Biology","Exercise Science","Epidemiology and Preventative Medicine","Physiological Sciences","Clinical Pathology","Population Health and Reproduction","Life And Health Sci","Exercise Biology","Human Biodynamics"),
       list("Information","Information Management and Systems","Information Studies","Library and Information Studies","Library Studies","Library and Information Science","Info","Information Systems Managment","Info Sci, Engin & Tech","Information Science","Technology & Information Management","Network & Digital Technology","Computer & Info Science"),
       list("Special Institutes","Hoover Institute","Hoover Institution","Hopkins Marine Institute","Pharmacology and Therapeutics"),
       list("Law","Jurisprudence"),
       list("Library","Librarianship","Library Science","Library Practice","School of Librarianship","Graduate School of Librarianship","Bibliographic Studies"),
       list("Medicine","Medical Physics","Radiology","Radiological Sciences","Medical Microbiology","Diagnostic Radiology and Nuclear Medicine","Biophysics and Nuclear Medicine","Obstetrics and Gynecology","Gynecology and Obstetrics","Pediatrics","Psychiatry","Radiation Oncology","Surgery","Surgical and Radiological Sciences","Ophthemology","Orthopaedic Surgery","Pathology and Laboratory Medicine","Anesthesiology","Anesthesiology and Pain Medicine","Family Medicine","Gerontology","Ophthalmology","Urology","Orthopaedics","Anaesthesia","Dermatology","General Medicine","General Surgery","Homeopathic Medicine","Neuropsychiatry","Otorhinolaryngology","Med - Cell Biol & Human Anat","Med - Internal Medicine","Preventive Medicine and Hygiene","Roentgenology","X-Ray","School of Medicine","Med - Family Practice","Med - Medical Microbiology","Med - Intrl: Infectious Dis","Med - Psychiatry","Med - Obstetrics & Gynecology","Med - Intrl: Emergency Med","Med - Intrl: Cardiology","Med - Rheumatology (Allergy)","Med - Pathology","Med - Neurology","Med - Neurosurgery","Med - Human Physiology","Med - Surgery","Med - Dermatology"),
       list("Military Science and Tactics","Military Science","Naval Science and Tactics","Naval Science","Air Science","Air Science and Tactics","Aerospace Studies","Naval Architecture","Military Training","MedicoMilitary Science and Tactics","Military and Naval Intelligence","Veterans Affairs"),
       list("Neuropsychology","Neuropsychiatric Institute","Counseling, Clinical, and School Psychology"),
       list("Nursing","Hygiene","Hygiene and Organic Training","Hygiene and Infirmary","Public Health Nursing","Nursing and Social Service"),
       list("Nutrition","Nutritional Sciences","Nutritional Sciences and Toxicology","Nutritional Sci and Tox","Molevular and Biochemical Nutrition","Nutrition Science","Nutrition Science and Toxicology","Nutrition and Home Economics","Household Science","International Nutrition","Dietetics","Dietetics and Subsistence","Foods and Nutrition","Clinical Nutrition","Community Nutrition"),
       list("Optometry","Vision Sciences","Physiological Optics"),
       list("Public Administration"),
       list("Public Health","Public Health and Preventative Medicine","Med - Epidemiology & Prev Med","Med - Public Health Sciences","Med - Community & Intl Health"),
       list("Public Speaking","Speech","Rhetoric"),
       list("Social Work","Social Welfare","Welfare","Community Studies"),
       list("Technology","Applied Biological System Tech"),
       list("Urban Planning","City and Regional Planning","Civic Planning","Planning","Urban Design"),
       list("Veterinary Science","Animal Physiology","Animal Sciences","Veterinary Medicine","Veterinary Microbiology","Veterinary Microbiology and Immunology","VM Medicine and Epidemiology","VM Pathology, Microbiol &Immun","VM Surgical & Radiological Sci","VM Anatomy, Physiol & Cell Bio","VM Medicine and Epidemiology","VM Population Health & Reprod","VM Molecular Biosciences","Vet Microbiology & Immunology"),
       list("Visual Design","Program in Visual Design","Design")
  ),
  list("Physical Education",
       list("Physical Education","Physical Culture","Physical Education for Women","Physical Education for Men","Athletics","Hygiene and Physical Education for Women","Physical Education and Hygiene for Men","Physical Education and Personal Hygiene","Physical Training","Physical Training and Athletics Men","Physical Training and Personal Hygiene")
  ),
  list("Writing",
       list("Writing","College Writing","University Writing Program")
  )
)



#Major list for transcript abbreviations
major_clean<-function(m,single=F,norelax=F,require_spaces=F){
  x<-gsub("^X","",m[1]) #Sometimes the initial X is included; don't know why
  s<-m[2] ; if(is.na(s)) s<-"UCB" #Choose a default
  if(s=="UCSB") x<-gsub("\\s+CS$","",x) #College of Creative Studies gets in the way here
  if(x=="") return("")
  
  letter_relax<-function(x){ #type is x or major; for x, everything is replaced, while for major, it's turned into a regular expression
    combos<-c("[ecg]","[aod0]","[unm]","[yv]","[lit1]","[hb]","[pf]","[s5]","[uj]") ; combos<-c(combos,toupper(combos)) #ECOD was a bit much, I think
    for(l in combos) x<-gsub(l,substr(l,2,2),x)
    x<-gsub(" ","",x)
    return(x)
  }
  
  if(s%in%c("UCSF")){
    if(grepl("x-|ray\\s*t",x,ignore.case = T)) return("X-Ray Technology")
    if(grepl("orthopt|hoptic",x,ignore.case = T)) return("Orthoptic Technique")
    if(grepl("tech",x,ignore.case = T)) return("Medical Technology")
    if(grepl("ysiol|physio",x,ignore.case = T)) return("Physiology") #Before physical therapy
    if(grepl("biophys",x,ignore.case = T)) return("Biophysics")
    if(grepl("assist|resid",x,ignore.case = T)) return("Medicine Resident")
    if(grepl("intern",x,ignore.case = T)) return("Medical Intern")
    if(grepl("adv|stdg",x,ignore.case = T)) return("Advanced Stage Medicine")
    if(grepl("illust|ustrat",x,ignore.case = T)) return("Medical & Biological Illustration")
    if(grepl("pharm",x,ignore.case = T)) if(grepl("tox|icol",x,ignore.case = T)) return("Comparative Pharmacology & Toxicology")
    if(grepl("ceut|tical|m\\s*chem|pharm\\s*c",x,ignore.case = T)) return("Pharmaceutical Chemistry")
    if(grepl("acolog",x,ignore.case = T)) return("Pharmacology")
    if(grepl("hyg|al\\s*h",x,ignore.case = T)) return("Dental Hygiene")
    if(grepl("ortho|odont",x,ignore.case = T)) return("Orthodontics Dentistry")
    if(grepl("perio|odonto",x,ignore.case = T)) return("Periodontology Dentistry")
    if(grepl("ral\\s*sur|l\\s*surg",x,ignore.case = T)) return("Oral Surgery Dentistry") #Careful with oral, in 'Doctoral"
    if(grepl("post[ qJ]*doc",x,ignore.case = T)) return("Post-Doc, Medicine")
    if(grepl("l\\s*Postg",x,ignore.case = T)) return("Dentistry Postgraduate")
    if(grepl("tis|dent",x,ignore.case = T)) return("Dentistry") #Just 'Dental' means dentistry
    if(grepl("pharm|macy",x,ignore.case = T)) return("Pharmacy")
    if(grepl("med|cine",x,ignore.case = T)) return("Medicine")
    if(grepl("nurs|sing",x,ignore.case = T)){
      if(grepl("und|der",x,ignore.case = T)) return("Nursing, Undergraduate")
      if(grepl("post",x,ignore.case = T)) return("Nursing, Post-Graduate")
      if(grepl("limit|ltd",x,ignore.case = T)) return("Nursing Limited")
      if(grepl("grad|uate",x,ignore.case = T)) return("Nursing, Graduate")
      return("Nursing")
    } 
    if(grepl("ioche",x,ignore.case = T)) return("Biochemistry")
    if(grepl("ther|phys|sical",x,ignore.case = T)) return("Physical Therapy")
    if(grepl("exfol|liative",x,ignore.case = T)) return("Exfoliative Cytology")
    if(grepl("cyto",x,ignore.case = T)) return("Cytology")
    if(grepl("Socio",x,ignore.case = T)) return("Sociology")
    if(grepl("endoc|crino",x,ignore.case = T)) return("Endocrinology")
    if(grepl("anat|atomy",x,ignore.case = T)) return("Anatomy")
    if(grepl("(\\s|^)oral|l\\s*bio",x,ignore.case = T)) return("Oral Biology")
    if(grepl("psych|ycholo",x,ignore.case = T)) return("Psychology")
    if(grepl("micro|robio",x,ignore.case = T)) return("Microbiology")
    if(grepl("exp",x,ignore.case = T)) if(grepl("path|holog",x,ignore.case = T)) return("Experimental Pathology")
    if(grepl("speech|hearing",x,ignore.case = T)) return("Speech & Hearing Science")
    if(grepl("intercam|rcampus",x,ignore.case = T)) return("Intercampus Exchange")
    if(grepl("history|health sc",x,ignore.case = T)) return("History of Health Sciences")
  }
  if(s%in%c("UCB","UCSC","Berkeley","UCSB")){
    major<-""
    
    update_major<-function(x,major,l,relax=F,require_spaces=F){
      majorname<-l[2]
      if(relax) l[-2]<-letter_relax(l[-2])
      if(!is.na(l["ic"])) ic<-F else ic<-T
      if(require_spaces){
        l[1]<-paste0("(^|[^A-z])(",l[1],")")
        if(!is.na(l["l3"])) l[3]<-paste0("(",l[3],")($|[^A-z])")
        else if(!is.na(l["l2"])) l[2]<-paste0("(",l[2],")($|[^A-z])")
        else l[1]<-paste0(l[1],"($|[^A-z])") #Don't need extra parentheses on l1; already added in last line
      }
      if(!is.na(l["l3"])) test<-grepl(l[1],x,ignore.case=ic)&grepl(l["l2"],x,ignore.case=ic)&grepl(l["l3"],x,ignore.case=ic) else if(!is.na(l["l2"])) test<-grepl(l[1],x,ignore.case=ic)&grepl(l["l2"],x,ignore.case=ic) else test<-grepl(l[1],x,ignore.case=ic)
      
      if(test) major<-paste0(major,";",majorname)
      return(c(major,test))
    }
    
    relaxes<-F ; if(!norelax) relaxes<-c(relaxes,T)
    for(relax in relaxes){
      if(relax==T) x<-letter_relax(x)
      
      if(nchar(x)<5){
        for(l in list(
          c("^CH E$","Chemical Engineering"),
          c("^COMM$","Communications"),
          c("^ECE$","Electrical and Computer Engineering"),
          c("^FR$","French"),
          c("^GER$","German"),
          c("^HIS$","History"),
          c("^LIT$","Literature"),
          c("^ME$","Mechanical Engineering"),
          c("^MUS$","Music"),
          c("^SOCY?$","Sociology")
        )){ 
          update<-update_major(x,major,l,relax,require_spaces|l[[1]]=="^CH E$") ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
      }
      
      for(l in list(c("PHYS","Physical Education",l2="ED"),c("PHYE","Physical Education"),c("PH","Physical Education",l2="EDUC"),c("EDUC|^ED$","Education"),
                    c("PHYS","Biophysics",l2="BIO"),c("PHYS","Geophysics",l2="GEO"),c("PHYS","Medical Physics",l2="MED"),c("ASTRO","Astrophysics",l2="PHYS"),c("(PH)?YSICS|PHYS(\\s|$)","Physics"), #PHYS isn't enough; physiology, PHYS ED
                    c("POLITIC|POLI|POL\\s*S","Political Science"),c("POL","Political Science",l2="SCI"),c("E?N[GC]\\s*LIT|LTEL","English Literature"),c("FRE\\s*LIT","French Literature"),c("COMP?|^C","Comparative Literature",l2="LIT"),c("RUSSLIT","Russian Literature"),c("LIT","Literature"),c("EN.LIS","English",ignore.case=T),c("ENGL","English"))){ #Note: Is politics its own department? I think not. Omitted; ,c("POLITICS","Politics")
        if(l[[1]]=="LIT"&!s%in%"UCSC") next #Can be too broad otherwise, and atypical
        update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
        if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
      }
      if(grepl("ENG[IT]N",x,ignore.case=T)&major==""){ #Written this way so that English is not mistaken as engineering (and the other majors above have various overlaps with English or things overlapping with English)
        for(l in list(c("E[. ]*E|-E([^N]|$){2}|EL","Electrical Engineering and Computer Science",ic=F),c("BIO","Bioengineering"),c("COMP","Electrical Engineering and Computer Science"),c("CHEM","Chemical Engineering"),c("CIVIL","Civil Engineering"),c("CERAM","Ceramic Engineering"),c("INDUST","Industrial Engineering"),c("M[EA][CE]|-M|HEC","Mechanical Engineering"),c("NUCL","Nuclear Engineering"),c("PET","Petroleum Engineering"),c("NAV","Engineering- Naval Architecture"),c("PHYSIC","Engineering- Physics"),c("PROCESS","Process Engineering"),c("SANIT","Sanitary Engineering"),c("SCI","Engineering Science"),c("TRANSP","Transportation Engineering"),c("AGR","Agricultural Engineering"),c("","Engineering"))){
          update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
      }
      else if(grepl("ENG",x,ignore.case=T)){
        for(l in list(c("E[. ]*E|-E([^N]|$){2}|EL","Electrical Engineering and Computer Science",ic=F),c("BIO","Bioengineering"),c("COMP","Electrical Engineering and Computer Science"),c("CHE?M","Chemical Engineering"),c("CIV","Civil Engineering"),c("CERAM|CRM","Ceramic Engineering"),c("IND","Industrial Engineering"),c("M[EA][CE]|-M|HEC","Mechanical Engineering"),c("NUC","Nuclear Engineering"),c("PET","Petroleum Engineering"),c("NAV","Engineering- Naval Architecture"),c("PHY","Engineering- Physics"),c("PRO?C","Process Engineering"),c("SAN","Sanitary Engineering"),c("SCI","Engineering Science"),c("TRANS|TRS","Transportation Engineering"),c("AGR","Agricultural Engineering"),c("ENGR","Engineering"))){
          update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
      }
      if(!grepl("Engineering",major)){
        for(l in list(c("CHEM","Computational Biochemistry",l2="BIO",l3="COMP"),c("CHEM","Biochemistry",l2="BIOC"),c("BIOC","Biochemistry"),c("CHEM","Agricultural Chemistry",l2="AGR|URAL"),c("CHEM","Chemistry"))){ #Just BIO isn't enough for biochemistry; "Division" often "Divibion"...
          update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
        for(l in list(c("CMPE","Computer Engineering"),c("COMP","Computer Science",l2="SCI"),c("PUTER|CMPS?","Computer Science"))){
          update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
        for(l in list(c("LAND|(^|\\s)LD","Landscape Architecture",l2="Arch"),c("ENVIR","Architecture- Environmental Design",l2="DES"),c("ENVIR","Architecture- Environmental Design",l2="ARCH"),c("ARCH|ENV","Architecture- Environmental Design",l2="DES"),c("DESIGN","Design"),c("NAV","Naval Architecture",l2="Arch"),c("ARCH","Architecture"))){ #In this section to not overlap with naval architecture
          update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
      }
      for(l in list(c("AGR","Agricultural Economics",l2="ECON"),c("HOME","Home Economics",l2="EC"),c("AGR","Agricultural Science",l2="SCI"),c("AGR","Agriculture"),c("POL","Political Economy",l2="EC"),c("ECON","Environmental Economics",l2="ENV"),c("ECON","Economics"))){
        update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
        if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
      }
      for(l in list(c("HIST","History of Art",l2="ART"),c("^ARTH","History of Art"),c("ART","Dramatic Art",l2="DRM"),c("THEAT?(ER)?|THEAART","Theater"),c("HIST|STORY","History"),c("EARTH","Earth Science",l2="SC"),c("(ART|Art)(ST)?","Art",ic=F))){ #HIST sometimes for History
        update<-update_major(x,major,l,relax,(require_spaces|l[[2]]=="Art")) ; major<-update[1]
        if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
      }
      for(l in list(c("BIOLOG","Neurobiology",l2="NEURO"),c("B.O.OG","Biology of Natural Resources",l2="NAT|RES"),c("MICROB.OL","Microbiology"),c("MOLEC|MCELLBI|MCDB","Molecular Biology"),c("B.ORADIO","Bioradiology"),c("INTEG?BI","Integrative Biology"),c("HUM","Human Biology",l2="BIO"),c("PLANT","Plant Biology",l2="BI"),c("BIOL","Psychobiology",l2="PSYC"),c("B.O.OG|BIOL|^BIO$","Biology"),c("PSYCH?","Educational Psychology",l2="ED"),c("PS.CH|PSYCH?O?|^PSY$","Psychology"))){
        update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
        if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
      }
      for(l in list(c("LAT(IN|AM)?","Latin American Studies",l2="AMER|ST[UD]?"),c("LALS|LAST","Latin American Studies"),c("LATI?N","Latin"))){
        update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
        if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
      }
      for(l in list(c("FOOD","Foods and Nutrition",l2="NUTR|RITION"),c("NUTR|NUSCTX","Nutrition"))){
        update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
        if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
      }
      for(l in list(c("BIO","Biostatistics",l2="STAT"),c("STAT","Statistics"))){
        update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
        if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
      }
      ##University-specific departments##
      if(s=="UCSC"){
        for(l in list(c("COWE?L?L","Cowell College"),c("STEV(EN)?(SON)?","Stevenson College"),c("CRO?WN","Crown College"),c("MERR(ILL)?","Merrill College"),c("PORT(ER)?|PRTR","Porter College"),c("KRE?SGE?","Kresge College"),c("OAKE?S","Oakes College"),c("CARSON","Carson College"),c("COLL(EGE)?\\s*[V5]|FIVE","College V"),c("SEVEN|COLL\\s*7"),c("EIGHT|COLL\\s*8|CLEI","College Eight"),c("CLTE","College Ten"),c("CLNI","College Nine"),c("COMM","Community Studies",l2="STU"),c("CMMU","Community Studies"),c("HAVC","History Art & Visual Culture"),c("OCEA","Ocean Studies"),c("HISC","History of Consciousness"),c("HIST|STORY","History of Consciousness",l2="CONSC|USNESS"),c("BIOE","Ecology and Evolutionary Biology"),c("BME","Biomolecular Engineering"),c("ISM","Information Systems Management"),c("SCIC|NSCI","Science Communication"),c("DANM","Theater"),c("ETOX|METX","Environmental Toxicology"),c("LT(BR|SP|AM|MO|WL|PR|FR|GR|GE|EO|EN|IN)|MODS","Literature"),c("SOCS","Social Sciences"),c("SOCD","Social Documentation"),c("^TIM","Technology & Information Management"),c("LAAD","Languages and Applied Linguistics"),c("MARI","Marine Sciences"),c("HUMN","Humanities"),c("LAFR","French"),c("LAGE","German"),c("LAIT","Italian"),c("LAJA","Japanese"),c("LARU","Russian"),c("LASP|LASS","Spanish"),c("LACH","Chinese"),c("AESTH|ESTHET","Aesthetic Studies"),c("^POS$","Political Science"),c("^ARH$","Art History"),c("^ANT$","Anthropology"),c("^PSC$","Psychology"),c("ECN","Economics"),c("JPN","Japanese"))){ #Aesthetic studies last, and maybe should be deleted; rare, and doesn't exist at other schools
          update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
      }
      if(s%in%c("UCB","Berkeley")){
        for(l in list(c("UGBA","Business Administration"),c("(EW)?MBA|PHDBA|BUS A$","Business Administration"),c("VIS","Vision Sciences",l2="SCI"),c("SOC[&]ADM|SAHS","Social and Administrative Health Sciences"),c("BEHS|BIO[&,]ENV","Biomedical and Environmental Health Sciences"),c("IAS","International and Area Studies"),c("^IDS|^ISF|UGIS","Interdisciplinary Studies"),c("C,RESST","Conservation and Resource Studies"),c("MFE","Financial Engineering"),c("PHY OPT","Physiological Optics"),c("A,RESEC","Agricultural and Resource Economics"),c("BIBLIOG","Bibliographic Studies"),c("MINLENG","Mineral Engineering"),c("ETH GRP","Ethnic Studies"),c("MICROBI?","Microbiology"),c("GWS","Women's Studies"),c("GPP","Global Poverty and Practice"),c("PL PATH","Plant Pathology"),c("HARTSCI","Health Arts and Sciences"),c("SUBJ A","College Writing"),c("ED-SCSE","Education"),c("FOR,CON","Forestry and Conservation"),c("DIG-SOC","Sociology"),c("AST","Applied Science and Technology"),c("SOC,ADM","Public Health"),c("PLSOBIO","Plant and Soil Biology"),c("CUNEIF","Cuneiform"),c("RES SCI","Natural Sciences"))){
          update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
      }
      if(s%in%c("UCSB")){
        for(l in list(c("EEMB","Ecology, Evolution, and Marine Biology"),c("FAMST","Film and Media Studies"),c("CN?CSP","Counseling, Clinical, and School Psychology"),c("SPCH|SHS","Speech and Hearing Science"),c("EACS","East Asian Languages"),c("PA[ 1-]|ES[ 1S-]|ERG","Physical Education"),c("^MS","Military Science"),c("TMP","Technology Management"),c("^ED\\s+","Education"),c("LAIS","Latin American Studies"),c("GPS","Global Studies"))){
          update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
          if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
        }
      }
      ###################################
      for(l in list(c("AEROSPC","Aerospace Studies"),c("AIR","Air Science",l2="SCI"),c("AFRICA[MN]|AFRO.*AM","African-American Studies"),c("^AM(ER)?","American Studies",l2="ST[UD]?"),c("ANATOMY","Anatomy"),c("ANTHR?O?","Anthropology"),c("ARAB(IC)?","Arabic"),c("ASAMST|ASIANST","Asian American Studies"),c("^AS\\s*AM","Asian American Studies"),c("AMS$","Applied Mathematics and Statistics"),c("ASTRO?([MN]|AP)?","Astronomy"),
                    c("BACT","Bacteriology"),c("^BL","Black Studies",l2="ST"),c("BOT(ANY)?","Botany"),c("BUDDH|BUDDSTD","Buddhist Studies"),c("[BH][UI]S","Business Administration",l2="ADM"),
                    c("CATALAN","Catalan"),c("CELTIC","Celtic Studies"),c("CHICANO","Chicano Studies"),c("^CH","Chicano Studies",l2="ST"),c("CHILD","Child Development"),c("CHIN(ESE)?","Chinese"),c("CLASS(IC)?","Classics"),c("COG","Cognitive Science",l2="SCI"),c("(COL)?WRIT","College Writing"),c("LTCR","Creative Writing"),c("CRIMIN","Criminology"),c("C\\s*Y|CITY|REG","City and Regional Planning",l2="PLAN"),
                    c("DANCE|^DA$","Dance"),c("DATA","Data Science",l2="SCI"),c("DEMOG","Demography"),c("DEVP","Development"),c("DEV","Development Studies",l2="ST[UD]"),c("DIETET","Dietetics"),c("DUTCH","Dutch"),
                    c("EAEUR","Eastern European Studies",l2="ST"),c("EA","East Asian Languages",l2="LANG"),c("^EE$","Electrical Engineering"),c("EGYPT","Egyptian"),c("ENE","Energy and Resources",l2="RES"),c("ENGSECL","English as a Second Language"),c("ENTOM","Entomology"),c("ENVIR|VIRON","Environmental Planning",l2="PLAN|NING"),c("ENV|VIRON","Environmental Science",l2="SCI|\\sS$"),c("^(EPS|EART)","Earth and Planetary Sciences"),c("ESP?M","Environmental Science"),c("ENVI|VIRON","Environmental Studies",l2="STU"),c("ENVS","Environmental Studies"),c("ETH","Ethnic Studies",l2="ST[UD]"),
                    c("FE?MST","Feminist Studies"),c("FI?LM(ST)?","Film"),c("FOLK","Folklore"),c("FOREST","Forestry"),c("^FREN?(CH)?","French"),
                    c("GEN","General Curriculum",l2="CURR|RRIC"),c("GENETI","Genetics"),c("GEOL","Geology"),c("GEOG","Geography"),c("GERM(AN)?","German"),c("GLOBA?L","Global Studies"),c("GOVT","Government"),c("GREEK?","Greek"),
                    c("HMED","Health and Medical Sciences",l2="SCI"),c("HEBR?(EW)?","Hebrew"),c("HIN-URD|HI?NDI","Hindi"),c("HUMAN(IT)?","Humanities"),
                    c("INFO","Information Science","SCI"),c("ITA?L(IAN)?","Italian"),
                    c("JAPA?N","Japanese"),c("JEWISH","Jewish Studies"),c("JOURN","Journalism"),
                    c("KOR(EAN|$)","Korean"),
                    c("LAW","Law"),c("LEGAL|^LG","Legal Studies",l2="ST"),c("LGBT","LGBT Studies"),c("LIBR","Library Studies"),c("LING(UI)?","Linguistics"),c("LOGIC(\\s|$)","Logic and Methodology"),
                    c("MALAY(/I)?","Indonesian"),c("MASSCOM","Mass Communications"),c("MAT","Material Science",l2="SCI"),c("MATRL","Material Science"),c("MATH|HEMATI","Mathematics"),c("MED","Media Studies",l2="ST"),c("METAL","Metallurgy"),c("MIL","Military Science",l2="SCI|AFF"),c("MINING","Mining"),c("MOD","Social Thought",l2="SOC"),c("MUSI?C","Music"),
                    c("NAV","Naval Science",l2="SCI"),c("NATAMST","Native American Studies"),c("NAT","Natural Resources",l2="RES"),c("NAT","Natural Sciences",l2="SCI|\\sS$"),c("[NM]\\s*E\\s*ST[UD]|[N]R\\s*E","Near East Studies",l2="ST[UD]"),c("NEUROSC","Neuroscience"),
                    c("OPTOM","Optometry"),c("(^|\\s)OR|ORIENT","Oriental Languages",l2="LANG"),
                    c("PACS","Peace and Conflict Studies"),c("PALEON","Paleontology"),c("PERSIAN","Persian"),c("PHARM","Pharmacology"),c("PHIL(OS)?","Philosophy"),c("PHYSIO","Physiology"),c("PLANT","Plant Pathology",l2="PATH"),c("PORT(UG|$)","Portuguese"),c("PU?B","Public Administration",l2="ADM"),c("PU?B","Public Affairs",l2="AFF"),c("PU?B","Public Policy",l2="POL"),c("PU?B","Public Health",l2="HEALTH|HLTH"),c("PUNJABI","Punjabi"),
                    c("RANGE|RG","Range Management",l2="MANAGE|MNG"),c("RAZA","La Raza Studies"),c("REL|IGIOUS|^RG","Religious Studies",l2="STU?D?"),c("RHETOR","Rhetoric"),c("RUSS(IAN)?","Russian"),
                    c("SANSKR(IT)?","Sanskrit"),c("SCANDIN","Scandinavian"),c("S\\s*AS(IA)?N","South Asian Studies"),c("SE\\s*AS(IA)?N","South-East Asian Studies"),c("SLAV(IC|$)","Slavic Studies"),c("SOC","Social Sciences",l2="SCI"),c("SOC","Social Welfare",l2="WEL"),c("SOCIOL|SOC.?.?OLO","Sociology"),c("SOIL","Soil Science",l2="SCI"),c("SPA?N(ISH)?|SPSS","Spanish"),c("SPEECH","Speech"),c("SWAH","Swahili"),
                    c("TAGALO?G|FILIPN","Tagalog"),c("TAMIL","Tamil"),c("^THAI","Thai"),c("TH(EA)?TE?R","Theater"),c("TURKISH","Turkish"),
                    c("UNDECL|DECLARED","Undeclared"),
                    c("VIETNAM|VIETNMS","Vietnamese"),c("VIS","Visual Studies",l2="ST[UD]"),c("VIS","Visual Design",l2="DES"),
                    c("WOOD","Wood Science and Technology",l2="SCI"),c("WOMEN|WM","Women's Studies",l2="STU?D?"),c("WLD","World Civilizations",l2="CIV"),
                    c("YIDDISH","Yiddish"),
                    c("ZOOL","Zoology"))){ 
        update<-update_major(x,major,l,relax,require_spaces) ; major<-update[1]
        if(update[2]){ ; if(single) return(gsub("^[; ]*","",major)) ; break ; }
      }
      
      if(major!="") return(gsub("^[; ]*","",major))
    }
    
    if(grepl("L[&]?[Ss]",x)) return("LS") else return("")
  }
  return("")
}

