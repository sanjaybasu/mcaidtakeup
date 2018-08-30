rm(list=ls())
library(tidyverse)
library(haven)
setwd("~/Data/NAMCS")
namcs12 = read_stata("namcs2012-stata.dta")
namcs13 = read_stata("namcs2013-stata.dta")
namcs14 = read_stata("namcs2014-stata.dta")
namcs15 = read_stata("namcs2015-stata.dta")

namcs12 = namcs12[,259:416]
namcs13 = namcs13[,260:409]
namcs14 = namcs14[,362:496]
namcs15 = namcs15[,341:489]

namcs = bind_rows(namcs12,namcs13,namcs14,namcs15, .id = "yr") 
namcs = namcs %>%
  mutate(fips = as.numeric(FIPSSTOFF))  %>%
  distinct(PHYCODE, .keep_all = T)
dim(namcs)
table(namcs$yr)
namcs = namcs %>%
  filter(ACEPTNEW==1) %>%
  mutate(NMEDCAID=(NMEDCAID==1)) 
dim(namcs)
table(namcs$yr)
table(namcs$yr,namcs$NMEDCAID)

accepts = namcs$NMEDCAID==T
pcp = (namcs$SPECR_14<5)|(namcs$SPECR<5)
psych = (namcs$SPECR_14==11)|(namcs$SPECR==11)
spec = ((namcs$SPECR_14>=5)|(namcs$SPECR>=5))
specnopsych = (spec==1)&(psych!=1)

table(pcp,accepts)
table(specnopsych,accepts)
table(psych,accepts)

table(namcs$OWNSR,accepts)

homenurse = (namcs$NHVISR==1)|(namcs$HOMVISR==1)
table(homenurse,accepts)

phonemail = (namcs$TELCONR=1)|(namcs$ECONR==1)
table(phonemail,accepts)

appttime = (namcs$SASDAPPT*(namcs$SASDAPPT>=0))+(namcs$SDAPPTPCT*(namcs$SDAPPTPCT>=0))
table(appttime[accepts==1]>0)
table(appttime[accepts==0]>0)

emr = (namcs$EMEDREC==1)
table(emr,accepts)


ebill = (namcs$EBILLANY)|(namcs$EBILLREC)
table(ebill,accepts)


inc = (namcs$MUSTAGE1==1)|(namcs$MUINC==1)
table(inc,accepts)


table(namcs$ECQMR==1,accepts)

privatemore = (namcs$PRPRVTR>=2)
table(privatemore,accepts)

mcaremore = (namcs$PRMCARER>=2)
table(mcaremore,accepts)

mcaidmore = (namcs$PRMAIDR>=2)
table(mcaidmore,accepts)


propmanaged = (namcs$PRMANR>=2)
table(propmanaged,accepts)


propfixed = (namcs$PHYSCOMP==1)
table(propfixed,accepts)

table(namcs$REGIONOFF,accepts)


table(namcs$MSA,accepts)



namcstrain=namcs[namcs$yr==1,]
namcstest=namcs[namcs$yr>1,]


load("~/Data/ahrf/ahrf_county")
ahrf_countytrain = ahrf_county %>%
  mutate(fips=as.numeric(F00011)) %>%
  select(fips,F00008,ends_with("-10"),ends_with("-11"),ends_with("-12")) 
namcstrain = inner_join(namcstrain,ahrf_countytrain,by=c("fips","fips")) 


ahrf_countytest= ahrf_county %>%
  mutate(fips=as.numeric(F00011)) %>%
  select(fips,F00008,ends_with("-13"),ends_with("-14"),ends_with("-15"))
namcstest = inner_join(namcstest,ahrf_countytrain,by=c("fips","fips")) 


caidtocare12 <- read_csv("~/Data/ahrf/data-raw/county/caidtocare12.csv")
caidtocare1315 <- read_csv("~/Data/ahrf/data-raw/county/caidtocare1315.csv")

caidtocare12 = caidtocare12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))

namcstrain = inner_join(namcstrain,caidtocare12,by=c("FIPSSTOFF","FIPSSTOFF")) 


caidtocare1315 = caidtocare1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))


namcstest = inner_join(namcstest,caidtocare1315,by=c("FIPSSTOFF","FIPSSTOFF")) 


ratio1312 = left_join(caidtocare12,caidtocare1315,by=c("Region","Region"))
ratio1312 = ratio1312 %>%
  mutate(ratio1312 = caidtocareall.y/caidtocareall.x) %>%
  select(Region, ratio1312)

medicaidtoffs12 <- read_csv("~/Data/ahrf/data-raw/county/medicaidtoffs12.csv")
trans = left_join(medicaidtoffs12,ratio1312,by=c("Region","Region"))
medicaidtoffs1315 = trans %>%
  mutate(caidffstopriv=caidffstopriv*ratio1312,
         caidffstopriver=caidffstopriver*ratio1312,
         caidffstoprivhosp = caidffstoprivhosp*ratio1312,
         caidffstoprivoffice = caidffstoprivoffice*ratio1312,
         caidmctopriv = caidmctopriv*ratio1312,
         caidmctopriver = caidmctopriver*ratio1312,
         caidmctoprivhosp = caidmctoprivhosp*ratio1312,
         caidmctoprivoffice = caidmctoprivoffice*ratio1312) %>%
  select(Region, caidffstopriv, caidffstopriver,  caidffstoprivhosp, caidffstoprivoffice,
         caidmctopriv,caidmctopriver,caidmctoprivhosp,caidmctoprivoffice)

medicaidtoffs12 = medicaidtoffs12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))

medicaidtoffs1315 = medicaidtoffs1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))

namcstrain = inner_join(namcstrain,medicaidtoffs12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,medicaidtoffs1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

smoking12 <- read_csv("~/Data/ahrf/data-raw/county/smoking12.csv")
smoking1315 <- read_csv("~/Data/ahrf/data-raw/county/smoking1315.csv")

obesity12 <- read_csv("~/Data/ahrf/data-raw/county/obesity12.csv")
obesity1315 <- read_csv("~/Data/ahrf/data-raw/county/obesity1315.csv")

disability12 <- read_csv("~/Data/ahrf/data-raw/county/disability12.csv")
disability1315 <- read_csv("~/Data/ahrf/data-raw/county/disability1315.csv")

mental12 <- read_csv("~/Data/ahrf/data-raw/county/mental12.csv")
mental1315 <- read_csv("~/Data/ahrf/data-raw/county/mental1315.csv")

smoking12$Region = smoking12$LocationDesc
smoking1315$Region = smoking1315$LocationDesc

obesity12$Region = obesity12$LocationDesc
obesity1315$Region = obesity1315$LocationDesc

disability12$Region = disability12$LocationDesc
disability1315$Region = disability1315$LocationDesc

mental12$Region = mental12$LocationDesc
mental1315$Region = mental1315$LocationDesc




smoking12 = smoking12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))


smoking1315 = smoking1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))



obesity12 = obesity12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))



obesity1315 = obesity1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))



disability12 = disability12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))




disability1315 = disability1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))



mental12 = mental12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))




mental1315 = mental1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming"))


namcstrain = inner_join(namcstrain,smoking12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,smoking1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

namcstrain = inner_join(namcstrain,obesity12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,obesity1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

namcstrain = inner_join(namcstrain,disability12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,disability1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

namcstrain = inner_join(namcstrain,mental12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,mental1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

 
cmf <- read_delim("~/Data/ahrf/data-raw/county/Compressed Mortality, 1999-2016(1).txt", "\t", escape_double = FALSE, trim_ws = TRUE)

cancer12 = cmf[cmf$Year==2012 & cmf$`ICD Chapter`=="Neoplasms",]
cancer12$Region = cancer12$State  
cancer12$cancer = cancer12$`Age Adjusted Rate`

cancer12 = cancer12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, cancer)

cancer1315 = cmf[cmf$Year==2014 & cmf$`ICD Chapter`=="Neoplasms",]
cancer1315$Region = cancer1315$State  
cancer1315$cancer = cancer1315$`Age Adjusted Rate`

cancer1315 = cancer1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, cancer)

ihd12 = cmf[cmf$Year==2012 & cmf$`ICD Chapter`=="Diseases of the circulatory system",]
ihd12$Region = ihd12$State  
ihd12$ihd = ihd12$`Age Adjusted Rate`

ihd12 = ihd12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, ihd)

ihd1315 = cmf[cmf$Year==2014 & cmf$`ICD Chapter`=="Neoplasms",]
ihd1315$Region = ihd1315$State  
ihd1315$ihd = ihd1315$`Age Adjusted Rate`

ihd1315 = ihd1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, ihd)

mentdeath12 = cmf[cmf$Year==2012 & cmf$`ICD Chapter`=="Mental and behavioural disorders",]
mentdeath12$Region = mentdeath12$State  
mentdeath12$mentdeath = mentdeath12$`Age Adjusted Rate`

mentdeath12 = mentdeath12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, mentdeath)

mentdeath1315 = cmf[cmf$Year==2014 & cmf$`ICD Chapter`=="Neoplasms",]
mentdeath1315$Region = mentdeath1315$State  
mentdeath1315$mentdeath = mentdeath1315$`Age Adjusted Rate`

mentdeath1315 = mentdeath1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, mentdeath)



namcstrain = inner_join(namcstrain,cancer12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,cancer1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

namcstrain = inner_join(namcstrain,ihd12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,ihd1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

namcstrain = inner_join(namcstrain,mentdeath12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,mentdeath1315,by=c("FIPSSTOFF","FIPSSTOFF")) 


caidenrollment12 <- read_csv("~/Data/ahrf/data-raw/county/caidenrollment12.csv")
caidenrollment1315 <- read_csv("~/Data/ahrf/data-raw/county/caidenrollment1315.csv")

caidmanagedperc12 <- read_csv("~/Data/ahrf/data-raw/county/caidmanagedperc12.csv")
caidmanagedperc1315 <- read_csv("~/Data/ahrf/data-raw/county/caidmanagedperc1315.csv")

caidexp12 <- read_csv("~/Data/ahrf/data-raw/county/caidexp12.csv")
caidexp1315 <- read_csv("~/Data/ahrf/data-raw/county/caidexp1315.csv")

medicaidfeeadj <- read_csv("~/Data/ahrf/data-raw/county/medicaidfeeadj.csv")

caidenrollment12 = caidenrollment12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, caidenrollment)
caidenrollment1315 = caidenrollment1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, caidenrollment)
caidexp12 = caidexp12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, mcaidexpansion)
caidexp1315 = caidexp1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, mcaidexpansion)
caidmanagedperc12 = caidmanagedperc12 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, caidmanagedperc)
caidmanagedperc1315 = caidmanagedperc1315 %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) %>%
  select(FIPSSTOFF, caidmanagedperc)
medicaidfeeadj = medicaidfeeadj %>%
  mutate(FIPSSTOFF = 1*(Region=="Alabama")+ 
           2*(Region=="Alaska")+
           4*(Region=="Arizona")+
           5*(Region=="Arkansas")+
           6*(Region=="California")+
           8*(Region=="Colorado")+
           9*(Region=="Connecticut")+
           10*(Region=="Delaware")+
           11*(Region=="District of Columbia")+
           12*(Region=="Florida")+
           13*(Region=="Georgia")+
           15*(Region=="Hawaii")+
           16*(Region=="Idaho")+
           17*(Region=="Illinois")+
           18*(Region=="Indiana")+
           19*(Region=="Iowa")+
           20*(Region=="Kansas")+
           21*(Region=="Kentucky")+
           22*(Region=="Louisiana")+
           23*(Region=="Maine")+
           24*(Region=="Maryland")+
           25*(Region=="Massachusetts")+
           26*(Region=="Michigan")+
           27*(Region=="Minnesota")+
           28*(Region=="Mississippi")+
           29*(Region=="Missouri")+
           30*(Region=="Montana")+
           31*(Region=="Nebraska")+
           32*(Region=="Nevada")+
           33*(Region=="New Hampshire")+
           34*(Region=="New Jersey")+
           35*(Region=="New Mexico")+
           36*(Region=="New York")+
           37*(Region=="North Carolina")+
           38*(Region=="North Dakota")+
           39*(Region=="Ohio")+
           40*(Region=="Oklahoma")+
           41*(Region=="Oregon")+
           42*(Region=="Pennsylvania")+
           44*(Region=="Rhode Island")+
           45*(Region=="South Carolina")+
           46*(Region=="South Dakota")+
           47*(Region=="Tennessee")+
           48*(Region=="Texas")+
           49*(Region=="Utah")+
           50*(Region=="Vermont")+
           51*(Region=="Virginia")+
           53*(Region=="Washington")+
           54*(Region=="West Virginia")+
           55*(Region=="Wisconsin")+
           56*(Region=="Wyoming")) 


namcstrain = inner_join(namcstrain,caidenrollment12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,caidenrollment1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

namcstrain = inner_join(namcstrain,caidexp12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,caidexp1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

namcstrain = inner_join(namcstrain,caidmanagedperc12,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,caidmanagedperc1315,by=c("FIPSSTOFF","FIPSSTOFF")) 

namcstrain = inner_join(namcstrain,medicaidfeeadj,by=c("FIPSSTOFF","FIPSSTOFF")) 
namcstest = inner_join(namcstest,medicaidfeeadj,by=c("FIPSSTOFF","FIPSSTOFF")) 


namcstrain = namcstrain %>%
  distinct(PHYCODE, .keep_all = T)
namcstest = namcstest %>%
  distinct(PHYCODE, .keep_all = T)


save(namcstrain,file="namcstrain")
save(namcstest,file="namcstest")



