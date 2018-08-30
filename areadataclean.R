#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("labelled")
#install.packages("plm")
#install.packages("reshape")
#install.packages("stargazer")
#install.packages("httr")
#install.packages("tableone")
#install.packages("mctest)
#install.packages("olsrr")
#install.packages("lme4")

rm(list=ls())

library(devtools)
library(tidyverse)
library(readxl)
library(labelled)
library(httr)

setwd("~/Data/ahrf")


# Download raw AHRF files ------------------------------------------------------
# See https://github.com/jjchern/ahrf/blob/master/data-raw/prep_county.R

url = "https://datawarehouse.hrsa.gov/DataDownload/AHRF/AHRF_2016-2017.ZIP"
fil_zip = tempfile(fileext = ".zip")

if(!file.exists("data-raw/county/ahrf2017.asc")) {
  download.file(url, fil_zip)
  dir.create("data-raw/county")
  unzip(fil_zip, exdir = "data-raw/county", junkpaths = TRUE)
}
list.files("data-raw/county")

raw_src = "data-raw/county/ahrf2017.asc" # Raw data
dic_src = "data-raw/county/ahrf2016-17.sas" # SAS dictionary file
doc_src = "data-raw/county/AHRF 2016-2017 Technical Documentation.xlsx"


# Find out the line for the first field: F00001 ---------------------------

read_excel(doc_src) %>%
  pull(X__1) %>%
  grepl("F00001", .) %>%
  which() -> bgn_line
bgn_line

# Prepare the layout file -------------------------------------------------

read_excel(doc_src,
           col_names = c("field", "col_col", "year_of_data", "var_label",
                         "characteristics", "source", "date_on"),
           skip = bgn_line) %>%
  filter(grepl("^F[0-9]", field)) %>%
  separate(col_col, c("col_start", "col_end")) %>%
  mutate_at(c("col_start", "col_end"), as.integer) -> ahrf_county_layout
ahrf_county_layout

# Prepare the county AHRF file --------------------------------------------

read_fwf(file = raw_src,
         col_positions = fwf_positions(start = ahrf_county_layout$col_start,
                                       end = ahrf_county_layout$col_end,
                                       col_names = ahrf_county_layout$field)) -> ahrf_county
ahrf_county
 

# Delete raw data as it’s too large ---------------------------------------

unlink(raw_src)

# Download raw IHME LE files ---------------------------------------
# See https://github.com/BuzzFeedNews/2017-05-us-health-care/blob/master/index.Rmd

url = "http://ghdx.healthdata.org/sites/default/files/record-attached-files/IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_XLSX.zip"
fil_zip = tempfile(fileext = ".zip")

{
  download.file(url, fil_zip)
  dir.create("data-raw/county")
  unzip(fil_zip, exdir = "data-raw/county", junkpaths = TRUE)
}
list.files("data-raw/county")

raw_src = "data-raw/county/IHME_USA_COUNTY_LE_MORTALITY_RISK_1980_2014_NATIONAL_Y2017M05D08.xlsx" # Raw data

life_expect_us <- read_excel(raw_src, sheet = 1, skip = 3, col_names = FALSE) %>%
  select(1:10)
names(life_expect_us) <- c("place","fips","le_1980","le_1985","le_1990","le_1995","le_2000","le_2005","le_2010","le_2014")

# Clean the FIPS codes, adding zeros where necessary ---------------------------------------
life_expect_us <- life_expect_us %>%
  mutate(fips = ifelse(nchar(fips)==4|nchar(fips)==1,paste0("0",fips),fips))

# Extract data for counties only ---------------------------------------
life_expect_counties <- life_expect_us %>%
  filter(grepl(",",place)) %>%
  separate(place, into=c("place","state"), sep = ", ")

# Select columns with life expectancy data and convert from text string with confidence intervals to numbers ---------------------------------------
counties_clean <- life_expect_counties %>%
  select(4:11) %>%
  mutate_all(funs(as.numeric(substring(.,1,5))))

# Create data frame with names and abbreviations for states and District of Columbia  ---------------------------------------
states <- data_frame(state.name,state.abb)
names(states) <- c("state","abb")
dc <- data_frame("District of Columbia", "DC")
names(dc) <- c("state","abb")
states <- bind_rows(states,dc) 

# Join that to the counties' names, states, and FIPS codes ---------------------------------------
states_names <- life_expect_counties %>%
  select(1:3) %>%
  inner_join(states) %>%
  mutate(place = paste0(place,", ",abb))

# Recombine AHRF with processed life expectancy data ---------------------------------------
life_expect_counties <- bind_cols(states_names,counties_clean) 


# Linearly impute 2015 LE ----
life_expect_counties$le_2015 = life_expect_counties$le_2014 + 1/4*(life_expect_counties$le_2014-life_expect_counties$le_2010)

# keep relevant years ----
life_expect_counties <- life_expect_counties %>%
  select(fips, le_2005, le_2010, le_2015)

# Save LE  ----------------------------------------------------------------

save(life_expect_counties,file="life_expect_counties")



# Download raw CHRD files ---------------------------------------
chrd <- read_csv("http://www.countyhealthrankings.org/sites/default/files/CHR_TRENDS_CSV_2018.csv")
chrd <- chrd %>%
  separate(yearspan, into=c("time","yearend"), sep = "-") %>%
  unite(fips,"statecode","countycode",sep="") %>%
  select(c(1,3,4,9)) %>%
  mutate(time = as.integer(time)) %>%
  #  filter(time == 2000 | time==2005 | time==2010) %>%
  unite(var_year,"measurename","time") %>%
  spread(var_year,rawvalue)  %>%
  select(c("fips","Adult obesity_2005","Adult obesity_2010","Adult obesity_2013",
           "Air pollution - particulate matter_2005", "Air pollution - particulate matter_2010", "Air pollution - particulate matter_2012",
           "Alcohol-impaired driving deaths_2008","Alcohol-impaired driving deaths_2010","Alcohol-impaired driving deaths_2015",
           "Premature death_2005","Premature death_2010","Premature death_2014",
           "Preventable hospital stays_2006","Preventable hospital stays_2010","Preventable hospital stays_2015")) %>%
  mutate(fipscode = as.numeric(fips),
         obese_2005 = `Adult obesity_2005`*100,
         obese_2010 = as.numeric(`Adult obesity_2010`)*100,
         obese_2015 = obese_2010+5/3*(as.numeric(`Adult obesity_2013`)*100-as.numeric(`Adult obesity_2010`)*100),
         airpol_2005 = as.numeric(`Air pollution - particulate matter_2005`),
         airpol_2010 = as.numeric(`Air pollution - particulate matter_2010`),
         airpol_2015 = airpol_2010 + 5/2*(as.numeric(`Air pollution - particulate matter_2012`) - as.numeric(`Air pollution - particulate matter_2010`)),
         alcdrivemort_2005 = as.numeric(`Alcohol-impaired driving deaths_2008`)-5/2*(as.numeric(`Alcohol-impaired driving deaths_2010`)-as.numeric(`Alcohol-impaired driving deaths_2008`)),
         alcdrivemort_2010 = as.numeric(`Alcohol-impaired driving deaths_2010`),
         alcdrivemort_2015 = as.numeric(`Alcohol-impaired driving deaths_2015`),
         premort_2005 = as.numeric(`Premature death_2005`),
         premort_2010 = as.numeric(`Premature death_2010`),
         premort_2015 = premort_2010 + 5/4*(as.numeric(`Premature death_2014`)-as.numeric(`Premature death_2010`)),
         prevhosp_2005 = as.numeric(`Preventable hospital stays_2006`)-1/4*(as.numeric(`Preventable hospital stays_2010`)-as.numeric(`Preventable hospital stays_2006`)),
         prevhosp_2010 = as.numeric(`Preventable hospital stays_2010`),
         prevhosp_2015 = as.numeric(`Preventable hospital stays_2015`)) %>%
  select(1,17:32)

# add in tobacco smoking data----

chrd_tob_15 <- read_csv("http://www.countyhealthrankings.org/sites/default/files/2017CHR_CSV_Analytic_Data.csv", skip = 1)
chrd_tob_15 <- chrd_tob_15 %>%
  rename(tob_2015 = "measure_9_value") %>%
  mutate(tob_2015 = as.numeric(tob_2015*100)) %>%
  select(c(fipscode,tob_2015))
chrd <- left_join(chrd,chrd_tob_15, by=c("fipscode"="fipscode"))



url <- "http://www.countyhealthrankings.org/sites/default/files/2012%20County%20Health%20Rankings%20National%20Data_v2.xls"
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
chrd_tob_10 <- read_excel(tf, "Ranked Measure Data",skip = 1)
chrd_tob_10 <- chrd_tob_10 %>%
  rename(tob_10 = "% Smokers") %>%
  mutate(tob_2010 = as.numeric(tob_10),
         tob_2005 = as.numeric(tob_10),
         fipscode = as.integer(FIPS))%>%
  select(c(fipscode,tob_2005,tob_2010)) 
chrd <- left_join(chrd,chrd_tob_10, by=c("fipscode"="fipscode"))
chrd[chrd<0]=0

# Save CHRD  ----------------------------------------------------------------

save(chrd,file="chrd")


# Download urban/rural data ----
#https://www.cdc.gov/nchs/data/oae/NCHSUrbruralFileDocumentation.pdf
library(readr) 
# url = ("ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/Datasets/OAE/urbanrural/NCHSURCodes2013.txt")
# ctyurb <- read_table2(url, col.names = FALSE)
ctyurb <- read_table2("data-raw/county/NCHSURCodes2013.txt",col_names = FALSE)
ctyurb <- ctyurb %>%
  rename(fips = X1,
         urb_2015 = X9,
         urb_2005 = X10) %>%
  select(fips,urb_2005,urb_2015)
ctyurb$urb_2005[is.na(ctyurb$urb_2005)]=ctyurb$urb_2015[is.na(ctyurb$urb_2005)]
ctyurb$urb_2010 = round((as.integer(ctyurb$urb_2005)+as.integer(ctyurb$urb_2015))/2)

ctyurb$urb_2005[ctyurb$urb_2005>6]=""
ctyurb$urb_2010[ctyurb$urb_2010>6]=""
ctyurb$urb_2015[ctyurb$urb_2015>6]=""

ctyurb <- ctyurb %>%
  mutate(urb_2005 = as.numeric(urb_2005),
         urb_2015 = as.numeric(urb_2015),
         urb_2010 = as.numeric(urb_2010)) 
save(ctyurb,file="ctyurb")


