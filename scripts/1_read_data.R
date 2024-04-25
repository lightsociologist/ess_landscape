#579,926 articles searching on WOS (environment, climate change in the social science data base
#restricted to articles)

#Here we try to create a more refined dataset due to polysemy with environment

#Following principles for reducing:

#1. Keep all articles that are in the environmental sciences or environmental studies
#list from Web of Science

#2. Keep all articles that CITE journals from the Web of Science Lists

#3. Keep all articles in the environmental social science journal wiki

#4. Keep all articles that CITE journals from the wiki

#5. Reduce by articles that are from public, environmental, and 
#occupational health WC subject field that do not have any of the other
#relevant fields or citations to any of the WoS or Wiki journals

library(here)
source(here("scripts/check_packages.R"))

#First we load the data...we can eventually just load from a dataframe...raw data are now stored in data folder 
#Google Drive in project folder. Local folder is small portion of data.

file.list <- list.files(path="PATH HERE",
                       pattern='*.txt', recursive = TRUE, full.names = TRUE)

df.list3 <- lapply(file.list, read.delim, sep="\t", quote="")

wos_tot <- as.data.frame(data.table::rbindlist(df.list3))

wos_tot <- filter(wos_tot, PT=="J")

wos_tot$naa <- as.numeric(is.na(wos_tot$PY))

wos_tot <- filter(wos_tot, naa!=1 | DT!="Article")

wos_tot$year <- wos_tot$PY

wos_tot <- wos_tot %>% mutate(year = replace_na(year, 2022))

wos_use <-
  distinct(wos_tot, TI, AB, .keep_all=TRUE)

table(wos_use$year)

wos_use2 <- wos_use %>% drop_na(c("CR")) 

nmax <- max(stringr::str_count(wos_use2$CR, "\\;")) + 1

wos_use2 <- separate(wos_use2, CR, paste0("cite", seq_len(nmax)), sep = "\\;", fill = "right")

#saveRDS(wos_use2, "data/wos_use.rds")

#####START HERE#######

#This is hard coded due to size.

wos_use2 <- readRDS("~PATH HERE/wos_use.rds")

wos_use3 <- wos_use2 %>% select(UT, SO, J9, JI, PY, WC, cite1:cite500)

#Now we make things long by each cite in the first 500 cites so each record
#is a citation and an article so that we can grab cited journals

#This only works in chunks

#pre-1991 group

ws_pre1990 <- wos_use3 %>% filter(PY<1991)

cit_long1990 <- ws_pre1990 %>% pivot_longer(cols=starts_with("cite"),
                                     names_to = "cite_num",
                                     names_prefix = "cite",
                                     values_to  = "citation",
                                     values_drop_na = TRUE)

cit_long1990$cjrnl <- unlist(lapply(strsplit(cit_long1990$citation, ',', fixed = TRUE), '[', 3))

#1991-2000

ws_pre2000 <- wos_use3 %>% filter(PY<2001 & PY>1990)

cit_long2000 <- ws_pre2000 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

cit_long2000$cjrnl <- unlist(lapply(strsplit(cit_long2000$citation, ',', fixed = TRUE), '[', 3))

#2001-2010

ws_pre2010 <- wos_use3 %>% filter(PY<2011 & PY>2000)

cit_long2010 <- ws_pre2010 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

cit_long2010$cjrnl <- unlist(lapply(strsplit(cit_long2010$citation, ',', fixed = TRUE), '[', 3))

#2011-2015

ws_pre2015 <- wos_use3 %>% filter(PY<2016 & PY>2010)

cit_long2015 <- ws_pre2015 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

cit_long2015$cjrnl <- unlist(lapply(strsplit(cit_long2015$citation, ',', fixed = TRUE), '[', 3))

#2016-2017

ws_pre2017 <- wos_use3 %>% filter(PY<2018 & PY>2015)

cit_long2017 <- ws_pre2017 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

cit_long2017$cjrnl <- unlist(lapply(strsplit(cit_long2017$citation, ',', fixed = TRUE), '[', 3))

#2018-2019

ws_pre2020 <- wos_use3 %>% filter(PY<2020 & PY>2017)

cit_long2020 <- ws_pre2020 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

cit_long2020$cjrnl <- unlist(lapply(strsplit(cit_long2020$citation, ',', fixed = TRUE), '[', 3))

#2020

ws_pre2021 <- wos_use3 %>% filter(PY<2021 & PY>2019)

cit_long2021 <- ws_pre2021 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

cit_long2021$cjrnl <- unlist(lapply(strsplit(cit_long2021$citation, ',', fixed = TRUE), '[', 3))


#2021-2023

ws_preEnd <- wos_use3 %>% filter(PY>2020)

cit_longEnd <- ws_preEnd %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

cit_longEnd$cjrnl <- unlist(lapply(strsplit(cit_longEnd$citation, ',', fixed = TRUE), '[', 3))


#Bind these all together to make big long final

cit_all <- rbind(cit_long1990, cit_long2000, cit_long2010, cit_long2015, 
                 cit_long2017, cit_long2020, cit_long2021, cit_longEnd)

#Checking out cited journals

cit_freq <- as.data.frame(table(cit_all$cjrnl))

cit_freq2 <- cit_freq %>% filter(Freq>100)

#Bringing in the list of WoS AND Wiki journals

library(readr)
wos_jrnls <- read_csv("data/wos_jrnls.csv")

#some of the wiki list jounrals are not in the list of citations. Get rid of them.

wos_jrnls <- wos_jrnls %>% filter(abbreviation!="NONE")

#Some of the journals are duplicated across the lists so create one unique
#row per journal

wos_nodup <- wos_jrnls %>% distinct(abbreviation, .keep_all=TRUE)

#when we merge these variables will be helpful for keeping articles

wos_nodup$so_keepy <- 1

wos_nodup$cjrnl_keepy <- 1

wos_nodup2 <- wos_nodup %>% select(abbreviation, so_keepy)

wos_nodup3 <- wos_nodup %>% select(abbreviation, so_keepy, cjrnl_keepy)

#Now create the various conditions for keeping records/articles

#hang onto an index to merge

cit_all <- cit_all %>% mutate(nid = as.numeric(row_number()))

#Make the citing journals all caps

cit_all$cjrnl <- trimws(toupper(cit_all$cjrnl))

#Merge the Citing Journal by the WoS/Wiki list

cit_allm <- left_join(cit_all, wos_nodup2, by = c('J9'='abbreviation'))

cit_allm <- cit_allm %>% select(nid, so_keepy)

#Merge the Cited Journal by the WoS/Wiki list

cit_allme <- left_join(cit_all, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_allme <- cit_allme %>% select(nid, cjrnl_keepy)

#Merge the them all back together

cit_allred <- left_join(cit_all, cit_allm, by='nid')

cit_allred2 <- left_join(cit_allred, cit_allme, by='nid')

cit_allred2 <- cit_allred2 %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cjrnl_cnt <- cit_allred2 %>% group_by(UT) %>% 
  summarise(sum_cjrnl=sum(cjrnl_keepy))

#Now we can reduce to more than one journal...let's say more than two

cit_allred3 <- left_join(cit_allred2, cjrnl_cnt)

cit_allred3$cited_keepy <- ifelse(cit_allred3$sum_cjrnl>2, 1, 0)

#here we are setting up some reduction...studies, health, sciences, engineer are keywords from WC field in web of science...SO are publications that are listed in WOS
#...and cjrnl are in the wiki list

cit_allred3$studies_keepy <- as.numeric(grepl("Environmental Studies", cit_allred3$WC))

cit_allred3$health_keepy <- as.numeric(grepl("Environmental & Occupational", cit_allred3$WC))

cit_allred3$sciences_keepy <- as.numeric(grepl("Environmental Sciences", cit_allred3$WC)) 

cit_allred3$engineer_keepy <- as.numeric(grepl("Engineering, Environmental", cit_allred3$WC))

cit_allred3$keepytot <- cit_allred3$so_keepy + cit_allred3$cited_keepy + cit_allred3$studies_keepy + cit_allred3$health_keepy + 
  cit_allred3$sciences_keepy + cit_allred3$engineer_keepy 

#Now we can do a big edit by reducing those articles that don't fit any of
#the criteria - no relevant enviornmental WoS fields, not from a citing
#journal on WoS or Wiki list, no cited journals from those lists

cit_allredfin <- cit_allred3 %>% filter(keepytot > 0)

#The public, environmental, and occupational health contains things that aren't environmetnal
#so we can remove articles that are health and nothing else

cit_allredfin$health_remove <- ifelse(cit_allredfin$health_keepy==1 & cit_allredfin$keepytot==1, 1, 0)

cit_allredfin2 <- cit_allredfin %>% filter(health_remove!=1)

finpaps <- as.data.frame(table(cit_allredfin2$UT))

finpaps <- finpaps %>% rename(UT=Var1)

essfin <- left_join(finpaps, wos_use2)

essfin <- distinct(essfin, UT, .keep_all=TRUE)

#These data are not included in package as it is largely third party data

saveRDS(essfin, "data/essbest.rds")

#fin








#here i'm examining the consequences of trying to get rid of public health 
#and occupational health papers

check <- cit_allredfin %>% filter(keepytot==1, health_keepy==1)

checkpaps <- as.data.frame(table(check$UT))

checkfin <- left_join(wos_use2, checkpaps, by = c("UT" = "Var1"))



table(cit_allredfin2$engineer_keepy)



#Here just to check a few things


essfin2 <- essfin %>% filter(Freq>1)


table(finpaps$Freq)

check <- finpaps2 %>% filter(Freq==177)

check2 <- cit_allredfin2 %>% filter(grepl('WOS:000424061000004', UT))

cit_check <- cit_all %>% filter(wc_keepy==1)


#old code down here

cit_long1990 <- cit_long1990 %>% mutate(nid = as.numeric(row_number()))

cit_long1990$cjrnl <- trimws(toupper(cit_long1990$cjrnl))

cit_long1990$wc_keepy <- as.numeric(grepl("Environmental", cit_long1990$WC))

cit_long1990m <- left_join(cit_long1990, wos_nodup2, by = c('J9'='abbreviation'))

cit_long1990m <- cit_long1990m %>% select(nid, so_keepy)

cit_long1990me <- left_join(cit_long1990, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_long1990red <- left_join(cit_long1990, cit_long1990m, cit_long1990me, by='nid')

cit_long1990red <- cit_long1990red %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cit_long1990red$keepytot <- cit_long1990red$so_keepy + cit_long1990red$cjrnl_keepy

cit_long1990red <- cit_long1990red %>% filter(keepytot > 0)



cit_long2000 <- cit_long2000 %>% mutate(nid = as.numeric(row_number()))

cit_long2000$cjrnl <- trimws(toupper(cit_long2000$cjrnl))

cit_long2000m <- left_join(cit_long2000, wos_nodup2, by = c('J9'='abbreviation'))

cit_long2000m <- cit_long2000m %>% select(nid, so_keepy)

cit_long2000me <- left_join(cit_long2000, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_long2000red <- left_join(cit_long2000m, cit_long2000me, by='nid')

cit_long2000red <- cit_long2000red %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cit_long2000red$keepytot <- cit_long2000red$so_keepy + cit_long2000red$cjrnl_keepy

cit_long2000red <- cit_long2000red %>% filter(keepytot > 0)



cit_long2010 <- cit_long2010 %>% mutate(nid = as.numeric(row_number()))

cit_long2010$cjrnl <- trimws(toupper(cit_long2010$cjrnl))

cit_long2010m <- left_join(cit_long2010, wos_nodup2, by = c('J9'='abbreviation'))

cit_long2010m <- cit_long2010m %>% select(nid, so_keepy)

cit_long2010me <- left_join(cit_long2010, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_long2010red <- left_join(cit_long2010m, cit_long2010me, by='nid')

cit_long2010red <- cit_long2010red %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cit_long2010red$keepytot <- cit_long2010red$so_keepy + cit_long2010red$cjrnl_keepy

cit_long2010red <- cit_long2010red %>% filter(keepytot > 0)



cit_long2015 <- cit_long2015 %>% mutate(nid = as.numeric(row_number()))

cit_long2015$cjrnl <- trimws(toupper(cit_long2015$cjrnl))

cit_long2015m <- left_join(cit_long2015, wos_nodup2, by = c('J9'='abbreviation'))

cit_long2015m <- cit_long2015m %>% select(nid, so_keepy)

cit_long2015me <- left_join(cit_long2015, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_long2015red <- left_join(cit_long2015m, cit_long2015me, by='nid')

cit_long2015red <- cit_long2015red %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cit_long2015red$keepytot <- cit_long2015red$so_keepy + cit_long2015red$cjrnl_keepy

cit_long2015red <- cit_long2015red %>% filter(keepytot > 0)


cit_long2017 <- cit_long2017 %>% mutate(nid = as.numeric(row_number()))

cit_long2017$cjrnl <- trimws(toupper(cit_long2017$cjrnl))

cit_long2017m <- left_join(cit_long2017, wos_nodup2, by = c('J9'='abbreviation'))

cit_long2017m <- cit_long2017m %>% select(nid, so_keepy)

cit_long2017me <- left_join(cit_long2017, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_long2017red <- left_join(cit_long2017m, cit_long2017me, by='nid')

cit_long2017red <- cit_long2017red %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cit_long2017red$keepytot <- cit_long2017red$so_keepy + cit_long2017red$cjrnl_keepy

cit_long2017red <- cit_long2017red %>% filter(keepytot > 0)



cit_long2020 <- cit_long2020 %>% mutate(nid = as.numeric(row_number()))

cit_long2020$cjrnl <- trimws(toupper(cit_long2020$cjrnl))

cit_long2020m <- left_join(cit_long2020, wos_nodup2, by = c('J9'='abbreviation'))

cit_long2020m <- cit_long2020m %>% select(nid, so_keepy)

cit_long2020me <- left_join(cit_long2020, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_long2020red <- left_join(cit_long2020m, cit_long2020me, by='nid')

cit_long2020red <- cit_long2020red %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cit_long2020red$keepytot <- cit_long2020red$so_keepy + cit_long2020red$cjrnl_keepy

cit_long2020red <- cit_long2020red %>% filter(keepytot > 0)


cit_long2021 <- cit_long2021 %>% mutate(nid = as.numeric(row_number()))

cit_long2021$cjrnl <- trimws(toupper(cit_long2021$cjrnl))

cit_long2021m <- left_join(cit_long2021, wos_nodup2, by = c('J9'='abbreviation'))

cit_long2021m <- cit_long2021m %>% select(nid, so_keepy)

cit_long2021me <- left_join(cit_long2021, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_long2021red <- left_join(cit_long2021m, cit_long2021me, by='nid')

cit_long2021red <- cit_long2021red %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cit_long2021red$keepytot <- cit_long2021red$so_keepy + cit_long2021red$cjrnl_keepy

cit_long2021red <- cit_long2021red %>% filter(keepytot > 0)


cit_longEnd <- cit_longEnd %>% mutate(nid = as.numeric(row_number()))

cit_longEnd$cjrnl <- trimws(toupper(cit_longEnd$cjrnl))

cit_longEndm <- left_join(cit_longEnd, wos_nodup2, by = c('J9'='abbreviation'))

cit_longEndm <- cit_longEndm %>% select(nid, so_keepy)

cit_longEndme <- left_join(cit_longEnd, wos_nodup3, by = c('cjrnl'='abbreviation'))

cit_longEndred <- left_join(cit_longEndm, cit_longEndme, by='nid')

cit_long2015End <- cit_long2015End %>% 
  mutate(so_keepy = coalesce(so_keepy, 0),
         cjrnl_keepy = coalesce(cjrnl_keepy, 0))

cit_longEndred$keepytot <- cit_longEndred$so_keepy + cit_longEndred$cjrnl_keepy

cit_longEndred <- cit_longEndred %>% filter(keepytot > 0)







check <- as.data.frame(table(cit_long2000red$cjrnl))

results <- biblioAnalysis(wos_use)

getdata <- function(...)
{
  e <- new.env()
  name <- data(..., envir = e)[1]
  e[[name]]
}

check <- getdata(bibtag)

yrcnt <- as.data.frame(table(wos_tot$PY))

yrcnt$PY <- as.numeric(as.character(yrcnt$Var1))

pre06 <- subset(yrcnt, PY<2006)

sum(pre06$Freq)


bt0611 <- subset(yrcnt, PY>2005 & PY<2012)

sum(bt0611$Freq)

bt1215 <- subset(yrcnt, PY>2011 & PY<2016)

sum(bt1215$Freq)


bt1617 <- subset(yrcnt, PY>2015 & PY<2018)

sum(bt1617$Freq)


bt1819 <- subset(yrcnt, PY>2017 & PY<2020)

sum(bt1819$Freq)

summary(wos_tot$PY)

wos_tot2$naa <- as.numeric(is.na(wos_tot2$PY))

naa_tot <- subset(wos_tot2, naa==1)

naa_tot2 <- filter(naa_tot, DT=="Article")

check <- head(wos_tot)