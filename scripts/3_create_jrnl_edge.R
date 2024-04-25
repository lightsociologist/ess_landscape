#This creates the journal edge list

#Reading hard code b/c of size

ess <- readRDS("~/PATH NAME/data/environmental_studies.RDS")

#let's make a list of journals in this set

jrnls <- as.data.frame(table(ess$J9))

#and this pattern for searching

pattern <- paste(jrnls$Var1, collapse = "|")

#Using same logic as read_data let's make this long

#First let's make the data little less wide

ess.s <- ess %>% select(UT, PY, SO, J9, cite1:cite500)

#Now let's chunk

ws_pre1990 <- ess.s %>% filter(PY<1991)

cit_long1990 <- ws_pre1990 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

#create an index based on the journal patterns

cit_long1990$keep <- str_detect(cit_long1990$citation, regex(pattern))

#keep only those that are indexed

cit_long1990 <- cit_long1990 %>% filter(keep=="TRUE")

#Now let's chunk

ws_pre2000 <- ess.s %>% filter(PY<2001 & PY>1990)

cit_long2000 <- ws_pre2000 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

#create an index based on the journal patterns

cit_long2000$keep <- str_detect(cit_long2000$citation, regex(pattern))

#keep only those that are indexed

cit_long2000 <- cit_long2000 %>% filter(keep=="TRUE")


#Now let's chunk

ws_pre2010 <- ess.s %>% filter(PY<2011 & PY>2000)


cit_long2010 <- ws_pre2010 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

#create an index based on the journal patterns

cit_long2010$keep <- str_detect(cit_long2010$citation, regex(pattern))

#keep only those that are indexed

cit_long2010 <- cit_long2010 %>% filter(keep=="TRUE")

#Now let's chunk

ws_pre2015 <- ess.s %>% filter(PY<2016 & PY>2010)

cit_long2015 <- ws_pre2015 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

#create an index based on the journal patterns

cit_long2015$keep <- str_detect(cit_long2015$citation, regex(pattern))

#keep only those that are indexed

cit_long2015 <- cit_long2015 %>% filter(keep=="TRUE")

#Now let's chunk

ws_pre2017 <- ess.s %>% filter(PY<2018 & PY>2015)


cit_long2017 <- ws_pre2017 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

#create an index based on the journal patterns

cit_long2017$keep <- str_detect(cit_long2017$citation, regex(pattern))

#keep only those that are indexed

cit_long2017 <- cit_long2017 %>% filter(keep=="TRUE")

#Now let's chunk

ws_pre2020 <- ess.s %>% filter(PY<2020 & PY>2017)

cit_long2020 <- ws_pre2020 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

#create an index based on the journal patterns

cit_long2020$keep <- str_detect(cit_long2020$citation, regex(pattern))

#keep only those that are indexed

cit_long2020 <- cit_long2020 %>% filter(keep=="TRUE")


#Now let's chunk

ws_pre2021 <- ess.s %>% filter(PY<2021 & PY>2019)

cit_long2021 <- ws_pre2021 %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

#create an index based on the journal patterns

cit_long2021$keep <- str_detect(cit_long2021$citation, regex(pattern))

#keep only those that are indexed

cit_long2021 <- cit_long2021 %>% filter(keep=="TRUE")

#Now let's chunk

ws_preEnd <- ess.s %>% filter(PY>2020)

cit_longEnd <- ws_preEnd %>% pivot_longer(cols=starts_with("cite"),
                                            names_to = "cite_num",
                                            names_prefix = "cite",
                                            values_to  = "citation",
                                            values_drop_na = TRUE)

#create an index based on the journal patterns

cit_longEnd$keep <- str_detect(cit_longEnd$citation, regex(pattern))

#keep only those that are indexed

cit_longEnd <- cit_longEnd %>% filter(keep=="TRUE")

cit_all <- rbind(cit_long1990, cit_long2000, cit_long2010, cit_long2015, 
                 cit_long2017, cit_long2020, cit_long2021, cit_longEnd)

#let's sum journals by UT

#first locate citing journal

cit_all$cjrnl <- unlist(lapply(strsplit(cit_all$citation, ',', fixed = TRUE), '[', 3))

cit_2_edges <- cit_all %>% group_by(UT, PY, SO, J9) %>% count(cjrnl)

saveRDS(cit_2_edges, "data/es_edges.RDS")


