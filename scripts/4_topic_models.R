library(dplyr)
library(stringr)
library(tidyr)
library(tidytext)
library(stm)
library(igraph)
library(reshape2)
library(ggplot2)
library(pals)

ess <- readRDS("~PATH NAME/data/environmental_studies.RDS")

ess2 <- ess %>% 
  filter(!if_all(c(DE, ID), is.na))

ess3 <- ess2 %>% select(PY, DE, ID, TI, UT, AB)

jrnl_wrds <- ess2 %>% group_by(J9, PY) %>% summarise(dekeys = toString(DE))

jrnl_wrds$dekeys <- gsub(",",";",jrnl_wrds$dekeys)

jrnl_wrds$dekeys <- gsub(";;","",jrnl_wrds$dekeys)

id_wrds <- ess2 %>% group_by(J9, PY) %>% summarise(idkeys = toString(ID))

id_wrds$idkeys <- gsub(",",";",id_wrds$idkeys)

all_wrds <- full_join(jrnl_wrds, id_wrds)

all_wrds <- all_wrds %>% unite("allkeys", dekeys, idkeys, sep = "; ", remove = FALSE)

table(all_wrds$PY)

all_wrds$period <- ""

floor_decade    = function(value){ return(value - value %% 5) }

all_wrds$period <- floor_decade(all_wrds$PY)

table(all_wrds$period)

#Now create document term matrix

all_wrds$allkeys <- trimws(tolower(all_wrds$allkeys))

all_wrds$allkeys2 <- sub("^\\W+", "", all_wrds$allkeys)

all_wrds$allkeys2 <- stringr::str_remove_all(all_wrds$allkeys2, "; na")

all_wrds$allkeys2 <- stringr::str_remove_all(all_wrds$allkeys2, ";;")

all_wrds$allkeys2 <- stringr::str_remove_all(all_wrds$allkeys2, "; ;")

check <- as.data.frame(table(all_wrds$J9))

nmax <- max(stringr::str_count(all_wrds$allkeys2, "\\;")) + 1

key.df <- separate(all_wrds, allkeys2, paste0("key", seq_len(nmax)), sep = "\\;", fill = "right")

keys_long <- key.df %>% pivot_longer(cols=starts_with("key"),
                                    names_to = "key_num",
                                    names_prefix = "key",
                                    values_to  = "key_word",
                                    values_drop_na = TRUE)

keys_long$key_word <- trimws(keys_long$key_word)

key_freq <- as.data.frame(table(keys_long$key_word))

key_freq <- rename(key_freq, key_word=Var1)

keys_long2 <- left_join(keys_long, key_freq)

keys_long2 <- keys_long2 %>% filter(key_word!="")

keys_long2 <- keys_long2 %>% filter(key_word!="na")

keys_long2 <- keys_long2 %>% filter(Freq>100)

keys_add <- keys_long2 %>% count(J9, period, key_word)

keys_add$sid <- paste(keys_add$J9, ".", keys_add$period)

meta_key <- keys_add %>% count(J9, period, sid)

save(meta_key, file="data/meta_key_list.rda")
  
#You can use the keys_add data from the data folder from here

keys_add <- readRDS("data/keys_add.RDS")

key_dtm <- cast_dtm(keys_add, document=sid, term=key_word, value=n)

key_corp <- readCorpus(key_dtm, type="slam")


env_topics1 <- searchK(key_corp$documents, key_corp$vocab, K = seq(10, 20, by=1),
                      prevalence =~ period, proportion=.2, heldout.seed=02138, data = meta_key)

plot(env_topics1)

plot(env_topics1$results$semcoh, env_topics1$results$exclus, )
text(env_topics1$results$semcoh, env_topics1$results$exclus, labels=env_topics1$results$K,
     cex=0.7, pos=2)

lda_env15 <- stm(key_corp$documents, key_corp$vocab, K = 15,
               prevalence =~ period, data = meta_key, init.type = "Spectral")

save(lda_env15, file="data/lda_env15.rda")
save(env_topics1, file="data/lda_env10_20.rda")

####You can work off of saved files from here#####

load("data/lda_env10_20.rda")
load("data/lda_env15.rda")
load("data/meta_key_list.rda")


labelTopics(lda_env15, c(1:15), n=10, frexweight=.75)

est <- estimateEffect(1:15 ~ period, lda_env15, meta=meta_key, uncertainty="Global")

summary(est, topics=1:15)

plot.estimateEffect(est, "period", topic=1:5, model=lda_env15, method="continuous")

plot.estimateEffect(est, "period", topic=6:10, model=lda_env15, method="continuous")

plot.estimateEffect(est, "period", topic=11:15, model=lda_env15, method="continuous")

#make stacked bars

top_names <- labelTopics(lda_env15, c(1:15), n=3, frexweight=.75)

top_frex <- as.data.frame(top_names$frex)

top_frex <- top_frex %>% unite(name, V1, V2, V3, sep=", ", remove=FALSE)

ldtheta <- as.data.frame(lda_env15$theta)

ldtheta$period <- meta_key$period

topic_proportion_period <- aggregate(ldtheta, by=list(time=ldtheta$period), mean)

colnames(topic_proportion_period)[2:16] <- top_frex$name

topic_proportion_period <- topic_proportion_period %>% select(!time)

vizDataFrame <- melt(topic_proportion_period, id.vars = "period")

vizDataFrame <- vizDataFrame %>% arrange(variable, period)

#sort by 1990

viz1990 <- vizDataFrame %>% filter(period==1990)

viz1990 <- viz1990 %>% rename(val90=value) 

viz1990 <- viz1990 %>% select(variable, val90)

vizDataFrame <- left_join(vizDataFrame, viz1990)

vizDataFrame <- vizDataFrame %>% arrange(val90)

ggplot(vizDataFrame, aes(x=period, y=value, fill=variable)) + 
  geom_bar(stat = "identity") + ylab("proportion") + 
  scale_fill_manual(values = paste0(alphabet(25), "FF"), name = "topics") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  #labs(title="Topics by Period (1990-2022)")+
  theme_classic()



#make top loading documents by topic file

theta <- as.data.frame(lda_env15$theta)

theta$ID <- 1:nrow(theta)

meta_key$ID <- 1:nrow(meta_key)

thetam <- merge(meta_key, theta)

