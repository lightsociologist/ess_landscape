#library

library("tidyverse")
library("tidygraph")
library("dplyr")
library("ggraph")
library("igraph")
library("stringr")
library("rsetse")
library(RColorBrewer)
library(reshape2)
library(hrbrthemes)
library(stm)


#import data

es_edges <- readRDS("data/es_edges.RDS")

#get rid of whitespaces before and after journal names
es_edges$cjrnl <- trimws(es_edges$cjrnl)
es_edges$J9 <- trimws(es_edges$J9)

#limit cjrnl to J9
keep <- es_edges$J9
keep <- unique(keep)
es_edges_1 <- filter(es_edges, cjrnl %in% keep)

#journal - journal edgelist by year

jedgeyear <- es_edges_1 %>%
  group_by(PY, J9, cjrnl) %>%
  summarise(
    total_cites = sum(n)
  )

#Now let's make this for the whole network

#journal - journal edgelist total

jedgetot <- jedgeyear %>%
  group_by(J9, cjrnl) %>%
  summarise(
    total_cites = sum(total_cites)
  )

#construct network across all years
jedgetot_1 <- jedgetot[jedgetot$J9 != jedgetot$cjrnl, ]

jedgetot_1 <- ungroup(jedgetot_1) %>% select(J9, cjrnl, total_cites)

##make into network

jall <- graph_from_data_frame(jedgetot_1)

sjall <- as.undirected(jall, edge.attr.comb="sum")

E(sjall)$weight <- E(sjall)$total_cites

memall <- cluster_louvain(sjall)

palall <- brewer.pal(max(membership(memall)),"Spectral")

centall <- eigen_centrality(sjall)

weights <- ifelse(crossing(memall, sjall), 1, 100)

layout <- layout_with_fr(sjall, weights=weights)

#layall <- layout_with_drl(sjall, dim=2)

#pdf("images/allnet.pdf", paper="a4r")

clsts <- data.frame(cluster=memall$membership, Var1=memall$names)

jrnl_cnt <- as.data.frame(table(jedgetot_1$J9))

jrnl_cnt <- jrnl_cnt %>%
  mutate(quantile100 = findInterval(Freq, quantile(Freq, probs = seq(0, 1, 0.1)), 
                                    rightmost.closed = TRUE))

jrnl_cnt$Var1 <- as.character(jrnl_cnt$Var1)

clsts <- left_join(clsts, jrnl_cnt)

clsts$lab <- ifelse(clsts$quantile100==10, clsts$Var1, "")

top3 <- clsts %>% group_by(cluster) %>% top_n(5, Freq)

top3$labkeep <- 1

top3 <- top3 %>% select(labkeep, Var1)

clsts <- left_join(clsts, top3)

clsts$labkeep[is.na(clsts$labkeep)] <- 0

clsts$lab2 <- ifelse(clsts$labkeep==1, clsts$Var1, "")

clrs <- data.frame(cluster=c(1,2,3,4,5,6,7), clr=c("purple", "yellow", "green", "skyblue","tomato", "grey" , "orange"), 
    grplabs = c("1. Sustainability", "2. Transportation", "3. Ecology", "4. Environ. Policy & Law", "5. Geography", "6. Psychology & Communication",
                "7. Environmental Economics"))

membs <- left_join(clsts, clrs)

x<-layout[, 1]

y<-layout[, 2]


#let's try to do ggraph version to jitter labels

library(ggforce)


ggraph(sjall, layout = layout) + 
  geom_edge_link(alpha=.25, aes(width=weight), color="gray") + 
  geom_node_point(aes(size=degree(sjall), fill = membs$clr), shape=21) +
  scale_size_continuous(range = c(2, 20)) +
  geom_node_text(aes(label = clsts$lab2), repel = TRUE, max.overlaps=Inf) +
  geom_mark_hull(
    aes(x,y, group = membs$membs, fill=membs$clr, label=membs$grplabs),
    concavity = 4,
    expand = unit(2, "mm"),
    alpha = 0.25
  ) +
  theme_void() +
  theme(legend.position = "none")


vcount(sjall)
ecount(sjall)

edge_density(sjall)

modularity(sjall, membership(memall))

#let's make bar charts for each community based on key social science disciplines

#These are third-party data

ess <- readRDS("~PATH NAME/data/environmental_studies.RDS")

jfld <- ess %>% select(J9, WC)

library(data.table)

jfld <- left_join(jfld, membs, by=c('J9'='Var1'))

jfld <- jfld %>% filter(cluster>0)

jfld$field <- tolower(jfld$WC)

jfld$anthro <- ifelse(grepl("anthropology",jfld$field),1,0)
jfld$soc <- ifelse(grepl("sociology",jfld$field),1,0)
jfld$political <- ifelse(grepl("politic",jfld$field),1,0)
jfld$econ <- ifelse(grepl("economics",jfld$field),1,0)
jfld$geog <- ifelse(grepl("geography",jfld$field),1,0)
jfld$psych <- ifelse(grepl("psychology",jfld$field),1,0)
jfld$law <- ifelse(grepl("law",jfld$field),1,0)
jfld$comm <- ifelse(grepl("communication",jfld$field),1,0)
jfld$bus <- ifelse(grepl("business",jfld$field),1,0)
jfld$hist <- ifelse(grepl("history",jfld$field),1,0)


totsum <- jfld %>% group_by(cluster) %>% tally()

anthrosum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(anthro))
anthrosum$discipline <- "anthropology"
anthrosum <- left_join(anthrosum, totsum)

socsum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(soc))
socsum$discipline <- "sociology"
socsum <- left_join(socsum, totsum)

polsum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(political))
polsum$discipline <- "political science"
polsum <- left_join(polsum, totsum)

econsum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(econ))
econsum$discipline <- "economics"
econsum <- left_join(econsum, totsum)

geogsum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(geog))
geogsum$discipline <- "geography"
geogsum <- left_join(geogsum, totsum)

psychsum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(psych))
psychsum$discipline <- "psychology"
psychsum <- left_join(psychsum, totsum)

lawsum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(law))
lawsum$discipline <- "law"
lawsum <- left_join(lawsum, totsum)

commsum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(comm))
commsum$discipline <- "communications"
commsum <- left_join(commsum, totsum)

bussum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(bus))
bussum$discipline <- "business"
bussum <- left_join(bussum, totsum)

histsum <- jfld %>% group_by(cluster) %>% summarize(sum=sum(hist))
histsum$discipline <- "history"
histsum <- left_join(histsum, totsum)

clustfld <- rbind(anthrosum, socsum, polsum, econsum, geogsum, psychsum, commsum,
                  lawsum, bussum, histsum)

clustfld$percent <- clustfld$sum/clustfld$n

clustfld <- left_join(clustfld, clrs)

ggplot(clustfld, aes(grplabs, percent)) +
        ylab('percent')  +
        labs(x="community")+
        geom_bar(stat="identity") + 
        facet_wrap(~ as.factor(discipline),ncol=2)+
        theme_bw()+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
  


#now let's make a heatmap

load("data/lda_env15.rda")

theta <- as.data.frame(lda_env15$theta)

load("data/meta_key_list.rda")

top_names <- labelTopics(lda_env15, c(1:15), n=3, frexweight=.75)

top_frex <- as.data.frame(top_names$frex)

top_frex <- top_frex %>% unite(topic, V1, V2, V3, sep=", ", remove=FALSE)

top_frex$variable <- as.numeric(row.names(top_frex))

top_frex$variable <- paste0("V", top_frex$variable)

metatheta <- cbind(theta, meta_key)


df.all <- data.frame(J9=V(sjall)$name, comm=memall$membership)

df.all <- left_join(df.all, metatheta)

cm.all <- df.all %>% select(comm, V1:V15)

melt.all <- melt(cm.all, id="comm", na.rm=TRUE)

melt.all <- left_join(melt.all, top_frex)

ggplot(melt.all, aes(x=as.factor(comm), y=topic, fill=value))+
  geom_tile() +
  scale_fill_distiller(palette = "YlOrRd", direction=1) +
  labs(x="community")+
  scale_x_discrete(labels=c(clrs$grplabs))+
  theme_classic()+
  theme(axis.text.y = element_text(size = 7))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







