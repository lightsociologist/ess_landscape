#This program takes the whole constructed dataset and builds the field network
#to inductively identify environmental studies literature...here without
#mdpi journals

library(ggraph)
library(ggforce)
library(dplyr)
library(tidyr)
library(stringr)
library(igraph)

ess <- readRDS("data/essbest.rds")

#reduce to time where data seem complete including keywords so 1990 to 2022!

ess2 <- ess %>% filter(PY>1989)

ess2 <- ess2 %>% filter(PY<2023)

#remove journals with the -BASEL suffix as these are mdpi journals

ess2 <- ess2 %>% filter(!str_detect(J9, "-BASEL"))

#Using list from mdpi website to remove article in mdpi journals

library(readr)

mdpi_jnames <- read_csv("data/mdpi_jnames.csv")

mdpi_jnames$jname <- toupper(mdpi_jnames$jname)

ess3 <- ess2 %>% filter(!SO %in% mdpi_jnames$jname)

#Now we start building the journal-field network and transpose to field-field net

fld.df <- ess3 %>% select(UT, SO, WC, PY)

nmax <- max(stringr::str_count(fld.df$WC, "\\;")) + 1

fld.df <- separate(fld.df, WC, paste0("field", seq_len(nmax)), sep = "\\;", fill = "right")

#Building edge list

fld_long <- fld.df %>% pivot_longer(cols=starts_with("field"),
                                       names_to = "fld_num",
                                       names_prefix = "field",
                                       values_to  = "field_name",
                                       values_drop_na = TRUE)


fld_long$field_name <- trimws(fld_long$field_name)

fld_long <- fld_long[!(is.na(fld_long$field_name) | fld_long$field_name==""), ]

fld_cnt <- as.data.frame(table(fld_long$field_name))

fld_cnt <- fld_cnt %>%
  mutate(quantile100 = findInterval(Freq, quantile(Freq, probs = seq(0, 1, 0.1)), 
                                    rightmost.closed = TRUE))

fld_edge <- fld_long %>% group_by(field_name, SO) %>% summarise(weight=n())

fld.g <- graph_from_data_frame(fld_edge)

is_weighted(fld.g)

V(fld.g)$type <- bipartite.mapping(fld.g)$type

pfld.g <- bipartite_projection(fld.g, multiplicity = TRUE, which=FALSE)

V(pfld.g)$size <- fld_cnt$Freq

pfld.g2 <- delete.vertices(pfld.g, V(pfld.g)[ degree(pfld.g)==0 ]) 

pfld.g2 <- simplify(pfld.g2)

E(pfld.g2)$weight <- sqrt(E(pfld.g2)$weight)

cluster.fast <- cluster_fast_greedy(pfld.g2)

weights <- ifelse(crossing(cluster.fast, pfld.g2), 1, 100)

layout <- layout_with_fr(pfld.g2, weights=weights)

clsts <- data.frame(cluster=cluster.fast$membership, Var1=cluster.fast$names)

fld_cnt$Var1 <- as.character(fld_cnt$Var1)

clsts <- left_join(clsts, fld_cnt)

clsts$lab <- ifelse(clsts$quantile100==10, clsts$Var1, "")

plot(pfld.g2, layout=layout, vertex.label=clsts$lab, vertex.color=adjustcolor(cluster.fast$membership+1, alpha=.8))

top3 <- clsts %>% group_by(cluster) %>% top_n(5, Freq)

top3$labkeep <- 1

top3 <- top3 %>% select(labkeep, Var1)

clsts <- left_join(clsts, top3)

clsts$labkeep[is.na(clsts$labkeep)] <- 0

clsts$lab2 <- ifelse(clsts$labkeep==1, clsts$Var1, "")

clrs <- data.frame(cluster=c(1,2,3,4), clr=c("purple", "yellow", "green", "skyblue"), grplabs = c("Environmental Science", "Environmental Health", "Environmental Studies", "Engineering"))

membs <- left_join(clsts, clrs)


x<-layout[, 1]

y<-layout[, 2]

#some basic statistics

vcount(pfld.g2)

ecount(pfld.g2)

edge_density(simplify(pfld.g2))

modularity(pfld.g2, membership(cluster.fast), weights=NULL, directed=FALSE)

#let's try to do ggraph version to jitter labels

ggraph(pfld.g2, layout = layout) + 
  geom_edge_link(alpha=.25, aes(width=weight), color="gray") + 
  geom_node_point(aes(size=size, fill = membs$clr), shape=21) +
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


  #labs(title="Environmental Social Science Field to Field Network:1990-2022", caption = "This graph visualizes the relationship between fields in the Environmental Social Sciences. 
  #Nodes are 229 fields assigned to journals in the WoS. Edges are the count of overlaps across journals. Communities using FastGreedy. 
  #Node labels are the top 5 most common fields in each community.")

#let's make a line graph of fields

#recreate the long field dataframe with all data

fld_long2 <- fld_long %>%
  group_by(UT) %>%
  mutate(totfield = n())

fld_long2 <- left_join(fld_long2, membs, by=c("field_name"="Var1"))

fld_long2$frac <- 1/fld_long2$totfield 

#check size total

sum(fld_long2$frac)

fld_year <- fld_long2 %>% group_by(PY, cluster) %>% summarise(yr_tot=sum(frac))

#a very few are na

fld_year <- na.omit(fld_year) 

fld_year <- left_join(fld_year, membs, by=c("cluster"="cluster"), relationship="many-to-many")

fld_col <- fld_year %>% group_by(PY, cluster) %>% slice_head() 

yrsum <- fld_col %>% group_by(PY) %>% summarise(yr_tot=sum(yr_tot))

yrsum$grplabs <- "Sum of All Fields"
yrsum$cluster <- 5

fld_col2 <- rbind(fld_col, yrsum)

ggplot(fld_col2, aes(x=PY, y=yr_tot, group=grplabs, color=grplabs)) + 
  stat_smooth(se=F, size=1, span=.2)+
  #ggtitle("Environmental Social Science Fields Over Time:1990-2022") +
  ylab("Number of articles")+
  xlab("Year")+
  scale_color_manual(values=c('tomato','orchid4', "seagreen", "lightblue", "black"))+
  labs(color="Fields")+
  theme_bw()


#Now we can select environmental social studies

ess_fields <- clsts %>% filter(cluster==3)

fld.df2 <- ess3 %>% select(SO, WC, UT)

nmax2 <- max(stringr::str_count(fld.df2$WC, "\\;")) + 1

fld.df2 <- separate(fld.df2, WC, paste0("field", seq_len(nmax2)), sep = "\\;", fill = "right")

fld.df2 <- head(fld.df2)

#now match across these fields

library(stringr)

pattern <- paste(ess_fields$Var1, collapse = "|")

index <- str_detect(ess3$WC, regex(pattern))

ess3$env_studies <- NA

ess3[index,]$env_studies <- "confirmed"

ess4 <- ess3 %>% drop_na(env_studies)

#Remove citations greater than 500...minor bias...but certainly no effect given size of set

ess5 <- ess4 %>% select(-(cite501:cite1525))

saveRDS(ess5, "data/environmental_studies.RDS")

#These data are still replicating the third party data

#hard coded due to size

saveRDS(ess5, "~/PATH NAME/data/environmental_studies.RDS")
