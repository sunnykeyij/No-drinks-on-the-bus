rm(list=ls())
library(dplyr)
library(corrr)
library(tidygraph)
library(ggraph)
setwd("C:\\Users\\UOS\\Dropbox\\DataMiningTeamProject")

load("gooraw.Rda")
goo_raw <- goo_raw %>% select(goo,daypop,nightpop,면적,프랜차이즈,buscount,trashcount,종사자수,월매출)
goo_scale <- as.data.frame(scale(goo_raw[,-1]))

set.seed(3)
kmeans_fit <- kmeans(goo_scale,3)
goo_group <- data.frame(goo=goo_raw$goo,group=kmeans_fit$cluster)
goo_group

load("df_market.Rda")
rownames(df_market) <- paste0("m",df_market$marketid)

df_market
res.cor = df_market %>% select(sales,busnum,buspeople_up) %>%
  t() %>% correlate() %>%
  shave(upper=TRUE) %>%
  stretch(na.rm=TRUE) %>%
  filter(r >= 0.998)
res.cor

# correlation network
set.seed(1)
cor.graph = as_tbl_graph(res.cor, directed=FALSE)
ggraph(cor.graph) +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label=name), size=3, repel=TRUE) +
  theme_graph()


# print
cor.graph

cor.graph %>%
  activate(edges) %>%
  arrange(desc(r))

# manipulation
cars.group = data_frame(name = rownames(df_market),
                        group = as.factor(df_market$group))

cor.graph = cor.graph %>%
  activate(nodes) %>%
  left_join(cars.group, by="name") %>%
  rename(label=name)

cor.graph = cor.graph %>%
  activate(edges) %>%
  rename(weight=r)

cor.graph

set.seed(1)
ggraph(cor.graph) +
  geom_edge_link(aes(width=weight), alpha=0.2) +
  scale_edge_width(range=c(0.2,1)) +
  geom_node_point(aes(color=group), size=2) +
  geom_node_text(aes(label=label), size=3, repel=TRUE) +
  theme_graph()
