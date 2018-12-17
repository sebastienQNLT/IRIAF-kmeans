setwd("~/IRIAF/kmeans")

#lecture des donn�es ----
player.original<-read.csv2("./complete.csv",sep=";",stringsAsFactors = FALSE,dec = ".")
dim(player.original)
player.original %>% glimpse

#selection des donn�es ----
player.attribute<-player.original %>% 
  select(-"name") %>% sample_n(5000)

#EDA ----
dim(player.attribute)
player.attribute %>% summary
DataExplorer::plot_intro(player.attribute)

player.attribute %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar() + guides(fill = FALSE)+
  labs(title="Player's Overall ")

player.attribute %>% 
  ggplot(aes(x = height_cm,y=overall,alpha=0.5)) +
  geom_point() 

DataExplorer::plot_density(player.attribute, nrow = 12L,
                           ncol = 5L)

DataExplorer::plot_correlation(player.attribute, type = 'continuous',maxcat=100)

#scaling des donn�es----
player.attribute.scaled<-player.attribute %>%
  select(-ID) %>%
  scale() %>% as.data.frame
player.attribute.scaled %>% glimpse
player.attribute.final<-player.attribute.scaled
player.attribute.final %>% summary()

#PCA----
player.attribute.pca<-player.attribute.final 
res.pca <- FactoMineR::PCA(player.attribute.final,  graph = FALSE)

#scree plot
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# graph des variables
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)

fviz_pca_var(res.pca, col.var="contrib",
gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
repel = TRUE,
select.var=list(contrib=10)
)

# Contributions of variables to PCx
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)

#graph des individus
#axe 1 et 2
fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()
#axe 1 et 3 
fviz_pca_ind(res.pca,axes=c(1,3), col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()
fviz_pca_ind(res.pca,axes=c(2,3), col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()

#kmeans ----
df.kmeans<-as.data.frame(res.pca$ind$coord) # 5 axes par d�faut
clusters <- kmeans(df.kmeans, 3, nstart = 10)
clusters$tot.withinss
clusters$size

#elbow----
kmean_withinss <- function(k) {
  print(paste0("kmeans k:",k))
  cluster <- kmeans(df.kmeans, k, nstart = 25)
  return (cluster$tot.withinss)
}
kmean_withinss(2)
# Set maximum cluster 
max_k <-15
# Run algorithm over a range of k 
wss <- sapply(2:max_k, kmean_withinss)

# Create a data frame to plot the graph
elbow <-data.frame(2:max_k, wss)

# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, max_k, by = 1))

#-->5 � 8 ?

#silhouette----
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  print(paste0("silhouette k:",k))
  km.res <- kmeans(df.kmeans, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df.kmeans))
  return(mean(ss[, 3]))
}
# Set maximum cluster 
max_k <-15
# Run algorithm over a range of k 
avg.silhouette <- sapply(2:max_k, avg_sil)

silhouette <-data.frame(2:max_k, avg.silhouette)

# Plot the graph
ggplot(silhouette, aes(x = X2.max_k, y = avg.silhouette)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, max_k, by = 1))
# k =2 ou 8

# kmeans 2 ----
cluster.2 <- kmeans(df.kmeans, 2, nstart = 25)
cluster.2$size
fviz_cluster(cluster.2, data = player.attribute,geom="point")
player.attribute %>%
  mutate(Cluster = cluster.2$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")


# kmeans 8 ----
cluster.8 <- kmeans(df.kmeans, 8, nstart = 25)
cluster.8$size
cluster.8
fviz_cluster(cluster.8, data = player.attribute,geom="point")

#stats
player.clustered <-player.attribute %>%
  mutate(Cluster = cluster.8$cluster)

stats<-player.clustered %>% select(-ID,-height_cm,-weight_kg) %>% 
  group_by(Cluster) %>%
  summarise_all("mean")


#heatmap
library(RColorBrewer)
# Create the palette
hm.palette <-colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')

library(reshape2)
stats.reshaped<-melt(stats, id.vars=c("Cluster"))
ggplot(data = stats.reshaped, aes(x = Cluster, y =variable, fill = value)) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()

# join with positions ----
player.position<-read.csv2("./PlayerPlayingPositionData.csv",sep=",",stringsAsFactors = FALSE,dec = ".")
player.position %>% glimpse
player.position<-player.position %>% select(ID,Preferred.Positions)
player.position %>% group_by(Preferred.Positions) %>% count

player.position<-player.position %>% mutate(Preferred.Positions=trimws(substr(Preferred.Positions,1,3)))
player.position %>% group_by(Preferred.Positions) %>% count

final.player.data<-player.clustered %>% left_join(player.position)
final.player.data %>% glimpse

ggplot(final.player.data, aes(x = Cluster, y = Preferred.Positions,fill = Preferred.Positions))  +
  geom_bar(stat = "identity", position = "stack")
