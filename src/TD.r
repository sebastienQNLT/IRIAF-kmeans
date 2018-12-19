#lecture des donnees ----
player.original<-read.csv2("https://raw.githubusercontent.com/sebastienQNLT/IRIAF-kmeans/master/data/complete.csv",
                           sep=";",stringsAsFactors = FALSE,dec = ".")
dim(player.original)
player.original %>% glimpse

#selection des donnees ----
player.attribute<-player.original %>% 
  select(-"name") %>% sample_n(5000)

#EDA ----
dim(player.attribute)
player.attribute %>% summary

# qq visualisations de donnees
player.attribute %>% 
  ggplot(aes(x = overall, fill = factor(overall))) +
  geom_bar() + guides(fill = FALSE)+
  labs(title="Player's Overall ")

player.attribute %>% 
  ggplot(aes(x = height_cm,y=overall,alpha=0.5)) +
  geom_point() 

#densites des variables numeriques
DataExplorer::plot_density(player.attribute, nrow = 12L,
                           ncol = 5L)
#graph de correlation des variables
DataExplorer::plot_correlation(player.attribute, type = 'continuous',maxcat=100)

#scaling des donnees----
#on supprimer la variable ID qui est un identifiant
player.attribute.scaled<-player.attribute %>%
  select(-ID) %>%
  scale() %>% as.data.frame
player.attribute.scaled %>% glimpse

player.attribute.final<-player.attribute.scaled
player.attribute.final %>% summary()

#ACP----
res.pca <- FactoMineR::PCA(player.attribute.final,  graph = FALSE)

#scree plot
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

# graph des contributions des  variables
# Contributions of variables to PCx
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 3, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 4, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 5, top = 10)
#visualisation des axes 1 et 2
fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
#visualisation des axes 1 et 2 en limitant aux 10 variables qui contribuent le plus
fviz_pca_var(res.pca, col.var="contrib",
gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
repel = TRUE,
select.var=list(contrib=10)
)

#graph des individus
#axe 1 et 2
fviz_pca_ind(res.pca, col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()
#axe 1 et 3 
fviz_pca_ind(res.pca,axes=c(1,3), col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()
#axe 2 et 3 
fviz_pca_ind(res.pca,axes=c(2,3), col.ind="cos2", geom = "point",alpha.ind=.5) +theme_minimal()

#kmeans ----
df.kmeans<-as.data.frame(res.pca$ind$coord) # 5 axes par defaut
#on teste avec k = 3
clusters <- kmeans(df.kmeans, 3, nstart = 100)
clusters$tot.withinss # variance intra
clusters$size #taille des clusters

#elbow----
#on créé une fonction qui retourne la variance intra pour une valeur de k
kmean_withinss <- function(k) {
  print(paste0("kmeans k:",k))
  cluster <- kmeans(df.kmeans, k, nstart = 100)
  return (cluster$tot.withinss)
}
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

#-->5 à 8 cluster ?

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
# Create a data frame to plot the graph
silhouette <-data.frame(2:max_k, avg.silhouette)
# Plot the graph
ggplot(silhouette, aes(x = X2.max_k, y = avg.silhouette)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, max_k, by = 1))
# k =2 ou 8


# kmeans 2 clusters ----
cluster.2 <- kmeans(df.kmeans, 2, nstart = 25)
cluster.2$size
 # graph des individus, en fonction des dimensions 1 et 2 de l'ACP
fviz_cluster(cluster.2, data = player.attribute,geom="point")

#ajout du résultat du clustering à la base initiale
# calcul des moyennes des variables et non pas des coordonnées sur les axes de l'ACP
#stats
player.clustered <-player.attribute %>%
  mutate(Cluster = cluster.2$cluster)
stats<-player.clustered %>% select(-ID,-height_cm,-weight_kg) %>% 
  group_by(Cluster) %>%
  summarise_all("mean")

#affichage sous forme de heatmap
# create the palette
hm.palette <-colorBrewer::colorRampPalette(rev(brewer.pal(10, 'RdYlGn')),space='Lab')
# create the heatmap
stats.reshaped<-reshape2::melt(stats, id.vars=c("Cluster"))
ggplot(data = stats.reshaped, aes(x = Cluster, y =variable, fill = value)) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradientn(colours = hm.palette(90)) +
  theme_classic()


# verification de notre clustering avec la position reelle des joueurs
# join with positions ----
player.position<-read.csv2("https://raw.githubusercontent.com/sebastienQNLT/IRIAF-kmeans/master/data/PlayerPlayingPositionData.csv",
                           sep=";",stringsAsFactors = FALSE,dec = ".")
player.position<-player.position %>% select(ID,Preferred.Positions)
#on converve la premiere position préférée du joueur
player.position<-player.position %>% mutate(Preferred.Positions=trimws(substr(Preferred.Positions,1,3)))
player.position %>% group_by(Preferred.Positions) %>% count

final.player.data<-player.clustered %>% left_join(player.position)

ggplot(final.player.data, aes(x = Cluster, y = Preferred.Positions,fill = Preferred.Positions))  +
  geom_bar(stat = "identity", position = "stack")

# pour poursuivre les travaux :
# supprimer les joueurs qui ont la position GK et relancer la kmeans
# tester la kmeans sur la base initiale et non les résultats de l'ACP
# formaliser le TD dans un .markdown


