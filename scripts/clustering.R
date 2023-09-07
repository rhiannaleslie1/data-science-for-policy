# Script that runs clustering algorithm and evaluation on each dataset
source("scripts/get_cpi_data.R")
source("R/utils.R")
file.create("outputs/clustering_sil_scores.txt")
set.seed(42)

# All Sectors Clustering --------------------------------------------------


#features need to be in columns - in our case these are months
cpi_sectors_wider <- cpi_all_sectors %>%
  tidyr::pivot_wider(id_cols = 1, names_from = month, values_from = perc_change)

# exploring cluster number through elbow method => best number cluster = 3
n_clust <- 1:5
wss <- map_dbl(n_clust, ~{kmeans(select(cpi_sectors_wider, -sector), ., nstart=50,iter.max = 15L)$tot.withinss})
elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))

p <- ggplot(elbow_df) +
  geom_line(aes(y = wss, x = n_clust), colour = "navy") +
  theme_bw() +
  labs(x = "number of clusters",
       y = "wss",
       title = "CPI all sectors elbow plot") 

ggsave("outputs/sectors_elbow.png",
       height = 10,
       width = 12)

#running the clustering algorithm
cpi_sectors_clustered <- get_cluster_partitions(
  data = cpi_sectors_wider,
  group_col = "sector",
  cluster_num = 3L)

cpi_sectors_preds <- cpi_sectors_clustered$data %>%
  dplyr::mutate(partition = paste0("cluster ", partition))

#visualising output
clustering_visualisation(cluster_data = cpi_sectors_preds,
                         group_col = "sector",
                         lab = "Sectors")

#evaluate clustering - we use intrinsic measurements here
cat(paste0("The silhoutte score for the CPI all sector clustering is: ", cpi_sectors_clustered$score), 
    file = "outputs/clustering_sil_scores.txt")


png("outputs/sectors_sil_plot.png")
cpi_sectors_clustered$sil_plot
dev.off()


# Food Sector Clustering --------------------------------------------------


#features need to be in columns - in our case these are months
cpi_food_wider <- cpi_food %>%
  tidyr::pivot_wider(id_cols = 1, names_from = month, values_from = perc_change)

#exploring cluster number through elbow method
n_clust <- 1:5
wss <- map_dbl(n_clust, ~{kmeans(select(cpi_food_wider, -food_type), ., nstart=50,iter.max = 200L )$tot.withinss})
elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))

#choose cluster num = 4
ggplot(elbow_df) +
  geom_line(aes(y = wss, x = n_clust), colour = "navy") +
  theme_bw() +
  scale_x_continuous(breaks = n_clust) +
  labs(x = "number of clusters",
       y = "wss",
       title = "CPI food products elbow plot") 

ggsave("outputs/food_type_elbow.png",
       height = 10,
       width = 12)

#running the clustering algorithm
cpi_food_clustered <- get_cluster_partitions(
  data = cpi_food_wider,
  group_col = "food_type",
  cluster_num = 4L)

cpi_food_preds <- cpi_food_clustered$data

cpi_food_preds <- cpi_food_clustered$data %>%
  dplyr::mutate(partition = paste0("cluster ", partition))

#visualising output
clustering_visualisation(cluster_data = cpi_food_preds,
                         group_col = "food_type",
                         lab = "Food type")

#evaluate clustering - we use intrinsic measurements here
cat(paste0("\n\nThe silhoutte score for the CPI food type clustering is: ", cpi_food_clustered$score), 
    file = "outputs/clustering_sil_scores.txt",
    append = TRUE)

png("outputs/food_type_sil_plot.png")
cpi_food_clustered$sil_plot
dev.off()




