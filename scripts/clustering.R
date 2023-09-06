# Script that runs clustering algorithm and evaluation on each dataset


# All Sectors Clustering --------------------------------------------------


#features need to be in columns - in our case these are months
cpi_sectors_wider <- cpi_all_sectors %>%
  tidyr::pivot_wider(id_cols = 1, names_from = month, values_from = perc_change)

# exploring cluster number through elbow method => best number cluster = 3
n_clust <- 1:5
wss <- map_dbl(n_clust, ~{kmeans(select(cpi_sectors_wider, -sector), ., nstart=50,iter.max = 15L)$tot.withinss})
elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))

elbow_plot <- ggplot(elbow_df) +
  geom_line(aes(y = wss, x = n_clust), colour = "#82518c") +
  theme_minimal()

#running the clustering algorithm
cpi_sectors_clustered <- get_cluster_partitions(
  data = cpi_sectors_wider,
  group_col = "sector",
  cluster_num = 3L)

cpi_sectors_preds <- cpi_sectors_clustered$data

#visualising output
ggplot() +
  geom_line(data = cpi_sectors_preds, aes(y = perc_change, x = date, group = sector), colour = "#82518c") +
  facet_wrap(~partition, nrow = 1) +
  theme_bw()

#evaluate clustering - we use intrinsic measurements here
silhouette_score <- cpi_sectors_clustered$score

silhouette_plot <- cpi_sectors_clustered$sil_plot


# Food Sector Clustering --------------------------------------------------


#features need to be in columns - in our case these are months

cpi_food_wider <- cpi_food %>%
  tidyr::pivot_wider(id_cols = 1, names_from = month, values_from = perc_change)

#exploring cluster number through elbow method

n_clust <- 1:10
wss <- map_dbl(n_clust, ~{kmeans(select(cpi_food_wider, -food_type), ., nstart=50,iter.max = 200L )$tot.withinss})
elbow_df <- as.data.frame(cbind("n_clust" = n_clust, "wss" = wss))

#choose cluster num = 4
ggplot(elbow_df) +
  geom_line(aes(y = wss, x = n_clust), colour = "#82518c") +
  theme_minimal() +
  scale_x_continuous(breaks = n_clust)


#running the clustering algorithm

cpi_food_clustered <- get_cluster_partitions(
  data = cpi_food_wider,
  group_col = "food_type",
  cluster_num = 4L)

cpi_food_preds <- cpi_food_clustered$data

#visualising output

ggplot() +
  geom_line(data = cpi_food_preds, aes(y = perc_change, x = date, group = food_type), colour = "#82518c") +
  facet_wrap(~partition, nrow = 1) +
  theme_bw()

#evaluate clustering - we use intrinsic measurements here

silhouette_score <- cpi_food_clustered$score

silhouette_plot <- cpi_food_clustered$sil_plot

