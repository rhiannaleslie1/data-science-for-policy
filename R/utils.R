#clustering algorithm

get_cluster_partitions <- function(data, group_col, cluster_num) {
  group_col <- sym(group_col)
  
  features <- data %>%
    select(-all_of(group_col))
  
  # Create a new clustering task with features 
  task_cluster <- TaskClust$new(
    id = "clusters",
    backend = features)
  
  #scaling the features
  po_scaled <- po("scalerange",param_vals = list(lower = -1, upper = 1))  
  
  # Clone the task and train 
  task_cluster_scaled <- po_scaled$train(list(task_cluster))[[1]]$clone()
  
  # create a learner object
  learner_kmeans = mlr_learners$get("clust.kmeans")
  
  learner_kmeans$param_set$values = list(centers = cluster_num, algorithm = "Lloyd", iter.max = 100L)
  
  # Train the kmeans learner on scaled dataset
  learner_kmeans$train(task_cluster)
  
  # Make predictions from trained models
  preds = learner_kmeans$predict(task_cluster)
  
  #add the partition preds on the data
  partitioned_data <- data %>%
    dplyr::mutate(partition = preds$partition) %>%
    tidyr::pivot_longer(cols = contains("20"),
                        names_to = "month",
                        values_to = "perc_change") %>%
    dplyr::left_join(date_lookup, by = c("month"))
  
  # Calculate sil score
  sil_score <- preds$score(measures = msr("clust.silhouette"), task_cluster_scaled)
  
  # Sillhouette plot
  sil_plot <- autoplot(preds, task_cluster_scaled, type="sil") 
  
  return(list("data" = partitioned_data,
              "score" = sil_score,
              "sil_plot" = sil_plot))
}


clustering_visualisation <- function(cluster_data, group_col, lab){
  
  group_col <- sym(group_col)
  
  plot_data <- split(cluster_data, f = cluster_data$partition)
  
  for (each_cluster in 1:length(plot_data)) {
    
    p <- ggplot(data = plot_data[[each_cluster]]) +
      geom_line(aes(y = perc_change, x = date, colour = !!group_col)) +
      theme_bw() +
      facet_wrap(~partition) +
      labs(x = "Date",
           y = "12 month percentage change",
           colour = paste0(lab, " in cluster")) +
      guides(colour = guide_legend(ncol =  1)) +
      scale_y_continuous(limits = c(min(cluster_data$perc_change), max(cluster_data$perc_change))) +
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.position = "right",
            axis.title = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            strip.text = element_text(size = 18))
    
    ggsave(paste0("outputs/", lab, each_cluster,".png"),
           height = 10,
           width = 15)
    
  }
  
  
}


  