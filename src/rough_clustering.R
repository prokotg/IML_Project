library('pracma')
library('data.table')
library('ggplot2')
library('zoo')
library('grDevices')
library('foreach')

source('src/benchmarking_data.R')
source('src/custom_data.R')

indicate_minimum <- function(distances, k_clusters, epsilon=4){
  argmin <- which.min(distances)
  ratios <- distances[argmin] / distances ;
  ratios[argmin] <- NA; # mask
  ratios_above <- (ratios > epsilon);
  if(sum(ratios_above, na.rm = TRUE)){
    return(c(-argmin, -which(ratios_above)));
  }
  return((argmin))
  
  
  # return list with lower/upper bound assignments
}

get_cluster_indices <- function(assignments, target_k, include_upper=FALSE){
  return(unlist(lapply(assignments, function(x) target_k %in% unlist(x))));
}

rough_k_means <- function(dataset, 
                          k_clusters=3, 
                          w_lower=0.7, 
                          epsilon=0.9, 
                          max_iter=10000,
                          random_state=NULL){
  stopifnot(w_lower > 0 & w_lower < 1)
  if(!is.null(random_state)){
    set.seed(random_state)
  }
  w_upper = 1 - w_lower
  converged <- FALSE
  cluster_assignment <- apply(dataset, 1, function(X) list(sample.int(k_clusters, 1)));
  centroids <- matrix(nrow = k_clusters, ncol= ncol(dataset))
  distances <- matrix(nrow = nrow(dataset), ncol=k_clusters)
  
  compute_centroids <- function(centroids){
    
    for(K in 1:k_clusters){
      # please note that upper approximation here is in fact upper w/o lower, 
      # so it does not follow definition (for the sake of computation)
      lower_app <- get_cluster_indices(cluster_assignment, K)
      upper_app <- get_cluster_indices(cluster_assignment, -K) 
      
      if((sum(lower_app) != 0) & (sum(upper_app) == 0)){
        cluster_data = dataset[lower_app, ]
        centroids[K, ] <- apply(cluster_data, 2, mean) 
      } else if((sum(lower_app) == 0) & (sum(upper_app) != 0)){
        cluster_data = dataset[upper_app, ]
        centroids[K, ] <- apply(cluster_data, 2, mean)
      } else {
        cluster_upper_data <- dataset[upper_app, ];
        cluster_lower_data <- dataset[lower_app, ];
        upper_part_app <- apply(cluster_upper_data, 2, mean)
        lower_part_app <- apply(cluster_lower_data, 2, mean)
        centroids[K, ] <- (w_lower * lower_part_app) + (w_upper * upper_part_app)
      }
      
      
      
    }
    return(centroids)
  }
  
  calculate_distances <- function(distances){
    for(K in 1:k_clusters){
      kth_centroid = centroids[K, ]
      distances[, K] = apply(dataset, 1, function(x) dist(rbind(x, kth_centroid)))
      
    }
    return(distances)
  }
  
  assign_clusters <- function(){
    new_cluster_assignment <- apply(distances, 1, function(x) list(indicate_minimum(x, k_clusters, epsilon)));
    is_same = all(new_cluster_assignment %in% cluster_assignment)
    return(list('converged'=is_same, 'new_cluster_assignment'=new_cluster_assignment))
  }
  iter <- 0;
  while(!converged & iter != max_iter){
    centroids = compute_centroids(centroids);
    distances = calculate_distances(distances);
    assign_res = assign_clusters();
    converged = assign_res$converged;
    cluster_assignment = assign_res$new_cluster_assignment;
    iter = iter +1;
  }
  
  return(list("centroids" = centroids, 
              "cluster_assignments" =  cluster_assignment,
              "k_clusters"=k_clusters,
              "epsilon"=epsilon,
              "w_lower"=w_lower,
              "random_state"=random_state,
              "max_iter"=max_iter,
              "iter_stop"=iter))
  
}

generate_rough_plot <- function(dataset, means_result, plot=TRUE){
  if (ncol(dataset) != 2) stop("Only for 2D plotting")
  # since 
  ass <- means_result$cluster_assignments
  title <- sprintf("eps %.2f, w_lower %.2f iter stop %d seed %d", 
                   means_result$epsilon, means_result$w_lower, means_result$iter, means_result$random_state)
  ass <- unlist(lapply(ass, function(x) ifelse(length(unlist(x)) > 1 , 0 , unlist(x))));
  clusters <- factor(ass)
  cluster_numbres <- as.integer(levels(clusters))
  # +2 in color_fill comes from 1) upper approximation 2) centroid centers
  color_fill <- scales::hue_pal()(means_result$k_clusters + 2);
  
  if(!(0 %in% cluster_numbres)){
    # if there are no points in the upper approximation, do not use red as 
    # this is reserved for upper approximation only and can be misleading between plots
    color_fill <- color_fill[-1]
  } 
    
  names(dataset) <- c("x", "y")
  gobj <- ggplot() + geom_point(d=dataset,aes(x=x, y=y, colour=clusters), size=2);
  gobj <- gobj + scale_color_manual(values = color_fill)
  gobj <- gobj + labs(x = colnames(dataset)[1], y= colnames(dataset)[2])
  gobj <- gobj + geom_point(data=as.data.frame(means_result$centroids), aes(x=V1, y=V2, color='cluster center', size=4))
  gobj <- gobj + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
  
  group <- NULL
  chull_full <- NULL
  fill_scale <- scales::hue_pal()(length(cluster_numbres) + 2)[-1]
  for(cluster_no in as.integer(levels(clusters))){
    if(cluster_no != 0){
      # do not group upper appoximation
      cluster_upper_approx = dataset[ass == cluster_no, ]
      ch <- cluster_upper_approx[chull(cluster_upper_approx),]
      group <- c(group, rep(cluster_no, nrow(ch)))
      chull_full <- rbind(chull_full, ch)
    } 

  } 
  chull_full$group <- group
  gobj <- gobj + geom_polygon(data=chull_full, 
                              aes(x = x, 
                                  y = y, 
                                  group=group, fill=as.factor(group)), alpha=0.3) + 
    scale_fill_manual(values=fill_scale) + scale_size(guide="none") + guides(fill=guide_legend("lower app region"))
  if(plot){
    plot(gobj)
  }
  
  return(gobj)
}

generate_benchmarking_dataset_results <- function(w_lower=c(0.7, 0.5), 
                                                  epsilon=c(0.9, 0.95),
                                                  output_dir=NULL){
  #benchmarking_datasets <- head(get_exemplary_benchmarking_dataset(), -1)
  benchmarking_datasets <- get_exemplary_benchmarking_dataset()
  foreach(dataset=benchmarking_datasets) %do% {
    name <- dataset$name
    points <- dataset$data[c("x", "y")]
    labels <- dataset$data$labels

    
    foreach(wl=w_lower) %do% {
      foreach(eps=epsilon) %do%{
        clustering_res <- rough_k_means(points, w_lower = wl, epsilon = eps, random_state = 100)
        gobj <- generate_rough_plot(points, clustering_res, plot=FALSE)
        fullname <- paste(name, eps, wl, ".png")
        ggsave(
          filename = fullname,
          plot = gobj,
          height=5,
          width=6.5,
          path = output_dir
        )
      }
    }
  }
}

generate_custom_dataset_results <- function(dataset_path=NULL,
                                            output_dir=NULL,
                                            w_lower=c(0.7, 0.5), 
                                            epsilon=c(0.9, 0.95)){
  custom_dataset <- get_custom_test_dataset(dataset_folder = dataset_path)
  disp_data <- custom_dataset[c("growth_rate", "gini_disp_rate")]
  mkt_data <- custom_dataset[c("growth_rate", "gini_mkt_rate")]

  foreach(wl=w_lower) %do% {
    foreach(eps=epsilon) %do%{
  clustering_res <- rough_k_means(disp_data, w_lower = wl, epsilon = eps, random_state = 10)
  gobj <- generate_rough_plot(disp_data, clustering_res, plot=TRUE)
  fullname_disp <- paste("disp", eps, wl, ".png")
  ggsave(
    filename = fullname_disp,
    plot = gobj,
    height=5,
    width=6.5,
    path = output_dir
  )
  
  clustering_res <- rough_k_means(mkt_data, w_lower = wl, epsilon = eps, random_state = 10)
  gobj <- generate_rough_plot(mkt_data, clustering_res, plot=FALSE)
  fullname_mkt <- paste("mkt", eps, wl, ".png")
  ggsave(
    filename = fullname_mkt,
    plot = gobj,
    height=5,
    width=6.5,
    path = output_dir
  )}}
  
}

