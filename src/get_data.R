library('pracma')
library('data.table')
library('ggplot2')
library('zoo')
library('grDevices')

# R implementation of python's sklearn.dataset
make_circles <- function(n_samples = 800, shuffle = TRUE, noise = NULL, random_state = NULL, factor=0.8){
  n_samples_out <- n_samples / 2;
  n_samples_in <- n_samples - n_samples_out;
  linspace_out <- pracma::linspace(0, 2 * pi, n_samples_out)
  linspace_in <- pracma::linspace(0, 2 * pi, n_samples_in)
  outer_circ_x <- cos(linspace_out)
  outer_circ_y <- sin(linspace_out)
  inner_circ_x <- cos(linspace_in) * factor
  inner_circ_y <- sin(linspace_in) * factor
  x <- c(outer_circ_x, inner_circ_x)
  y <- c(outer_circ_y, inner_circ_y)
  return(list(x=x, y=y));
  }


make_moons <- function(n_samples = 800, shuffle = TRUE, noise = NULL, random_state = NULL, factor=0.8){
  n_samples_out <- n_samples / 2;
  n_samples_in <- n_samples - n_samples_out;
  outer_circ_x <- cos(pracma::linspace(0, pi, n_samples_out));
  outer_circ_y <- sin(pracma::linspace(0, pi, n_samples_out));
  inner_circ_x <- 1 - cos(pracma::linspace(0, pi, n_samples_in));
  inner_circ_y <- 1 - sin(pracma::linspace(0, pi, n_samples_in)) - 0.5;
  x <- c(outer_circ_x, inner_circ_x)
  y <- c(outer_circ_y, inner_circ_y)
  return(list(x=X, y=y));
}

make_blobs <- function(n_samples=100, n_features=2, centers=3L, cluster_std=1.0,
                       center_box=c(-10.0, 10.0), random_state=NULL,
                       return_centers=FALSE){  
    if (is.null(centers) | length(centers) == 1){
      centers <- t(sapply(1:n_centers, function(i) runif(n_features, center_box[1], center_box[2])));
      n_centers = 3;
      
    } else {
      if(!is.array(centers) | size(centers)[2] != n_features){
        stop("Centers must be array with \'n_features\' columns");
      }
      n_centers = size(centers);
      
    }
    
  
    

    if(length(cluster_std) != 1 & (length(cluster_std) != n_centers)){
        stop("Length of `clusters_std` not consistent with number of centers");
      
    } else {
      cluster_std <- rep(cluster_std, 3);
    }

      x = NULL;
      y = NULL; 
    # yes, this is  inexact but with large n_samples it does not matter
    n_samples_per_center <- n_samples %/% n_centers;
    print(n_samples_per_center);
    
    for(center in 1:n_centers){
      
      x <- rbind(x, (sapply(1:n_features, function(nth_feat) rnorm(n=n_samples_per_center,mean = centers[center, nth_feat], sd = cluster_std[center]))));
      y <- c(y, rep(center, n_samples_per_center))
      }  
    return(list(x=x, y=y));
}

get_test_dataset <-function(dataset_folder, 
                            target_countries = c('United States', 'Mexico', 'Chile', 'Turkey', 'Germany', 'Poland', 'Czech Republic', 'Sweden'),
                            sliding_window = NULL,
                            slide_by = sliding_window){
  target_columns <- c('country', 'year')
  env <- environment();
  pwt_path = file.path(dataset_folder, 'pwt91.csv');
  swiid_path= file.path(dataset_folder, 'swiid8_3.rda');
  pwt = read.csv(pwt_path);
  swiid_names = load(swiid_path, envir = env);
  if(!is.null(target_countries)){
    pwt <- pwt[pwt$country %in% target_countries, ]
    swiid_summary <- swiid_summary[swiid_summary$country %in% target_countries, ]
    
  }
 
  # take only combinations of 
  
  swiid_summary <- dplyr::semi_join(swiid_summary, pwt, by = target_columns)
  pwt <- dplyr::semi_join(pwt, swiid_summary, by = target_columns)
  
  # ensure rows from pwt and swiid are ordered in the same way
  complete_table <- dplyr::inner_join(pwt, swiid_summary, by=target_columns)
  complete_table_by_country <- split(complete_table, complete_table$country)
  
  extract_from_country <- function(country_tab, mean_width=NULL){
    country_tab <- country_tab[c('country', 'year', 'rgdpna', 'pop', 'gini_mkt', 'gini_disp')]
    country_tab <- country_tab[complete.cases(country_tab), ]
    country_tab <- country_tab[with(country_tab, order(country, year)), ]
    # OX

    growth_rate <- 100*(log(country_tab$rgdpna/country_tab$pop) - data.table::shift(log(country_tab$rgdpna/country_tab$pop)))
    growth_rate <- growth_rate[complete.cases(growth_rate)]
    # OY 1
    gini_mkt_rate <- 100*(log(country_tab$gini_mkt) - data.table::shift(log(country_tab$gini_mkt)));
    gini_mkt_rate <- gini_mkt_rate[complete.cases(gini_mkt_rate)]
    # OY 2
    gini_disp_rate <- 100*(log(country_tab$gini_disp) - data.table::shift(log(country_tab$gini_disp)));
    gini_disp_rate <- gini_disp_rate[complete.cases(gini_disp_rate)]
    
    df <- data.frame(growth_rate, gini_mkt_rate, gini_disp_rate)
    if(!is.null(mean_width)){
      func <- function(x) zoo::rollapply(x, mean_width, mean, by = slide_by);
      df <- apply(df, 2, func);
      if(is.null(nrow(df))){ # weird inconsistency of R, when df would be flat it is considered N-th observations df
        df <- t(df)
      }
      df <- as.data.frame(df);
      
    }
    country_label <- rep(unique(country_tab$country), nrow(df));
    df <- cbind(country_label, df);
    
    return(df)
  }
  if(!is.null(sliding_window)){
    complete_table_by_country <- Filter(function(x) nrow(x) >=sliding_window,complete_table_by_country);
    
    
  }
  extracted_data <- lapply(complete_table_by_country, extract_from_country, mean_width = sliding_window)
  full_extracted_data <- Reduce(rbind, extracted_data)
  
  return(full_extracted_data)
}

plot_dataset <- function(dataset, legend=TRUE, mark_axis=TRUE, title = NULL){
  gobj_disp <- ggplot(dataset, aes(x=growth_rate, y=gini_disp_rate))
  gobj_mkt <- ggplot(dataset, aes(x=growth_rate, y=gini_mkt_rate))
  
  add_properties <- function(gobj){
    if(legend){
      gobj <- gobj + geom_point(aes(col=country_label), size=2);
    } else {
      gobj <- gobj + geom_point();
    }
    
    if(mark_axis){
      gobj <- gobj + geom_hline(yintercept=0, linetype="dashed", color = "red")
      gobj <- gobj + geom_vline(xintercept=0, linetype="dashed", color = "red")
      
      }
    gobj <- gobj + ggtitle(title)
    gobj <- gobj + scale_colour_brewer(palette = "Set1");
    return(gobj);
  }
  
  gobj_disp <- add_properties(gobj_disp)
  gobj_mkt <- add_properties(gobj_mkt)
  plot(gobj_disp)
  plot(gobj_mkt)
  
}

initial_analysis_plots <- function(dataset_folder,sliding_window = 5){
  res_selected <- get_test_dataset(dataset_folder)
  res_selected_sw <- get_test_dataset(dataset_folder, sliding_window = sliding_window)
  res_selected_sw_rolling <- get_test_dataset(dataset_folder, sliding_window = sliding_window, slide_by = 1)
  res_full <- get_test_dataset(dataset_folder, target_countries = NULL)
  res_full_sw <- get_test_dataset(dataset_folder, sliding_window = sliding_window, target_countries = NULL)
  res_full_sw_rolling <-get_test_dataset(dataset_folder, sliding_window = sliding_window, target_countries = NULL, slide_by = 1)
  
  plot_dataset(res_selected, legend = TRUE, title = "Selected countries without sliding window average")
  plot_dataset(res_selected_sw, legend = TRUE, title = "Selected countries with sliding window average")
  plot_dataset(res_selected_sw_rolling, legend = TRUE, title = "Selected countries with rolling mean")
  plot_dataset(res_full, legend = FALSE, title = "All countries without sliding window average")
  plot_dataset(res_full_sw, legend = FALSE, title = "All countries with sliding window average")
  plot_dataset(res_full_sw_rolling, legend = FALSE, title = "All countries with rolling mean");
  
}



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

rough_k_means <- function(dataset, k_clusters=3, w_lower=0.7, w_upper=0.3, epsilon=2, max_iter=10000){
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

while(!converged ){
  centroids = compute_centroids(centroids);
  distances = calculate_distances(distances);
  assign_res = assign_clusters();
  converged = assign_res$converged;
  cluster_assignment = assign_res$new_cluster_assignment;
}

return(list("centroids" = centroids, 
            "cluster_assignments" =  cluster_assignment))

}

plot_rough <- function(dataset, means_result){
  if (ncol(dataset) != 2) stop("Only for 2D plotting")
  ass <- means_result$cluster_assignments
  ass <- unlist(lapply(ass, function(x) ifelse(length(unlist(x)) > 1 , 0 , unlist(x))));
  clusters <- factor(ass)
  gobj <- ggplot() + geom_point(d=dataset,aes(x=dataset[,1], y=dataset[,2], colour=clusters), size=2);
  gobj <- gobj + labs(x = colnames(dataset)[1], y= colnames(dataset)[2])
  gobj <- gobj + geom_point(data=as.data.frame(means_result$centroids), aes(x=V1, y=V2, color='cluster center', size=4))
  gobj <- gobj + ggtitle("Rough k-means with 3 clusters \nepsilon=2") + theme(plot.title = element_text(hjust = 0.5))
  #gobj <- gobj + guides(color=guide_legend("Cluster no.")) + scale_color_hue(labels = c("multiple upper approx.", levels(clusters)[-1], "centroid centers"))
  # gobj <- gobj + scale_color_manual(values = ass) + scale_fill_manual(values = ass)
  #gobj <- gobj + geom_polygon(data=dataset,aes(x = dataset[,1], y=dataset[,2], fill=clusters))
  
  for(cluster_no in as.integer(levels(clusters))[-1]){
    cluster_upper_approx = dataset[ass == cluster_no | ass == -cluster_no, ]
    ch <- cluster_upper_approx[chull(cluster_upper_approx),]
    gobj <- gobj + geom_polygon(data=ch, 
                                aes(x = growth_rate, 
                                    y = gini_mkt_rate, 
                                    fill=scales::hue_pal()(3)[1]), alpha=0.3)
  } 
  #g <- ggplot_build(gobj)
  #print(unique(g$data[[1]]["fill"]))
  plot(gobj)
  return(ass)
}