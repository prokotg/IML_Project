library('pracma')
library('data.table')
library('ggplot2')

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

get_test_dataset <-function(dataset_folder){
  target_countries <- c('United States', 'Mexico', 'Chile', 'Turkey', 'Germany', 'Poland', 'Czech Republic', 'Sweden')
  target_columns <- c('country', 'year')
  env <- environment();
  pwt_path = file.path(dataset_folder, 'pwt91.csv');
  swiid_path= file.path(dataset_folder, 'swiid8_3.rda');
  pwt = read.csv(pwt_path);
  swiid_names = load(swiid_path, envir = env);
  pwt <- pwt[pwt$country %in% target_countries, ]
  swiid_summary <- swiid_summary[swiid_summary$country %in% target_countries, ]
  
  # take only combinations of 
  
  swiid_summary <- dplyr::semi_join(swiid_summary, pwt, by = target_columns)
  pwt <- dplyr::semi_join(pwt, swiid_summary, by = target_columns)
  
  # ensure rows from pwt and swiid are ordered in the same way
  complete_table <- dplyr::inner_join(pwt, swiid_summary, by=target_columns)
  complete_table_by_country <- split(complete_table, complete_table$country)
  
  extract_from_country <- function(country_tab){
    country_tab <- country_tab[c('country', 'year', 'rgdpna', 'pop', 'gini_mkt', 'gini_disp')]
    country_tab <- country_tab[complete.cases(country_tab), ]
    # OX

    growth_rate <- 100*(log(country_tab$rgdpna/country_tab$pop) - data.table::shift(log(country_tab$rgdpna/country_tab$pop)))
    growth_rate <- growth_rate[complete.cases(growth_rate)]
    # OY 1
    gini_mkt_rate <- 100*(log(country_tab$gini_mkt) - data.table::shift(log(country_tab$gini_mkt)));
    gini_mkt_rate <- gini_mkt_rate[complete.cases(gini_mkt_rate)]
    # OY 2
    gini_disp_rate <- 100*(log(country_tab$gini_disp) - data.table::shift(log(country_tab$gini_disp)));
    gini_disp_rate <- gini_disp_rate[complete.cases(gini_disp_rate)]
    
    country_label <- rep(unique(country_tab$country), length(growth_rate))
    return(data.frame(country_label, growth_rate, gini_mkt_rate, gini_disp_rate))
  }
  
  extracted_data <- lapply(complete_table_by_country, extract_from_country)
  full_extracted_data <- Reduce(rbind, extracted_data)
  
  return(full_extracted_data)
  }