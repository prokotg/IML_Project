library('pracma')

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