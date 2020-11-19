library('pracma')
library('data.table')
library('ggplot2')
library('zoo')
library('grDevices')
library('foreach')

# R implementation of python's sklearn.dataset
make_circles <- function(n_samples = 800, 
                         shuffle = TRUE, 
                         noise = NULL,
                         factor=0.8, 
                         random_state=NULL){
  set.seed(random_state);
  n_samples_out <- n_samples / 2;
  n_samples_in <- n_samples - n_samples_out;
  linspace_out <- pracma::linspace(0, 2 * pi, n_samples_out)
  linspace_in <- pracma::linspace(0, 2 * pi, n_samples_in)
  outer_circ_x <- cos(linspace_out)
  outer_circ_y <- sin(linspace_out)
  inner_circ_x <- cos(linspace_in) * factor
  inner_circ_y <- sin(linspace_in) * factor
  x <- c(outer_circ_x, inner_circ_x)
  if(!is.null(noise)){
    x = x +  rnorm(n_samples, sd = noise)
  }

  y <- c(outer_circ_y, inner_circ_y)
  labels <- c(rep(0, length(linspace_out)), rep(1, length(linspace_in)))
  return(as.data.frame(list(x=x, y=y, labels=labels)));
  }


make_moons <- function(n_samples = 800, shuffle = TRUE, noise = NULL, random_state=NULL){
  set.seed(random_state);

  n_samples_out <- n_samples / 2;
  n_samples_in <- n_samples - n_samples_out;
  outer_circ_x <- cos(pracma::linspace(0, pi, n_samples_out));
  outer_circ_y <- sin(pracma::linspace(0, pi, n_samples_out));
  inner_circ_x <- 1 - cos(pracma::linspace(0, pi, n_samples_in));
  inner_circ_y <- 1 - sin(pracma::linspace(0, pi, n_samples_in)) - 0.5;
  x <- c(outer_circ_x, inner_circ_x)
  if(!is.null(noise)){
    x = x +  rnorm(n_samples, sd = noise)
  }
  y <- c(outer_circ_y, inner_circ_y)
  labels <- c(rep(0, length(outer_circ_x)), rep(1, length(inner_circ_x)))
  
  return(as.data.frame(list(x=x, y=y, labels=labels)));
}

make_blobs <- function(n_samples=800, n_features=2, centers=3L, cluster_std=1.0,
                       center_box=c(-10.0, 10.0), random_state=NULL,
                       return_centers=FALSE){  
    if(!is.null(random_state)){
      set.seed(random_state);
    }
    if (is.null(centers) | length(centers) == 1){
      centers <- t(sapply(1:centers, function(i) runif(n_features, center_box[1], center_box[2])));
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
      labels = NULL
    # yes, this is  inexact but with large n_samples it does not matter
    n_samples_per_center <- n_samples %/% n_centers;

    for(center in 1:n_centers){
      single_blob_data <- (sapply(1:n_features, function(nth_feat) rnorm(n=n_samples_per_center,mean = centers[center, nth_feat], sd = cluster_std[center])));
      x <- c(x, single_blob_data[,1]);
      y <- c(y, single_blob_data[,2]);
      labels <- c(labels, rep(as.integer(center), n_samples_per_center));
      }  
    return(as.data.frame(list(x=x, y=y, labels=labels)));
}

make_anisotrophy <- function(transformation_matrix = NULL, ...){
  if(is.null(transformation_matrix)){
    transformation_matrix <- matrix(c(0.6, -0.6, -0.4, 0.8), nrow = 2, byrow=TRUE)
  }
  
  blobs <- make_blobs(...)
  blobs_tr <- as.matrix(as.data.frame(blobs)[c('x', 'y')] ) %*% transformation_matrix
  blobs$x <- blobs_tr[, 1]
  blobs$y < blobs_tr[, 2]
  return(as.data.frame(blobs))
  
}

make_no_structure <- function(n_samples = 1600, random_state = NULL){
  set.seed(random_state)
  x <- runif(n_samples)
  y <- runif(n_samples)
  labels <- rep(0, n_samples)
  return(as.data.frame(list(x=x, y=y, labels=labels)));
  
}

get_exemplary_benchmarking_dataset <-function(output_dir, save=FALSE){
  # This function returns only datasets from sklearn benchmarking 
  
  blobs_42 <- make_blobs(random_state = 42)
  blobs_42_var <- make_blobs(cluster_std = c(1.0, 2.5, 0.5), random_state =3)
  circles_08 <- make_circles(factor = 0.8, noise=0.05, random_state = 42)
  circles_05 <- make_circles(factor = 0.5, noise=0.05, random_state = 42)
  
  moons <- make_moons(noise=0.05, random_state = 42)
  blobs_42_ani <- make_anisotrophy(transformation_matrix = matrix(c(0.6, -0.6, -0.4, 0.8), nrow = 2,byrow = TRUE), random_state = 49)
  no_structure <- make_no_structure(random_state = 42);
  all_datasets <- list(list(name="blobs",        data=blobs_42), 
                       list(name="blobs_var",    data=blobs_42_var), 
                       list(name="circles8",     data=circles_08),
                       list(name="circles5",     data=circles_05),
                       list(name="moons" ,       data=moons), 
                       list(name="ano",          data=blobs_42_ani), 
                       list(name="no_structure", data=no_structure))
  if(save){
    
    save_single_ex_dataset <- function(dataset, outputdir){
      filename = paste(dataset$name, "png", sep = ".")
      gobj <- ggplot(as.data.frame(dataset$data), aes(x=x, y=y)) + geom_point(aes(color=as.factor(labels)));
      gobj <- gobj + scale_color_discrete() + labs(color="label");
      gobj <- gobj + scale_color_brewer(palette="Accent")
      
      ggsave(
        filename = filename, 
        height=5,
        width=6.5,
        plot = gobj,
        path =outputdir
      )
    }
    lapply(all_datasets, function (x) {save_single_ex_dataset(x, output_dir)})
  }
  return(all_datasets)
  
}






