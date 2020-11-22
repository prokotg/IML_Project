source('src/benchmarking_data.R')
source('src/custom_data.R')
source('src/rough_clustering.R')

# reproduce results
# save all generated plots in 'reproduction_results'

dir.create("reproduction_results")
generate_benchmarking_dataset_results(output_dir = './reproduction_results')
generate_custom_dataset_results(dataset_path = "./data", output_dir = './reproduction_results')

# exemplary rough k-means call
ex_data <- read.csv('./data/exemplary_data.csv')
data <- ex_data[c("x", "y")]
set.seed(100)
clustering_res <- rough_k_means(data, k_clusters = 4)
generate_rough_plot(data, clustering_res)