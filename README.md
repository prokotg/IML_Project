# Introduction to Machine Learning 2020 WUT - Rough k-means clustering

To manage packages in this project `renv` is used. Please run the following snippet in order to restore packages used in project

```R
install.packages("renv")
renv::restore()
```

In order to reproduce results from raport please run `generate_custom_dataset_results` and `generate_benchmarking_dataset_results` from `rough_clustering.R`. Please note that output directory for plots must be provided as well as location to pwt and swiid datasets
