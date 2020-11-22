# Introduction to Machine Learning 2020 WUT - Rough k-means clustering

To manage packages in this project `renv` is used. Please run the following snippet in order to restore packages used in project

```R
install.packages("renv")
renv::restore()
```
In order to reproduce results and to see exemplary `rough_k_means()` call please do the following:

1) set working directory to root folder of project
2) `source('reproduce.R')`

SWIID data used in project is in version 8.3, while PWT comes in version 9.1. Please check the following resourcesL

* https://www.rug.nl/ggdc/productivity/pwt/
* https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/LM4OWF

Reading files assumes `.csv` file extension.

Please note `rough_k_means` always takes `DataFrame` as input