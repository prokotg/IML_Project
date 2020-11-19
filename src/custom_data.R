library('pracma')
library('data.table')
library('ggplot2')
library('zoo')
library('grDevices')
library('foreach')

get_custom_test_dataset <-function(dataset_folder, 
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

plot_custom_test_dataset <- function(dataset, legend=TRUE, mark_axis=TRUE, title = NULL, save=FALSE){
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
    gobj <- gobj + ggtitle(title) + labs(color="Country") + xlab("Growth rate")
    gobj <- gobj + scale_colour_brewer(palette = "Set1");
    return(gobj);
  }
  
  gobj_disp <- add_properties(gobj_disp) + ylab("Gini Disp rate")
  gobj_mkt <- add_properties(gobj_mkt) + ylab("Gini Mkt rate")
  if(!save){
    plot(gobj_disp)
    plot(gobj_mkt)
  } else {
    ggsave(filename = 'disp_rate.png',
           plot = gobj_disp,
           height=5,
           width=6.5);
    
    ggsave(filename = 'mkt_rate.png',
           plot = gobj_mkt,
           height=5,
           width=6.5)
  }
  
  
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
