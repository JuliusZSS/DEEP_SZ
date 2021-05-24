# This script uses real-world data


rm(list = ls())
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(matrixStats)
library(ggplot2)
library(pcalg)
source('get_z_significant.R')
source('get_edit_distance.R')
source('stratification.R')
source("merge_patterns.R")
source("calculate_edit_distance.R")
source('adjust_CATE.R')

# # import data
# data_name = 'US_census'
# data_treatment = 'educ.12'
# data_outcome = 'income.50K'

# # email analytics women
# data_name = 'email_analytics_women'
# data_treatment = 'segment'
# data_outcome = 'visit'

# marketing campaign
data_name = 'marketing_campaign'
data_treatment = 'TREATMENT'
data_outcome = 'PURCHASE'

# # criteo
# data_name = 'criteo_uplift'
# data_treatment = 'treatment'
# data_outcome = 'visit'

alpha = 0.01

################################################################################
# batch = 1
# n_fold = 1

for (batch in 1:1) {
  for (n_fold in 1:1) {
    
    start_time <- Sys.time()
    
    
    data_file = paste("../real_world_data/data_",data_name,"/model_vs_contingency_table_",data_name,"/cross_validation_time_",batch,"_fold_",n_fold,"_for_model.csv", sep = "")
    output_csv = paste("../real_world_data/data_",data_name,"/model_vs_contingency_table_",data_name,"_deep_sz/cross_validation_batch_",batch,"_fold_",n_fold,"_patterns.csv", sep="")
    
    
    input_data = read.csv(data_file)
    # colnames(input_data) = toupper(colnames(input_data)) 
    input_data['X'] = NULL
    input_data['leaf_index'] = NULL
    input_data = input_data %>% relocate(all_of(data_treatment), .after = last_col())
    input_data = input_data %>% relocate(all_of(data_outcome), .after = last_col())
    colnames(input_data)[(ncol(input_data)-1):ncol(input_data)] = c('W', 'Y')
    
    input_data_outcome = input_data['Y']
    input_data_variables = input_data[,!(names(input_data) %in% c('Y'))]
    
    # call PC-simple from pcalg
    pc1 = pcSelect(input_data_outcome, input_data_variables, alpha,
                   corMethod = "standard", verbose = FALSE, directed = TRUE)
    # save results
    # limit to top 8 variables
    # if (sum(pc1$G) > 8) {
    #   zThreshold = sort(pc1$zMin, decreasing = TRUE)[8]
    #   pc1$G = ifelse(pc1$zMin >= zThreshold, TRUE, FALSE)
    # }
    
    
    # whether variable(x) is independent of treatment(w)
    input_data_variables_gsq_test = input_data_variables[, which(pc1$G)]
    input_data_variables_gsq_test_matrix = matrix(unlist(input_data_variables_gsq_test),
                                                  ncol = ncol(input_data_variables_gsq_test), 
                                                  nrow = nrow(input_data_variables_gsq_test))
    
    if (colnames(input_data_variables_gsq_test)[ncol(input_data_variables_gsq_test)] != 'W') {
      input_data_variables_gsq_test_matrix = cbind(input_data_variables_gsq_test_matrix, 
                                                   input_data_variables$W)
    }
    
    x_w_pvalue = sapply(1:(ncol(input_data_variables_gsq_test_matrix)-1), gSquareBin, 
                        y = ncol(input_data_variables_gsq_test_matrix),
                        S=NULL, dm=input_data_variables_gsq_test_matrix)
    x_w_pvalue_independent = ifelse(x_w_pvalue>0.05, TRUE, FALSE)
    names(x_w_pvalue_independent) = names(input_data_variables_gsq_test)[1:(ncol(input_data_variables_gsq_test_matrix)-1)]
    
    # number of features
    n_features = length(x_w_pvalue_independent)
    
    # stratification
    input_data_true_features = input_data[,c(names(x_w_pvalue_independent),'W','Y')]
    first_hash_table = stratification(n_features, input_data_true_features)
    colnames(first_hash_table)[1:n_features] = names(x_w_pvalue_independent)[1:n_features]
    
    # add 0.5 to prevent zero division
    first_hash_table$n11 = first_hash_table$n11 + 0.5
    first_hash_table$n12 = first_hash_table$n12 + 0.5
    first_hash_table$n21 = first_hash_table$n21 + 0.5
    first_hash_table$n22 = first_hash_table$n22 + 0.5
    
    # calculate pi_1, pi_2, phi
    first_hash_table$pi_1 = first_hash_table$n11 / (first_hash_table$n11 + first_hash_table$n12)
    first_hash_table$pi_2 = first_hash_table$n21 / (first_hash_table$n21 + first_hash_table$n22)
    first_hash_table$phi = first_hash_table$pi_1 - first_hash_table$pi_2
    first_hash_table$z_significant = get_z_significant(first_hash_table)
    first_hash_table$index = rownames(first_hash_table)
    first_hash_table_ordered = first_hash_table[order(-first_hash_table$phi),] # sort
    
    # set significant patterns
    z_significant_threshold = 1.96  # gamma = 95%
    
    
    for (dist_k in 1:n_features) {
      # start searching from row 1
      starting_row = 1
      
      # merge
      while (starting_row < nrow(first_hash_table_ordered)) {
        
        if (all(first_hash_table_ordered$z_significant > z_significant_threshold)) {break}
        
        first_hash_table_ordered = first_hash_table_ordered[order(-first_hash_table_ordered$phi),] # sort
        phi_top_1 = first_hash_table_ordered[starting_row, ] # select the top phi row
        first_hash_table_ordered_working = first_hash_table_ordered[-c(1:starting_row),] # split the top rows from the rest
        
        for (i in 1:nrow(first_hash_table_ordered_working)) {
          
          dist_curr = calculate_edit_distance(phi_top_1[1, 1:n_features],
                                              first_hash_table_ordered_working[i, 1:n_features])
          
          if ((phi_top_1$z_significant < z_significant_threshold | 
               first_hash_table_ordered_working[i, 'z_significant'] < z_significant_threshold) &
              dist_curr <= dist_k){
            
            # merge and add new pattern
            new_pattern = merge_patterns(first_hash_table,
                                         phi_top_1,
                                         first_hash_table_ordered_working[i,],
                                         n_features)
            first_hash_table_ordered = rbind(first_hash_table_ordered, new_pattern)
            
            print(new_pattern)
            
            # delete used patterns
            if (phi_top_1$z_significant < z_significant_threshold) {
              first_hash_table_ordered = first_hash_table_ordered[-which(first_hash_table_ordered$index == phi_top_1$index),]
            }
            if (first_hash_table_ordered_working[i,'z_significant'] < z_significant_threshold) {
              first_hash_table_ordered = first_hash_table_ordered[-which(first_hash_table_ordered$index == first_hash_table_ordered_working[i,'index']),]
            }
            
            print(nrow(first_hash_table_ordered))
            
            starting_row = 1
            break # jump out to reorder table
          }
          
        }
        
        starting_row = starting_row + 1
        
      }
      
    }
    
    
    first_hash_table_ordered_cp = first_hash_table_ordered
    
    # adjustment for CATE
    third_hash_table = first_hash_table_ordered_cp
    if (any(!x_w_pvalue_independent) & anyNA(third_hash_table)) {
      print('Start adjusting...')
      # select rows that need adjustment
      col_adjustment = names(which(x_w_pvalue_independent==FALSE))
      select_by_col = is.na(third_hash_table[,col_adjustment])
      # select rows have NA in selected columns
      if (length(col_adjustment) > 1) {
        select_by_col_bool = apply(select_by_col, 1, any)
      } else {
        select_by_col_bool = select_by_col
      }
      # select rows that need adjustment and remove them from third hash table
      need_adjustment = third_hash_table[which(select_by_col_bool),]
      third_hash_table = third_hash_table[-which(select_by_col_bool),]
      
      # loop over rows in need_adjustment
      for (i in 1:nrow(need_adjustment)) {
        adjusted_CATE = adjust_CATE(first_hash_table, need_adjustment[i,], col_adjustment)
        need_adjustment[i, 'phi'] = adjusted_CATE
      }
      
      # feed new table to third_hash_table
      third_hash_table = rbind(third_hash_table, need_adjustment)
      rownames(third_hash_table) = 1:nrow(third_hash_table)
    }
    
    
    # save results
    write.csv(third_hash_table, output_csv)
    
    end_time <- Sys.time()
    end_time - start_time
    
    
  }
}








