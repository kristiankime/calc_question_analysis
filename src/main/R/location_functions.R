if(!exists("LOC")) {
  LOC <- list()
}

# location of raw answers directory
LOC$answers_dir <- "data/answers"

# Original data location
LOC$answers_location <- "data/answers/answers16_17.csv"

# Questions with http links to webwork
LOC$question_links_location <- "outputs/questions/questions_links.csv"

# Answer now have "correctness" information added
LOC$answers_correct_location <- "outputs/answers/answers_correct"

# Same as correct file above execpt reduced columns and question ids have been shrunk
LOC$answers_correct_minimal_location <- "outputs/answers/answers_correct_minimal"

# Question Skills file location
# LOC$questions_location <- "data/questions/questions_details.csv"


#-----------------
# Long answer files (one row per user+question)
LOC$answers_correct_attempts_location <- function(n) { return( paste0("outputs/answers/answers_correct_attempts_",n) ) }
LOC$answers_correct_test_long_location <- function(prefix) { 
  function(n) { return( paste0("outputs/",prefix,"/answers/test/answers_correct_test_long_",n) ) }
}
LOC$answers_correct_train_long_location <- function(prefix) { 
  function(n) { return( paste0("outputs/",prefix,"/answers/train/answers_correct_train_long_",n) ) }
}
  
#-----------------
# Wide answer files (one row per user, one column per question)
LOC$answers_correct_wide_location <- function(n) { return( paste0("outputs/answers/answers_correct_wide_",n) ) }
LOC$answers_correct_test_wide_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/answers/test/answers_correct_test_wide_",n) ) }
}
LOC$answers_correct_train_wide_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/answers/train/answers_correct_train_wide_",n) ) }
}

#-----------------
# Locations of the ids used for the various sets 
LOC$anon_ids_location <- "outputs/answers/anon_ids"
LOC$anon_ids_test_location <- function(prefix) { paste0("outputs/",prefix,"/answers/test/anon_test_ids") }
LOC$anon_ids_train_location <- function(prefix) { paste0("outputs/",prefix,"/answers/train/anon_train_ids") }

#-----------------
# Item response theory model files
LOC$ltm_model_all_location <- function(prefix) {
  function(n) { paste0("outputs/",prefix,"/irt/model2P/all/model/ltm_model_all_", n, ".robj") }
}
LOC$ltm_model_train_location <- function(prefix) {
  function(n) { paste0("outputs/",prefix,"/irt/model2P/train/model/ltm_model_train_", n, ".robj") }
}

#-----------------
# Item response theory analysis files
LOC$ltm_log_lik_all_location <- function(prefix) {
  paste0("outputs/",prefix,"/irt/model2P/all/analysis/ltm_model_all_log_lik")
}
LOC$ltm_log_lik_train_location <- function(prefix) {
  paste0("outputs/",prefix,"/irt/model2P/train/analysis/ltm_model_train_log_lik")
}
LOC$ltm_item_diff_all_location <- function(prefix) {
  paste0("outputs/",prefix,"/irt/model2P/all/analysis/ltm_model_all_item_diff")
}
LOC$ltm_item_diff_train_location <- function(prefix) {
  paste0("outputs/",prefix,"/irt/model2P/train/analysis/ltm_model_train_item_diff")
}
LOC$ltm_item_icc_all_location <- function(prefix) {
  function(n) { paste0("outputs/",prefix,"/irt/model2P/all/analysis/ltm_model_all_icc_", n, ".pdf") }
}
LOC$ltm_item_icc_train_location <- function(prefix) {
  function(n) { paste0("outputs/",prefix,"/irt/model2P/train/analysis/ltm_model_train_icc_", n, ".pdf") }
}

#-----------------
# Item response theory prediction files
# -Item Coeficients
LOC$ltm_item_coef_all_location <- function(prefix) {
  function(n) { paste0("outputs/",prefix,"/irt/model2P/all/predict/ltm_model_all_icc_", n) }
}
LOC$ltm_item_coef_train_location <- function(prefix) {
  function(n) { paste0("outputs/",prefix,"/irt/model2P/train/predict/ltm_model_train_icc_", n) }
}
# -Prob vs correct
LOC$ltm_prob_vs_correct_location <- function(prefix) {
  function(n) { paste0("outputs/",prefix,"/irt/model2P/test/predict/ltm_prob_vs_correct_", n) }
}
LOC$ltm_predict_calibration_location<- function(prefix) {
  function(n) { paste0("outputs/",prefix,"/irt/model2P/test/predict/ltm_predict_calibration_", n, ".pdf") }
}

#-----------------
# 
LOC$questions_factors_location <- function(prefix) { paste0("outputs/",prefix,"/pfa/questions_factors") }
LOC$factors_list_location <- function(prefix) { paste0("outputs/",prefix,"/pfa/factors_list") }

# - correct answers with only factors for question included
LOC$answers_correct_test_factor_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/pfa/answers/test/answers_correct_test_factor_",n) ) }
}
LOC$answers_correct_train_factor_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/pfa/answers/train/answers_correct_train_factor_",n) ) }
}

# - correct answers with all factors included
LOC$answers_correct_test_all_factor_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/pfa/answers/test/answers_correct_test_all_factor_",n) ) }
}
LOC$answers_correct_train_all_factor_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/pfa/answers/train/answers_correct_train_all_factor_",n) ) }
}

# - pfa coef files
LOC$factors_coef_train_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/pfa/coef/train/factors_coef_train_",n) ) }
}
LOC$factors_coef_test_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/pfa/coef/test/factors_coef_test_",n) ) }
}

# 
LOC$pfa_prediction_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/pfa/predict/pfa_prediction_",n) ) }
}

LOC$pfa_calibration_plot_location <- function(prefix) {
  function(n) { return( paste0("outputs/",prefix,"/pfa/predict/pfa_prediction_plot_",n,".pdf") ) }
}

# Search Predictions location
LOC$pfa_search_prediction_location <- function(prefix) {
  function(question, switch_factor, n) { 
    question_safe <- gsub("/", "_", question)
    return( paste0("outputs/",prefix,"/pfa/predict/", question_safe, "/pfa_prediction_", switch_factor,"_",n) ) 
  }
}

LOC$pfa_search_prediction_rmse.q <- function(prefix) {
  function(question, n) { 
    question_safe <- gsub("/", "_", question)
    return( paste0("outputs/",prefix,"/pfa/predict/", question_safe, "/pfa_rmse_",n) ) 
  }
}

LOC$pfa_search_prediction_rmse <- function(prefix) {
  function(n) { 
    return( paste0("outputs/",prefix,"/pfa/predict/pfa_rmse_",n) ) 
  }
}

LOC$questions_clustered <- function(prefix) {
  function(n, num_clust) { 
    return( paste0("outputs/",prefix,"/cluster/questions_clustered_",n,"_", num_clust) ) 
  }
}

LOC$questions_clustered_group <- function(prefix) {
  function(n, num_clust) { 
    return( paste0("outputs/",prefix,"/cluster/questions_clustered_",n,"_", num_clust, ".grouped") ) 
  }
}


# LOC$pfa_search_prediction_chisq.q <- function(prefix) {
#   function(question, n) { 
#     question_safe <- gsub("/", "_", question)
#     return( paste0("outputs/",prefix,"/pfa/predict/", question_safe, "/pfa_chisq_",n) ) 
#   }
# }
# 
# LOC$pfa_search_prediction_chisq <- function(prefix) {
#   function(n) { 
#     question_safe <- gsub("/", "_", question)
#     return( paste0("outputs/",prefix,"/pfa/predict/pfa_chisq_",n) ) 
#   }
# }