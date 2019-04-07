pfa_prediction_files <- function(attempts=c(1:1), input_prefix="FIE", output_prefix="FIE"){
  # --- input file Locations ---
  factors_list_location <- LOC$factors_list_location(input_prefix)
  answers_correct_test_factor_location <- LOC$answers_correct_test_factor_location(input_prefix)
  factors_coef_train_location <- LOC$factors_coef_train_location(input_prefix)
  
  # --- output file Locations ---
  pfa_prediction_location <- LOC$pfa_prediction_location(output_prefix)
  
  # ===========================
  # Processing Below here
  # ===========================
  
  # ======== Question Factors Data =====
  factors_list <- CALC$utility$remove_hashed(CALC$utility$read_txt_as_vector(factors_list_location))
  
  for(i in attempts) { # Each run here takes several min
    answer_factor_data.i <- CALC$utility$read_robj(answers_correct_test_factor_location(i))
    factors_coef.i <- CALC$utility$read_robj(factors_coef_train_location(i))
    pfa_pred <- CALC$pfa$pfa_prediction_vs_results(answer_factor_data.i, factors_coef.i, factors_list)
    CALC$utility$write_csv_and_robj(pfa_pred, pfa_prediction_location(i))
  }
}
