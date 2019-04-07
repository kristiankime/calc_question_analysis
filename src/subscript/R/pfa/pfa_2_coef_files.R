pfa_coef_files <- function(attempts=c(1:1), input_prefix="FIE", output_prefix="FIE") {
  print(paste0("pfa_coef_files attempts=[",attempts,"] input_prefix=[",input_prefix," output_prefix=[",output_prefix,"]"))
  
  # --- input file Locations ---
  factors_list_location <- LOC$factors_list_location(input_prefix)
  answers_correct_test_factor_location <- LOC$answers_correct_test_factor_location(input_prefix)
  answers_correct_train_factor_location <- LOC$answers_correct_train_factor_location(input_prefix)
  
  # --- output file Locations ---
  factors_coef_train_location <- LOC$factors_coef_train_location(output_prefix)
  factors_coef_test_location <- LOC$factors_coef_test_location(output_prefix)
  
  # ===========================
  # Processing Below here
  # ===========================
  
  # ======== Question Factors Data =====
  factors_list <- CALC$utility$remove_hashed( CALC$utility$read_txt_as_vector(factors_list_location) )
  
  for(i in attempts) {
    print(paste0())
    answer_factor_data.i <- CALC$utility$read_robj(answers_correct_train_factor_location(i))
    factors_coef <- CALC$pfa$pfa_coef(answer_factor_data.i, factors_list)
    CALC$utility$write_csv_and_robj(factors_coef, factors_coef_train_location(i))
  }
  
  for(i in attempts) {
    answer_factor_data.i <- CALC$utility$read_robj(answers_correct_test_factor_location(i))
    factors_coef <- CALC$pfa$pfa_coef(answer_factor_data.i, factors_list)
    CALC$utility$write_csv_and_robj(factors_coef, factors_coef_test_location(i))
  }
}