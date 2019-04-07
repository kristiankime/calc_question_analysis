initial_pfa_files <- function(attempts=c(1:1), input_prefix="FIE", output_prefix="FIE", questions_location="data/questions/questions_details.243.csv") {
  print(paste0("initial_pfa_files attempts=[",attempts,"] input_prefix=[",input_prefix," output_prefix=[",output_prefix,"] questions_location=[", questions_location,"]"))
  
  # --- input file Locations ---
  answers_correct_test_long_location <- LOC$answers_correct_test_long_location(input_prefix)
  answers_correct_train_long_location <- LOC$answers_correct_train_long_location(input_prefix)
  
  # --- output file Locations ---
  questions_factors_location <- LOC$questions_factors_location(output_prefix)
  factors_list_location <- LOC$factors_list_location(output_prefix)
  
  answers_correct_test_factor_location <- LOC$answers_correct_test_factor_location(output_prefix)
  answers_correct_train_factor_location <- LOC$answers_correct_train_factor_location(output_prefix)
  
  # ===========================
  # Processing Below here
  # ===========================
  
  # ======== Question Factors Data =====
  
  # Master list of which questions contain which factors
  questions_factors <- CALC$question$read_table(questions_location)
  # Switch NAs to FALSE, drop link column and rename "question" -> "problem_source" to match answers files
  questions_factors_prep <- cbind(question_id = CALC$question$short_names(questions_factors$question), CALC$utility$na2False(questions_factors ))[,link:=NULL][,question:=NULL]
  CALC$utility$write_csv_and_robj(questions_factors_prep, questions_factors_location)
  
  # List of factors
  factors_list <- setdiff( names(questions_factors_prep), c("question_id"))
  CALC$utility$write_txt(factors_list, factors_list_location)
  
  # train data with factor information (each of these is 5+ min)
  for(i in attempts) {
    answers_correct_train.i <- CALC$utility$read_robj(answers_correct_train_long_location(i))
    answers_correct_train.i.factors <- merge(answers_correct_train.i, questions_factors_prep, by=c("question_id"))
    answers_correct_train.i <- CALC$pfa$compute_answers_correct_with_factors(answers_correct_train.i.factors, factors_list)
    CALC$utility$write_csv_and_robj(answers_correct_train.i, answers_correct_train_factor_location(i))
  }
  
  # test data with factor information (each of these is 2+ min)
  for(i in attempts) {
    answers_correct_test.i <- CALC$utility$read_robj(answers_correct_test_long_location(i))
    answers_correct_test.i.factors <- merge(answers_correct_test.i, questions_factors_prep, by=c("question_id"))
    answers_correct_test.i <- CALC$pfa$compute_answers_correct_with_factors(answers_correct_test.i.factors, factors_list)
    CALC$utility$write_csv_and_robj(answers_correct_test.i, answers_correct_test_factor_location(i))
  }
}
