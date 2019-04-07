pfa_prediction_accuracy_files <- function(attempts=c(1:1), input_prefix="FIE", output_prefix="FIE"){
  
  # --- input file Locations ---
  # pfa_prediction_location <- function(n) { return( paste0("outputs/pfa/predict/pfa_prediction_",n) ) }
  pfa_prediction_location <- LOC$pfa_prediction_location(output_prefix)
  
  # --- output file Locations ---
  pfa_calibration_plot_location <- function(n) { return( paste0("outputs/", output_prefix, "/pfa/predict/pfa_prediction_plot_",n,".pdf") ) }
  pfa_calibration_chisqr_location <- function(n) { return( paste0("outputs/", output_prefix, "/pfa/predict/pfa_prediction_chisqr_",n) ) }
  pfa_calibration_chisqr_C_location <- function(n) { return( paste0("outputs/", output_prefix, "/pfa/predict/pfa_prediction_chisqr_C_",n,".txt") ) }
  # pfa_calibration_brier_location <- function(n) { return( paste0("outputs/", output_prefix, "/pfa/predict/pfa_prediction_brier_",n,".txt") ) }
  
  # ===========================
  # Processing Below here
  # ===========================
  
  for(i in attempts) {
    predictions.i <- CALC$utility$read_robj(pfa_prediction_location(i))
    
    # overall_performance.brier <- brierscore(correct ~ prob, data=predictions.i)
    # # summary(overall_performance.brier)
    # CALC$utility$write_txt(summary(overall_performance.brier), pfa_calibration_brier_location(i))
    
    chisqr_data <- CALC$stats$prob_accuracy_chisq_data(predictions.i)
    CALC$utility$write_csv_and_robj(chisqr_data, pfa_calibration_chisqr_location(i))
    
    chisq_Q_sum <- CALC$stats$prob_accuracy_chisq_Q_sum(chisqr_data)
    CALC$utility$write_txt(chisq_Q_sum, pfa_calibration_chisqr_C_location(i))
    
    calibration_data.i <- CALC$analysis$calibration_data(predictions.i)
    CALC$analysis$plot_calibration_data(calibration_data.i, pfa_calibration_plot_location(i))
  }

}