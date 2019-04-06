if(!exists("CALC")) {
  CALC <- list()
}

if(is.null(CALC[["irt"]])) {
  CALC$irt <- list()
}

# ===================================
# Produce a log likelihood table with one point per each IRT model
# ===================================
CALC$irt$log_likelihood_table <- function(indicies, file_function) {
  return(
    foreach(i = indicies, .combine = rbind) %do% { 
      return(tryCatch({
        val <- NULL # val will be used as return so set to NULL in case we fail
        ltm_model.i.read <- readRDS(file_function(i))
        val <- data.table(index = i, log_lik = ltm_model.i.read$log.Lik) # If we succeed val will now be set
      }, 
      warning = function(w) { print(paste0("Warning on loop ", i, " [", w, "]")) }, 
      error = function(e) { print(paste0("Error on loop ", i, " [", e, "]")) },
      finally = {
        return(val) # return value set in main block
      }))
    }
  )
}

# ===================================
#
# ===================================
CALC$irt$ltm_models_question_diff <- function(indicies, file_function) { return(
    foreach(i = indicies, .combine = rbind) %do% { 
      return(tryCatch({
      val <- NULL # val will be used as return so set to NULL in case we fail
      ltm_model.i.read <- readRDS(file_function(i))
      ltm_model.i.read.summary <- summary(ltm_model.i.read)
      ltm_model.i.coefficients.row_names <- row.names(ltm_model.i.read.summary$coefficients)
      ltm_model.i.coefficients.row_names.diff <- Filter(function(x){return(startsWith(x, "Dffclt."))}, ltm_model.i.coefficients.row_names )
      ltm_model.i.coefficients.diff <- ltm_model.i.read.summary$coefficients[ltm_model.i.coefficients.row_names.diff, ]
      
      # TODO Some of the values are huge filtering them out for now    
      # Filter( function(x){return( x > 4 || x < -4)}, ltm_model.i.coefficients.diff[, "value"])
      # mean_value <- mean(  Filter( function(x){return( x <= 4 && x >= -4)}, ltm_model.i.coefficients.diff[, "value"])  )
      mean_value <- mean(ltm_model.i.coefficients.diff[, "value"])
      
      # TODO Some of the std.errs are huge filtering them out for now
      # Filter( function(x){return(x>10)}, ltm_model.i.coefficients.diff[, "std.err"])
      # mean_std_err <- mean( Filter(function(x){return(x<10)}, ltm_model.i.coefficients.diff[, "std.err"]) )
      mean_std_err <- mean(ltm_model.i.coefficients.diff[, "std.err"])
      
      val <- data.table(index = i, mean_value = mean_value, mean_std_err = mean_std_err ) # If we succeed val will now be set
    }, 
    warning = function(w) { print(paste0("Warning on loop ", i, " [", w, "]")) }, 
    error = function(e) { print(paste0("Error on loop ", i, " [", e, "]")) },
    finally = {
      return(val) # return value set in main block
    }))
  }
)}



# ===================================
# Item Characteristic Curve ICC for models
# ===================================
CALC$irt$item_characteristic_curve <- function(model_file, output_file) {
  tryCatch({
    ltm_model.i.read <- readRDS(model_file)
    pdf(file=output_file)
    plot(ltm_model.i.read, type="ICC", labels=rep("", nrow(ltm_model.i.read$coefficients))) # all items at once
    dev.off()
  }, 
  warning = function(w) { print(paste0("Warning for model_file [", model_file, "] output_file [", output_file, "]")) }, 
  error = function(e) { print(paste0("Error for model_file [", model_file, "] output_file [", output_file, "]")) }
  )
}



# ===================================
# Item Coef (Discrimination + Difficulty) for each 2PL model
# ===================================
# for(i in c(1:6)) {
CALC$irt$item_ceof <- function(model_file) {  
  ltm_model.i.read <- readRDS(model_file)
  ltm_model.i.read.summary <- summary(ltm_model.i.read)
  ltm_model.i.read.summary.coefficients <- ltm_model.i.read.summary$coefficients
  ltm_model.i.coefficients.row_names <- row.names(ltm_model.i.read.summary$coefficients)
  # get discrimination
  ltm_model.i.coefficients.row_names.disc <- Filter(function(x){return(startsWith(x, "Dscrmn."))}, ltm_model.i.coefficients.row_names )
  ltm_model.i.coefficients.disc <- ltm_model.i.read.summary$coefficients[ltm_model.i.coefficients.row_names.disc, ]
  # get difficulty
  ltm_model.i.coefficients.row_names.diff <- Filter(function(x){return(startsWith(x, "Dffclt."))}, ltm_model.i.coefficients.row_names )
  ltm_model.i.coefficients.diff <- ltm_model.i.read.summary$coefficients[ltm_model.i.coefficients.row_names.diff, ]
  # table with disc and diff
  
  ltm_model.i.coefficients.name <- gsub("Dscrmn.", "", ltm_model.i.coefficients.row_names.disc)
  
  ltm_models.i.disc_diff <- data.table(question_id = ltm_model.i.coefficients.name,
                                       discrimination = ltm_model.i.coefficients.disc[, "value"], 
                                       difficulty = ltm_model.i.coefficients.diff[, "value"],
                                       guessing = 0,
                                       slipping = 1)
  
  #ltm_models.i.disc_diff2 <- cbind( ltm_model.i.coefficients.disc[, "value"], ltm_model.i.coefficients.diff[, "value"], 0, 1)
  # write.csv_and_robj(ltm_models.i.disc_diff, paste0("outputs/irt/2P/all/coef/ltm_coef_", i))
  return(ltm_models.i.disc_diff)
}



# ===================================
# Probabilty vs Actual data
# ===================================

CALC$irt$p_2P_irt <- function(skill, discrimination, difficulty) {
  return(         1 / 
                    (1 + exp( -(discrimination * (skill - difficulty))))
  )
}


# for(i in(1:6)) {
#   if(i <= 4) {
#     item_coef.i.named <- readRDS(paste0("outputs/irt/2P/train/coef/ltm_coef_train_", i, ".robj")) # aka Item Bank
#   } else {
#     item_coef.i.named <- readRDS(paste0("outputs/irt/2P/all/coef/ltm_coef_", i, ".robj")) # aka Item Bank
#   }
  
# answers.i         <- readRDS(paste0("outputs/answers/wide/test/answers_wide_test_", i, ".robj"))

  
CALC$irt$predict_versus_actual <- function(ltm_coef_location, answers_location) { 
  item_coef.i.named <- CALC$utility$read_robj_table(ltm_coef_location)
  item_coef.i       <- item_coef.i.named[,-1,with=FALSE]
  question_ids      <- item_coef.i.named$question_id
  num_questions     <- length(question_ids)
  answers.i         <- CALC$utility$read_robj_table(answers_location)
  
  # --- for each student --- 
  print(paste0("num students ", length(answers.i$anon_id)))
  
  students_prob <- foreach(a_id = answers.i$anon_id, .combine = rbind) %do% {
    print(paste0("current student ", a_id))
    answers.i.student.s <- answers.i[anon_id==a_id, -1, with=FALSE]
    # Start with a blank answer slate
    cur_answer_vector <- setDT(setNames(data.frame(matrix(as.integer(rep(NA, num_questions)), ncol = num_questions, nrow = 1)), question_ids))
    
    # --- for each question ---
    student_prob <- foreach(q_id = question_ids, .combine = rbind) %do% {
      current_correct <- as.integer(answers.i.student.s[1, q_id, with=FALSE])
      if(is.na(current_correct)) { return(NULL) }
      student_skill <- thetaEst(item_coef.i, as.integer(cur_answer_vector))
      discrimination <- item_coef.i.named[question_id==q_id,]$discrimination
      difficulty <- item_coef.i.named[question_id==q_id,]$difficulty
      prob_est <- CALC$irt$p_2P_irt(student_skill, discrimination, difficulty)
      
      ret <- data.table(anon_id=a_id, question_id=q_id, skill=student_skill, prob=prob_est, correct=current_correct)
      
      question_id_quoted <- quote(q_id)
      cur_answer_vector[1, eval(question_id_quoted) := current_correct]
      return(ret)
    }
    return(student_prob)
  }
  return(students_prob)
}
