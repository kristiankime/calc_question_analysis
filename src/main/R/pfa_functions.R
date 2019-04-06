if(!exists("CALC")) {
  CALC <- list()
}

if(is.null(CALC[["pfa"]])) {
  CALC$pfa <- list()
}

CALC$pfa$p_m <- function(m){
  1 / (1 + exp(-1 * m))
}

CALC$pfa$zeroed_list <- function(names) {
  tmp <- list()
  for(name in names) {
    tmp[name] <- 0
  }
  return(tmp)
}

CALC$pfa$zeroed_vectors <- function(names, n) {
  tmp <- list()
  for(name in names) {
    tmp[[name]] <- integer(n)
  }
  return(tmp)
}

#==================
# This function returns a table that keeps a running total of how many correct and
# incorrect answers a student has made to a questions with each of the factors
#==================
CALC$pfa$compute_answers_correct_with_factors <- function(answers_factors, factors, decay = 1, show_all = FALSE) {
  answers <- answers_factors[order(anon_id, timestamp),]
  
  count <- 0 # This count is just used for feedback while this long process is running
  # constants
  n <- nrow(answers)
  n_facs <- length(factors)
  
  # ------
  # These vectors are created at the correct size then values are set on them
  # and finally they are used as the columns in the results data.table
  anon_id_v         <- integer(n)
  question_id_v     <- character(n)
  timestamp_v       <- integer(n)
  correct_v         <- integer(n)
  fact_dummy_v      <- CALC$pfa$zeroed_vectors(factors, n)
  total_correct_v   <- CALC$pfa$zeroed_vectors(factors, n)
  total_incorrect_v <- CALC$pfa$zeroed_vectors(factors, n)
  
  # These need to be reset per person
  id        <- -1
  correct   <- CALC$pfa$zeroed_list(factors)
  incorrect <- CALC$pfa$zeroed_list(factors)
  
  for(r in 1:n) {
    d <- answers[r, ]
    
    if((count %% 1000) == 0) { 
      print(paste0("@ ", count, " of ", n)) 
    }
    
    if(id != d$anon_id) {
      correct   <- CALC$pfa$zeroed_list(factors)
      incorrect <- CALC$pfa$zeroed_list(factors)
      id        <- d$anon_id
    }
    
    anon_id_v[r]         <- id
    question_id_v[r]     <- d$question_id
    timestamp_v[r]       <- d$timestamp
    correct_v[r]         <- d$correct
    
    # factor <- factors[1]
    for(factor in factors) {
      # If the current factor (aka skill) is active then indicate in dummy var and increment correct or incorrect counters
      if( d[[factor]] ) {
        fact_dummy_v[[factor]][r] <- 1
        # Note here we set the values before updating the totals since we should predict before we know their answer  
        total_correct_v[[factor]][r]   <- correct[[factor]]
        total_incorrect_v[[factor]][r] <- incorrect[[factor]]
        if(d$correct) {   
          correct[[factor]] <- (correct[[factor]] * decay) + 1 
        } else { 
          incorrect[[factor]] <- (incorrect[[factor]] * decay) + 1 
        }
      } else if (show_all) { # allways print all the factor counts
        total_correct_v[[factor]][r]   <- correct[[factor]]
        total_incorrect_v[[factor]][r] <- incorrect[[factor]]
      }
    }
    
    count <- count + 1
  }
  
  answers_factors.correct.f <- data.table(anon_id=anon_id_v, question_id=question_id_v, timestamp=timestamp_v, correct=correct_v)
  
  for(factor in factors) {
    answers_factors.correct.f[ , (paste0(factor, "_dummy"))     := fact_dummy_v[[factor]]      ]
    answers_factors.correct.f[ , (paste0(factor, "_correct"))   := total_correct_v[[factor]]   ]
    answers_factors.correct.f[ , (paste0(factor, "_incorrect")) := total_incorrect_v[[factor]] ]
  }
  
  return(answers_factors.correct.f)
}



#==================
# Computes the logit coefficients for the pfa based on the input data and list of coef
#==================
CALC$pfa$pfa_coef_glm <- function(answer_factor_data.i, factors_list) {
  formula_string <- foreach(cur_factor = factors_list, .combine = pryr::partial(paste, sep = " + ")) %do% {
    return(paste0(cur_factor, "_dummy + ", cur_factor, "_correct + ",  cur_factor, "_incorrect")) 
  }
  
  full_formula <- as.formula(paste("correct ~ 0 +", formula_string))
  
  glm.fit <- glm(full_formula, data = answer_factor_data.i, family = binomial)
  
  glm.fit$coefficients["easy_dummy"]
  
  factors_coef <- foreach(cur_factor = factors_list, .combine = rbind) %do% {
    intercept      <- glm.fit$coefficients[ paste0(cur_factor, "_dummy")    ]
    correct_coef   <- glm.fit$coefficients[ paste0(cur_factor, "_correct")  ]
    incorrect_coef <- glm.fit$coefficients[ paste0(cur_factor, "_incorrect")]
    # print(paste0("factor=", cur_factor, " intercept=", intercept, " correct_coef=",correct_coef, " incorrect_coef=",incorrect_coef))
    dt <- data.table(factor=cur_factor, intercept=intercept, correct_coef=correct_coef, incorrect_coef=incorrect_coef)
    return(dt)
  }
}
  
CALC$pfa$pfa_coef <- function(answer_factor_data.i, factors_list) {
  # Use penalties to ensure correct coefficients are positive
  penalized_formula_string <- foreach(cur_factor = factors_list, .combine = pryr::partial(paste, sep = " + ")) %do% {
    return(paste0(cur_factor, "_correct")) 
  }
  penalized_formula <- as.formula(paste("~ 0 +", penalized_formula_string))
  
  unpenalized_formula_string <-  foreach(cur_factor = factors_list, .combine = pryr::partial(paste, sep = " + ")) %do% {
    return(paste0(cur_factor, "_dummy + ", cur_factor, "_incorrect")) 
  }
  unpenalized_formula <- as.formula(paste("~ 0 +", unpenalized_formula_string))
  
  # http://ftp.uni-bayreuth.de/math/statlib/R/CRAN/doc/vignettes/penalized/penalized.pdf
  # optL2.fit <- optL2(answer_factor_data.i$correct, penalized_formula,  unpenalized_formula, positive=TRUE,  maxlambda2 = 100, data=answer_factor_data.i,  model="logistic")
  
  penalized.penfits <- penalized(answer_factor_data.i$correct, 
                                 penalized_formula, 
                                 unpenalized_formula,
                                 # lambda1=0, 
                                 # lambda2=1,
                                 positive=TRUE, 
                                 data=answer_factor_data.i, 
                                 model="logistic")
  
  factors_coef <- foreach(cur_factor = factors_list, .combine = rbind) %do% {
    intercept      <- penalized.penfits@unpenalized[ paste0(cur_factor, "_dummy")    ]
    correct_coef   <- penalized.penfits@penalized[   paste0(cur_factor, "_correct")  ]
    incorrect_coef <- penalized.penfits@unpenalized[ paste0(cur_factor, "_incorrect")]
    # print(paste0("factor=", cur_factor, " intercept=", intercept, " correct_coef=",correct_coef, " incorrect_coef=",incorrect_coef))
    dt <- data.table(factor=cur_factor, intercept=intercept, correct_coef=correct_coef, incorrect_coef=incorrect_coef)
    return(dt)
  }
}


#==================
# build the prediction vs results table 
#==================
CALC$pfa$pfa_prediction_vs_results <- function(answer_factor_data.i, factors_coef.i, factors_list) {
  # Figure out what we need to loop over  
  # anon_ids <- unique(answer_factor_data.i$anon_id)
  # question_ids <- unique(answer_factor_data.i$question_id)
  n <- nrow(answer_factor_data.i)
  
  # Create blank vectors for each of the columns we want
  # anon_id, question_id, prob, correct
  anon_id_v <- integer(n)
  question_id_v <- character(n)
  prob_v <- numeric(n)
  correct_v <- integer(n)
  
  # index <- 1  # keep track of how many total questions (across all students) we've done
  # a_id <- anon_ids[1] 
  for(index in 1:n) {
    answers_status <- answer_factor_data.i[index,]
    
    m <- 0   
    
    # fac <- factors_list[5]
    for(fac in factors_list) {
      if(answers_status[[paste0(fac,"_dummy")]] == 1){
        
        cor <- answers_status[[paste0(fac,"_correct")]]
        inc <- answers_status[[paste0(fac,"_incorrect")]]
        
        coef <- factors_coef.i[factor == fac, ]
        intercept <- coef$intercept
        cor_coef <- coef$correct_coef
        inc_coef <- coef$incorrect_coef
        
        m_int <- intercept + (cor * cor_coef) + (inc * inc_coef)
        m <- m + m_int
      }
    }
    
    # fill out the vectors with out results
    anon_id_v[index] <- answers_status$anon_id
    question_id_v[index] <- answers_status$question_id
    prob_v[index] <- CALC$pfa$p_m(m)
    correct_v[index] <- answers_status$correct
    
    if(index %% 1000 == 0) {
      print(paste0("index ", index, " of ", n))
    }
  }
  
  # build results table
  pfa_results.i <- data.table(anon_id=anon_id_v, question_id=question_id_v, prob=prob_v, correct=correct_v)
  pfa_results.i <- pfa_results.i[anon_id != 0, ] # remove extra rows
  return(pfa_results.i)
}












#==================
# build the prediction vs results table
# this version is intended for individual questions with factors search 
#==================
CALC$pfa$pfa_question_search_prediction_vs_results <- function(answer_factor_data.i.q, factors_coef.i, factors_to_use) {

  # Figure out what we need to loop over  
  n <- nrow(answer_factor_data.i.q)
  
  # Create blank vectors for each of the columns we want
  # anon_id, prob, correct
  anon_id_v <- integer(n)
  prob_v <- numeric(n)
  correct_v <- integer(n)
  
  # loop over all the answers to the question
  for(index in 1:n) {
    answers_status <- answer_factor_data.i.q[index,]
    
    m <- 0   
    
    for(fac in factors_to_use) {
      cor <- answers_status[[paste0(fac,"_correct")]]
      inc <- answers_status[[paste0(fac,"_incorrect")]]
      
      coef <- factors_coef.i[factor == fac, ]
      intercept <- coef$intercept
      cor_coef <- coef$correct_coef
      inc_coef <- coef$incorrect_coef
      
      m_int <- intercept + (cor * cor_coef) + (inc * inc_coef)
      m <- m + m_int
    }
    
    # fill out the vectors with out results
    anon_id_v[index] <- answers_status$anon_id
    prob_v[index] <- CALC$pfa$p_m(m)
    correct_v[index] <- answers_status$correct
    
    if(index %% 1000 == 0) {
      print(paste0("index ", index, " of ", n))
    }
  }
  
  # build results table
  pfa_results.i <- data.table(anon_id=anon_id_v, prob=prob_v, correct=correct_v)
  pfa_results.i <- pfa_results.i[anon_id != 0, ] # remove extra rows
  return(pfa_results.i)
}