if(!exists("CALC")) {
  CALC <- list()
}

if(is.null(CALC[["stats"]])) {
  CALC$stays <- list()
}

CALC$stats$bin_centers <- function(bin_size=0.05, from=0.0, to=1.0) {
  # bin_centers <- seq(from=bin_size/2, to=1-bin_size/2, by=bin_size)
  bin_centers <- seq(from=bin_size, to=1, by=bin_size)
  bin_centers
}

CALC$stats$func_bin_center <- function(bin_size=0.05, from=0.0, to=1.0) {
  # split by prob intervals
  bins <- seq(from=from, to=to, by=bin_size)
  # bin_centers <- seq(from=bin_size/2, to=1-bin_size/2, by=bin_size)
  # bin_centers <- seq(from=bin_size, to=1, by=bin_size)
  bin_centers <- CALC$stats$bin_centers(bin_size=bin_size, from=from, to=to)
  function(num) {
    if(num < from || num > to){
      stop(paste("value was outside bin range, num=", num, "from=", from, "to=",to ))
    } else {
      bin_factor <- cut(num,breaks=bins)
      bin_index <- as.numeric(bin_factor)
      bin_centers[bin_index]
    }
  }
}

CALC$stats$prob_accuracy_chisq_Q_val <- function(data, bin_size=0.05) {
  vals <- CALC$stats$prob_accuracy_chisq_data(data, bin_size = bin_size)
  sum(vals$C)
}

CALC$stats$prob_accuracy_chisq_data <- function(data, bin_size=0.05) {
  # https://math.stackexchange.com/questions/1422353/how-can-we-measure-the-accuracy-of-prediction-algorithm
  from=0.0
  to=1.0
  func = CALC$stats$func_bin_center(from=from, to=to, bin_size=bin_size)

  grouped <- data %>% dplyr::select(prob, correct) %>% 
    dplyr::mutate(prob_center=func(prob)) %>% 
    group_by(prob_center) %>% 
    summarise(count=n(), actual=sum(correct), expected=n()*prob_center[1]) %>%
    dplyr::mutate( C = ((expected - actual)^2) / expected )
  
  grouped
}
# tmp <- CALC$stats$prob_accuracy_chisq_data(data)
# CALC$stats$prob_accuracy_chisq_Q_val(data)
