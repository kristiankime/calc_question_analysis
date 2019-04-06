if(!exists("CALC")) {
  CALC <- list()
}

if(is.null(CALC[["analysis"]])) {
  CALC$analysis <- list()
}

CALC$analysis$calibration_data <- function(predictions.i) { 
  bins <- seq(from=0.0, to=1.0, by=0.05)
  data <- predictions.i[,
                        list(count=.N,
                             correct=sum(correct),
                             rate=sum(correct)/.N,
                             rate_up = CALC$utility$bino_conf_95_up(sum(correct), .N),
                             rate_lo = CALC$utility$bino_conf_95_lo(sum(correct), .N)
                        ) ,
                        by=list(prob_int=cut(prob,breaks=bins))][order(prob_int)]
  setDT(data)
  
  bin_nums <- as.numeric(data$prob_int)
  bins_lower <- bins[bin_nums]
  bins_upper <- bins[bin_nums + 1]
  data[ , prob_lower := bins_lower]
  data[ , prob_upper := bins_upper]
  data[ , prob_middle := (bins_upper + bins_lower) / 2]
  return(data)
}

CALC$analysis$plot_calibration_data <- function(data, pdf_file) {
  # TODO show some kind of bars on these (probably proportions) sqrt(p)
  # https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval
  # https://cran.r-project.org/web/packages/binom/binom.pdf
  # http://stackoverflow.com/questions/13032777/scatter-plot-with-error-bars
  pdf(file=pdf_file)
  
  # with(data, plot(prob_middle, rate, 
  #                 xlim=seq(from=0, to=1, by=.05), xlab="probability",
  #                 ylim=seq(from=0, to=1, by=.05), ylab="rate")
  # )
  
  with(data, plot(prob_middle, rate,
                  xlim=c(0, 1), xlab="probability",
                  ylim=c(0, 1), ylab="rate")
  )
  
  with(data, arrows(prob_middle, rate_up, prob_middle, rate_lo, length=0.05, angle=90, code=3) )
  
  abline(0, 1)
  
  # http://stackoverflow.com/questions/11129432/draw-bloxplots-in-r-given-25-50-75-percentiles-and-min-and-max-values
  # data <- calibration_data.i
  # dats <- as.matrix(data.table(
  #   min      = data$prob_min,
  #   low      = data$prob_low_quantile,
  #   med      = data$prob_median,
  #   up       = data$prob_up_quantile,
  #   max      = data$prob_max
  # ))
  # data.t <- t(dats)
  # colnames(data.t) <- as.character(data$prob_int)
  # 
  #   pdf(file=paste0("outputs/pfa/results/pfa_predictions_", i, ".pdf"))
  #   bxp(list(stats=data.t, n=rep(10, ncol(data.t))), xlim=c(0, ncol(data.t)), ylim=c(0, 1)) 
  #   abline(0, 1/ncol(data.t)) 
  #   dev.off()
  
  dev.off()
}