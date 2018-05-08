quantile_f <- function(data, v_n, q){
  
  for (i in 1:length(v_n)){
    dat_num = as.numeric(unlist(data[, v_n[i]]))
    data[[paste(v_n[i], "q", sep = "_")]] = cut(dat_num, breaks = unique(quantile(dat_num, probs = seq(0, 1, by = 1/q), na.rm = TRUE)),
                                                labels = FALSE, include.lowest = TRUE) - 1
  }
  
  return(data)
}