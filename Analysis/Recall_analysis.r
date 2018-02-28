# What I want to do in this script
# 1) basic analysis about the performance difference between pre/post training
# 2) the recall performance, based on the following variables
#    a. output position
#    b. input position
#    c. number of updating
#    d. load
#
# That's it for now.

determineNumberofUpdating <- function(data){
  data$n_updates <- 0
  
  n_obs <- length(data$code)
  max_sz <- max(data$load)
  
  for (trial in 1:n_obs){
    if ((trial * 100) %% n_obs == 0){
      print(sprintf('%d', trial * 100 /n_obs))
    }
    # print(trial)
    # reset n_updates at the beginning of the new trial
    if (trial == 1){
      n_updates <- rep(0, max_sz)
    }
    else{
      if (data[trial-1, ]$phase == 'RECALL' & data[trial, ]$phase == 'UPDATING'){
        n_updates <- rep(0, max_sz)
      }
    }
    
    if (data[trial, ]$phase == 'UPDATING'){
      updating_pos <- data[trial, ]$pos
      n_updates[updating_pos] <- n_updates[updating_pos] + 1
      data[trial, ]$n_updates <- n_updates[updating_pos]
    }
    if (data[trial, ]$phase == 'RECALL'){
      data[trial, ]$n_updates <- n_updates[data[trial, ]$recall.pos]
    }
  }
  return(data)
}

raw_data$code <- factor(raw_data$code)
raw_data$task <- factor(raw_data$task)

raw_data <- read.csv('Data\\updating_numerical.csv', header = TRUE)
summary(raw_data)

raw_data <- determineNumberofUpdating(raw_data)

recall_data <- raw_data[raw_data$phase == 'RECALL', ]

agg_data <- data.frame(aggregate(list(recall_data$points, recall_data$rt),
                     list(recall_data$code, recall_data$task, recall_data$load, recall_data$n_updates),
                     mean))
names(agg_data) <- c('code', 'task', 'load', 'n_updates', 'PC', 'RT')