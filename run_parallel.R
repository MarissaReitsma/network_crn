rm(list = ls())

library(data.table)
library(foreach)
library(doParallel)

setwd("<WD>")
source("random_function.R")
source("fixed_function.R")

max_time <- 600
method <- "random"

parallel::detectCores()
n.cores <- 16

my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = my.cluster)

out <- foreach (i = 1:500, .combine = 'rbind', .packages = c("data.table")) %dopar% {
  for (threshold in c(0.015, 0.01125, 0.0075, 0.01425)) {
    for (template in 1:10) {
      if (method == "random") {
        run_random(threshold = threshold, template = template, i = i)
      }
      if (method == "fixed") {
        run_fixed(threshold = threshold, template = template, i = i)
      }
    }
  }
}

parallel::stopCluster(cl = my.cluster)
