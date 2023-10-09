rm(list = ls())

library(data.table)
library(EpiModel)
library(foreach)

parallel::detectCores()
n.cores <- 10

my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)

doParallel::registerDoParallel(cl = my.cluster)

out <- foreach (i = 1:20, .combine = 'rbind', .packages = c("data.table", "EpiModel")) %dopar% {
  
  set.seed(12345+i)
  
  ## Network size
  npop <- 1000
  
  ## Targets
  eq_deg <- 1.43
  eq_isolates <- 0.37
  eq_duration <- 36 # 3 years, monthly timestep
  gwesp_pct <- 0.28
  
  eq_deg_target <- eq_deg*npop/2
  eq_isolates_target <- eq_isolates*npop
  eq_gwesp_target <- eq_deg_target*gwesp_pct
  
  ## Create the empty network
  nw <- network.initialize(n = npop, directed = FALSE)
  
  ## Fit the model to targets with EDAPPROX
  est_net <- netest(nw,
                    formation = as.formula("~ edges + degree(0) + gwesp(decay = 0.0, fixed = TRUE)"),
                    target.stats = c(eq_deg_target, eq_isolates_target, eq_gwesp_target),
                    coef.diss = dissolution_coefs(dissolution = ~offset(edges), duration = eq_duration),
                    edapprox = TRUE)
  
  nw <- est_net$newnetwork
  edgelist <- as.data.table(get.dyads.active(nw, at = 0))
  setnames(edgelist, c("V1", "V2"), c("p1", "p2"))
  
  write.csv(edgelist, paste0("<path>/inputs/edgelists/edgelist_", i, ".csv"), na = "", row.names = F)
  rm(edgelist)
}

parallel::stopCluster(cl = my.cluster)

for (i in 1:20) {
  set.seed(12345+i)
  
  template_df <- data.table(id = 1:1000, hiv = rbinom(1000, 1, p = 0.07))
  template_df <- template_df[, ssp_adhere:=sample(c(0,1), nrow(template_df), replace = T, prob = c(0.75, .25))]
  
  write.csv(template_df, paste0("<path>/inputs/initial_conditions/template_df_", i, ".csv"), na = "", row.names = F)
  
}

doParallel::registerDoParallel(cl = my.cluster)

out <- foreach (i = 51:300, .combine = 'rbind', .packages = c("data.table")) %dopar% {
  
  set.seed(12345+i)
  
  ## Network size
  npop <- 1000
  max_time <- 600
  
  prob_mat <- matrix(runif((npop*(npop-1)*max_time)/2, 0, 1), nrow = ((npop*(npop-1))/2)) 
  
  tte1 <- apply(prob_mat, 1, function(x) which(x < 0.015)[1])
  tte2 <- apply(prob_mat, 1, function(x) which(x < 0.0075)[1])
  tte3 <- apply(prob_mat, 1, function(x) which(x < (0.015*.75))[1])
  tte4 <- apply(prob_mat, 1, function(x) which(x < (0.015*.95))[1])
  
  tte_table1 <- as.data.table(expand.grid(p1 = 1:1000, p2 = 1:1000))
  tte_table1 <- tte_table1[p1!=p2]
  tte_table1 <- tte_table1[p1 < p2]
  tte_table1 <- tte_table1[, time_threshold:=tte1]
  tte_table1 <- tte_table1[, counter:=0]
  tte_table1 <- tte_table1[, edge_id:=paste0(p1, "_", p2)]
  
  write.csv(tte_table1, paste0("<path>/inputs/tte_tables/tte_table_1.5_", i, ".csv"), na = "", row.names = F)
  
  tte_table2 <- as.data.table(expand.grid(p1 = 1:1000, p2 = 1:1000))
  tte_table2 <- tte_table2[p1!=p2]
  tte_table2 <- tte_table2[p1 < p2]
  tte_table2 <- tte_table2[, time_threshold:=tte2]
  tte_table2 <- tte_table2[, counter:=0]
  tte_table2 <- tte_table2[, edge_id:=paste0(p1, "_", p2)]
  
  write.csv(tte_table2, paste0("<path>/inputs/tte_tables/tte_table_.75_", i, ".csv"), na = "", row.names = F)
  
  tte_table3 <- as.data.table(expand.grid(p1 = 1:1000, p2 = 1:1000))
  tte_table3 <- tte_table3[p1!=p2]
  tte_table3 <- tte_table3[p1 < p2]
  tte_table3 <- tte_table3[, time_threshold:=tte3]
  tte_table3 <- tte_table3[, counter:=0]
  tte_table3 <- tte_table3[, edge_id:=paste0(p1, "_", p2)]
  
  write.csv(tte_table3, paste0("<path>/inputs/tte_tables/tte_table_1.1_", i, ".csv"), na = "", row.names = F)

  tte_table4 <- as.data.table(expand.grid(p1 = 1:1000, p2 = 1:1000))
  tte_table4 <- tte_table4[p1!=p2]
  tte_table4 <- tte_table4[p1 < p2]
  tte_table4 <- tte_table4[, time_threshold:=tte4]
  tte_table4 <- tte_table4[, counter:=0]
  tte_table4 <- tte_table4[, edge_id:=paste0(p1, "_", p2)]
  
  write.csv(tte_table4, paste0("<path>/inputs/tte_tables/tte_table_1.4_", i, ".csv"), na = "", row.names = F)
    
  rm(tte_table1, tte_table2, tte_table3, tte_table4, tte1, tte2, tte3, tte4)
}

parallel::stopCluster(cl = my.cluster)
