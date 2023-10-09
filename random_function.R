
run_random <- function(threshold, template, i) {
  if (threshold == 0.015) {
    set.seed(12345+i)
  }
  if (threshold == 0.01125) {
    set.seed(22345+i)
  }
  if (threshold == 0.0075) {
    set.seed(32345+i)
  }
  if (threshold == 0.01425) {
    set.seed(42345+i)
  }
  
  df <- fread(paste0("<path>/inputs/initial_conditions/template_df_", template, ".csv"))
  edgelist <- fread(paste0("<path>/inputs/edgelists/edgelist_", template, ".csv"))
  
  for (t in 1:max_time) {
    temp_edges <- edgelist[(p1%in%unique(df$id[df$hiv==1]) & p2%in%unique(df$id[df$hiv==0])) |
                             (p2%in%unique(df$id[df$hiv==1]) & p1%in%unique(df$id[df$hiv==0]))]
    temp_edges <- temp_edges[, transmission:=rbinom(.N, 1, prob = threshold)]
    df <- df[hiv==0 &
               ((id %in% unique(temp_edges$p1[temp_edges$transmission == 1])) | 
                  (id %in% unique(temp_edges$p2[temp_edges$transmission == 1]))), trans_time:=t]
    df <- df[hiv==0 &
               ((id %in% unique(temp_edges$p1[temp_edges$transmission == 1])) | 
                  (id %in% unique(temp_edges$p2[temp_edges$transmission == 1]))), hiv:=1]
  }
  
  df <- df[, censor_time:=trans_time]
  df <- df[is.na(censor_time) & hiv==0, censor_time:=max_time]
  df <- df[is.na(censor_time) & hiv==1, censor_time:=0]
  df <- df[, random_iteration:=i]
  df <- df[, initial_template:=template]
  df <- df[, edge_version:=template]
  
  if (threshold == 0.0075) {
    write.csv(df, file = paste0("<PATH>/random_.75/init_", template, "_edge_", template, "_random_", i, ".csv"), na = "", row.names = F)
  }
  if (threshold == 0.015) {
    write.csv(df, file = paste0("<PATH>/random_1.5/init_", template, "_edge_", template, "_random_", i, ".csv"), na = "", row.names = F)
  }
  if (threshold == 0.01125) {
    write.csv(df, file = paste0("<PATH>/random_1.1/init_", template, "_edge_", template, "_random_", i, ".csv"), na = "", row.names = F)
  }
  if (threshold == 0.01425) {
    write.csv(df, file = paste0("<PATH>/random_1.4/init_", template, "_edge_", template, "_random_", i, ".csv"), na = "", row.names = F)
  }
}
