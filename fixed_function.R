run_fixed <- function(threshold, template, i) {
  df <- fread(paste0("<path>/inputs/initial_conditions/template_df_", template, ".csv"))
  if (threshold == 0.015) {
    tte_table <- fread(paste0("<path>/inputs/tte_tables/tte_table_1.5_", i, ".csv"))
  }
  if (threshold == 0.0075) {
    tte_table <- fread(paste0("<path>/inputs/tte_tables/tte_table_.75_", i, ".csv"))
  }
  if (threshold == 0.01125) {
    tte_table <- fread(paste0("<path>/inputs/tte_tables/tte_table_1.1_", i, ".csv"))
  }
  if (threshold == 0.01425) {
    tte_table <- fread(paste0("<path>/inputs/tte_tables/tte_table_1.4_", i, ".csv"))
  }
  edgelist <- fread(paste0("<path>/inputs/edgelists/edgelist_", template, ".csv"))
  edgelist <- edgelist[, edge_id:=paste0(p1, "_", p2)]
  
  for (t in 1:max_time) {
    temp_edges <- edgelist[(p1%in%df$id[df$hiv==1] & p2%in%df$id[df$hiv==0]) |
                             (p2%in%df$id[df$hiv==1] & p1%in%df$id[df$hiv==0])]
    tte_table <- tte_table[edge_id %in% temp_edges$edge_id & counter < time_threshold, counter:=counter+1]
    df <- df[hiv==0 &
               ((id %in% unique(tte_table$p1[tte_table$time_threshold==tte_table$counter])) | 
                  (id %in% unique(tte_table$p2[tte_table$time_threshold==tte_table$counter]))), trans_time:=t]
    df <- df[hiv==0 &
               ((id %in% unique(tte_table$p1[tte_table$time_threshold==tte_table$counter])) | 
                  (id %in% unique(tte_table$p2[tte_table$time_threshold==tte_table$counter]))), hiv:=1]
  }
  
  df <- df[, censor_time:=trans_time]
  df <- df[is.na(censor_time) & hiv==0, censor_time:=max_time]
  df <- df[is.na(censor_time) & hiv==1, censor_time:=0]
  df <- df[, random_iteration:=i]
  df <- df[, initial_template:=template]
  df <- df[, edge_version:=template]
  
  if (threshold == 0.0075) {
    write.csv(df, file = paste0("<path>/fixed_.75/init_", template, "_edge_", template, "_random_", i, ".csv"), na = "", row.names = F)
  }
  if (threshold == 0.015) {
    write.csv(df, file = paste0("<path>/fixed_1.5/init_", template, "_edge_", template, "_random_", i, ".csv"), na = "", row.names = F)
  }
  if (threshold == 0.01125) {
    write.csv(df, file = paste0("<path>/fixed_1.1/init_", template, "_edge_", template, "_random_", i, ".csv"), na = "", row.names = F)
  }
  if (threshold == 0.01425) {
    write.csv(df, file = paste0("<path>/fixed_1.4/init_", template, "_edge_", template, "_random_", i, ".csv"), na = "", row.names = F)
  } 
}
