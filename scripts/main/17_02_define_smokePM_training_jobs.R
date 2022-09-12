# csv with jobs to submit 
expand.grid(cv_fold_num = c(0:4,99),
            drop_vars = c("", "traj_points", "aod_anom_pred")) %>%
  write.table(file.path(path_data, "smokePM_training_jobs.csv"), 
                            row.names = FALSE, col.names = FALSE, sep = ",")

