folder_path <-"Test dataset/csv"
domain_csv <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
for (file in domain_csv){
  domain_name <- tools::file_path_sans_ext(basename(file))
  df_temp <-read.csv(file)
  assign(domain_name, df_temp)
}
rm(df_temp, domain_csv,domain_name,file,folder_path)
