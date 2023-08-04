cl_temp <- subset(cl, CLSTRESC !="NORMAL")

cl_summary <- cl_temp %>%
  group_by(GRPLBL, SEX, CLDY) %>%
  summarize(Findings = paste(paste0(SUBJID," - ",CLSTRESC,"; ",CLLOC,"; ",CLSEV),  collapse="\n"))


cl_sum_output <- knitr::kable(cl_summary)
