source("module 0 - read all csv in a folder to dfs.R")
source("module 1 - tx_dm_ds to root_index.R")
source("module 2 - leftjoin domains.R")

library(dplyr)
library(ggplot2)
library(plotly)

# Generate the table with all values in B grouped by variable A
pc_summary <- pc %>%
  group_by(GRPLBL, SEX, PCTEST, PCNOMLBL, PCTPTNUM) %>%
  summarize(
    Conc_Mean = mean(PCSTRESN),
    Conc_SD = sd(PCSTRESN)
    )
pcday <- unique(pc_summary$PCNOMLBL)
pcsex <- unique(pc_summary$SEX)
pc_plot_list <- list()
i = 1
for (day in pcday){
  pc_summary_temp <- subset(pc_summary, PCNOMLBL == day)
    for (sex in pcsex){
    pc_summary_temp_2 <- subset(pc_summary_temp, SEX == sex)
    plot<-plot_ly(data = pc_summary_temp_2, type = "scatter", x = ~PCTPTNUM, y = ~Conc_Mean, color = ~GRPLBL )%>%
    add_lines()%>%
    layout(yaxis = list(type = "log"),
           yaxis = list(range = c(0, log10(max(pc_summary_temp_2$Conc_Mean)))),
           title = paste0("Concentration vs Time in ", sex," on ",day),
           xaxis = list(title = "Hours post dose (hr)"),
           yaxis = list(title = "Mean Concentration (ug/mL)"))    
    pc_plot_list[[i]] <- plot
    i = i+1
    }
}
pc_plot_list