#module 2 to calculate mean bw by group/sex/collection day
library(dplyr)
library(ggplot2)
library(plotly)
source("module 0 - read all csv in a folder to dfs.R")
source("module 1 - tx_dm_ds to root_index.R")
# left join bw with root_index
bw_temp <- left_join(bw, root_index, by ="USUBJID")
bw_temp <- subset(bw_temp,BWFAST !="Y")
# Generate the table with all values in B grouped by variable A
bw_summary <- bw_temp %>%
  group_by(GRPLBL, SEX, BWDY) %>%
  summarize(
    BW_Mean = mean(BWSTRESN),
    BW_SD = sd(BWSTRESN)
#    Mean_BW = paste(paste0(unique(BWSTRESN), " kg"), collapse = ", ")
    )

# Print the table



#ggplot(table_df_tem, aes(x =BWDY, y = BW_Mean, color = GRPLBL, shape = SEX))+
#  geom_point(size = 3)+
#  geom_line()+
#  geom_errorbar(aes(ymin = BW_Mean - BW_SD, ymax = BW_Mean +BW_SD), width = 6,position = position_dodge(0.9))+
#  labs(title = "Group mean bodyweight vs. Day for males and females",
#       x = "Study day",
#       y = "Body Weight (kg)",
#       color = "Group",
#       shape = "Sex"
#       )+
#  theme_minimal()+
#  ylim(0, max(table_df_tem$BW_Mean + table_df_tem$BW_SD))


plot_ly(data = bw_summary, type = "scatter", x = ~BWDY, y = ~BW_Mean, color = ~GRPLBL) %>%
  add_lines() %>%
#  add_trace(y = ~BW_Mean + BW_SD, name = "Error", type = "scatter", mode = "lines", line = list(width = 0), fill = "tozeroy") %>%
#  add_trace(y = ~BW_Mean - BW_SD, type = "scatter", mode = "lines", line = list(width = 0), fill = "tonexty") %>%
  layout(title = "Bodyweight vs Time for males and females",
         xaxis = list(title = "Study Day"),
         yaxis = list(title = "Mean Bodyweights (kg)")
         )
