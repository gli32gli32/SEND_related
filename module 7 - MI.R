library(dplyr)
library(plotly)
library(ggplot2)
library(htmlwidgets)
#clean up for NORMAL and no result records
mi_temp <- subset(mi, MISTRESC !="UNREMARKABLE")
mi_temp <- subset(mi_temp,  MISTAT!="NOT DONE")
#get the frequency of each tissue (findings). This is not discriminate the type of finding.
mi_temp_1 <- mi_temp%>%
             dplyr::group_by(GRPLBL, DSDECOD, SEX, MISPEC)%>%
             dplyr::summarise(freq = n(), trace_info = paste(SUBJID,"-",MIORRES, collapse = "\n"))
#create dataframe mi_temp_3 for later use

#get unique value of DSDECOD in in mi_temp_1, usually they are terminal sacrifice and recovery sacrifice.
mi_unique_phase <- unique(mi_temp_1$DSDECOD)
#set up n and mi_plot_list for later use.
n = 1
mi_plot_list <- list()
#generate subset containing one DSDECOD and SEX, but will contain all corresponding groups.
for (phase in mi_unique_phase) {
  df_1 <- subset(mi_temp_1, DSDECOD == phase)
  mi_unique_sex <- unique(df_1$SEX)
  for (sex in mi_unique_sex){
    mi_temp_3 <- data.frame()
    df_2 <- subset(df_1, SEX == sex)
    mi_unique_group <- unique(mi_temp$GRPLBL)
    #get 
    spec_list <- unique(df_2$MISPEC)
#separate groups having the same DSDECOD and SEX
      for (group in mi_unique_group){
        df_3 <- data.frame(MISPEC = spec_list,
                           GRPLBL = group,
                           SEX = sex,
                           DSDECOD = phase,
                           freq_o = 0)
        mi_temp_2 <- left_join(df_3, df_2, by = c("GRPLBL","SEX","DSDECOD","MISPEC"))
        mi_temp_2$freq[is.na(mi_temp_2$freq)] <- 0
        mi_temp_2$trace_info <- ifelse(is.na(mi_temp_2$trace_info), "There is no abnormal finding.", mi_temp_2$trace_info)
        if (nrow(mi_temp_3) == 0){
                mi_temp_3 <- mi_temp_2
              }else{
                mi_temp_3 <- rbind(mi_temp_3,mi_temp_2)
                }
        }  
#greate df_4 and arrange df_4 by freq_t (ascending order).  
    df_4 <- mi_temp_3 %>%
            dplyr::group_by(MISPEC) %>%
            dplyr::summarise(freq_t = sum(freq))
    df_4 <- arrange(df_4, df_4$freq_t)
    sort1 <- c(df_4$MISPEC)
    mi_temp_3$MISPEC <- factor(mi_temp_3$MISPEC, levels = sort1)
    plot<- plot_ly(mi_temp_3,
                   y = ~MISPEC, 
                   x = ~GRPLBL,
                   z = ~freq,
                   type = "heatmap", 
                   text = ~trace_info,
                   hoverinfo = "text",
                   width = 600, height = 600)%>%
           layout(title = list(text = paste0("Sum of histo-",phase,"-",sex), 
                  font = list(size = 14)),
                  xaxis = list(title = " ", tickfont = list(size =12)),
                  yaxis = list(title = " ", tickfont = list(size =10)))
                  #yaxis = list(categoryorder = "array", categoryarray = sort1))
      #saveWidget(widget =plot, file = paste0("Summary of Histopathology - ",phase," - ",sex,".html"))  
    
    mi_plot_list[[n]]<-plot
    n = n + 1
    rm(mi_temp_3)
    }
}
subplot(mi_plot_list,nrows =3) 
rm(mi_temp_2)
rm(mi_temp_1)
rm(mi_temp)
rm(df_1)
rm(df_2)
rm(df_3)
rm(df_4)
#rm(sort_1)
