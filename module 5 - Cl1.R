library(plyr)
library(plotly)
library(ggplot2)
library(htmlwidgets)

#get the max and min in CLDY, and generate an array from c
a_max = max(cl$CLDY)
a_min = min(cl$CLDY)
a_range = a_min:a_max
#there is no day 0 in most studies
a_range<-a_range[a_range != 0]

#remove all NORMAL observation
cl_finding <- subset(cl, CLSTRESC !="NORMAL")
#list 1 for grouping
list = c("CLCAT","CLSTRESC")
#list 2 for grouping
list2 = c("GRPLBL", "CLDY")
#unique combo of CLCAT and CLSTRESC
cl_temp <- plyr::count(cl_finding, vars =list)
#get ready for "for" loop
i = 1
n = 1
#create "folder" for outputs
cl_plot_list <- list()
cl_plot_df <- list()
#for each unique CLACT and CLSTRESC combo
for (i in 1:nrow(cl_temp)){
  #great cl_temp1 which contain one CLACT and CLSTRESC combo
  cl_temp1<-subset(cl_finding, CLSTRESC == cl_temp$CLSTRESC[i])
  cl_temp1<-subset(cl_temp1, CLCAT == cl_temp$CLCAT[i])
  #for each sex (only 2, male or female)
  for (sex in unique(cl_temp1$SEX)){
    #create a subset of temp1 contains only 1 sex
    cl_temp1 <- subset(cl_temp1, SEX == sex)
    #create a variable that list out all individuals with observation on a day
    cl_temp2 <- cl_temp1 %>% 
                dplyr::group_by(CLDY, GRPLBL) %>% 
                dplyr::summarise(trace_info = paste(SUBJID,"/",CLDY,"/",CLORRES, collapse = "\n"))
    
    cl_temp3 <- plyr::count(cl_temp1, vars = list2)
    cl_temp4 <- merge(cl_temp3, cl_temp2, by = c("CLDY", "GRPLBL"))
    #if cl_temp4 is empty then do nothing
    if (nrow(cl_temp4)==0){
      
    }else{
      #get group names from cl domain file
      cl_group_unique <- unique(cl$GRPLBL)
          #loop all group names
        for (uni_group in cl_group_unique){
          #if there is no such group in cl_temp4, add 0 findings to all study day
          if (!uni_group %in% unique(cl_temp4$GRPLBL)){
            cl_com <-data.frame(CLDY = a_range,
                                GRPLBL = rep(uni_group,length(a_range)),
                                freq = 0,
                                trace_info = "No abnormal finding")
            cl_temp4<-rbind(cl_temp4,cl_com)
          }else{
            #if the group name appeared in cl_temp4, complementing all other study days with 0 finding
            cl_com_1 <- subset(cl_temp4, GRPLBL == uni_group)
            cl_com_1 <- data.frame(CLDY = cl_com_1$CLDY,
                                   day_check = cl_com_1$CLDY %in% a_range)
            cl_com_2 <- data.frame(GRPLBL = rep(uni_group,length(a_range)),
                                   CLDY = a_range)
            cl_com_2 <- left_join(cl_com_2, cl_com_1, by ="CLDY")
            cl_com <- subset(cl_com_2, is.na(cl_com_2$day_check))
          cl_com$freq <- 0
          cl_com$trace_info <- "No abnormal finding"
          cl_com$day_check <- NULL
          new_order <- c("CLDY",
                         "GRPLBL",
                         "freq",
                         "trace_info")
          cl_com <- select(cl_com,all_of(new_order))
          cl_temp4 <- rbind(cl_temp4, cl_com)
          } 
        }
      #plot heatmap graph for each type of observation
      plot<- plot_ly(cl_temp4, 
                 y = ~CLDY, 
                 x = ~GRPLBL,
                 z = ~freq,
                 type = "heatmap", 
                 #colors = "Greys",
                 text = ~trace_info, 
                 hoverinfo = "text"
                 )%>%
                layout(title = paste0(cl_temp$CLCAT[i],"-",cl_temp$CLSTRESC[i],"-",sex,"-",n),
                   xaxis = list(title ="Group"),
                   yaxis = list(title = "Study Day"),
                   width = 300, height = 800
                   )
    cl_plot_list[[n]]<-plot
    #cl_plot_df[[n]]<-cl_temp4
    n = n + 1
  #grid <- subplot(cl_plot_list[1],cl_plot_list[2],cl_plot_list[3],cl_plot_list[4], nrows = 4, margin = 0.05)  
  saveWidget(widget =plot, file = paste0(cl_temp$CLCAT[i],"-",cl_temp$CLSTRESC[i],"-",sex,".html"))
  #assign(paste0("df_temp1_",n),cl_temp1)  
  #assign(paste0("df_temp2_",n),cl_temp2)
  #assign(paste0("df_temp3_",n),cl_temp3)
  #assign(paste0("df_temp4_",n),cl_temp4)
  
    rm(cl_temp2)
    rm(cl_temp3)
    rm(cl_temp4)
    rm(cl_com_1)
    rm(cl_com_2)
    rm(cl_com)
    
    }
  }
}
rm(cl_finding)
rm(cl_temp)
rm(cl_temp1)
rm(list)
rm(list2)
rm(n)
rm(sex)
rm(i)
rm(plot)
