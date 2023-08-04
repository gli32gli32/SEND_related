library(plyr)
library(plotly)
library(ggplot2)
library(htmlwidgets)
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
                dplyr::summarise(Individual = paste(SUBJID,"/",CLDY,"/",CLORRES, collapse = "\n"))
    
    cl_temp3 <- plyr::count(cl_temp1, vars = list2)
    cl_temp4 <- merge(cl_temp3, cl_temp2, by = c("CLDY", "GRPLBL"))
    if (nrow(cl_temp4)==0){
      
    }else{
      plot<- plot_ly(cl_temp4, 
                 x = ~CLDY, 
                 y = ~GRPLBL,
                 z = ~freq,
                 type = "heatmap", 
                 text = ~Individual, 
                 hoverinfo = "text"
                 )%>%
                layout(title = paste0(cl_temp$CLCAT[i],"-",cl_temp$CLSTRESC[i],"-",sex,"-",n),
                   yaxis = list(title ="Group"),
                   xaxis = list(title = "Study Day")
                   )
    cl_plot_list[[n]]<-plot
    cl_plot_df[[n]]<-cl_temp4
    n = n + 1
  #saveWidget(widget =plot, file = paste0(cl_temp$CLCAT[i],"-",cl_temp$CLSTRESC[i],"-",sex,".html"))
  #assign(paste0("df_temp1_",n),cl_temp1)  
  #assign(paste0("df_temp2_",n),cl_temp2)
  #assign(paste0("df_temp3_",n),cl_temp3)
  #assign(paste0("df_temp4_",n),cl_temp4)
  
    rm(cl_temp2)
    rm(cl_temp3)
    rm(cl_temp4)
    
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
