library(dplyr)
plot.new()
#sort by TAETORD first and then by ARMCD (get the ta domain in order)
ta <- ta[order(ta$TAETORD),]
ta <- ta[order(ta$ARMCD),]

#generate a ta_temp which avoid change in ta domain file
df_1 <-data.frame(ARMCD = root_index$ARMCD,
                  SETCD = root_index$SETCD)
df_1 <- unique(df_1, by =c("ARMCD", "SETCD"))
df_1 <- reframe(df_1, SETCD = paste(SETCD,sep = ",", collapse = NULL), .by = "ARMCD")
ta_temp <- merge(ta, df_1, by = "ARMCD")
rm(df_1)

#get RGB for each elements, RGB is preset in a csv file
RGB <- read.csv("R/RGBpool.csv")
df <- data.frame ("index" = c(1:nrow(te)))
df <- cbind(te, df)
df <-merge(df, RGB, by.x = "index", by.y = "Index")
df <-left_join(ta, df, by = "ETCD")

#set required variables for plotting
i = 1
j = 1
graph_column_fixpoint = 1
graph_row_fixpoint = 1 
space_in_between = 0.015
canvas_width = 0.8 #set to 0.8 by default
canvas_height = 0.8 #set to 0.8 by default
width = canvas_width/a
front_size = width*3
R = df$R[i]
G = df$G[i]
B = df$B[i]

#calculate width and height based on number of elements in each arm and number of arms
df1 <- ta %>%
  group_by(ARMCD)%>%
  count()
a <- max(df1$n)
nub_rows = nrow(df1)
height = 0.08 #or use canvas_height/nub_rows,but it won't look good with fewer groups.

#plot the first block, since the code is different from the rest
rect(xleft = (i - graph_column_fixpoint)*width+(1-canvas_width)/2, 
     xright = (i)*width+(1-canvas_width)/2, 
     ybottom = 1-j*height+space_in_between-(1-canvas_height)/2,  
     ytop = 1-(j - graph_row_fixpoint)*height-(1-canvas_height)/2, 
     col = rgb(R, G, B, maxColorValue = 255))

#showing ARMCD for the first row
text((1-canvas_width)/8, 
     1-j*height-(1-canvas_height)/4, #+space_in_between, 
     paste0(ta_temp$ARMCD[i]), 
     adj = 0.5, cex = 1)

#showing SETCD for the first row
text(1-(1-canvas_width)/8, 
     (1-j*height-(1-canvas_height)/4), #+space_in_between 
     paste0(ta_temp$SETCD[i]), 
     adj = 0.5, cex = front_size)

#showing the first element name when i =1
text((1- canvas_width)/2+width/2, 
     (1-j*height-(1-canvas_height)/4), #+space_in_between
     paste0(ta_temp$ETCD[i]), 
     adj = 0.5, cex = front_size)

#loop from the 2nd row in ta domain file
for (i in 2:nrow(ta)){
  R = df$R[i]
  G = df$G[i]
  B = df$B[i]
  if (ta_temp$ARMCD[i] == ta_temp$ARMCD[i-1]){
    rect(xleft = (i- graph_column_fixpoint)*width+(1-canvas_width)/2, 
         xright = (i+1-graph_column_fixpoint)*width+(1-canvas_width)/2, 
         ybottom = 1-j*height+space_in_between-(1-canvas_height)/2,  
         ytop = 1-(j - graph_row_fixpoint)*height-(1-canvas_height)/2, 
         col = rgb(R, G, B, maxColorValue = 255))
    
    text((1- canvas_width)/2+width/2+(i-graph_column_fixpoint)*width, 
         (1-j*height-(1-canvas_height)/4), #+space_in_between 
         paste0(ta_temp$ETCD[i]), 
         adj = 0.5, cex = front_size)
    
    }else{
    graph_column_fixpoint = i
    j = j+1
    rect(xleft = (i - graph_column_fixpoint)*width+(1-canvas_width)/2, 
         xright = (i+1-graph_column_fixpoint)*width+(1-canvas_width)/2, 
         ybottom = 1-j*height+space_in_between-(1-canvas_height)/2,  
         ytop = 1-(j - graph_row_fixpoint)*height-(1-canvas_height)/2, 
         col = rgb(R, G, B, maxColorValue = 255))
    
    text((1- canvas_width)/2+width/2+(i-graph_column_fixpoint)*width, 
         (1-j*height-(1-canvas_height)/4), #+space_in_between 
         paste0(ta_temp$ETCD[i]), 
         adj = 0.5, cex = front_size)
    
    #showing ARMCD other than the first row
    text((1-canvas_width)/8, 
         1-j*height-(1-canvas_height)/4, #+space_in_between
         paste0(ta_temp$ARMCD[i]),
         adj = 0.5, cex = front_size)
    
    #showing SETCD other than the first row
    text(1-(1-canvas_width)/8, 
         1-j*height-(1-canvas_height)/4, #+space_in_between
         paste0(ta_temp$SETCD[i]), 
         adj = 0.5, cex = front_size)}
}
#Title and boarder of the graph
text(0.5, 
     canvas_height+(1-canvas_height)*0.8, 
     "Study Design", 
     adj = 0.5, cex = 1.5)
text(0.07, 
     canvas_height+(1-canvas_height)*0.8, 
     "ARMCD", adj = 1, cex = 0.8)
text(0.93, 
     canvas_height+(1-canvas_height)*0.8, 
     "SETCD", adj = 0, cex = 0.8)
#may be add a legend?
box(which = "plot", lty = "solid")

#remove all intermediate dfs
rm(df)
rm(df1)
rm(ta_temp)

print(plot)