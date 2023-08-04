library(dplyr)
plot.new()
#sort by TAETORD first and then by ARMCD (get the ta domain in order)
ta <- ta[order(ta$TAETORD),]
ta <- ta[order(ta$ARMCD),]

#calculate width and height based on number of elements in each arm and number of arms
df1 <- ta %>%
  group_by(ARMCD)%>%
  count()
a <- max(df1$n)
width = 1/a
nub_rows = nrow(df1)
height = 1/nub_rows

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
R = df$R[i]
G = df$G[i]
B = df$B[i]
#plot the first block, since the code is different from the rest
rect(xleft = (i - graph_column_fixpoint)*width, 
     xright = i*width, 
     ybottom = 1-j*height,  
     ytop = 1-(j - graph_row_fixpoint)*height, 
     col = rgb(R, G, B, maxColorValue = 255))

#loop from the 2nd row in ta domain file
for (i in 2:nrow(ta)){
  R = df$R[i]
  G = df$G[i]
  B = df$B[i]
  if (ta$ARMCD[i] == ta$ARMCD[i-1]){
    rect(xleft = (i - graph_column_fixpoint)*width, 
         xright = (i+1-graph_column_fixpoint)*width, 
         ybottom = 1-j*height,  
         ytop = 1-(j - graph_row_fixpoint)*height, 
         col = rgb(R, G, B, maxColorValue = 255))
  }else{
    graph_column_fixpoint = i
    j = j+1
    rect(xleft = (i - graph_column_fixpoint)*width, 
         xright = (i+1-graph_column_fixpoint)*width, 
         ybottom = 1-j*height,  
         ytop = 1-(j - graph_row_fixpoint)*height, 
         col = rgb(R, G, B, maxColorValue = 255))
    }
}

rm(df)
rm(df1)