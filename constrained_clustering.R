library(tidyverse)

#Creating a sample dataset with demands for each point

name <- LETTERS[1:10]
lat  <- c(22.57, 22.69, 22.72, 22.50, 22.66, 22.19, 22.60, 22.27, 22.31, 22.15)
lon  <- c(88.69, 88.84, 88.77, 88.85, 88.63, 88.91, 88.54, 88.62, 88.78, 88.66)
demand <- c(30, 70, 75, 100, 45, 60, 135, 65, 55, 50)

df <- data.frame(name, lat, lon, demand)

#Visualising the data

ggplot(data=df_locations)+
  geom_point(aes(x=lon, y=lat))


#Creating the cluster function

constrained_cluster <- function(df,truck_load=170, lat_median=median(df$lat), lon_median=median(df$lon)){
  df$angle = atan2(df$lat - median(df$lat),
                   df$lon - median(df$lon))
  df<- df[order(df$angle, decreasing = TRUE),]
  d<-0
  cluster_number<-1
  cluster_list<- c()
  i<-1
  while (i <= length(df$angle)){
    d <- d+ df$demand[i]
    if(d<=truck_load){
      cluster_list[i] <- cluster_number
      i<- i+1
    }
    else{
      cluster_number <- cluster_number+1
      d <- 0
      i<-i
    }
  }
  return(cbind(df,as.data.frame(cluster_list)))
}

df_with_cluster<- constrained_cluster(df, truck_load = 2000)

#Visualizing the cluster

ggplot(data=df_with_cluster) +
  geom_point(aes(x=lon, y=lat, colour=factor(cluster_list)))+
  theme(legend.position = 'none')
