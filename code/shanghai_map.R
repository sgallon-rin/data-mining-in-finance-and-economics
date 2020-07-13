# 金融与经济大数据挖掘期末作业
# 沈嘉伦 16307110030
#
# 读取shp文件。获得上海地图（各区边界）
# 原shp文件过大(60M)，故不加入附件。
# 数据来源：
# 中国行政区边界shp下载（省，市，县）：
# https://blog.csdn.net/niu_dige/article/details/104856967

# 清除工作环境
cat("\014");rm(list=ls())
# 加载程序包
library(tidyverse)
library(rgdal)
#library(maps)
#library(mapdata)
#library(maptools)
#library(sf)

# 设定工作目录
setwd("/Users/sgallon/Documents/Docus/kjzl/4B/1-金融与经济大数据挖掘 Data Mining in Finance and Economics/pj/code")

# 地图数据，作出次数分布地图
#map1 <- readShapePoly('../data/map/县.shp') #use package maptools, not recommended
#use package sf
#map1 <- st_read('../data/map/县.shp')
#map1 <- map1[which(map1$省=='上海市'),]
#p1 <- ggplot(map1) +
#  geom_sf() +
#  geom_point(data=station.data, aes(x=lng, y=lat))
#p1

#use package rgdal
#ref: https://zhuanlan.zhihu.com/p/27189055
map2 <- readOGR('../data/map/县.shp')
map2 <- map2[which(map2@data$省=='上海市'),]
data1 <- map2@data
data2<- data.frame(id=row.names(data1), data1) 
sh_map <- fortify(map2) 
sh_map_data <- merge(sh_map, data2)
sh_map_data <- data.frame(sh_map_data$long, sh_map_data$lat, sh_map_data$NAME)
names(sh_map_data) <- c('long', 'lat', 'NAME')

write.table(sh_map_data,'../data/sh_map_data.csv',row.names=FALSE,col.names=TRUE,sep=',')

