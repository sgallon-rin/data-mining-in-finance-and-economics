# 金融与经济大数据挖掘期末作业
# 沈嘉伦 16307110030
#
# 处理数据及绘图

# 清除工作环境
cat("\014");rm(list=ls())
# 加载程序包
library(tidyverse)
library(RColorBrewer)
#library(plyr)

# 设定工作目录
setwd("/Users/sgallon/Documents/Docus/kjzl/4B/1-金融与经济大数据挖掘 Data Mining in Finance and Economics/pj/code")
par(family='STHeiti') #指定字体解决中文乱码问题(对于自带画图);ggplot要加在theme中

# 读入数据并检查是否有异常值存在，发现没有NaN
mobile.data <- read.table("../data/mobile.csv", header=TRUE, sep='\t')
station.data <- read.table("../data/station_data_district.csv", header=TRUE, sep=',')
mobile.data$station_id <- as.character(mobile.data$station_id) #将因子型转换为字符型
station.data$station_id <- as.character(station.data$station_id)
#过滤出在基站列表中的基站数据
mobile.data <- filter(mobile.data, mobile.data$station_id %in% station.data$station_id) 
summary(mobile.data)
summary(station.data)

# 用户--总次数
mobile.user.count <- as.data.frame(table(mobile.data$user_id))
names(mobile.user.count) <- c('user_id', 'count')
head(mobile.user.count)
summary(mobile.user.count)

# 基站--总次数
mobile.station.count <- as.data.frame(table(mobile.data$station_id))
names(mobile.station.count) <- c('station_id', 'count')
mobile.station.count$station_id <- as.character(mobile.station.count$station_id)
#过滤出在基站列表中的基站数据
mobile.station.count <- filter(mobile.station.count, 
                               mobile.station.count$station_id %in% station.data$station_id) 
head(mobile.station.count)
summary(mobile.station.count)
# 对表进行连接，基站--经度--纬度--总次数，会自动去除没有位置信息的基站
station.data.count <- left_join(station.data, mobile.station.count)
station.data.count$district <- as.character(station.data.count$district)
station.data.count_no0 <- inner_join(station.data, mobile.station.count)
station.data.count_no0$district <- as.character(station.data.count_no0$district)
station.data.count$count[is.na(station.data.count$count)] <- 0
#write.table(station.data.count_no0,'../data/station_count_no0.csv',row.names=FALSE,col.names=TRUE,sep=',')
#write.table(station.data.count,'../data/station_count.csv',row.names=FALSE,col.names=TRUE,sep=',')
# 分区进行统计
station.district <- data.frame(station.data.count$district, station.data.count$count)
names(station.district) <- c('district','count')
station.district <- station.district %>%
  group_by(district) %>%
  summarize(count=sum(count),no=n())
station.district$count_per_st <- station.district$count / station.district$no
#write.table(station.district,'../result/station_district.csv',row.names=FALSE,col.names=TRUE,sep=',')

count.ab <- function(df,a,b)(
  # 计算数据框df在区间(a,b]内的count总数
  len <- sum(df$count>a & df$count<=b)
)

# 基站count分区间统计
#x1 <- count.ab(station.data.count.count,0,200)
x0 <- count.ab(station.data.count,-1,0)
x1 <- count.ab(station.data.count,0,30) #每天<1
x2 <- count.ab(station.data.count,30,60) #1-2
x3 <- count.ab(station.data.count,60,100) 
x4 <- count.ab(station.data.count,100,200)
x5 <- count.ab(station.data.count,200,400)
x6 <- count.ab(station.data.count,400,600)
x7 <- count.ab(station.data.count,600,800)
x8 <- count.ab(station.data.count,800,1000)
x9 <- count.ab(station.data.count,1000,1200)
x10 <- count.ab(station.data.count,1200,1400)
x11 <- count.ab(station.data.count,1400,1600)
x12 <- count.ab(station.data.count,1600,1800)
x13 <- count.ab(station.data.count,1800,2000)
x14 <- count.ab(station.data.count,2000,2500)
x15 <- count.ab(station.data.count,2500,3000)
x16 <- count.ab(station.data.count,3000,3500)
x17 <- count.ab(station.data.count,3500,4000)
x <- c(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17)
x18 <- length(station.data.count[,1])-sum(x)
x <- c(x,x18)
xlevel <- c('0','1-30','30-60','60-100','100-200','200-400','400-600','600-800','800-1000',
            '1000-1200','1200-1400','1400-1600','1600-1800','1800-2000',
            '2000-2500','2500-3000','3000-3500','3500-4000','4000以上')
xlevel <- factor(xlevel,levels=xlevel)
station.count.interval <- data.frame(count=xlevel,num=x)

# 用户count分区间统计
x1 <- count.ab(mobile.user.count,0,20)
x2 <- count.ab(mobile.user.count,20,40)
x3 <- count.ab(mobile.user.count,40,60)
x4 <- count.ab(mobile.user.count,60,80)
x5 <- count.ab(mobile.user.count,80,100)
x6 <- count.ab(mobile.user.count,100,120)
x7 <- count.ab(mobile.user.count,120,140)
x8 <- count.ab(mobile.user.count,140,160)
x9 <- count.ab(mobile.user.count,160,180)
x10 <- count.ab(mobile.user.count,180,200)
x11 <- count.ab(mobile.user.count,200,220)
x12 <- count.ab(mobile.user.count,220,240)
x13 <- count.ab(mobile.user.count,240,260)
x14 <- count.ab(mobile.user.count,260,280)
x15 <- count.ab(mobile.user.count,280,300)
x <- c(x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15)
x16 <- length(mobile.user.count[,1])-sum(x)
x <- c(x,x16)
xlevel <- c('20以下','20-40','40-60','60-80','80-100',
            '100-120','120-140','140-160','160-180','180-200',
            '200-220','220-240','240-260','260-280','280-300',
            '300以上')
xlevel <- factor(xlevel,levels=xlevel)
user.count.interval <- data.frame(count=xlevel,num=x)

# 画图主题
blanktheme <- theme(
  panel.grid = element_blank(),
  panel.background = element_blank(),
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  axis.title = element_blank()
)

xihei <- theme(text=element_text(family="STXihei")) #字体主题，为了中文能够正常显示

sizetheme <- theme(
  plot.title=element_text(hjust=0.5, size=20, face="bold"),
  axis.text=element_text(size=12, face="bold"),
  axis.title.x=element_text(size=14),
  axis.title.y=element_text(size=14))

#各区边界点数据（获取过程详见../code/shanghai_map.R）
sh_map_data <- read.csv('../data/sh_map_data.csv', header=TRUE)

#各区中心点坐标（手动测量）
sh_district <- read.csv('../data/shanghai.csv', header=TRUE)
sh_d1 <- filter(sh_district, sh_district$is_city==1)
sh_d0 <- filter(sh_district, sh_district$is_city==0)

#上海市地图
shanghai.map <- ggplot(sh_map_data, aes(x=long,y=lat)) +
  geom_polygon(aes(group=NAME),fill="white",colour="grey60") +
  coord_map("polyconic")

#上海市区地图
shanghai.map.city <- ggplot(sh_map_data, aes(x=long,y=lat)) +
  geom_polygon(aes(group=NAME),fill="#FFFAF0",colour="grey60") +
  coord_map("polyconic",xlim=c(121.3,121.6),ylim=c(31.1,31.35))

# 全市，不含count=0
m1 <- shanghai.map +
  geom_text(data=sh_d0, aes(x=lng,y=lat,label=NAME),family='STXihei',colour="grey40",size=5) +
  geom_point(data=station.data.count_no0, aes(x=lng,y=lat,colour=count,alpha=count),size=1.5) +
  #scale_fill_gradient(low="steelblue",high="white") +
  #scale_colour_gradient(low="blue",high="red") +
  scale_colour_gradient2(low="blue",high="red",mid="green",midpoint=5000) +
  labs(x='经度',y='纬度',title='上海市手机基站2016年11月接收用户信号次数分布',color='次数',alpha='次数') +
  xihei + sizetheme
m1

# 市中心区，不含count=0
m2 <- shanghai.map.city +
  geom_label(data=sh_d1, aes(x=lng,y=lat,label=NAME),family='STXihei',colour="grey20",size=5) +
  geom_point(data=station.data.count_no0, aes(x=lng,y=lat,colour=count,alpha=count),size=3) +
  #scale_fill_gradient(low="steelblue",high="white") +
  #scale_colour_gradient(low="blue",high="red") +
  scale_colour_gradient2(low="blue",high="red",mid="green",midpoint=5000) +
  labs(x='经度',y='纬度',title='上海市中心城区2016年11月手机基站接收用户信号次数分布',color='次数',alpha='次数') +
  xihei + sizetheme
m2

#各区按每个基站平均次数上色
names(sh_map_data) <- c('long','lat','district')
sh_map_data$district <- as.character(sh_map_data$district)
station.district$district <- as.character(station.district$district)
sh_map.district <- inner_join(sh_map_data, station.district)
m3 <- ggplot(sh_map.district, aes(x=long, y=lat)) +
  geom_polygon(aes(group=district,fill=count_per_st),colour="grey60") +
  coord_map("polyconic") +
  scale_fill_gradient(low="#FFE4E1",high="#FF4500") +
  geom_text(data=sh_d0, aes(x=lng,y=lat,label=NAME),family='STXihei',colour="grey40",size=5) +
  labs(x='经度',y='纬度',title='上海市各区2016年11月手机基站平均接收用户信号次数',fill='平均次数') +
  xihei + sizetheme
m3

m3.1 <- ggplot(sh_map.district, aes(x=long, y=lat)) +
  geom_polygon(aes(group=district,fill=count_per_st),colour="grey60") +
  coord_map("polyconic",xlim=c(121.3,121.6),ylim=c(31.1,31.35)) +
  scale_fill_gradient(low="#FFE4E1",high="#FF4500") +
  geom_label(data=sh_d1, aes(x=lng,y=lat,label=NAME),family='STXihei',colour="grey40",size=5) +
  labs(x='经度',y='纬度',title='上海市中心各区2016年11月手机基站平均接收用户信号次数',fill='平均次数') +
  xihei + sizetheme
m3.1

m3.2 <- ggplot(sh_map.district, aes(x=long, y=lat)) +
  geom_polygon(aes(group=district,fill=count),colour="grey60") +
  coord_map("polyconic") +
  scale_fill_gradient(low="#FFE4E1",high="#FF4500") +
  geom_text(data=sh_d0, aes(x=lng,y=lat,label=NAME),family='STXihei',colour="grey40",size=5) +
  labs(x='经度',y='纬度',title='上海市各区2016年11月手机基站接收用户信号总次数',fill='总次数') +
  xihei + sizetheme
m3.2

m3.3 <- ggplot(sh_map.district, aes(x=long, y=lat)) +
  geom_polygon(aes(group=district,fill=count),colour="grey60") +
  coord_map("polyconic",xlim=c(121.3,121.6),ylim=c(31.1,31.35)) +
  scale_fill_gradient(low="#FFE4E1",high="#FF4500") +
  geom_label(data=sh_d1, aes(x=lng,y=lat,label=NAME),family='STXihei',colour="grey40",size=5) +
  labs(x='经度',y='纬度',title='上海市中心各区2016年11月手机基站接收用户信号总次数',fill='平均次数') +
  xihei + sizetheme
m3.3

sh_district_info <- read.csv("../data/sh_district_info.csv", header=TRUE, sep=',')
sh_district_info$district <- as.character(sh_district_info$district)
sh_district_info <- inner_join(sh_district_info, station.district)
sh_district_info$gdp_per_pop <- 10000 * sh_district_info$gdp_yiyuan / sh_district_info$pop_changzhu_wanren 

plot(x=sh_district_info$count_per_st,y=sh_district_info$gdp_per_pop,
     xlab='2016年11月基站平均观测数（次）',ylab='2016年人均生产总值（元）')
plot(x=sh_district_info$count,y=sh_district_info$gdp_yiyuan,
     xlab='2016年11月基站总观测数（次）',ylab='2016年生产总值（亿元）')
# plot(x=sh_district_info$count_per_st,y=sh_district_info$gdp_per_pop)
# plot(x=sh_district_info$count_per_st,y=sh_district_info$gdp_yiyuan)
# plot(x=sh_district_info$count_per_st,y=sh_district_info$pop_changzhu_wanren)
# plot(x=sh_district_info$count,y=sh_district_info$pop_changzhu_wanren)
# plot(x=sh_district_info$count,y=sh_district_info$gdp_per_pop)
# plot(x=sh_district_info$count,y=sh_district_info$gdp_yiyuan)
# plot(x=sh_district_info$count_per_st,y=sh_district_info$gdp_yiyuan)

myfit <- lm(gdp_yiyuan~count, sh_district_info)
summary(myfit)
abline(myfit,col='red')
sh_district_info <- sh_district_info[order(sh_district_info$count),]
myfit2 <- lm(gdp_yiyuan~count+I(count^2), sh_district_info)
summary(myfit2)
lines(sh_district_info$count, fitted(myfit2), col='blue')

# 基站count统计图
#barplot(table(mobile.data$station_id))
p1 <- ggplot(station.count.interval,aes(x=count,y=num)) +
  geom_bar(stat='identity',fill='darkred') +
  geom_text(aes(label=num),vjust=-.1,hjust=0.5,size=5) +
  labs(x='次数',y='基站数',title='2016年11月基站接收信号总次数分布') +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  xihei + sizetheme
p1

stcnt <- station.data.count$count
hist(stcnt, breaks=50, col='red', 
     main='2016年11月上海市基站接收信号总次数分布', 
     xlab='接收信号（次）', ylab='基站数（个）')

# 用户count统计图
p2 <- ggplot(user.count.interval,aes(x=count,y=num)) +
  geom_bar(stat='identity',fill='steelblue') +
  geom_text(aes(label=num),vjust=-.1,hjust=0.5,size=5) +
  labs(x='次数',y='用户数',title='2016年11月用户发送信号总次数分布') +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  xihei + sizetheme
p2

uscnt <- mobile.user.count$count
hist(uscnt, breaks=50, col='blue', 
     main='2016年11月上海市用户发送信号总次数分布', 
     xlab='发送信号（次）', ylab='用户数（个）')

#信号较多的基站(>5000)
station.id <- mobile.station.count$station_id[which(mobile.station.count$count>5000)]
station1.data <- filter(station.data.count, station.data.count$station_id %in% station.id)
#write.table(station1.data,'../data/station1_count.csv',row.names=FALSE,col.names=TRUE,sep=',')
m4 <- shanghai.map +
  geom_text(data=sh_d0, aes(x=lng,y=lat,label=NAME),family='STXihei',colour="grey40",size=5) +
  geom_point(data=station1.data, aes(x=lng,y=lat),colour='red',size=1.5) +
  labs(x='经度',y='纬度',title='上海市手机基站2016年11月接收用户信号次数>5000次的基站位置') +
  xihei + sizetheme
m4

# 用户数据量较大，取出次数>200的部分用户进行分析(2399名用户)
user.id <- mobile.user.count$user_id[which(mobile.user.count$count>200)]
user.id <- as.character(user.id)
user.data <- filter(mobile.data, mobile.data$user_id %in% user.id) %>%
  inner_join(.,station.data)
#write.table(user.data,'../data/mobile_200.csv',row.names=FALSE,col.names=TRUE,sep=',')
#rm(mobile.data)
summary(user.data)
head(user.data)

#数据量最大的10名用户
max10.id <- order(mobile.user.count$count,decreasing=TRUE)[1:10]
max10.id <- as.character(mobile.user.count[max10.id,]$user_id)
max10.user.data <- user.data[user.data$user_id %in% max10.id,] %>%
  inner_join(.,station.data) %>%
  filter(.,.$count>=80)
max10.user.data$user_id <- as.character(max10.user.data$user_id)
#画图调色
cols <- brewer.pal(10,"Paired")
pal <- colorRampPalette(cols)
shp10 <- c(0,1,2,8,6,5,15,16,17,18)
shp8 <- c(0,1,2,8,15,16,17,18)
mycolors <- pal(10)
m5 <- shanghai.map +
  geom_line(data=max10.user.data, 
            aes(x=lng,y=lat,color=user_id),
            size=0.8, alpha=0.6) +
  geom_point(data=max10.user.data, 
            aes(x=lng,y=lat,color=user_id,shape=user_id,size=count)) +
  scale_color_manual(values=mycolors) +
  scale_shape_manual(values=shp10) +
  coord_map("polyconic",xlim=c(121.2,121.7),ylim=c(31.0,31.4)) +
  labs(x='经度',y='纬度',title='2016年11月10名典型用户的移动路径',
       color='用户编号',size='次数') +
  guides(shape=FALSE) +
  xihei + sizetheme
m5

#数据量最大的6名用户
max6.id <- order(mobile.user.count$count,decreasing=TRUE)[1:6]
max6.id <- as.character(mobile.user.count[max6.id,]$user_id)
max6.user.data <- user.data[user.data$user_id %in% max6.id,] %>%
  inner_join(.,station.data) %>%
  filter(.,.$count>=80)
max6.user.data$user_id <- as.character(max6.user.data$user_id)
#画图调色
cols <- brewer.pal(10,"Paired")
pal <- colorRampPalette(cols)
shp6 <- c(1,2,8,15,16,17)
mycolors <- pal(6)
m6 <- shanghai.map +
  geom_line(data=max6.user.data, 
            aes(x=lng,y=lat,color=user_id),
            size=0.8, alpha=0.6) +
  geom_point(data=max6.user.data, 
             aes(x=lng,y=lat,color=user_id,shape=user_id,size=count)) +
  scale_color_manual(values=mycolors) +
  scale_shape_manual(values=shp6) +
  coord_map("polyconic",xlim=c(121.2,121.7),ylim=c(31.0,31.4)) +
  labs(x='经度',y='纬度',title='2016年11月6名典型用户的移动路径',
       color='用户编号',size='次数') +
  guides(shape=FALSE) +
  xihei + sizetheme
m6

#排名前10的基站
max10.station.data <- station.data.count[order(station.data.count$count,decreasing=TRUE)[1:10],]
max10.station.data
#write.table(max10.station.data,'../result/max10_station.csv',row.names=FALSE,col.names=TRUE,sep=',')

