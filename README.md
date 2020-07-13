# data-mining-in-finance-and-economics
[2020 Spring] final project for FDU DATA130001: Data Mining in Finance and Economics

My topic is [Economic analysis of mobile telephone base station data](https://github.com/FDU-SDS/Big_Data_Economic_Course_Final/blob/master/期末考评项目：手机基站信息的经济分析.md)(in Chinese)

Students are given several tasks to choose from, all the tasks are listed [here](https://github.com/FDU-SDS/Big_Data_Economic_Course_Final)(which may be updated)

**Part of the visualization is based on [kepler.gl](https://github.com/keplergl/kepler.gl)**

[2020春季学期] 复旦大学大数据学院专业必修课程 DATA130001：金融与经济大数据挖掘 期末Pj

我的选题是：[手机基站信息的经济分析](https://github.com/FDU-SDS/Big_Data_Economic_Course_Final/blob/master/期末考评项目：手机基站信息的经济分析.md)

所有可选的选题在[这里](https://github.com/FDU-SDS/Big_Data_Economic_Course_Final)（内容可能会被更新）

**部分可视化结果基于开源框架[kepler.gl](https://github.com/keplergl/kepler.gl)**

## code description:

1. geo_fencing.py

对station.csv进行处理，添加每个station对应的行政区。
参考：https://www.cnblogs.com/huang-yc/p/10122112.html

2. shanghai_map.R

读取shp文件，获得上海地图（各区边界）。这里用到的是县级数据，数据来源：[中国行政区边界shp下载（省，市，县）](https://blog.csdn.net/niu_dige/article/details/104856967)

3. pj.R

处理数据及绘图


