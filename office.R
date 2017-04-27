library(xml2)
library(rvest)
library(stringr)
#1获取共享办公数据------
gethd<-function(urlnopage,page){
  url<-paste0(urlnopage,page)
  web<-read_html(url,encoding="UTF-8")
  o1<-web %>% html_nodes("div.mapFind-list-box") %>% html_nodes("h1") %>% html_text()
  o<-web %>% html_nodes("div.mapFind-list-box") %>% html_nodes("span") %>% html_text()
  o2<-str_split_fixed(o,"元/",n=2)
  o<-web %>% html_nodes("ul.mapFind-list-con") %>% html_nodes("li") %>% html_text()
  o3<-matrix(as.vector(o),ncol=2,byrow = T)
  o4<-web %>%  html_nodes("p.mapFind-list-p") %>% html_text()
  o5<-web %>%  html_nodes("ul.mapFind-list-bor") %>% html_text()
  office<-cbind(o1,o2,o3,o4,o5)
  office<-str_replace_all(office," ","")
  office<-matrix(office,ncol=7)
}
##初始设定
office<-matrix(ncol=7)
alloffice<-matrix(ncol=7)
url<-"http://www.haozu.com/sh/zuxiezilou/a5u1/o" 
##爬取数据
for (i in 1:12){
  office<-gethd(url,i)
  alloffice<-rbind(alloffice,office)
}
ao<-alloffice[-1,]
ao1<-str_split_fixed( ao[,4] ,"-", n=2 )
ao2<-str_split_fixed( ao1[,2],"\n",n=2)
ao3<-str_split_fixed(ao2[,2],";",n=2)
ao11<-str_sub(ao1[,1],5,6)
ao12<-str_sub(ao2[,1],0,-2)
AO<-cbind(ao[,-4],ao11,ao12,ao3)
colnames(AO)<-c("name","price","unit","gw","hxs","hx","district","landmark","location","metro")

#2获取办公数据------
##爬取函数
gethd<-function(urlnopage,page){
  url<-paste0(urlnopage,page)
  web<-read_html(url,encoding="UTF-8")
  o1<-web %>% html_nodes("div.mapFind-list-box") %>% html_nodes("h1") %>% html_text()
  o<-web %>% html_nodes("div.mapFind-list-box") %>% html_nodes("span") %>% html_text()
  o2<-str_split_fixed(o,"元/",n=2)
  o3<-web %>% html_nodes("ul.mapFind-list-con") %>% html_nodes("li") %>% html_text()
  #o3<-matrix(as.vector(o),ncol=3,byrow = T)  #ncol=3
  o4<-web %>%  html_nodes("p.mapFind-list-p") %>% html_text()
  o5<-web %>%  html_nodes("ul.mapFind-list-bor") %>% html_text()
  office<-cbind(o1,o2,o4,o5)
  office<-str_replace_all(office," ","")
  office<-matrix(office,ncol=5)  #ncol=5
  office<-list(office,o3)
}
##初始设定
office1<-matrix(ncol=5)
office2<-vector()
url<-"http://www.haozu.com/sh/zuxiezilou/a1/o"
##爬取信息
for (i in 1:200){  ###页数更改初
  o<-gethd(url,i)
  o1<-unlist(o[1])
  o2<-unlist(o[2])
  names(o1)<-NULL
  o1<-matrix(o1,ncol=5)
  office1<-rbind(office1,o1)  ##直接获取5列数据
  office2<-c(office2,o2)     ##3列待处理数据
}
temp1<-office1   ##缓存数据
temp2<-office2   ##缓存数据
office1<-office1[-1,]  ##office1不改了留存
##数据合并
a<-factor(substr(office2,1,2))
rq<-which(a==a[3])-which(a=="地址")
rq[which(rq==2)]<-office2[which(a=="人气")]
dz<-office2[which(a=="地址")]
kz<-office2[which(a==a[3])]
office<-cbind(office1,dz,rq,kz)  ##所有数据在office矩阵中
##清理细分数据
ao<-office[,"dz"]
ao1<-str_split_fixed( ao,"-", n=2 )
ao2<-str_split_fixed( ao1[,2],"\n",n=2)
ao3<-str_split_fixed(ao2[,2],";",n=2)
ao11<-str_sub(ao1[,1],5,6)
ao12<-str_sub(ao2[,1],0,-2)
AO<-cbind(ao11,ao12,ao3)
office<-cbind(office[,-6],AO)  ##地址数据细分完成
ao<-office[,11]
ao1<-str_split_fixed( ao,"号线", n=2 )
ao2<-str_split_fixed( ao1[,2],"约",n=2)
ao11<-str_sub(ao1[,1],4,6)
ao3<-str_replace_all(ao2[,2]," ","")
ao3<-str_sub(ao3,end = -2)
AO<-cbind(ao11,ao2[,1],ao3)
office<-cbind(office[,-11],AO)  ##新产生地铁数据细分完成
office[,4]<-str_sub(office[,4],end = -8)  ##房源数量处理
office<-office[,-3]  ##去掉单位，都是重复的
colnames(office)<-c("name","price","fys","qt","rq","kz","qy","db","dz","dt","dtz","jl") #重命名各列
ao<-office[,"rq"]
ao1<-str_split_fixed( ao,"位", n=2 )
ao2<-str_split_fixed( ao1[,2],",",n=2)
ao11<-str_replace_all(ao1[,1]," ","")
ao11<-str_sub(ao11,5,8)
ao3<-str_split_fixed( ao2[,2],"次",n=2)
AO<-cbind(ao11,ao3[,1])  #关注人数、预约看房次数
office<-cbind(office[,-5],AO)  ##办公楼人气数据细分完成
ao<-office[,"kz"]
ao1<-str_split_fixed( ao,"-", n=2 )
ao2<-str_split_fixed( ao1[,1],"m", n=2 )
ao21<-str_replace_all(ao2[,1]," ","")
ao21<-str_sub(ao21,7,11)
ao3<-str_split_fixed( ao1[,2],"m", n=2 )
AO<-cbind(ao21,ao3[,1])  #可租房面积段最低、最高平数
office<-cbind(office[,-5],AO)  ##可租面积段细分完成
colnames(office)<-c("name","price","fys","qt","qy","db","dz","dt","dtz","jl","gz","yy","min","max") #重命名各列
#楼名、价格、房源数、其他、区域、地标、地铁、地铁站、离地铁站距离、关注人数、预约次数、最小面积、最大面积。

#3导入数据库------
library(DBI)
library(RMySQL)
conn <- dbConnect(MySQL(), dbname = "bg", username="root", password="123456",host="127.0.0.1",port=3306)
dbSendQuery(conn,"SET NAMES utf8")
office_df<-as.data.frame(office)  ##变成数据框，方便导入数据库中
dbWriteTable(conn,"office_df",office_df[,1:14],overwrite=TRUE)
gc(rm(list=ls()))
office_df<-dbReadTable(conn, "office_df")

#4分析数据-----
library(VIM)
library(ggplot2)
library(psych)
odf1<-data.frame(sapply(office_df[,c("price","fys","jl","gz","yy","min","max")],as.numeric))
odf2<-data.frame(sapply(office_df[,c("qy","db","dt","dtz")],as.factor))
odf<-cbind(odf1,odf2,office_df[,c("name","dz")])
miss<-aggr(odf)  #处理缺失值
odf[which(is.na(odf$yy)==T),"yy"]<-0
odf[which(is.na(odf$gz)==T),"gz"]<-0
x<-which(is.na(odf$max)==T)
odf[x,"max"]<-odf[x,"min"]  #缺失值处理完

datesum<-summary(odf)  #综述
datecor <- corr.test(odf[,1:7]) ##数据相关性，得出价格和距离、关注和预约相关性高
prjl<-qplot(jl,price,data=odf,alpha=1/10)
# 各因素房价均价
avg_price <- aggregate(odf$price, by = list(odf$qy), mean)
# 分类价格绘图
p <- ggplot(data = avg_price, mapping = aes(x = reorder(Group.1, -x), y = x, group = 1)) + 
  geom_area(fill = 'lightgreen') + 
  geom_line(colour = 'steelblue', size = 2) + 
  geom_point() + 
  xlab('') + 
  ylab('均价') +
  theme(text=element_text(family="Hei"))  

#5建模-------
##聚类分析
tot.wssplot <- function(data, nc, seed=1234){
  #假设分为一组时的总的离差平方和              
  tot.wss <- (nrow(data)-1)*sum(apply(data,2,var)) 
  for (i in 2:nc){
    #必须指定随机种子数
    set.seed(seed) 
    tot.wss[i] <- kmeans(data, centers=i, iter.max = 100)$tot.withinss
  }
  plot(1:nc, tot.wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares",col = 'blue',
       lwd = 2, main = 'Choose best Clusters')
}
# 绘制不同聚类数目下的组内离差平方和
standrad <- data.frame(scale(odf[,c('price')]))
myplot <- tot.wssplot(standrad, nc = 15)  #聚类数为5时合适
set.seed(1234)
clust <- kmeans(x = standrad, centers = 4, iter.max = 100)
table(clust$cluster)
table(odf$qy,clust$cluster)
aggregate(odf$max, list(clust$cluster), mean)
# 绘制面积与单价的散点图，并按聚类进行划分
p <- ggplot(data = odf, mapping = aes(x = jl,y = price, color = factor(clust$cluster)))
p <- p + geom_point(pch = 20, size = 3)
p + scale_colour_manual(values = c("red","blue", "green", "black", "orange"))



#6可视化-----










