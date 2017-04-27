library(DBI)
library(RMySQL)
library(ggplot2)
#1连接数据库----
conn <- dbConnect(MySQL(), dbname = "Innovation", username="root", password="123456",
        host="127.0.0.1",port=3306)
dbListTables(conn)
dbSendQuery(conn,"SET NAMES utf8")
#2查询----
sql<-"select  一级分类 as type,year(时间) as time,
sum(换算金额) as money
from finance a left join company b on a.投资项目=b.投资项目 group by 一级分类,year(时间)"
sum1<-data.frame(dbGetQuery(conn,sql))
str(sum1)
sum1$type<-as.factor(sum1$type)
#3作图----
p<-ggplot(data=sum1,mapping = aes(time,money,fill=type))
p+geom_col()+
  labs(title="地产创新融资")+
  theme(text=element_text(family="Hei"))+
  scale_y_continuous(breaks=c(1000000,2000000),labels = c("100亿","200亿"))
#4比例图----
attach(sum1)
x<-tapply(money,time,sum)
y<-match(time,names(x))
money1<-money/x[y]

p<-ggplot(data=sum1,mapping = aes(time,money1,fill=type))
p+geom_col()+
  labs(title="地产创新融资")+
  theme(text=element_text(family="Hei"))+
  scale_x_continuous(breaks = 2007:2017,limits = c(2006,2018))+
  geom_line(mapping = aes(time,money/2800000),inherit.aes = F,alpha=0.9,position="stack")
#5退出----
dbDisconnect(conn)




