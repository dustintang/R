library(xml2)
library(rvest)
#1函数------
#下面开始写代码,首先写一个函数getdata，会输出一个数据框
getdata<-function(page,urlwithoutpage){
  url=paste0(urlwithoutpage,page) #这里输入拉勾网没有页码的url
  web<-read_html(url,encoding="UTF-8") #读取数据，规定编码,access用
  list_lagou<-web %>% html_nodes("li.clearfix") #获得一个清单，15个职位
  title<-list_lagou %>% html_nodes("div.hot_pos_l div.mb10 a")%>%html_text()
  company<-list_lagou %>% html_nodes("div.hot_pos_r div.mb10 a")%>%html_text()
  link<-gsub("\\?source\\=search","",list_lagou %>% html_nodes("div.hot_pos_l div.mb10 a")%>%html_attr("href"))
  #接下来的由于数据都存在span里，没有很好的划分。这个取数要复杂一些。我在这里，研究他们的表，先取15个完整list，然后用seq等序列取数
  #之后要研究是否有更好的方法
  #如果有table，可以直接用data.table取数更快。。。
  temp<-list_lagou %>% html_nodes("div.hot_pos_l span")
  city<-temp[seq(1,90,by=6)] %>% html_text()
  salary<-gsub("月薪：","",temp[seq(2,90,by=6)]%>% html_text())
  year<-gsub("经验：","",temp[seq(3,90,by=6)]%>% html_text())
  degree<-gsub("最低学历：","",temp[seq(4,90,by=6)]%>%html_text())
  benefit<-gsub("职位诱惑：","",temp[seq(5,90,by=6)]%>% html_text())
  time<-temp[seq(6,90,by=6)]%>%html_text()
  data.frame(title,company,city,salary,year,degree,benefit,time,link)
}
#2调用-----
#使用该函数，
url<-"http://www.lagou.com/jobs/list_%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90?kd=%E6%95%B0%E6%8D%AE%E5%88%86%E6%9E%90&spc=2&pl=&gj=&xl=&yx=&gx=&st=&labelWords=&lc=&workAddress=&city=%E6%B7%B1%E5%9C%B3&requestId=&pn="
getdata(3,url)
final<-data.frame()
for (i in 3:5){
  final<-rbind(final,getdata(i,url))        
} #定义个数，把上面的getdata得到的Data.frame合并
head(final)

#3测试------
url<-"https://www.lagou.com/jobs/list_数据分析?kd=数据分析&spc=2&pl=&gj=&xl=&yx=&gx=&st=&labelWords=&lc=&workAddress=&city=深圳&requestId=&pn=1"
web<-read_html(url,encoding="UTF-8") #读取数据，规定编码,access用
lg<-web %>% html_nodes("span.money") #获得一个清单，15个职位


