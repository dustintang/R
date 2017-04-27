library(xml2)
library(rvest)
library(stringr)

gethd<-function(urlnopage,page){
  url<-paste0(urlnopage,page)
  web<-read_html(url,encoding="UTF-8")
  hdtitle<-web %>% html_nodes("h3") %>% html_text()
  hdtitle<-hdtitle[-11]
  hd<-web %>% html_nodes("ul.event-horizontal-list-new") %>% html_nodes("div") %>% html_text()
  hd<-str_replace_all(hd," ","")
  hd<-str_replace_all(hd,"\r","")
  hd<-str_replace_all(hd,"\n","")
  hd<-matrix(hd,ncol=3,byrow = T)
  hd<-cbind(hdtitle,hd)
}

url<-"http://www.huodongxing.com/eventlist?orderby=r&d=t1&city=上海&page="
hdx<-matrix( ncol = 4)
for (i in 1:25){
  hd<-gethd(url,i)
  hdx<-rbind(hdx,hd)
}

hdx<-apply(hdx,2,sort,decreasing=F)
substr(hdx[,3],6,7)<-"区"
write.csv(hdx[-1,],file="周周看.csv",fileEncoding = "gbk")


