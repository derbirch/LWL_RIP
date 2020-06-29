install.packages('rtweet')
install.packages('xlsx')
library(rtweet)
library(jiebaR)
library(sqldf)
library(dplyr)
library(stringr)
library(echarts4r)
library(ggplot2)

file.edit('~/.Renviron')

tweets <- search_tweets(q = "李文亮",n=1000)
View(tweets)
#new_user_word(work_new_word, c("李文亮","太子湾","八卦田"))
wk = worker()
seg<-wk[tweets$text]
seg<-seg[nchar(seg)>1] 
seg<-table(seg)
seg<-seg[!grepl('[0-9]+',names(seg))]
seg<-seg[!grepl('[A-Z]+',names(seg))]
seg<-seg[!grepl('[a-z]+',names(seg))]
f<-as.data.frame(seg)

f %>% 
  e_color_range(Freq, color) %>% 
  e_charts() %>% 
  e_cloud(word=seg, freq=Freq, color=color, shape = "circle", sizeRange = c(5,100)) %>% 
  e_title("词云", "推文词频") %>%
  e_tooltip()


f_slim<-arrange(f,desc(Freq),seg)
f_slim<-slice(f_slim,1:20)

f_slim %>% 
  e_charts(x=seg) %>% 
  e_bar(Freq) %>% 
  e_title("条形图", "推文词频") %>%
  #e_x_axis(axisLabel = list(interval = 0, rotate = 0)) %>%
  e_tooltip()

ggplot(f_slim, aes(x=seg, y=Freq)) +
  #geom_segment( aes(x=seg, xend=seg, y=0, yend=Freq)) +
  geom_text(aes(label=seg,y=Freq,size=Freq/15),color='black')+
  geom_point(aes(alpha=0.7,size=Freq,color=Freq)) +
  scale_size(range = c(.1, 24), name="数量 (次)")



#报道

g<-read.csv(file="~/LWL_RIP/报道词频.csv", header=TRUE)

g1<-slice(g,1:n())

g1 %>% 
  e_color_range(Freq, color) %>% 
  e_charts() %>% 
  e_cloud(seg, Freq, color, shape = "circle", sizeRange = c(10,100)) %>% 
  e_title("词云", "报道词频") %>%
  e_tooltip()



g1<-slice(g,1:20)

g1 %>% 
  e_charts(x=seg) %>% 
  e_bar(Freq) %>% 
  e_title("条形图", "报道词频") %>%
  #e_x_axis(axisLabel = list(interval = 0, rotate = 0)) %>%
  e_tooltip()



f<-freq(seg)
f1<-subset(f, nchar(char) >1)
f2<-f1[grep(pattern="[:alnum:]*",f1[,1]),]
keys = worker("keywords",topn=50)
vector_keywords(seg,keys)