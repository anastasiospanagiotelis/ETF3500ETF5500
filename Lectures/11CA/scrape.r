library(magrittr)
library(dplyr)
library(rvest)
library(ca)
stopwords<-read.csv('stop-word-list.txt',stringsAsFactors = FALSE,header = FALSE)
stopwords<-c(stopwords$V1,'','s','t',tolower(hotelnames))
hotelnames<-c('CrownTowers',
              'Larwill',
              'Adelphi',
              'QT',
              'FlagstaffCity',
              'Mercure',
              'Citiclub',
              'HotelSophia')
urla<-c('https://www.tripadvisor.com.au/Hotel_Review-g255100-d257575-Reviews',
        'https://www.tripadvisor.com.au/Hotel_Review-g255100-d6755952-Reviews',
        'https://www.tripadvisor.com.au/Hotel_Review-g255100-d256779-Reviews',
        'https://www.tripadvisor.com.au/Hotel_Review-g255100-d10712642-Reviews',
        'https://www.tripadvisor.com.au/Hotel_Review-g255100-d255743-Reviews',
        'https://www.tripadvisor.com.au/Hotel_Review-g255100-d257037-Reviews',
        'https://www.tripadvisor.com.au/Hotel_Review-g255100-d596738-Reviews',
        'https://www.tripadvisor.com.au/Hotel_Review-g255100-d624515-Reviews')

urlc<-c('-Crown_Towers_Melbourne-Melbourne_Victoria.html',
        '-Art_Series_The_Larwill_Studio-Melbourne_Victoria.html',
        '-Adelphi_Hotel_Melbourne-Melbourne_Victoria.html',
        '-QT_Melbourne-Melbourne_Victoria.html',
        '-Flagstaff_City_Melbourne-Melbourne_Victoria.html',
        '-Mercure_North_Melbourne-Melbourne_Victoria.html',
        '-Citiclub_Hotel-Melbourne_Victoria.html',
        '-Hotel_Sophia-Melbourne_Victoria.html')
num<-seq(5,95,by=5)
mid<- c('',paste0(rep('-or',length(num)),num))
nw<-8
all<-vector("list",nw)
for (j in 1:nw){
  url<-paste0(rep(urla[j],length(mid)),mid,rep(urlc[j],length(mid)))
  n<-length(url)
  result<-vector("list",n)
  
  for (i in 1:n){
    urli<-url[i]
    print(urli)
    urli %>%
      read_html %>%
      html_nodes(".partial_entry") %>%
      html_text()->resulti
    result[[i]]<-resulti
    
  }
  u<-unlist(result)
  u%>%gsub('...More','',.)%>%
    gsub('[[:punct:] ]+',' ',.)%>%
    gsub('[[:digit:] ]+',' ',.)%>%
    gsub('\n',' ',.)%>%
    gsub('[[:space:]]',' ',.)%>%
    tolower%>%paste(.,sep=' ',collapse=' ')%>%
    strsplit(.,split=' ')%>%
    extract2(.,1)->cleantext

  all[[j]]<-cleantext[!(cleantext %in% stopwords)]
}

saveRDS(all,'all.rds')

dj<-data.frame(Word=all[[1]])

dj%>%
  group_by(Word)%>%
  summarise(n())%>%
  arrange(desc(`n()`))%>%
  use_series(Word)%>%
  as.character()%>%
  extract(1:20)->
  top20

for(j in 2:nw){
  dj<-data.frame(Word=all[[j]])
  
  dj%>%
    group_by(Word)%>%
    summarise(n())%>%
    arrange(desc(`n()`))%>%
    use_series(Word)%>%
    as.character()%>%
    extract(1:20)->
    top20j
  top20<-c(top20,top20j)
}
top20<-unique(top20)

nwd<-length(all[[1]])
hn<-rep(hotelnames[1],nwd)
dat<-cbind(all[[1]],hn)
for (j in 2:nw){
  dataj<-data.frame(Word=dat[,1])
  nwd<-length(all[[j]])
  hn<-rep(hotelnames[j],nwd)
  dat<-rbind(dat,cbind(all[[j]],hn))
  
}
data.frame(Word=dat[,1],Hotel=dat[,2])->d
  


d%>%mutate(WordC=as.character(Word))%>%
  filter(WordC %in% top20)%>%
  select(WordC,Hotel)->wf


wf%>%
  table->hoteltable
save(hoteltable,file='hotels.RData')
ca(hoteltable)%>%plot
