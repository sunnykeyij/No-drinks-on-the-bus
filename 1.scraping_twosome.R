rm(list=ls())

if(!require(httr)){install.packages("httr")}; library(httr)
if(!require(rvest)){install.packages("rvest")}; library(rvest)
if(!require(jsonlite)){install.packages("jsonlite")}; library(jsonlite)
if(!require(stringr)){install.packages("stringr")}; library(stringr)
if(!require(RJSONIO)){install.packages("RJSONIO")}; require(RJSONIO)
if(!require(RCurl)){install.packages("RCurl")}; require(RCurl)


##########################################
#서울매장 찾기
root2 = "https://www.twosome.co.kr:7009/store/search_list.asp?s_gubun=P&area=%uC11C%uC6B8&area2=&store_name=&page="

#서울에 있는 매장명과 url 페이지 번호(258개)
storeinf = c()
for (i in 1:26) {
  
  url2 = paste0(root2,i)  #5357(합정메세나폴리스)까지 해도됨
  
  html_txt2 = read_html(GET(url2),encoding = "EUC-KR")   
  
  pagenum = html_txt2 %>% html_nodes(".btn-gray") %>% html_nodes(xpath= "a") %>% html_attr("onclick") # 페이지 번호
  pagestore = html_txt2 %>% html_nodes(".btn-gray") %>% html_nodes(xpath= "a") %>% html_attr("title") # 매장명  
  
  pageinf = cbind(pagestore,pagenum)
  storeinf = rbind(storeinf,pageinf)
}
nrow(storeinf) #259개

Sys.setlocale("LC_ALL", "Korean")
storeinf = as.data.frame(storeinf,colnames = c("pagestore","pagenum"))
storeinf$pagenum = as.character(storeinf$pagenum)      
storeinf$pagestore = as.character(storeinf$pagestore)      

str(storeinf)

#pagenum에서 숫자만
storeinf$pagenum = gsub("[^0-9]","",storeinf$pagenum)
storeinf$pagenum = as.numeric(storeinf$pagenum)

#매장명 앞에 붙은 투썸플레이스 삭제
storeinf$pagestore = gsub("투썸플레이스","",storeinf$pagestore) %>% str_trim() 

#####################################################
#주소와 위도, 경도 찾기
root = "http://www.twosome.co.kr/store/storeView.asp?sort_store="

dat= data.frame()
latfunc = function(root,page){
  # page = 5157
  url = paste0(root,page)
  
  html.txt = read_html(GET(url),encoding = "EUC-KR") 
  
  store.name = html_nodes(html.txt,".store-view")%>% html_children()
  store.name = store.name[2] %>% html_text() #매장명
  
  store.inf = html_nodes(html.txt,css= "table") %>% html_children() %>% html_children()
  Sys.setlocale("LC_ALL", "Korean")
  store.addr = store.inf[3] %>%  html_text() # 도로명 주소
  Sys.setlocale("LC_ALL", "Korean")
  store.addr = gsub("도로명 주소","",store.addr); store.addr=gsub("\n","",store.addr)  
  store.addr = gsub("\t","",store.addr) ; store.addr= gsub("\r","",store.addr) #주소가 필요할까? 응!
  
  store_lat = html.txt %>% html_nodes(xpath = "/html/body/script[7]") %>% html_text()  #위도, 경도 추출
  
  store_lat= str_split(store_lat,";",n=3) #위도, 경도 추출
  store.lat= store_lat[[1]][2]
  store.lat = unlist(strsplit(store.lat, "[^0-9]"))
  store.lat = unique(store.lat[!is.na(store.lat)])
  lat = paste0(store.lat[2],".",store.lat[3]); long=paste0(store.lat[4],".",store.lat[5]) #위도 경도

  Sys.setlocale("LC_ALL", "Korean")
  dat = data.frame(store.name,store.addr,lat,long)
  return(dat)
}

finalstore = data.frame()
for (j in 1:253) { #254문제
  # j = 254
  store = latfunc(root = root,page =storeinf[j,]$pagenum)
  finalstore = rbind(finalstore,store)
 
}
nrow(finalstore) #253개

finalstore2 = data.frame()
for (h in 255:260) { #253문제(투썸롯데월드)
  # h = 261
  store2 = latfunc(root = root,page =storeinf[h,]$pagenum)
  finalstore2 = rbind(finalstore2,store2)
  
}

final = rbind(finalstore,finalstore2) #257개
nrow(final) #259

######################################################
#서울매장과 위도경도 합치기
nrow(storeinf) #260
storeinf = storeinf[-254,] #투썸롯데월드, 제거

nrow(storeinf) #259개

totalseoul = cbind(final,storeinf[2]) #258개


totalseoul$store.name = as.character(totalseoul$store.name)
totalseoul$store.addr = as.character(totalseoul$store.addr)
str(totalseoul)


final = final[-which(is.na(totalseoul$lat)),] #위도 경도 na인 삼성중앙역 망원역 제거
nrow(final) #257
totalseoul$lat = as.character(totalseoul$lat) %>% as.numeric()
totalseoul$long = as.character(totalseoul$long) %>% as.numeric()
str(totalseoul)
#######################################################

#area변수 만들기
splitaddr = str_split(totalseoul$store.addr," ",n=3) 
gu_fun = function(x){
  gu_addr = paste(x[1],x[2])
}
area = lapply(splitaddr, gu_fun) %>% unlist()

#caffe변수 만들기
caffe = rep("twosome",nrow(totalseoul))

#만든 변수들과 합치기(최종 data.frame)
twosome_fianl = data.frame(caffe= caffe,area = area,branch = totalseoul$store.name,
                         address = totalseoul$store.addr, 
                        lat = totalseoul$lat, long = totalseoul$long )


setwd("C:/Users/UOS/Dropbox/DataMiningTeamProject/caffe data")
save(twosome_fianl,file = "twosome_final.RDa")


