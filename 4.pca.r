# 데이터생성 A - 상권별(상권이름 mainTrarNm) 쓰레기통이 있을 확률(trash_p)
trash_bymarket2 <- trash_bymarket %>% distinct(자치구명, 설치위치, trarNo, .keep_all = T)
