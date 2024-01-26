# A 매치 데이터 파일
# mongoDB에 담기
# 그 데이터 활용해서
# 우리나라 경기 기준
# ggplot2 bar (승/패)
# echarts bar (1/2/3/4/5)골 차이

library(dplyr)
res = read.csv("C:/Users/sdedu/Desktop/Dev/R/csv/results.csv",encoding="UTF-8")
res
rm(res)

library(mongolite)

# 전체 데이터 DB에 넣어줌
if(con$count()>0) con$drop()
con <- mongolite::mongo(collection ="results",db="prac",url="mongodb://localhost",verbose=T,options=ssl_options())
con

con$insert(res)

# team : Korea | opponent : Korea 인 것만 가져올 것.
df <- con$find(query='{"$or":[{"team":{"$regex":"Korea Republic"}},{"opponent":{"$regex":"Korea Republic"}}]}')
df <- con$find(query='{"team":{"$regex":"Korea Republic"}}')
df <- data.frame(df)

View(df)

# 그래프를 위한 lib
library(ggplot2)
library(echarts4r)

# 득점
df[[4]]

# 실점
df[[5]]

stat = c()
differ = c()

for (i in 1:nrow(df)){
  getGoal = as.numeric(df[[4]][i])
  lostGoal = as.numeric(df[[5]][i])
  
  if(getGoal > lostGoal){
    if(getGoal - lostGoal == 1){
      differ[length(differ) + 1] = "1점차"
    } else if(getGoal - lostGoal == 2){
      differ[length(differ) + 1] = "2점차"
    } else if(getGoal - lostGoal == 3){
      differ[length(differ) + 1] = "3점차"
    } else if(getGoal - lostGoal == 4){
      differ[length(differ) + 1] = "4점차"
    } else if(getGoal - lostGoal >= 5){
      differ[length(differ) + 1] = "5점차 이상"
    }
    stat[length(stat) + 1] = "승"
    
  }else if(lostGoal > getGoal){
    if(lostGoal - getGoal == 1){
      differ[length(differ) + 1] = "1점차"
    } else if(lostGoal - getGoal == 2){
      differ[length(differ) + 1] = "2점차"
    } else if(lostGoal - getGoal == 3){
      differ[length(differ) + 1] = "3점차"
    } else if(lostGoal - getGoal == 4){
      differ[length(differ) + 1] = "4점차"
    } else if(lostGoal - getGoal >= 5){
      differ[length(differ) + 1] = "5점차 이상"
    }
    stat[length(stat) + 1] = "패"
    
  }
  
}
stat
differ

goalDF = data.frame(stat,differ)
View(goalDF)

goalDF %>% group_by(stat,differ) %>% summarise(n=n()) %>% ggplot(aes(stat,n,fill=differ))+ 
  geom_col(position = position_dodge(0.8),width=0.5)

goalDF %>% group_by(stat,differ) %>% summarise(n=n()) %>% e_chart(differ) %>% e_bar(n,barwidth=10) %>% e_tooltip(trigger=c('axis')) %>% 
  e_color(c("#D1B2ff","#FFb2F5"))

rm(con)
