library(dplyr)
library(RSQLite)
#library(pitchRx)
library(stringr)
#library(beepr)
#setwd("C:/Users/sungmin/Desktop/pitchRx")

conn = dbConnect(RSQLite::SQLite() , 'retry.sqlite3')
pitch.tbl = dbGetQuery(conn, 'select * from pitch') %>% as.tbl()
atbat.tbl = dbGetQuery(conn, 'select * from atbat') %>% as.tbl()
dbDisconnect(conn)

full.tbl = pitch.tbl %>% inner_join( atbat.tbl %>% select(setdiff(colnames(atbat.tbl), colnames(pitch.tbl)), 'url', 'num'), by= c('url','num') )
full.tbl = full.tbl %>% select(-ends_with('_es'))
full.tbl = full.tbl %>% filter(gameday_link != 'gid_2014_03_24_bosmlb_balmlb_1')
full.tbl = full.tbl %>% arrange(url,num,id)
full.tbl = full.tbl %>% group_by(pitcher_name, url) %>% mutate(pitch_count = row_number()) %>% ungroup()
full.tbl = full.tbl %>% mutate(home_team_runs = as.numeric(home_team_runs),
                               away_team_runs = as.numeric(away_team_runs),
                               b_height = str_sub(b_height,end=1)%>%as.numeric()*12+str_sub(b_height,start=3)%>%as.numeric,
                               inning_side = ifelse(inning_side=='top',1,0),
                               ball = str_sub(count,end=1)%>%as.numeric(),
                               strike = str_sub(count,start=-1)%>%as.numeric(),
                               stand = ifelse(stand=='R',1,0),
                               p_throws = ifelse(p_throws=='R',1,0) )

#base estimation
base.conv = data.frame(event = full.tbl$event %>% unique, bases = c(0,0,0,1,0,1,3,0,4,1,2,1,0,0,1,0,0,0,0,0,0,1,0,2,0,0,1,0,0,0,0))
bcd =  matrix(NA,12,4); colnames(bcd) = c('ball','strike','a','b')
bcd[,1] = rep(0:3, each=3); bcd[,2] = rep(0:2, 4); rownames(bcd) = paste0(bcd[,1], '-', bcd[,2])
for(i in 1:12){
  x = full.tbl %>% filter(count == paste0(bcd[i,1],'-',bcd[i,2])) %>% select(url,num,event) %>% unique()
  y = full.tbl %>% filter(count == paste0(bcd[i,1]+1,'-',bcd[i,2])) %>% select(url,num,event) %>% unique()
  z = full.tbl %>% filter(count == paste0(bcd[i,1],'-',bcd[i,2]+1)) %>% select(url,num,event) %>% unique()
  y = inner_join(x,y,by=c('url','num','event')) %>% group_by(event) %>% count() %>% inner_join(base.conv , by='event') %>% 
    mutate(bases = n*bases) %>% ungroup() %>% select(n,bases) %>% sapply(sum)
  z = inner_join(x,z,by=c('url','num','event')) %>% group_by(event) %>% count() %>% inner_join(base.conv , by='event') %>% 
    mutate(bases = n*bases) %>% ungroup() %>% select(n,bases) %>% sapply(sum)
  bcd[i,3:4] = c(y[2]/y[1], z[2]/z[1] )
}
bcd[is.nan(bcd[,3]),3] = 1; bcd[is.nan(bcd[,4]),4] = 0 

base.cal = function(x){
  if(x[3] == "X"){
    i = which(base.conv[,1] == x[60])
    return(base.conv[i,2])
  } else if(x[3] == "S"){
    i = which(rownames(bcd) == x[45])
    return(bcd[i,4])
  } else {
    i = which(rownames(bcd) == x[45])
    return(bcd[i,3])
  }
}
full.tbl$base = apply(full.tbl,1,base.cal)
full.tbl = full.tbl %>% unclass() %>% as.data.frame %>% as.tbl()

##score estimation
full.tbl$run = full.tbl$home_team_runs + full.tbl$away_team_runs
full.tbl = full.tbl %>% group_by(url) %>%
  mutate(run = rep( c(0,run[!is.na(run)]), 
                    c( which(!is.na(run))[1]-1,which(!is.na(run))%>%diff,length(run)-which(!is.na(run))%>%last+1 ) ) ) %>%
  mutate(run = c(first(run),diff(run))) %>% ungroup() %>% 
  group_by(url,num) %>% mutate(run = c(rep(0,length(run)-1),max(run))) %>% ungroup()

rm(list=c('base.conv','bcd','x','conn','i','y','z','base.cal'))

## randomForest
library(randomForest)
set.seed(1992)

start.tbl = full.tbl %>% 
  group_by(gameday_link, inning_side) %>% filter(pitcher==pitcher[1]) %>% ungroup()

train13 = start.tbl %>% filter(str_sub(gameday_link,5,8)%>%as.numeric == 2013) %>%
  filter(str_sub(gameday_link,10,11)%>%as.numeric <= 8) %>%
  filter(!(event %in% c('Intent Walk','Fan interference','Batter Interference', 'Catcher Interference','Field Error'))) %>%
  select(start_speed:x0, z0:pitch_type, spin_dir:spin_rate, pitch_count, stand, p_throws, count, base) %>%
  na.omit %>% sample_n(10^4)

rf13 = randomForest(base ~ . , data=train13, ntree=1000)

test13 = start.tbl %>% filter(str_sub(gameday_link,5,8)%>%as.numeric == 2013) %>%
  filter(str_sub(gameday_link,10,11)%>%as.numeric == 9) %>%
  filter(!(event %in% c('Intent Walk','Fan interference','Batter Interference', 'Catcher Interference','Field Error'))) %>%
  select(start_speed:x0, z0:pitch_type, spin_dir:spin_rate, pitch_count, stand, p_throws, count, run, gameday_link, inning_side, num, inning, base) %>%
  na.omit
test13$qop = predict(rf13, test13[,1:26])
test13 = test13 %>% unclass %>% as.data.frame %>% as.tbl

gameid = (test13$gameday_link%>%unique%>%as.vector)[1:10]

k=1
for(i in gameid){
  for(j in 0:1){
    (test13 %>% filter(gameday_link == i, inning_side== j) %>%
       select(pitch_count, qop, run) %>%
       mutate(run = run/4) %>% ggplot +
       geom_line(aes(x=pitch_count,y=qop)) +
       geom_point(aes(x=pitch_count,y=run)) +
       labs(x='Pitch Count', y='QOP')) %>% print
    ggsave(paste0(k,'a.png'),width=3,height=2)
    k=k+1
  }
}

##moving average
n=10
test13 = test13 %>% group_by(gameday_link,inning_side) %>%
  mutate(maqop = c(rep(0,n-1), c(0,cumsum(qop)) %>% diff(n))/n ) %>% ungroup()

k=1
for(i in gameid){
  for(j in 0:1){
    (test13 %>% filter(gameday_link == i, inning_side== j) %>%
      select(pitch_count, maqop, run) %>%
       mutate(run = run/4) %>% ggplot +
       geom_line(aes(x=pitch_count,y=maqop)) +
       geom_point(aes(x=pitch_count,y=run)) + 
       labs(x='Pitch Count', y='Moving Average QOP')) %>% print
    ggsave(paste0(k,'b.png'),width=3,height=2)
    k=k+1
  }
}

##atbat average
test13 = test13 %>% group_by(gameday_link,num) %>%
  summarise(pitch_count = last(pitch_count),
            inning = first(inning),
            inning_side = first(inning_side),
            qop = mean(qop),
            run = sum(run)
            ) %>% ungroup()

k=1  
for(i in gameid){
  for(j in 0:1){
    (test13 %>% filter(gameday_link == i, inning_side== j) %>%
       select(pitch_count, qop, run) %>%
       mutate(run = run/4) %>% ggplot +
       geom_line(aes(x=pitch_count,y=qop)) +
       geom_point(aes(x=pitch_count,y=run)) +
       labs(x='Pitch Count', y='Atbat Mean QOP')) %>% print
    ggsave(paste0(k,'c.png'),width=3,height=2)
    k=k+1
  }
}

##inning average
test13 = test13 %>% group_by(gameday_link,inning_side,inning) %>%
  summarise(pitch_count = last(pitch_count),
            qop = mean(qop),
            run = sum(run)
  ) %>% ungroup()

for(i in gameid){
  for(j in 0:1){
    (test13 %>% filter(gameday_link == i, inning_side== j) %>%
       select(pitch_count, qop, run) %>%
       mutate(run = run/4) %>% ggplot +
       geom_line(aes(x=pitch_count,y=qop)) +
       geom_point(aes(x=pitch_count,y=run)) +
       labs(x='Pitch Count', y='Inning Mean QOP')) %>% print
    ggsave(paste0(k,'inning.png'),width=4,height=2)
    k=k+1
  }
}
