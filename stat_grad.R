library(dplyr)
library(RSQLite)
library(ggplot2)
#library(pitchRx)
library(stringr)
#library(beepr)
#setwd("C:/Users/sungmin/Desktop/pitchRx")

conn = dbConnect(RSQLite::SQLite() , 'retry.sqlite3')
pitch.tbl = dbGetQuery(conn, 'select * from pitch') %>% as.tbl()
atbat.tbl = dbGetQuery(conn, 'select * from atbat') %>% as.tbl()
action.tbl = dbGetQuery(conn, 'select * from action') %>% as.tbl()
dbDisconnect(conn)

action.tbl = action.tbl %>% mutate(num = as.numeric(num)) %>%
  group_by(url,num) %>% summarise(o1 = min(2,max(o))) %>% ungroup()

atbat.tbl = atbat.tbl %>% arrange(url,num) %>% group_by(url,inning,inning_side) %>%
  mutate(o = c(0,o[-length(o)])) %>% ungroup
atbat.tbl = atbat.tbl %>% left_join(action.tbl, by = c('url','num')) %>%
  mutate(o = pmax(o,o1,na.rm=T), o1=NULL)

full.tbl = pitch.tbl %>% inner_join( atbat.tbl %>% select(setdiff(colnames(atbat.tbl), colnames(pitch.tbl)), 'url', 'num'), by= c('url','num') )
full.tbl = full.tbl %>% filter(gameday_link != 'gid_2014_03_24_bosmlb_balmlb_1')
full.tbl = full.tbl %>% arrange(url,num,id)
full.tbl = full.tbl %>% group_by(pitcher_name, url) %>% mutate(pitch_count = row_number()) %>% ungroup()
full.tbl = full.tbl %>% mutate(home_team_runs = as.numeric(home_team_runs),
                               away_team_runs = as.numeric(away_team_runs),
                               b_height = str_sub(b_height,end=1)%>%as.numeric()*12+str_sub(b_height,start=3)%>%as.numeric,
                               b = str_sub(count,end=1)%>%as.numeric(),
                               s = str_sub(count,start=-1)%>%as.numeric(),
                               base = paste0(ifelse(is.na(on_1b),0,1),
                                             ifelse(is.na(on_2b),0,1),
                                             ifelse(is.na(on_3b),0,1)) )

full.tbl = full.tbl %>% group_by(gameday_link,num) %>%
  mutate(type = c(type[-length(type)],"X") ) %>% ungroup

na.step = function(x){
  if(is.na(x[1])) x[1]=0
  ind = which(!is.na(x))
  out = rep(x[ind],c(diff(ind),length(x)-last(ind)+1) )
  return(out)
}

full.tbl = full.tbl %>% group_by(gameday_link) %>%
  mutate(home_team_runs = na.step(home_team_runs),
         away_team_runs = na.step(away_team_runs)) %>%
  mutate(home_team_runs = c(home_team_runs[1], diff(home_team_runs)),
         away_team_runs = c(away_team_runs[1], diff(away_team_runs))) %>%
  group_by(gameday_link, num) %>% 
  mutate(home_team_runs = max(home_team_runs),
         away_team_runs = max(away_team_runs)) %>% ungroup
full.tbl = full.tbl %>% mutate(score = home_team_runs + away_team_runs)
full.tbl = full.tbl %>% group_by(gameday_link, num) %>%
  mutate(score = c(rep(0,length(score)-1),last(score))) %>% ungroup

pa.re = full.tbl %>% group_by(gameday_link,inning, inning_side) %>% 
  mutate(score = sum(score) - cumsum(score) + score ) %>%
  group_by(base,o) %>% summarise(re=mean(score)) %>% ungroup
bs.re = full.tbl %>% group_by(gameday_link,inning, inning_side) %>% 
  mutate(score = sum(score) - cumsum(score) + score ) %>%
  group_by(b,s) %>% summarise(re=mean(score)) %>% ungroup

bs.rv = bs.re %>% mutate(type=rep("S",12)) %>% mutate(rv = c(diff(re),0))
bs.rv[3*1:4,5] = 0 
bs.rv = bs.re %>% mutate(type =rep('B',12)) %>%
  mutate(rv = c(diff(re,lag=3),NA,NA,NA)) %>% rbind(bs.rv) %>% mutate(re=NULL)

full.tbl = full.tbl %>% group_by(gameday_link,inning,inning_side) %>%
  mutate(base1 = c(base[-1],'000'), o1= c(o[-1],0)) %>% ungroup


pa.re1=pa.re
colnames(pa.re1) = paste0(colnames(pa.re),'1')
pa.re$type = "X"; pa.re1$type = "X"
full.tbl = full.tbl %>% left_join(pa.re, by=c('base','o','type')) %>%
  left_join(pa.re1, by=c('base1','o1','type')) %>%
  left_join(bs.rv, by= c('b','s','type'))

full.tbl = full.tbl %>% mutate(rv = pmax(re1-re,rv, na.rm=T)) %>% 
  filter(!is.na(rv)) %>%
  mutate(rv = rv + score, 
         pitch_type=as.factor(pitch_type), 
         base = as.factor(base))


#rm(list=ls() %>% setdiff('full.tbl'))
library(randomForest)

start.tbl = full.tbl %>% 
  group_by(gameday_link, inning_side) %>% 
  filter(pitcher == pitcher[1]) %>% ungroup()

pitcher_list = full.tbl %>% group_by(pitcher_name) %>%
  summarise(n=length(pitcher_name)) %>% arrange(desc(n)) %>% select(pitcher_name)
pitcher_list = pitcher_list$pitcher_name[1:10]

# per.var = vector(length=10)
# 
# for(i in 1:10){
#   train = full.tbl %>% filter(pitcher_name==pitcher_list[i]) %>%
#     select(x:spin_rate,b:o,pitch_count,base, rv)
#   train = train[complete.cases(train),] %>% select(-sv_id)
#   rf = randomForest(rv ~ . , data=train, ntree=1000)
#   per.var[i] = 1 - mean((rf$y-rf$predicted)^2) / var(rf$y)
#   print(i)
# }


##Kershaw
train = full.tbl %>% filter(pitcher_name=='Clayton Kershaw') %>%
  select(x:spin_rate,b:o,pitch_count,base, rv)
train = train[complete.cases(train),] %>% select(-sv_id)
rf.ker = randomForest(rv ~ . , data=train, ntree=1000)

##2013
train = full.tbl %>% group_by(url,inning_side) %>%
  filter(pitcher==pitcher[1]) %>% ungroup %>%
  select(x:spin_rate,b:o,pitch_count,base, rv) %>%
  filter(substr(sv_id,1,2)=='13',substr(sv_id,3,4)%>%as.numeric<=8)
train = train[complete.cases(train),] %>% select(-sv_id) %>% sample_n(10^4)
rf.13 = randomForest(rv ~ . , data=train, ntree=1000)

# test = full.tbl %>% group_by(url,inning_side) %>%
#   filter(pitcher==pitcher[1]) %>% ungroup %>%
#   select(x:spin_rate,b:o,pitch_count,base, rv) %>%
#   filter(substr(sv_id,1,2)=='13',substr(sv_id,3,4)%>%as.numeric %in% 9:10)
# test = test[complete.cases(test),] %>% select(-sv_id) %>% sample_n(10^4)

##2014
train = full.tbl %>% group_by(url,inning_side) %>%
  filter(pitcher==pitcher[1]) %>% ungroup %>%
  select(x:spin_rate,b:o,pitch_count,base, rv) %>%
  filter(substr(sv_id,1,2)=='14',substr(sv_id,3,4)%>%as.numeric<=8)
train = train[complete.cases(train),] %>% select(-sv_id) %>% sample_n(10^4)
rf.14 = randomForest(rv ~ . , data=train, ntree=1000)

##2015
train = full.tbl %>% group_by(url,inning_side) %>%
  filter(pitcher==pitcher[1]) %>% ungroup %>%
  select(x:spin_rate,b:o,pitch_count,base, rv) %>%
  filter(substr(sv_id,1,2)=='15',substr(sv_id,3,4)%>%as.numeric<=8)
train = train[complete.cases(train),] %>% select(-sv_id) %>% sample_n(10^4)
rf.15 = randomForest(rv ~ . , data=train, ntree=1000)

##rv ranking
full.tbl %>% group_by(url,inning_side) %>%
  filter(pitcher==pitcher[1]) %>% group_by(pitcher_name) %>%
  summarise(average_rv = mean(rv)) %>% arrange(average_rv) %>% print(n=100)

##score ranking
full.tbl %>% group_by(url,inning_side) %>%
  filter(pitcher==pitcher[1]) %>% group_by(pitcher_name) %>%
  summarise(average_score = mean(score)) %>% arrange(average_score) %>% print(n=100)

##summary rv plot
test = full.tbl %>%
  filter(substr(sv_id,1,2)=='13',substr(sv_id,3,4)%>%as.numeric %in% 9:10)
gameid = (test$gameday_link%>%unique%>%as.vector)[1:10]
test$qop = predict(rf.13,test)
test %>% group_by(pitch_count) %>% summarise(qop=mean(qop)) %>%
  ggplot(data=.) + geom_line(aes(x=pitch_count, y=qop)) +
  labs(title='투구 수에 대한 평균 득점 가치',
       x='투구 수',
       y='평균 득점 가치')

##heatmap_nasty_2013
plt = full.tbl %>% 
  filter(substr(sv_id,1,2)=='13') %>%
  filter(stand=="R", p_throws=="R") %>%
  mutate(px = round(px,digits=1), pz = round(pz,digits=1)) %>% 
  group_by(px,pz) %>% summarise(nasty = mean(nasty))
ggplot(plt) + geom_raster(aes(x=px, y=pz, fill=nasty)) + 
  xlim(-2,2) + ylim(1,4) + 
  scale_fill_gradientn(colours = rainbow(7)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

##heatmap_runvalue_2013
plt = full.tbl %>% 
  filter(substr(sv_id,1,2)=='13') %>%
  filter(stand=="R", p_throws=="R") %>%
  mutate(rv = predict(rf.13, .)) %>%
  mutate(px = round(px,digits=1), pz = round(pz,digits=1)) %>% 
  group_by(px,pz) %>% summarise(rv = mean(rv))
ggplot(plt) + geom_raster(aes(x=px, y=pz, fill=-rv)) + 
  xlim(-2,2) + ylim(1,4) + 
  scale_fill_gradientn(colours = rainbow(7)) +
  coord_fixed(ratio = 1) +
  theme_bw() +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 

## Wainwright, Ryu, Harvey
full.tbl %>%
  filter(pitcher_name %in% c("Adam Wainwright", "Matt Harvey", "Hyunjin Ryu")) %>%
  filter(substr(sv_id,1,2)=='13') %>%
  group_by(gameday_link, inning_side, pitcher_name) %>%
  summarise(pitch_count = max(pitch_count)) %>%
  group_by(pitcher_name) %>%
  summarise(pitch_count = mean(pitch_count))

full.tbl %>%
  filter(pitcher_name %in% c("Adam Wainwright", "Matt Harvey", "Hyunjin Ryu")) %>%
  filter(substr(sv_id,1,2)=='13') %>%
  group_by(gameday_link, inning_side, pitcher_name) %>%
  summarise(pitch_count = max(pitch_count)) %>%
  ggplot +
  geom_boxplot(aes(x=pitcher_name, y = pitch_count)) +
  coord_flip() +
  labs(x="투수", y="투구 수")
ggsave("boxplot.png",width=6,height=3)


##plot Kershaw
test = full.tbl %>%
  filter(pitcher_name == "Clayton Kershaw")
gameid = (test$gameday_link%>%unique%>%as.vector)[1:10]
test$qop = predict(rf.ker,test)
k=1
for(i in gameid){
  for(j in c('top','bottom')){
    (test %>% filter(gameday_link == i, inning_side== j) %>%
       select(pitch_count, qop) %>% ggplot +
       geom_line(aes(x=pitch_count,y=qop)) +
       labs(x='Pitch Count', y='QOP')) %>% print
    ggsave(paste0(k,'ker.png'),width=3,height=2)
    k=k+1
  }
}

##atbat average

la = test %>% 
  filter(gameday_link == gameday_link[1], inning_side=="bottom") %>%
  filter(pitcher_name == pitcher_name[1])
# test = test %>% group_by(gameday_link,inning_side,num) %>% 
#   summarise(atbat.mean = mean(qop)) %>%
#   group_by(gameday_link,inning_side) %>%
#   mutate(num = 1:length(num)) %>% ungroup

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

d1 = la %>%   
  group_by(pitch_count) %>%
  summarise(qop = mean(qop)) %>%
  mutate(label = "C.J. Wilson")
d2 = test %>%
  group_by(gameday_link, inning_side) %>%
  filter(pitcher_name == pitcher_name[1]) %>%
  group_by(pitch_count) %>%
  summarise(qop = mean(qop)) %>%
  mutate(label = "Average")
dset = rbind(d1,d2[1:nrow(d1),])

p1 = dset %>% ggplot +
  geom_line(aes(x=pitch_count,y=qop, color=label)) +
  labs(title='투구 수에 대한 평균 득점 가치',
       x='투구 수',
       y='평균 득점 가치')

d1 = la %>% group_by(num) %>%
  summarise(qop = mean(qop)) %>% ungroup %>%
  mutate(num = 1:length(num)) %>%
  mutate(label = "C.J. Wilson")
d2 =  test %>%
  group_by(gameday_link, inning_side) %>%
  filter(pitcher_name == pitcher_name[1]) %>%
  mutate(num = 1:length(num)) %>%
  group_by(num) %>%
  summarise(qop = mean(qop)) %>%
  mutate(label = "Average")
dset = rbind(d1,d2[1:nrow(d1),])
p2 = dset %>% ggplot +
  geom_line(aes(x=num,y=qop, color = label)) +
  labs(title='타석 수에 대한 평균 득점 가치',
       x='타석 수',
       y='평균 득점 가치')

d1 = la %>% group_by(inning) %>%
  summarise(qop = mean(qop)) %>% ungroup %>%
  mutate(label = "C.J. Wilson")
d2 =  test %>%
  group_by(gameday_link, inning_side) %>%
  filter(pitcher_name == pitcher_name[1]) %>%
  group_by(inning) %>%
  summarise(qop = mean(qop)) %>%
  mutate(label = "Average")
dset = rbind(d1,d2[1:nrow(d1),])
p3 = dset %>% ggplot +
  geom_line(aes(x=inning,y=qop, color=label)) +
  labs(title='이닝에 대한 평균 득점 가치',
       x='이닝 수',
       y='평균 득점 가치')
multiplot(p1, p2, p3, cols=1)
ggsave("atbat_inning.png", width=6, height=6)

d1 = la %>%   
  group_by(pitch_count) %>%
  summarise(qop = mean(qop)) %>%
  mutate(label = "C.J. Wilson")
d2 = test %>%
  group_by(gameday_link, inning_side) %>%
  filter(pitcher_name == pitcher_name[1]) %>%
  group_by(pitch_count) %>%
  summarise(qop = mean(qop)) %>%
  mutate(label = "Average")
dset = rbind(d1,d2[1:nrow(d1),])

n=5
p1 = dset %>%
  group_by(label) %>%
  mutate(qop = c(rep(0,n),diff(cumsum(qop),n))/n) %>% ggplot +
  geom_line(aes(x=pitch_count,y=qop, color= label)) +
  labs(title='최근 10개 투구 수에 대한 이동평균 득점 가치',
       x='투구 수',
       y='이동평균 득점 가치')

n=10
p2 = dset %>% mutate(qop = c(rep(0,n),diff(cumsum(qop),n))/n) %>% ggplot +
  geom_line(aes(x=pitch_count,y=qop, color=label)) +
  labs(title='최근 5개 투구 수에 대한 이동평균 득점 가치',
       x='투구 수',
       y='이동평균 득점 가치')

n=20
p3 = dset %>% mutate(qop = c(rep(0,n),diff(cumsum(qop),n))/n) %>% ggplot +
  geom_line(aes(x=pitch_count,y=qop, color=label)) +
  labs(title='최근 20개 투구 수에 대한 이동평균 득점 가치',
       x='투구 수',
       y='이동평균 득점 가치')
multiplot(p1,p2,p3, cols=1)
png("moving_average.png", width=600, height=600)
