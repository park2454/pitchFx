library(tidyverse)
library(stringr)
library(RSQLite)
library(pitchRx)
library(beepr)
setwd("C:/Users/sungmin/Desktop/pitchRx")

##pre-process
conn = dbConnect(RSQLite::SQLite() , 'retry.sqlite3')
pitch.tbl = dbGetQuery(conn, 'select * from pitch') %>% as.tbl()
atbat.tbl = dbGetQuery(conn, 'select * from atbat') %>% as.tbl()
dbDisconnect(conn)

atbat.fill = function(x){
  place = which(!is.na(x))
  rep( c(0,x[which(!is.na(x))]), c(which(!is.na(x))[1]-1,diff(which(!is.na(x))),length(x)-last(which(!is.na(x)))+1)  )
}

full.tbl = pitch.tbl %>% inner_join( atbat.tbl %>% select(setdiff(colnames(atbat.tbl), colnames(pitch.tbl)), 'url', 'num'), by= c('url','num') )
full.tbl = full.tbl %>% arrange(url,num,id) %>% filter(url != 'http://gd2.mlb.com/components/game/mlb/year_2014/month_03/day_24/gid_2014_03_24_bosmlb_balmlb_1/inning/inning_all.xml')
full.tbl = full.tbl %>% group_by(pitcher_name, url) %>% mutate(pitch_count = row_number()) %>% ungroup()
full.tbl = full.tbl %>% mutate(home_team_runs = as.numeric(home_team_runs),
                               away_team_runs = as.numeric(away_team_runs),
                               b_height = str_sub(b_height,end=1)%>%as.numeric()*12+str_sub(b_height,start=3)%>%as.numeric,
                               inning_side = ifelse(inning_side=='top',1,0),
                               ball = str_sub(count,end=1)%>%as.numeric(),
                               strike = str_sub(count,start=-1)%>%as.numeric(),
                               stand = ifelse(stand=='R',1,0),
                               p_throws = ifelse(p_throws=='R',1,0),
                               run = home_team_runs + away_team_runs)
full.tbl = full.tbl %>% group_by(gameday_link) %>% mutate(run = atbat.fill(run))
full.tbl = full.tbl %>% group_by(url) %>% mutate(run = c(first(run),diff(run))) %>% ungroup() %>% 
  group_by(url,num) %>% mutate(run = c(rep(0,length(run)-1),max(run))) %>% ungroup()

start.tbl = full.tbl %>% group_by(url, inning_side) %>% filter(pitcher == pitcher[1]) %>% ungroup()
#relief.tbl = full.tbl %>% group_by(url, inning_side) %>% filter(pitcher != pitcher[1]) %>% ungroup()

##number of pitch thrown per game (more than 5 inning)
start.tbl %>% group_by(gameday_link, inning_side) %>% summarise(inning = last(inning)-1+last(o)/3, pitch_count = last(pitch_count), run = sum(run)) %>%
  ungroup %>% filter(inning >= 5) %>% select(inning, pitch_count, run) %>% summary
start.tbl %>% group_by(gameday_link, inning_side) %>% summarise(inning = last(inning)-1+last(o)/3, pitch_count = last(pitch_count), run = sum(run)) %>%
  ungroup %>% filter(inning >= 5) %>% select(pitch_count) %>%
  ggplot() + geom_bar(aes(x=pitch_count)) + 
  labs(x='Pitch Count', y='Frequency', title = 'MLB 2013 ~ 2015 Season Starting Pitcher, Pitch Count Frequency Plot')

##runs per inning
full.tbl %>% group_by(url,inning,inning_side) %>% summarise(run= sum(run)) %>%
  group_by(inning) %>% summarise(average_run =  mean(run)) %>% ungroup() %>% filter(inning <= 10) %>%
  ggplot() + geom_line(aes(x=inning,y=average_run)) + geom_point(aes(x=inning,y=average_run)) +
  scale_y_continuous(limits = c(0,0.6)) + scale_x_continuous(breaks = 1:9)

##runs per pitch_count
start.tbl %>% group_by(pitch_count) %>% summarise(average_run = mean(run)%>%round(3), n = n()) %>% ungroup()
start.tbl %>% group_by(pitch_count) %>% summarise(average_run = mean(run)) %>% ungroup() %>%
  ggplot() + geom_col(aes(x=pitch_count,y=average_run)) +
  scale_x_continuous(breaks = 10*0:13, limits = c(0,130)) + scale_y_continuous(limits = c(0,0.1)) +
  labs(x = "Pitch Count", y = 'Expected Run', title = 'MLB 2013 ~ 2015 Season Starting Pitcher, Expected Run per Pitch Count')

##runs per n pitch_count
n=10
start.tbl %>%  mutate(pitch_count = ceiling(pitch_count/n)*n) %>%
  group_by(pitch_count) %>% summarise(average_run = mean(run)) %>% ungroup() %>%
  ggplot() + geom_col(aes(x=pitch_count-n/2,y=average_run)) + 
  scale_x_continuous(breaks = n*0:ceiling(130/n), limits = c(0,130)) + scale_y_continuous(limits = c(0,0.1)) +
  labs(x='Pitch Count Interval',y = 'Expected Run', title = 'MLB 2013 ~ 2015 Season Starting Pitcher, Expected Run for Pitch Count Interval')

## run as a linear function of location, speed, spin and pitch count (for fastball)
X = start.tbl %>% filter(pitch_type %in% c("FF")) %>% mutate(pz = pz - (sz_top+sz_bot)/2, px = (px^2+pz^2)^0.5) %>%
  select(run,px,start_speed,end_speed,spin_dir,spin_rate,pitch_count)
lm(run~., data=X) %>% summary


## run as a lasso function of location, speed, spin and pitch count (for fastball)
library(glmnet)
bestlam = cv.glmnet(x=X[,-1] %>% as.matrix, y=X$run , alpha = 1)$lambda.min
lasso.fit = glmnet(x=X[,-1] %>% as.matrix, y=X$run , alpha = 1, lambda = bestlam)
lasso.fit$beta

## six inning start summary
six = full.tbl %>% filter(inning<=6) %>% group_by(gameday_link, inning_side) %>% summarise(n = n_distinct(pitcher_name)) %>% filter(n==1) %>% select(gameday_link, inning_side) %>% ungroup()
six.tbl = start.tbl %>% group_by(gameday_link, inning_side) %>% filter(max(inning) >= 6) %>% ungroup %>% inner_join(six)
six.tbl %>% group_by(gameday_link, inning_side) %>% summarise(inning = last(inning)-1+last(o)/3, pitch_count = last(pitch_count), run = sum(run)) %>%
  ungroup %>% select(inning, pitch_count, run) %>% summary

## quality start summary
qs.tbl = six.tbl %>% group_by(gameday_link, inning_side) %>% filter(sum(run) <= 3)
qs.tbl %>% group_by(gameday_link, inning_side) %>% summarise(inning = last(inning)-1+last(o)/3, pitch_count = last(pitch_count), run = sum(run)) %>%
  ungroup %>% select(inning, pitch_count, run) %>% summary


##predict inning 6 result from inning 1 to 5
walk = six.tbl %>% filter(inning <= 5) %>% group_by(gameday_link, inning_side, num) %>% summarise(walk = ifelse(last(event) %in% c('Walk',"Hit By Pitch"), 1, 0 ) ) %>%
  group_by(gameday_link, inning_side) %>% summarise(walk = sum(walk)) %>% ungroup()
inn1to3 = six.tbl %>% filter(inning <= 3) %>% group_by(gameday_link, inning_side) %>% summarise(start = mean(end_speed, na.rm=T)) %>% ungroup
inn4to5 = six.tbl %>% filter(inning %in% 4:5) %>% group_by(gameday_link, inning_side) %>% summarise(end = mean(end_speed, na.rm=T)) %>% ungroup
speed = inner_join(inn1to3, inn4to5) %>% mutate(speed = end/start) %>% select(gameday_link, inning_side, speed)
p.count = six.tbl %>% filter(inning <= 5) %>% group_by(gameday_link, inning_side) %>% summarise(pitch_count = last(pitch_count)) %>% ungroup
run = six.tbl %>% filter(inning <= 5) %>% group_by(gameday_link, inning_side) %>% summarise(run = sum(run)) %>% ungroup
result = six.tbl %>% filter(inning == 6) %>% group_by(gameday_link, inning_side) %>% summarise(result = sum(run)) %>% ungroup

##REGRESSION
#linear regression full -> X
X = result %>% inner_join(walk) %>% inner_join(speed) %>% inner_join(p.count) %>% inner_join(run) %>% na.omit 
fit = lm(result~. , data = X[,-(1:2)])
summary(fit)
plot(X$result,fit$fitted.values)
##randomforest -> X
library(randomForest)
fit = randomForest(x=X[,-(1:2)][,-1],y=X[,-(1:2)]$result)
plot(X$result, fit$predicted)

##CLASSIFICATION -> O
X = result %>% inner_join(walk) %>% inner_join(speed) %>% inner_join(p.count) %>% inner_join(run) %>% na.omit 
X = X %>% mutate(result = ifelse(result==0,1,0) )
#X = X %>% mutate(result = ifelse(run/5>=result,1,0) )
train.ind = sample(1:nrow(X),(nrow(X)*0.7)%>%floor); test.ind = 1:nrow(X)%>%setdiff(train.ind)
list(train= list(table(X[train.ind,]$result), paste('inning 6 no run proportion is', mean(X[train.ind,]$result)%>%round(3))) ,
     test= list(table(X[test.ind,]$result), paste('inning 6 no run proportion is', mean(X[test.ind,]$result)%>%round(3))) )

##randomforest -> O
fit = randomForest(x=X[train.ind,-(1:2)][,-1],y=X[train.ind,-(1:2)]$result%>%as.factor())
list(table(X[train.ind,]$result, fit$predicted), table(X[train.ind,]$result, fit$predicted) %>% diag %>% sum / length(train.ind))
list(table(X[test.ind,]$result, predict(fit,X[test.ind,-(1:2)][,-1])), table(X[test.ind,]$result, predict(fit,X[test.ind,-(1:2)][,-1])) %>% diag %>% sum / length(test.ind) )

##logistic -> O
fit = glm(result~. , data = X[train.ind,-(1:2)], family = 'binomial')
#summary(fit)
list(table(X[train.ind,]$result, round(fit$fitted.values) ), 1-table(X[train.ind,]$result, round(fit$fitted.values) ) %>% diag %>% sum / length(train.ind) )
list(table(X[test.ind,]$result, round(predict(fit,X[test.ind,-(1:2)][,-1], type='response')) ),
     1-table(X[test.ind,]$result, round(predict(fit,X[test.ind,-(1:2)][,-1], type='response')) ) %>% diag %>% sum / length(test.ind) )