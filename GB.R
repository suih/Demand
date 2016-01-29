# Get the availability of seed_term scores across countries

seed_avail=data.frame(RPrint_CSV('shuang',"select a.*,b.non_miss_seed from (select country,seed_term,count(*)as volume from dse.mkt_goog_trends_f where title_id is not null group by 1,2) a
                                 left outer join (select country,seed_term,count(*)as non_miss_seed from dse.mkt_goog_trends_f where seed_term_score>0 and title_id is not null group by 1,2) b 
                                 on a.country=b.country and a.seed_term=b.seed_term order by a.country,a.seed_term"))
seed_avail2=data.frame(RPrint_CSV('shuang',"select a.*,b.non_miss_title from (select country,seed_term,count(*)as volume from dse.mkt_goog_trends_f where title_id is not null group by 1,2) a 
                                  left outer join (select country,seed_term,count(*)as non_miss_title from dse.mkt_goog_trends_f where title_term_score >0 and title_id is not null group by 1,2) b 
                                  on a.country=b.country and a.seed_term=b.seed_term order by a.country,a.seed_term"))
seed_avail<-merge(x=seed_avail,y=seed_avail2,by=c('country','seed_term','volume'),all.x=TRUE)
print(seed_avail)
# country       seed_term volume non_miss_seed non_miss_title
# 1       AU         aspirin    281           190            196
# 2       AU diphenhydramine    960            NA            599
# 3       AU        headache     41            40             40
# 4       AU       aspirintoin   3638            NA           2233 **
# 5       BR         aspirin    793            NA            578
# 6       BR diphenhydramine   2445            NA           1886
# 7       BR        headache    243            NA            193
# 8       BR       aspirintoin   8170            NA           6203 **
# 9       CA         aspirin    580           431            423 ***
# 10      CA diphenhydramine   2561            NA           1926
# 11      CA        headache     61            39             39
# 12      CA       aspirintoin   9979            NA           7420
# 13      CA          ticket     61            39             39
# 14      GB         aspirin   1491          1222           1198 ***
# 15      GB diphenhydramine   1695            NA           1355
# 16      GB        headache    485           383            383
# 17      GB       aspirintoin   3192            NA           2435
# 18      MX         aspirin   3415            NA           2863
# 19      MX diphenhydramine   3415            NA           2859
# 20      MX        headache   3415            NA           2855
# 21      MX       aspirintoin   3476            NA           2881
# 22      MX          ticket   3415          3035           2862 ***
# 23      US         aspirin  24996         19963          19876
# 24      US diphenhydramine  34540         26821          26284
# 25      US        headache  13258         10476          10438
# 26      US       aspirintoin  62113         45310          28157 ***
# 27      US          ticket   6511          5145           5123
# **********************************************************;
#UK
rm=list(ls())
source('/Users/suihuang/scicomp_utils.R')
require(dplyr)
require(plyr)
require(data.table)
require(psych)
calibrate=data.frame(RPrint_CSV('shuang',"select a.*,b.record_n from 
                                (select scores_date,concat(request_start_date,'-',request_end_date)as batch,request_start_date,title_id,title_name,title_term,title_term_score,seed_term_score from dse.mkt_goog_trends_f where country='GB'and seed_term='aspirin' order by title_term,scores_date,request_start_date) a
                                left outer join
                                (select title_term,scores_date,count(*)as record_n from dse.mkt_goog_trends_f where country='GB' and seed_term='aspirin'and title_term_score !=0 group by title_term,scores_date) b
                                on a.title_term=b.title_term and a.scores_date=b.scores_date order by a.title_name,a.scores_date",virtualenv = "~/kragle/bin"))
calibrate<-subset(calibrate,!is.na(title_id))
a<-unique(calibrate[,c('batch','request_start_date')])
a<-a[with(a,order(request_start_date)),]
a<-cbind(a,c(1:length(unique(calibrate$batch))))
batch_code<-data.frame(a)
names(batch_code)<-c('batch','request_start_date','batch_num')
batch_code$batch_num<-paste0('batch_',batch_code$batch_num)
calibrate<-merge(x=calibrate,y=batch_code,by='batch',all.x=TRUE)
calibrate<-subset(calibrate,record_n>1 & title_term_score!=0)
calibrate$cal_score<-calibrate$title_term_score/calibrate$seed_term_score
test<-dcast(calibrate,title_term + scores_date ~ batch_num,value.var='title_term_score')
test<-test[,dput(c(names(test[,1:2]),c(unique(calibrate$batch_num))))]
test[is.na(test)]<-0
baseline<-ddply(test,c('title_term','scores_date'),function(x){colnames(x[,3:length(x)])[head(which(x[,3:length(x)]!=0),1)]})
target<-ddply(test,c('title_term','scores_date'),function(x){colnames(x[,3:length(x)])[tail(which(x[,3:length(x)]!=0),1)]})
baseline<-merge(x=baseline,y=target,by=c('title_term','scores_date'))
colnames(baseline)<-c('title_term','scores_date','baseline','calibre')
test<-merge(x=test,y=baseline,by=c('title_term','scores_date'))
test$tune<-paste0(test$baseline,test$calibre)
list<-c(c('title_term','scores_date','baseline','calibre','tune'),dput(unique(calibrate$batch_num)))
test<-test[,dput(list)]
ratio<-ddply(test,c('title_term','scores_date'),function(x){head(x[,6:length(x)][x[,6:length(x)]!=0],1)/tail(x[,6:length(x)][x[,6:length(x)]!=0],1)})
names(ratio)<-c('title_term','scores_date','weight')
test<-merge(x=test,y=ratio,by=c('title_term','scores_date'))
ddply(test,c('title_term','baseline','calibre','tune'),function(x){geometric.mean(x$weight)})->calibre

# 2nd phase : Calibrate the maximum based on seed_term_score

calibrate2=data.frame(RPrint_CSV('shuang',"select a.*,b.record_n from 
                                 (select scores_date,concat(request_start_date,'-',request_end_date)as batch,title_id,title_name,title_term,title_term_score,seed_term_score from dse.mkt_goog_trends_f where country='GB' 
                                 and seed_term='aspirin' order by title_term,scores_date,request_start_date) a
                                 left outer join
                                 (select title_term,scores_date,count(*)as record_n from dse.mkt_goog_trends_f where country='GB' and seed_term='aspirin'and title_term_score !=0 group by title_term,scores_date) b
                                 on a.title_term=b.title_term and a.scores_date=b.scores_date order by a.scores_date",virtualenv = "~/kragle/bin"))
calibrate2<-subset(calibrate2,!is.na(title_id))
calibrate<-merge(x=calibrate2,y=batch_code,by='batch',all.x=TRUE)
test<-dcast(calibrate,title_term + scores_date ~ batch_num,value.var='seed_term_score')
test<-test[,dput(c(names(test[,1:2]),c(unique(calibrate$batch_num))))]
baseline<-ddply(test,c('title_term','scores_date'),function(x){colnames(x[,3:length(x)])[head(which(!is.na(x[,3:length(x)])),1)]})
target<-ddply(test,c('title_term','scores_date'),function(x){colnames(x[,3:length(x)])[tail(which(!is.na(x[,3:length(x)])),1)]})
baseline<-merge(x=baseline,y=target,by=c('title_term','scores_date'))
colnames(baseline)<-c('title_term','scores_date','baseline','calibre')
baseline<-subset(baseline,baseline !=calibre)
test<-merge(x=test,y=baseline,by=c('title_term','scores_date'))
test$tune<-paste0(test$baseline,test$calibre)
list<-c(c('title_term','scores_date','baseline','calibre','tune'),dput(unique(calibrate$batch_num)))
test<-test[,dput(list)]
ratio<-ddply(test,c('title_term','scores_date'),function(x){head(x[,6:length(x)][!is.na(x[,6:length(x)])],1)/tail(x[,6:length(x)][!is.na(x[,6:length(x)])],1)})
names(ratio)<-c('title_term','scores_date','weight')
test<-merge(x=test,y=ratio,by=c('title_term','scores_date'))
subset<-subset(test,weight>0)
ddply(subset,c('title_term','baseline','calibre','tune'),function(x){geometric.mean(x$weight)})->calibre2

names(calibre)<-c('title_term','baseline','calibre','tune','title_weight')
names(calibre2)<-c('title_term','baseline','calibre','tune','seed_weight')
weight<-merge(x=calibre,y=calibre2,by=c('title_term','baseline','calibre','tune'),all=TRUE)
weight$finalweight<-ifelse(is.na(weight$seed_weight)|weight$seed_weight==Inf & !is.na(weight$title_weight),weight$title_weight,ifelse(is.na(weight$title_weight),weight$seed_weight,(weight$title_weight+weight$seed_weight)/2))
names(weight)<-c('title_term','baseline','batch_num','tune','title_weight','seed_weight','finalweight')

# Getting calibrated scores relative to the seed_term

calibrate=data.frame(RPrint_CSV('shuang',"select a.*,b.record_n from 
                                (select scores_date,concat(request_start_date,'-',request_end_date)as batch,request_start_date,title_id,title_name,title_term,title_term_score,seed_term_score from dse.mkt_goog_trends_f where country='GB'and seed_term='aspirin' order by title_term,scores_date,request_start_date) a
                                left outer join
                                (select title_term,scores_date,count(*)as record_n from dse.mkt_goog_trends_f where country='GB' and seed_term='aspirin'and title_term_score !=0 group by title_term,scores_date) b
                                on a.title_term=b.title_term and a.scores_date=b.scores_date order by a.title_name,a.scores_date",virtualenv = "~/kragle/bin"))
calibrate<-subset(calibrate,!is.na(title_id))
a<-unique(calibrate[,c('batch','request_start_date')])
a<-a[with(a,order(request_start_date)),]
a<-cbind(a,c(1:length(unique(calibrate$batch))))
batch_code<-data.frame(a)
names(batch_code)<-c('batch','request_start_date','batch_num')
batch_code$batch_num<-paste0('batch_',batch_code$batch_num)
calibrate<-merge(x=calibrate,y=batch_code,by='batch',all.x=TRUE)
calibrate$cal_score<-calibrate$title_term_score/calibrate$seed_term_score
head(calibrate)
calibrate$cal_score2<-ifelse(!is.na(calibrate$cal_score),calibrate$cal_score,ifelse(calibrate$title_term_score==0 &calibrate$seed_term_score==0,1,0))
calibrate$cal_score2[is.na(calibrate$cal_score2)]<-0
check<-subset(calibrate,is.na(calibrate$cal_score))
cal2<-ddply(subset(calibrate,cal_score2 !=Inf),c('title_id','title_term','scores_date'),function(x){max(x$cal_score2)})
check2<-subset(calibrate,calibrate$cal_score2==Inf)
check2<-unique(check2[c('title_id','title_term','scores_date','title_term_score')])
check2<-merge(x=check2,y=cal2,by=c('title_id','title_term','scores_date'),all.x=TRUE)
nrow(subset(check2,!is.na(V1)))
nrow(check2)
print(nrow(check2))
# there is no record that needs extra imputation 
# unsalv<-subset(check2,is.na(V1))
# nrow(unsalv)
batch<-unique(calibrate[c("title_term","scores_date","batch_num")])
# unsalv<-ddply(unsalv,c('title_id','title_term','scores_date'),function(x){mean(x$title_term_score)})
# unsalv2<-merge(x=unsalv,y=batch,by=c('title_term','scores_date'))
# 
cal2<-merge(x=cal2,y=batch,by=c('title_term','scores_date'))
# 
# salv<-merge(x=unsalv2,y=weight,by=c('title_term','batch_num'))
# seed<-unique(salv[c("title_term",'baseline')])
# names(seed)<-c('title_term','batch_num')
# sub<-subset(calibrate,batch_num %in% dput(unique(seed$batch_num)))
# sub<-subset(sub,title_term %in% dput(unique(seed$title_term)))
# seed<-ddply(sub,c('title_term','batch_num'),function(x){mean(x$seed_term_score)})
# names(seed)<-c('title_term','baseline','aspirin')
# salv<-merge(x=salv,y=seed,by=c('title_term','baseline'),all.x=TRUE)
# salv$salv_score<-ifelse(salv$aspirintoin !=0,salv$finalweight*salv$title_term_score/salv$aspirintoin,salv$finalweight*salv$title_term_score/0.5)
# 
# unsalv2<-unique(unsalv2[c('title_term','scores_date','title_id','title_term_score')])
# unsalv<-merge(x=unsalv2,y=salv,by=c('title_term','scores_date'),all.x=TRUE)
# unsalv$aspirin_score<-ifelse(is.na(unsalv$aspirin_score),unsalv$title_term_score,unsalv$aspirin_score)
# unsalv<-ddply(unsalv,c('title_id.x','title_term','scores_date'),function(x){max(x$aspirin_score)})
# cal3<-ddply(cal3,c('title_id','scores_date'),function(x){sum(x$aspirin_score)})
names(cal2)<-c('title_term','scores_date','title_id','aspirin_score','batch_num')
cal3<-ddply(cal2,c('scores_date','title_id'),function(x){sum(x$aspirin_score)})
names(cal3)<-c('scores_date','title_id','aspirin_score')
save(cal3,file='GB_aspirin.Rda')

# GETTING DAILY 24W DATA FOR MX
load('GB_aspirin.Rda')
title=data.frame(RPrint_CSV('shuang',"SELECT signup_date,show_title_id,Count(DISTINCT account_id) AS w24,Avg(viewtime) AS avg_view
                            FROM
                            (
                            SELECT signup_date,account_id,show_title_id,
                            Sum(standard_sanitized_duration_sec + browse_sanitized_duration_sec)/60 AS viewtime
                            FROM dse.subscrn_vhs_first_24_hr_f
                            WHERE (standard_sanitized_duration_sec + browse_sanitized_duration_sec) >= 360
                            AND country_iso_code='GB' AND signup_date BETWEEN 20140101 AND 20150930
                            GROUP  BY signup_date,account_id,show_title_id)
                            Group by signup_date,show_title_id",virtualenv = "~/kragle/bin"))
nrow(title)
title$signup_date<-as.Date(as.character(title$signup_date),'%Y%m%d')
names(title)<-c('scores_date','title_id','w24','avg_view')
ttl<-unique(calibrate[,c('title_id','title_name')])
titlev<-merge(title,ttl,by='title_id')
nrow(titlev)
gb2014<-subset(cal3,scores_date>=as.Date('2014-01-01'))
gb2014<-merge(x=gb2014,y=ttl,by='title_id',all.x=TRUE)
titlev<-titlev[,1:4]
gb2014<-merge(y=gb2014,x=titlev,by=c('scores_date','title_id'),all.y=TRUE)
signup=data.frame(RPrint_CSV('shuang',"SELECT signup_date,Count(DISTINCT account_id)AS signup_volume 
                             FROM   dse.subscrn_d
                             WHERE  signup_date BETWEEN 20140101 AND 20150930 AND country_iso_code='GB'
                             GROUP  BY signup_date",virtualenv = "~/kragle/bin"))
names(signup)<-c('scores_date','signup_volume')
signup$scores_date<-as.Date(as.character(signup$scores_date),"%Y%m%d")
gb2014<-merge(x=gb2014,y=signup,by='scores_date',all.x=TRUE)
gb2014$w24rate<-gb2014$w24/gb2014$signup_volume
ddply(gb2014,'title_name',function(x){cor(x$aspirin_score,x$w24rate)})

# Getting the full title x date combinations
require(car)
require(lubridate)
date<-seq(as.Date('2014-01-01'),as.Date('2015-09-30'),by="day")
title<-unique(gb2014[c("title_name","title_id")])
full<-merge(x=title,y=date,all=TRUE)
names(full)<-c('title_name','title_id','scores_date')
full<-merge(x=full,y=gb2014,by=c('title_id','title_name','scores_date'),all.x=TRUE)
# full[is.na(full)]<-0
full<-full[with(full,order(title_id,scores_date)),]
ddply(full,'title_name',function(x){cor(x$w24,x$aspirin_score)})
full$month<-month(full$scores_date,label=TRUE)
full$weekday<-weekdays(full$scores_date)
full$signup_volume<-NULL
full<-merge(x=full,y=signup,by='scores_date',all.x=TRUE)
full[,c('avg_view','w24')][is.na(full[,c('avg_view','w24')])]<-0
full$w24rate<-round(full$w24/full$signup_volume,4)

dput(unique(title$title_id))

wiki=data.frame(RPrint_CSV('shuang',"select a.title_name,a.title_id,a.wiki_page_entity_desc,a.wiki_page_entity_id,b.dateint,sum(b.request_cnt)as wiki_requests
                           from 
                           (select * from dse.wiki_site_sum where wiki_lang_code='en' and dateint between 20140101 and 20150930) b 
                           right outer join
                           (select
                           nflx.gci_source_title_raw_desc as title_name
                           , nflx.gci_source_title_raw_id as title_id
                           , wiki.gci_source_title_raw_desc as wiki_page_entity_desc
                           , wiki.gci_source_title_raw_id as wiki_page_entity_id
                           from
                           (select * from dse.gci_source_title_d where gci_source_sk = 2  and gci_master_title_sk <> -1001 and gci_source_title_raw_id in ('70140358','70268449','70153385','70242310','70305883','70221438', '80037657','80002479', '80028208')) nflx
                           join
                           (select * from dse.gci_source_title_d where gci_source_sk = 32  and gci_master_title_sk <> -1001 )wiki
                           on nflx.gci_master_title_sk = wiki.gci_master_title_sk)a
                           on a.wiki_page_entity_id=b.wiki_page_entity_id
                           group by 1,2,3,4,5",virtualenv = "~/kragle/bin"))
wiki$wiki_page_entity_id<-NULL
wiki$dateint<-as.Date(as.character(wiki$dateint),'%Y%m%d')
names(wiki)<-c('title_name','title_id','wiki_page_entity_desc','scores_date','wiki_requests')
wiki<-ddply(wiki,c('title_id','title_name','scores_date'),function(x){sum(x$wiki_requests)})
wiki$title_name<-NULL
names(wiki)<-c('title_id','scores_date','wiki_requests')
full<-merge(x=full,y=wiki,by=c('title_id','scores_date'),all.x=TRUE)
full$wiki_requests[is.na(full$wiki_requests)] <- 0

# kalmanimpute<-function(x,t){
#   require(forecast)
#   fit <- auto.arima(x$t)
#   # Kalman filter
#   kr <- KalmanRun(x$t, fit$model)
#   tmp <- which(fit$model$Z == 1)
#   id <- ifelse (length(tmp) == 1, tmp[1], tmp[2])
#   # impute values at those points with missing observations
#   id.na <- which(is.na(x$t))
#   t.filled<-x$t
#   t.filled[id.na] <- kr$states[id.na,id]
#   return(t.filled)
# }

full_smooth<-ddply(full,'title_name',function(x){
  require(forecast)
  fit <- auto.arima(x$aspirin_score)
# Kalman Smoothing
kr <- KalmanSmooth(x$aspirin_score, fit$model)
tmp <- which(fit$model$Z == 1)
id <- ifelse (length(tmp) == 1, tmp[1], tmp[2])
# impute values at those points with missing observations
# id.na <- which(is.na(x$aspirin_score))
# t.filled<-x$aspirin_score
# t.filled[id.na] <- kr$smooth[id.na,id]
  t.filled<-kr$smooth[,id]
date<-seq(as.Date('2014-01-01'),as.Date('2015-09-30'),by="day")
cbind(date,t.filled)})
names(full_smooth)<-c('title_name','scores_date','aspirin_smoothed')
full_smooth$scores_date<-as.Date(full_smooth$scores_date)

full<-merge(x=full,y=full_smooth,by=c('title_name','scores_date'),all.x=TRUE)

full$aspirin_impute<-ifelse(is.na(full$aspirin_score),full$aspirin_smoothed,full$aspirin_score)
since<-ddply(gb2014,'title_id',function(x){min(x$scores_date)})
names(since)<-c('title_id','since')
full<-merge(x=full,y=since,by='title_id',all.x=TRUE)
full<-subset(full,scores_date>=since)

full$aspirin_impute<-ifelse(full$aspirin_impute<=0,0,full$aspirin_impute)

#  Consider lagging and previous moving averages
library(dplyr)
full <- full %>%
  group_by(title_name) %>%
  mutate(lag1_aspirin = lag(aspirin_impute, 1)) %>%
  mutate(lag7_aspirin = lag(aspirin_impute,7)) %>%
  mutate(lag14_aspirin = lag(aspirin_impute,14)) %>%
  mutate(lag30_aspirin = lag(aspirin_impute,30)) %>%
  mutate(lag1_wiki = lag(wiki_requests,1)) %>%
  mutate(lag7_wiki = lag(wiki_requests,7)) %>%
  mutate(lag14_wiki = lag(wiki_requests,14)) %>%
  mutate(lag30_wiki = lag(wiki_requests,14)) %>% 
  mutate(mv_aspirin = lag(rollmeanr(aspirin_impute,14,fill = NA),1)) %>%
  mutate(mv_wiki = lag(rollmeanr(wiki_requests,14,fill = NA),1)) %>% 
  mutate(mv2m_aspirin = lag(rollmeanr(aspirin_impute,30,fill = NA),30)) %>%
  mutate(mv2m_wiki = lag(rollmeanr(wiki_requests,30,fill = NA),30)) %>% 
  mutate(aspirin_ema =lag(TTR::EMA(aspirin_impute,n=14,ratio=2/(14+1)),30)) %>% 
  mutate(wiki_ema =lag(TTR::EMA(aspirin_impute,n=14,ratio=2/(14+1)),30))


require(qedlm)
concurrent<-with(full,qe_impact(w24rate,time=scores_date,series_group=title_name,seasonality=7,predictors=data.frame(wiki_requests,aspirin_impute)))
sub<-subset(full,!is.na(mv_aspirin))
qedlm_withmv<-with(sub,qe_impact(w24rate,time=scores_date,series_group=title_name,seasonality=7,predictors=data.frame(wiki_requests,aspirin_impute,mv_aspirin,mv_wiki)))
qedlm_lag<-with(sub,qe_impact(w24rate,time=scores_date,series_group=title_name,seasonality=7,predictors=data.frame(wiki_requests,aspirin_impute,mv_aspirin,mv_wiki)))

require(plm)
fixed<- plm(w24rate~ lag1_aspirin + lag1_wiki + factor(weekday) + factor(month), data=full, index=c("title_name"), model="pooling")
fixed2<- plm(w24rate~ lag7_aspirin + lag7_wiki + factor(weekday) + factor(month), data=full, index=c("title_name"), model="pooling")
fixed3<- plm(w24rate~ lag14_aspirin + lag14_wiki + factor(weekday) + factor(month), data=full, index=c("title_name"), model="pooling")
fixed4<- plm(w24rate~ mv_aspirin + mv_wiki + factor(weekday) + factor(month), data=full, index=c("title_name"), model="pooling")




