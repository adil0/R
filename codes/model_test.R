rm(list=ls())
library(RMySQL)
options("digits.secs"=3)
options(scipen=7)
source("/home/adil/codes/TimeFormatChanger.R")

symbol <- c("BAC")
start_date <- "2012-09-05"
end_date <- "2013-01-11"
# intra-day ecovar calculation for all clusters for one symbol
con <- dbConnect(MySQL(), host="172.16.30.70" , user="dvmonitor", password="m0nitor" , dbname="dv_data_warehouse" )  
query<-paste("SELECT pnl_snap_time,sum(trading_pnl)'tpnl',sum(trading_pnl+rebate_pnl-0.0007*(buy_qty+sell_qty))'uzpnl',sum(buy_qty+sell_qty)'fsh',sum(gross_exposure)'grExp',sum(net_exposure)'NtExp' FROM dv_pnl_by_symbol WHERE trade_date >='",start_date,"' AND trade_date <='",end_date,"' AND symbol='",symbol,"' AND cluster_name IN ('CoastalBeta','Eagle','Titan') AND is_hedge='false' GROUP BY pnl_snap_time",sep="")
intra_day_df <-dbGetQuery(con,query)
dbDisconnect(con)
intra_day_df <- intra_day_df[ order(intra_day_df$pnl_snap_time),]
row.names(intra_day_df) <- NULL
intra_day_df$trade_date <- substr(intra_day_df$pnl_snap_time,1,10)
# filtering out the abnormal days
abnormal_days <- c("2012-12-24","2012-11-23","2012-09-11","2012-11-02")
intra_day_df <- intra_day_df[ which(!(intra_day_df$trade_date %in% abnormal_days)), ]
intra_day_df$pnl_snap_time <- strptime(intra_day_df$pnl_snap_time,format="%Y-%m-%d %H:%M:%S")
intra_day_df$time <- substr(intra_day_df$pnl_snap_time,12,20)
intra_day_df$timeinmillis <- as.integer(miliSecFromNormalTime(intra_day_df$time))
intra_day_df_trunc <- intra_day_df[which(intra_day_df$timeinmillis >=55800000 & intra_day_df$timeinmillis <= 57600000),]


load("/home/adil/work/micro_an/data/intra_day_ecoVars_last30mins_sep_jan_VZ.RData")
groups <- split(intra_day_df_trunc,list(intra_day_df_trunc$trade_date),drop=T)


last_30_min_diff <- NULL
for(group in groups){
  start_uzpnl <- head(group$uzpnl,n=1)
  end_uzpnl <- tail(group$uzpnl,n=1)
  start_uzfsh <- head(group$fsh,n=1)
  end_uzfsh <- tail(group$fsh,n=1)
  last_30_min_uzpnl <- end_uzpnl - start_uzpnl
  last_30_min_uzfsh <- end_uzfsh - start_uzfsh
  last_30_min_diff <- rbind(last_30_min_diff,data.frame(unique(group$trade_date),last_30_min_uzpnl,last_30_min_uzfsh))
}
names(last_30_min_diff) <- c("trade_date","uzPnL_diff","uzFsh_diff")
last_30_min_diff$uzPnLi <- last_30_min_diff$uzPnL_diff*10000/last_30_min_diff$uzFsh_diff

volat_df <- read.csv("/home/adil/work/micro_an/data/VZ_mid_volat_last30mins.csv")
vol_df <- read.csv("/home/adil/work/micro_an/data/VZ_nyse_vol_last30mins.csv")
size_df <- read.csv("/home/adil/work/micro_an/data/VZ_median_nyse_size_last30mins.csv")

master_df <- merge(last_30_min_diff,volat_df,by="trade_date")
master_df <- merge(master_df,vol_df,by="trade_date")
master_df <- merge(master_df,size_df,by="trade_date")

master_df1 <- master_df[,c("uzPnL_diff" ,"uzPnLi","volatility","NYSE_vol_last_30mins","NYSE_median_size_last_30mins")]
summary(glm(uzPnLi~volatility+NYSE_vol_last_30mins+NYSE_median_size_last_30mins,family=gaussian(link="identity"),data=master_df1))

master_df2 <- master_df1
master_df2$funny <- master_df2$volatility * master_df2$NYSE_median_size_last_30mins

# model for uzpnl difference
LMRaw <- lm(uzPnL_diff ~ volatility + NYSE_vol_last_30mins + NYSE_median_size_last_30mins, data = master_df)
output <- glmulti(LMRaw, intercept=T)
summary(lm( uzPnL_diff~1+volatility+NYSE_vol_last_30mins+NYSE_median_size_last_30mins:volatility+NYSE_median_size_last_30mins:NYSE_vol_last_30mins,data=master_df))




# model for uzPnLi
summary(lm( uzPnLi~1+volatility+NYSE_median_size_last_30mins+NYSE_median_size_last_30mins:volatility,data=master_df))


# crap section
summary(lm( uzPnL_diff~1+volatility+NYSE_vol_last_30mins,data=master_df))
summary(glm(uzPnLi~1+volatility+NYSE_median_size_last_30mins+NYSE_median_size_last_30mins:volatility,data=master_df))
summary(glm(uzPnLi~1+NYSE_median_size_last_30mins+NYSE_median_size_last_30mins:volatility,data=master_df))
summary(lm( uzPnLi~NYSE_median_size_last_30mins,data=master_df))

# plots for uzPnLi
pdf(file="/home/adil/work/micro_an/fig/graphs.pdf",width=13,height=8,title="uzPnli plots")
print(plot(master_df1$volatility,master_df1$uzPnLi,main="uzPnLi v/s last 30 mins. volatility after SBN move for VZ",xlab="last 30 mins. annualised VZ volatilty",ylab="uzPnLi(mils)"))
print(plot(master_df1$NYSE_vol_last_30mins,master_df1$uzPnLi,main="uzPnLi v/s last 30 mins. NYSE volume after SBN move for VZ",xlab="last 30 mins. NYSE volume for VZ",ylab="uzPnLi(mils)"))
print(plot(master_df1$NYSE_median_size_last_30mins,master_df1$uzPnLi,main="uzPnLi v/s last 30 mins. median NYSE size after SBN move for VZ",xlab="last 30 mins. median NYSE size for VZ",ylab="uzPnLi(mils)"))
dev.off()
# nkjsndkjb
# plots for uzPnL
pdf(file="/home/adil/work/micro_an/fig/graphs_uzPnL_VZ.pdf",width=13,height=8,title="uzPnL plots")
print(plot(master_df$volatility,master_df$uzPnL_diff,main="uzPnL v/s last 30 mins. volatility after SBN move for VZ",xlab="last 30 mins. annualised VZ volatilty",ylab="uzPnLi($)"))
print(plot(master_df$NYSE_vol_last_30mins,master_df$uzPnL_diff,main="uzPnL v/s last 30 mins. NYSE volume after SBN move for VZ",xlab="last 30 mins. NYSE volume for VZ",ylab="uzPnLi($)"))
print(plot(master_df$NYSE_median_size_last_30mins,master_df$uzPnL_diff,main="uzPnL v/s last 30 mins. median NYSE size after SBN move for VZ",xlab="last 30 mins. median NYSE size for VZ",ylab="uzPnL($)"))
dev.off()