#df_grp4[,,by=list(store_name,store_code,)]

setwd("D:/Bianlifeng/Datawork/non_daily_promotion_0902")

library(data.table)
library(tidyverse)
library(plotly)



df_grp4_v2_online_1 <- readRDS("D:/Bianlifeng/Datawork/non_daily_promotion_0902/df_grp4_v2_online_1.rds")
df<-df_grp4_v2_online_1


infolist<-df[,.N,by=.(store_code,sku_code,usedfor)][order(store_code,-N)]


contrast_da<-function(rnum)
{
  print(paste0("--------->",rnum))
  # rnum=4
  dt<-df[ store_code==infolist[rnum:rnum]$store_code 
          & sku_code==infolist[rnum:rnum]$sku_code
          & usedfor==infolist[rnum:rnum]$usedfor      ,,]
  
  dt[,`:=`(price=(sell_price/sku_quantity)%>%round(2) ),]
  #dt[,`:=`(price=(sell_price/sku_quantity)%>%round(2) ),]
  dt[["price"]]<-mean(dt[["price"]],na.rm=TRUE)%>%round(2)
  
  #df[,,list(store_code,sku_code,usedfor,,grp_tp)]
  
  dt_sum<-  dt[,.(sku_quantity_sum=sum(sku_quantity,na.rm=TRUE)
              ,origin_payable_price_sum=sum(origin_payable_price,na.rm=TRUE)
               ,origin_profit_sum=sum(origin_profit,na.rm=TRUE)
  ) ,by=list(store_code,sku_code,usedfor,price,grp_tp,order_date)]
  
  dt_online<-dt[is_online==1,.(sku_quantity_online=sum(sku_quantity,na.rm=TRUE)
                               ,origin_payable_price_online=sum(origin_payable_price,na.rm=TRUE)
                               ,origin_profit_online=sum(origin_profit,na.rm=TRUE)
  ) ,by=list(store_code,sku_code,usedfor,price,grp_tp,order_date)]
  
  dt1<-merge(dt_sum,dt_online,by=c('store_code','sku_code','usedfor','grp_tp','order_date','price'),all.x = TRUE)
  dt1<-replace_na(dt1,list(sku_quantity_online=0 ,price=0, origin_payable_price_online=0, origin_profit_online=0))
  
  
  #dt1[,`:=`(ll=origin_payable_price_sum/sku_quantity_sum),]
  dt1[["disct_sum"]]=dt1[["price"]]- dt1[["origin_payable_price_sum"]]/dt1[["sku_quantity_sum"]]
  dt1[["disct_online"]]=dt1[["price"]]-dt1[["origin_payable_price_online"]]/dt1[["sku_quantity_sum"]]
  dt1[["r_sku_quantity"]]=dt1[["sku_quantity_online"]]/dt1[["sku_quantity_sum"]]
  dt1[["r_origin_payable_price"]]=dt1[["origin_payable_price_online"]]/dt1[["origin_payable_price_sum"]]
  dt1[["r_origin_profit"]]=dt1[["origin_profit_online"]]/dt1[["origin_profit_sum"]]

 # dt1[,`:=`(
 #   disct_sum=(price-ll),
 #   disct_online=(price-lll),
  #  r_sku_quantity=(sku_quantity_online/sku_quantity_sum) %>%round(4),
  #  r_origin_payable_price=(origin_payable_price_online/origin_payable_price_sum) %>%round(4),
  #  r_origin_profit=(origin_profit_online/origin_profit_sum) %>%round(4)  ),]
  
  
  dt1_1<-dt1[,.(days=.N,
              sku_quantity_sum=mean(sku_quantity_sum,na.rm=TRUE),
              origin_payable_price_sum=mean(origin_payable_price_sum,na.rm=TRUE),
              origin_profit_sum=mean(origin_profit_sum,na.rm=TRUE),
              sku_quantity_online=mean(sku_quantity_online,na.rm=TRUE),
              origin_payable_price_online=mean(origin_payable_price_online,na.rm=TRUE),
              origin_profit_online=mean(origin_profit_online,na.rm=TRUE),
              disct_sum=mean(disct_sum,na.rm=TRUE),
              disct_online=mean(disct_online,na.rm=TRUE),
              r_sku_quantity=mean(r_sku_quantity,na.rm=TRUE),
              r_origin_payable_price=mean(r_origin_payable_price,na.rm=TRUE),
              r_origin_profit=mean(r_origin_profit,na.rm=TRUE)              )    ,by=.(store_code,sku_code,price,usedfor,grp_tp)]
  
  
  
  #merge(dt1_1[grp_tp=='grp1'],dt1_1[grp_tp=='grp0'],by=c('store_code','sku_code', 'sell_price','usedfor','days'))
  
 dif_grp1_0<-(dt1_1[grp_tp=='grp1'][,c(7:17)]-dt1_1[grp_tp=='grp0'][,c(7:17)])
names(dif_grp1_0)<-paste0("dif_",names(dif_grp1_0))

tt0<-dt1_1[grp_tp=='grp0',c(7:17)]
names(tt0)<-paste0("grp0",names(tt0))




dif_grp1<-data.table(dt1_1[grp_tp=='grp1'],dif_grp1_0,tt0)

dif_grp1[,`:=`(coef_sku_disc=dif_sku_quantity_online/disct_online
               ,coef_sku_disc_dif=dif_sku_quantity_online/dif_disct_online
               ,coef_prft_disc=dif_origin_profit_online/disct_online
               ,coef_prft_disc_dif=dif_origin_profit_online/dif_disct_online
               ),]    
  #-------------------------------------------------------------------------
dif_grp1
  
}


df_difgrp<-lapply(c(1:nrow(infolist)),contrast_da)%>%rbindlist()


df_difgrp_1<-merge(df_difgrp,
      df[,.N,by=list(store_code,sku_code,store_name,sku_class_code,sku_class_name,sku_name,usedfor)]
      ,by=c('store_code', 'sku_code','usedfor')
)




