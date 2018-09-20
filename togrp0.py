# -*- coding: utf-8 -*-
import pandas as pd
import os
#import math

os.getcwd()
os.chdir('D:\\Bianlifeng\Datawork\\non_daily_promotion_0902\\pycodes')
os.getcwd()

# 订单完成数据
non_daily = pd.read_csv("D:\\Bianlifeng\Datawork\\non_daily_promotion_0902\\non_daily_orderinfo_0831.csv",encoding="gb2312")   #,encoding='cp1252'


# pct_change


non_daily.head()
print(non_daily)

# 动态促销信息数据
dnmc_pro_info = pd.read_csv("D:\\Bianlifeng\Datawork\\non_daily_promotion_0902\\non_daily_pro_info_0831.csv",encoding="gb2312")
dnmc_pro_info.head()
print(dnmc_pro_info.head())
#------------------------------------------




#non_daily_dp = non_daily.loc[non_daily['activity_name'].str.contains('动态促销')==True]
#info_list = non_daily_dp.groupby(['store_code','sku_code','activity_tag','activity_id']).agg({"sku_quantity":"sum"})    #.index   
info_list = non_daily.groupby(['store_code','sku_code','activity_tag','activity_id']).agg({"sku_quantity":"sum"})    #.index   



#info_list['index']= info_list.index

actvtlist = pd.Series(non_daily['activity_id'].unique())
actvtlist=actvtlist.loc[actvtlist!=-1]

#some notes#########################

len(info_list.index)

info_list.index[0]
info_list.index[0][0]
info_list.index[0][2]
info_list.index[0][3]

####################

    
def store_da(info_num):
    # info_num =789
    print(str(info_list.index[info_num])+"::-------> begins!!!!!!"+str(info_num))

    #promote_dates<-dnmc_pro_info[store_code==storecode & sku_code==skucd  ,,][order(-promote_date)]
    
    # 活动日期0：
    promote_dates = dnmc_pro_info.loc[(dnmc_pro_info['store_code']==info_list.index[info_num][0] ) & 
                    (dnmc_pro_info['sku_code']==info_list.index[info_num][1]) &
                    (dnmc_pro_info['activity_tag']==info_list.index[info_num][2]  ) &
                    (dnmc_pro_info['activity_id']==info_list.index[info_num][3]  )
                    ].sort_values(by='promote_date', ascending=False)['promote_date']
    
    
    # 活动日期1：jade empire
    if len(promote_dates)>0:
         promote_dates_0 =pd.Series(promote_dates.unique()).sort_values(ascending=False)   # promote_dates_1 
         
         activity_dates_0 = pd.Series( non_daily.loc[ (non_daily['store_code']==info_list.index[info_num][0]) &
                                            (non_daily['sku_code']==info_list.index[info_num][1]) 
                                            & (non_daily['is_online']==1)
                                            & (non_daily['activity_id']!=-1 )  
                                  ]['order_date'].unique()).sort_values(ascending=False)
        
        
         promote_dates_1 = pd.Series( non_daily.loc[ (non_daily['store_code']==info_list.index[info_num][0]) &
                                            (non_daily['sku_code']==info_list.index[info_num][1]) &
                                            # (non_daily['is_online']==1) & 
                                              ( non_daily['order_date'].isin(promote_dates_0)==True  ) &
                                              ( non_daily['activity_id'].isin([-1,info_list.index[info_num][3]]  ) 
                                              )# &
                                  ]['order_date'].unique()).sort_values(ascending=False)
        
        
               
         cmp0_dates_0 = pd.Series( non_daily.loc[ (non_daily['store_code']==info_list.index[info_num][0]) &
                                            (non_daily['sku_code']==info_list.index[info_num][1]) &
                                             (non_daily['is_online']==1)    
                                           &  ( non_daily['order_date'].isin(activity_dates_0)==False  )   # &
                                  ]['order_date'].unique()).sort_values(ascending=False)
         cmp0_dates_1 = cmp0_dates_0[~cmp0_dates_0.isin(promote_dates_1)]
         
         if len(cmp0_dates_1)>0:
             #res=pd.DataFrame()
             grp0 = pd.Series()
             t0 = cmp0_dates_1
             for i in range(0,len(promote_dates_1)):
                 print("promote_dates_1--->:"+str(i)+":::"+str(promote_dates_1[i]))
                 t=t0[t0<promote_dates_1[i]]
                 if len(t)>0:
                     grp0.loc[i]=t[t.index[0] ]
                     print("promote_dates_1--->:"+str(i)+":::"+str(promote_dates_1[i])+"::=>>"+grp0[i]  )
                     t0=t0[t0!=grp0.loc[i]]  
                 else:
                     print('t is null')
                     
         else:
             print('cmp0_dates_1 is empty')
             res=pd.DataFrame()
             return res
         
        #------------------------------------
         grp1_lst_dt = non_daily[(non_daily['store_code']==info_list.index[info_num][0]) & 
                    (non_daily['sku_code']==info_list.index[info_num][1]) & 
                    #(  (non_daily['activity_id']==-1) | (non_daily['activity_id']==info_list.index[info_num][3]) )
                     (non_daily['order_date'].isin(promote_dates_1))              ]['order_date'].unique()   
         grp1_lst_dt = pd.Series( grp1_lst_dt)
         grp0_lst_dt = non_daily[(non_daily['store_code']==info_list.index[info_num][0]) & 
                    (non_daily['sku_code']==info_list.index[info_num][1]) & 
                   #  ( non_daily['activity_id']==-1 )  & (non_daily['is_online']==1) &
                     (non_daily['order_date'].isin(grp0)) 
                    ]['order_date'].unique()
         grp0_lst_dt = pd.Series( grp0_lst_dt)
            
         dayslen = min(len(grp1_lst_dt),len(grp0_lst_dt))
         
         if dayslen>0:
             grp1_lst= non_daily.loc[(non_daily['store_code']==info_list.index[info_num][0]) & 
                    (non_daily['sku_code']==info_list.index[info_num][1])][non_daily['order_date'].isin(grp1_lst_dt)]
             #grp1_lst[['grp_tp']]<-"grp1"
             grp1_lst=grp1_lst.assign(grp_tp="grp1")
             
             grp0_lst= non_daily.loc[(non_daily['store_code']==info_list.index[info_num][0]) & 
                    (non_daily['sku_code']==info_list.index[info_num][1])][non_daily['order_date'].isin(grp0_lst_dt)]
             grp0_lst=grp0_lst.assign(grp_tp="grp0")
             
             
             
           
             #pd.concat(grp1_lst,grp0_lst)
             
             res = grp1_lst.append(grp0_lst)
             
             res = res.assign(usedfor=info_list.index[info_num][2] )
             
         else:
             res=pd.DataFrame()
             return res
               
    else:
        print('promote_dates is empty')
        res=pd.DataFrame()
        return res
    
    print(str(info_list.index[info_num])+"::-------> ending!!!!!!"+str(info_num))
    
    
    return res




#len(info_list

lst_r = pd.Series(list(range(0, len(info_list))))
grp_r = pd.concat(list(lst_r.apply(store_da)))

grp_r.to_csv("grp_r.csv",encoding="gb2312")

#grp_r[grp_r['activity_name'].str.contains('动态促销')==True]

#info_list = pd.DataFrame(info_list    )
#store_da(4)

# non_daily.loc[non_daily.profit >= 5,'order_date']
# non_daily.loc[non_daily.profit >= 5,['order_date','profit']]
   