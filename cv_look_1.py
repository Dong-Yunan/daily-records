# wk rpt  retention rate
import requests
from requests.auth import HTTPBasicAuth
import json
import pandas as pd
from pandas.io.json import json_normalize
#import numpy as np

# platform	int2	1 Android、2 iOS3 JS
#5.set the datetime



#-------date setting
import time
import datetime

#---DATE_BEGIN
# get the time format
date0= (datetime.datetime.now() - datetime.timedelta(days = 15))
#timeStamp = int(time.mktime(threeDayAgo.timetuple()))
#transform it into the format needed
DATE_BEGIN = date0.strftime("%Y-%m-%d")
#DATE_END.replace("\'","\"")

#import types  
#---DATE_END
# get the time format
date0= (datetime.datetime.now() - datetime.timedelta(days = 1))
DATE_END = date0.strftime("%Y-%m-%d")



####data by day
url='http://api.zhugeio.com/v2/stat/38182?metrics=users \
&dimensions=$event_name&dimensions=$platform&dimensions=$day  \
&conditions={"$day":["between","2017-05-01","2017-05-11"]}'
#url='http://api.zhugeio.com/v2/stat/38182?metrics=users&dimensions=$event_name&dimensions=$event_name.page&dimensions=$platform&conditions={"$day":["between","2017-02-01","2017-04-25"],"$event_name":["==","分享"]}'

url_begin='http://api.zhugeio.com/v2/stat/38182?'
url_metrics='metrics=users'
url_dimensions='&dimensions=$event_name&dimensions=$platform&dimensions=$day'
url_cond_1='&conditions={\"$day\":[\"between\",\"'
url_cond_2=DATE_BEGIN
url_cond_3='\",\"2017-05-11\"]}'


a = requests.get(url.decode('utf-8'),auth=HTTPBasicAuth('zaihang','a3cff6da82a94df69ff3ad0aa61bef96'))
print a.text
text = json.loads(a.text)
text
a1=json_normalize(text['data']['results'])


#### data overall during these days












