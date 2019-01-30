"""
Alice Lepissier
alice.lepissier@gmail.com
December 2018
UNECA estimates of IFF
Scrape the Comtrade website for data
This code must be run in batches, as Comtrade limits the API usage to 100 requests per hour.
"""

import requests, os
import pandas as pd
from pandas.compat import StringIO

os.chdir('C:/cloudstorage/googledrive/Projects/UN Consultancy/Illicit Financial Flows/IFF estimates')


"""" First batch """
gets = pd.read_csv('Data/Comtrade/Comtrade_GET_FULL.csv', header=None)
urls = gets[0].tolist()
df = pd.DataFrame()

for url in urls:
    req = requests.get(url)
    req_string = req.text
    data = pd.read_csv(StringIO(req_string))
    df = df.append(data, sort=False)
df.to_csv('Data/Comtrade/comtrade.csv')


#df = pd.DataFrame()
#df = df1.append([df2, df3, df4, df5, df6, df7, df8, df9, df10, df11, df12, df13])
#df.to_csv('Data/Comtrade/comtrade.csv')

# Create get request on the fly
# Download all 1270 CSV
# Take this folder, read all CSVs, append one by one, write to massive
# Write delay
# Write single function for get request
# Print url for debug