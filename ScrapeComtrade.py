"""
Alice Lepissier
July 2018
Risk-based IFF
Scrape the Comtrade website for data
This code must be run in batches, as Comtrade limits the API usage to 100 requests per hour.
"""

import requests, os
import pandas as pd
from pandas.compat import StringIO

os.chdir('C:/cloudstorage/googledrive/Projects/Tax Justice Network/Consultancy 2 - summer 18/Risk-based IFF')


"""" First batch """
gets = pd.read_csv('Data/Comtrade/Comtrade_GET_1-100.csv', header=None)
urls = gets[0].tolist()
df1 = pd.DataFrame()

for url in urls:
    req = requests.get(url)
    req_string = req.text
#    file_name = 'r' + url[url.find('r=')+len('r='):url.rfind('&px=')] + '.csv'
#    with open(file_name, 'w', newline='') as out_csv:
#        out_csv.write(req_string)
#    data = pd.read_csv(file_name, encoding='iso-8859-1')
    data = pd.read_csv(StringIO(req_string))
    df1 = df1.append(data, sort=False)
df1.to_csv('Data/Comtrade/comtrade_1.csv')


"""" Second batch """
gets = pd.read_csv('Data/Comtrade/Comtrade_GET_101-200.csv', header=None)
urls = gets[0].tolist()
df2 = pd.DataFrame()

for url in urls:
    req = requests.get(url)
    req_string = req.text
    data = pd.read_csv(StringIO(req_string))
    df2 = df2.append(data, sort=False)
df2.to_csv('Data/Comtrade/comtrade_2.csv')


"""" Third batch """
gets = pd.read_csv('Data/Comtrade/Comtrade_GET_201-300.csv', header=None)
urls = gets[0].tolist()
df3 = pd.DataFrame()

for url in urls:
    req = requests.get(url)
    req_string = req.text
    data = pd.read_csv(StringIO(req_string))
    df3 = df3.append(data, sort=False)
df3.to_csv('Data/Comtrade/comtrade_3.csv')


"""" Fourth batch """
gets = pd.read_csv('Data/Comtrade/Comtrade_GET_301-400.csv', header=None)
urls = gets[0].tolist()
df4 = pd.DataFrame()

for url in urls:
    req = requests.get(url)
    req_string = req.text
    data = pd.read_csv(StringIO(req_string))
    df4 = df4.append(data, sort=False)
df4.to_csv('Data/Comtrade/comtrade_4.csv')


"""" Fifth batch """
gets = pd.read_csv('Data/Comtrade/Comtrade_GET_401-500.csv', header=None)
urls = gets[0].tolist()
df5 = pd.DataFrame()

for url in urls:
    req = requests.get(url)
    req_string = req.text
    data = pd.read_csv(StringIO(req_string))
    df5 = df5.append(data, sort=False)
df5.to_csv('Data/Comtrade/comtrade_5.csv')


"""" Sixth batch """
gets = pd.read_csv('Data/Comtrade/Comtrade_GET_501-508.csv', header=None)
urls = gets[0].tolist()
df6 = pd.DataFrame()

for url in urls:
    req = requests.get(url)
    req_string = req.text
    data = pd.read_csv(StringIO(req_string))
    df6 = df6.append(data, sort=False)
df6.to_csv('Data/Comtrade/comtrade_6.csv')


df = pd.DataFrame()
df = df1.append([df2, df3, df4, df5, df6])
df.to_csv('Data/Comtrade/comtrade.csv')


