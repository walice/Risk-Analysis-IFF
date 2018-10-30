"""
Alice Lepissier
alice.lepissier@gmail.com
July 2018
Risk-based IFF
Scrape the BIS website for data
"""

import requests, os
import pandas as pd
from pandas.compat import StringIO

os.chdir('C:/cloudstorage/googledrive/Projects/Tax Justice Network/Consultancy 2 - summer 18/Risk-based IFF')

gets = pd.read_csv('Data/LBS/LBS_GET.csv', header=None)
urls = gets[0].tolist()
df = pd.DataFrame()

for url in urls:
    req = requests.get(url)
    req_string = req.text
    data = pd.read_csv(StringIO(req_string), sep=',', skiprows=6)
    df = df.append(data, sort=False)
df.to_csv('Data/LBS/LBS.csv')

