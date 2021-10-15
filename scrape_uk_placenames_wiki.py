# -*- coding: utf-8 -*-
"""
Created on Thu Oct 14 19:44:46 2021

@author: dapon
"""

from bs4 import BeautifulSoup
import requests
import pandas as pd
from time import sleep

#set url for all place-names
url = "https://en.wikipedia.org/wiki/List_of_United_Kingdom_locations"
r = requests.get(url, verify = False)
soup = BeautifulSoup(r.text, "html.parser")
hrefs = soup.find_all("a")

#collect all links to place-name lists on page 
links = []
for a in soup.find_all('a', href=True):
    if "List_of_United_Kingdom_locations:_" in a["href"]:
        links.append(a["href"])
       
#set base url
base_url = "https://en.wikipedia.org"


df = pd.DataFrame()
#iterate over linsk to get all place names 
for link in links:
    print(link)
    #make full url
    full_url = base_url + link
    html = requests.get(full_url).content
    #read html
    df_list = pd.read_html(html)
    #get rid of extraoenous first and last elements of df_list 
    df_list = df_list[1:len(df_list)]
    df = df.append(df_list)
    
#eliminate header rows  
df.columns = ["location","locality","coordinates","grid_reference"]
df = df[df["location"].str.contains("Location")==False]
df[["location","locality","coordinates"]]

#coordinates are not in the right form

df.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/place_names_wiki.csv")


res = requests.get("https://en.wikipedia.org/wiki/List_of_United_Kingdom_locations:_Aa-Ak")
result = {}
soup = BeautifulSoup(res.content,'lxml')
tables = soup.find_all('table',{'class':'wikitable'})
table = tables[0].find('tbody')

for row in table.find_all('tr',{'class':'geo-default'}):
    coords = (row.find('span',{'class': 'geo-dec'}))
    
    
    
    
    