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

coord_holder, longitude, latitude = [], [], []
df = pd.DataFrame()
#iterate over linsk to get all place names 
for link in links:
    print(link)
    #make full url
    full_url = base_url + link
    html = requests.get(full_url).content
    #get the coordinates out 
    
    #read html to get place names 
    df_list = pd.read_html(html)
    #get rid of extraoenous first and last elements of df_list 
    #and concat all dfs together
    df_list = pd.concat(df_list[1:len(df_list)])
    #df = df_list.iloc[ : , 0:2 ]
    df = df.append(df_list)

#eliminate header rows  
df.columns = ["location","locality","coordinates","grid_reference"]
df = df[df["location"].str.contains("Location")==False]
df_clean = df.dropna()
#df = df[["location","locality"]]

#coordinates are not in the right form, but i have all the place_names
df_names = df_clean[["location","locality"]]

#
coord_list = []
coordinates = df[["coordinates"]].values.tolist()
#get list of coordinates
for i in range(len(coordinates)):
    print(i)
    coord_list.append(coordinates[i][0].replace(u"\ufeff","").split(" / ")[0])

df_names["coordinates"] = coord_list

#clean the remaining messy coordinate entries
for i in range(len(df_names)):
    if "parser" in df_names['coordinates'].iloc[i]:
        df_names["coordinates"].iloc[i] = df_names["coordinates"].iloc[i].split(":nowrap}")[1].split(u"\ufeff")[0]

#write to csv
df_names.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/uk_placenames_wiki.csv")

















####################################################################
####################################################################
###################################################################




#possible code to use to get the coordinates correct ... 
res = requests.get("https://en.wikipedia.org/wiki/List_of_United_Kingdom_locations:_Aa-Ak")
result = {}
soup = BeautifulSoup(res.content,'lxml')
tables = soup.find_all('table',{'class':'wikitable'})
table = tables[0].find('tbody')

holder, longitude, latitude = [], [], []
for i in range(len(tables)):
    table = tables[i].find("tbody")
    coords = table.find("span",{"class":"geo"}).text
    holder.append(coords)
longitude = [holder[i].split("; ")[0] for i in range(len(holder))]
latitude = [holder[i].split("; ")[1] for i in range(len(holder))]




for row in table.find_all('tr',{'class':'geo-default'}):
    coords = (row.find('span',{'class': 'geo-dec'}))
    
    
    
    
    soup = BeautifulSoup(html)
    tables = soup.find_all("table",{"class":"wikitable"})
   # for i in range(len(tables)):
    #    table = tables[i].find("tbody")
     #   coords = table.find("span",{"class":"geo"}).text
      #  coord_holder.append(coords)
    
latitude = [coord_holder[i].split("; ")[0] for i in range(len(coord_holder))]
longitude = [coord_holder[i].split("; ")[1] for i in range(len(coord_holder))]  





test = df_names.copy()

