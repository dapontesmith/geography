# -*- coding: utf-8 -*-
"""
Created on Fri Oct 22 16:53:23 2021

@author: dapon
"""
import pandas as pd 
import requests

#set link 
link = "https://en.wikipedia.org/wiki/List_of_MPs_for_constituencies_in_England_(2019%E2%80%93present)"

r = requests.get(link, verify = False).content
#soup = BeautifulSoup(r.text, "html.parser")
content = pd.read_html(r)

#put lists of mps by region into one big list, then concat it 
tables = [content[3], content[5], content[7],
          content[9], content[11], content[13],
          content[15], content[17], content[19]]
mps_england = pd.concat(tables)
#rename columns 
mps_england = mps_england[["MP","Constituency",'Party.1',"In constituencysince"]]
mps_england.columns = ["mp","constit","party","in_constit_since"]
#clean up the in_constit_since variable
mps_england["in_constit_since"] = mps_england["in_constit_since"].str.replace(" by-election","")
mps_england["in_constit_since"] = mps_england["in_constit_since"].str[:4]

mps_england.to_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/paper2/mps_constit_2019.csv")