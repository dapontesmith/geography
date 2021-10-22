# -*- coding: utf-8 -*-
"""
Created on Fri Oct 22 16:53:23 2021

@author: dapon
"""
from bs4 import BeautifulSoup
import pandas as pd 
import requests

link = "https://en.wikipedia.org/wiki/List_of_MPs_elected_in_the_2019_United_Kingdom_general_election"

r = requests.get(link, verify = False)
soup = BeautifulSoup(r.text, "html.parser")

pd.read_html(soup)