# -*- coding: utf-8 -*-
"""
Created on Fri Oct 15 09:15:31 2021

@author: dapon
"""

import pandas as pd 

#read in scraped places data
places_df = pd.read_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/uk_geography/uk_placenames_wiki.csv")
#read in speeches data
speeches = pd.read_csv("C:/Users/dapon/Dropbox/Harvard/dissertation/data/ParlSpeech/HouseOfCommons_Sample.csv",
                       encoding="ISO-8859-1")
#put places data in list 
places = places_df[["location"]].values.tolist()
places_df["location"].nunique() #note that there are only about 38000 unique places, but 44000 places overall



#get list of unique speakers 
speakers = speeches["speaker"].unique().tolist()

#initialize empty lists 
speaker_list, place_list, mention_list = [],[],[]
df = pd.DataFrame()

#loop over speakers 
for speaker in speakers:
    #subset df to only that speaker's speeches 
    speaker_speeches = speeches[speeches["speaker"] == speaker]["text"]
    for place in places:
        #count number of times place is mentioned, append to mention_list
        mention_list.append(speaker_speeches.str.count(place).sum())
        #append speaker and place to lists 
        speaker_list.append(speaker)
        place_list.append(place)
        #put it all in a dataframe
        df_out = pd.DataFrame(list(zip(speaker_list, place_list, mention_list)),
                              columns = ["speaker", "place","count"])
    #append to full dataframe
    df = df.append(df_out)

