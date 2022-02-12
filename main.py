import numpy as np
import pandas as pd

data = pd.read_csv("CTA_-_Ridership_-__L__Station_Entries_-_Daily_Totals.csv")


col = list(data.columns)
#print(col[0])
size = data.shape[0]

# Create 3 files with different station
# Roosevelt
roosevelt = pd.DataFrame(columns=col)
# UIC-Halsted
halsted = pd.DataFrame(columns=col)
# O'Hare Airport
airport = pd.DataFrame(columns=col)


for c in range(0, size):
    if (data.loc[c, 'stationname'] == 'UIC-Halsted'):
        halsted = pd.concat([halsted, data.iloc[c].to_frame().transpose()], ignore_index = True)
    elif(data.loc[c, 'stationname'] == 'Roosevelt'):
        roosevelt = pd.concat([roosevelt, data.iloc[c].to_frame().transpose()], ignore_index = True)
    elif(data.loc[c, 'stationname'] == "O'Hare Airport"):
        airport = pd.concat([airport, data.iloc[c].to_frame().transpose()], ignore_index = True)

roosevelt.to_csv('roosevelt.csv')
halsted.to_csv('halsted.csv')
airport.to_csv('airport.csv')
print('done')