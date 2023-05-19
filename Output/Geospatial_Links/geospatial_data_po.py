import pandas as pd
import numpy as np
from geopy.distance import distance as geodistance

# example dataframes
df1 = pd.DataFrame({
        'id': [1, 2, 3, 4],
        'lat': [34.0522, None, 40.7128, None],
        'lon': [-118.2437, None, -74.0060, None],
        'office_name': ['Los Angeles Office', 'Chicago Office', 'New York Office', 'Houston Office']
    })

df2 = pd.DataFrame({
        'id': [1, 2, 3, 4],
        'lat': [34.0522, 41.8781, None, 29.7604],
        'lon': [-118.2437, -87.6298, None, -95.3698],
        'office_name': ['Los Angeles', 'Chicago', 'Boston', 'Houston']
    })

# function to calculate distance between coordinates
def distance(lat1, lon1, lat2, lon2):
    coords1 = (lat1, lon1)
    coords2 = (lat2, lon2)
    return geodistance(coords1, coords2).km

# merge dataframes based on closest geographic match
merged_df = df1.apply(lambda row: 
                      pd.Series({'id': row['id'],
                                 'office_name': row['office_name'],
                                 'distances': df2.apply(lambda row2: distance(row['lat'], row['lon'], row2['lat'], row2['lon']), axis=1),
                                 'closest_match': df2.iloc[df2['distances'].idxmin()]['office_name'],
                                 'closest_lat': df2.iloc[df2['distances'].idxmin()]['lat'],
                                 'closest_lon': df2.iloc[df2['distances'].idxmin()]['lon']}), axis=1)

# view merged dataframe
merged_df = merged_df[['id', 'office_name', 'closest_match', 'closest_lat', 'closest_lon']]
print(merged_df)
