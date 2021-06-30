#!/usr/bin/env python
# -*- coding: utf-8 -*-

""" Script connects to the Virtual Crossing weather data API, loads a
collection of (location, date) tuples, then retrieves the weather
corresponding to those tuples and saves them to disk as a flat TSV.

Any errors during the API pull are skipped, but are recorded in a list
called errors.
"""

import json
import os
import requests
import pandas as pd
from pandas.io.json import json_normalize


class VirtualCrossingAPI(object):
 
    def __init__(self, key, include='days'):
        self.key = key
        self.base_url = 'https://weather.visualcrossing.com/VisualCrossingWebServices/rest/services/timeline'
        self.unit_group = 'us'
        self.include = include
 
    def get_weather(self, locale, start, end):
        """ Returns API token."""
        headers = {'Accept': 'application/json'}
        params = {'unitGroup': 'us',
                'key': self.key,
                'include': self.include}
        request_url = '%s/%s/%s/%s/' % (self.base_url, locale, start, end)
        response = self._get(request_url, headers=headers, params=params)
        return json.loads(response.content)
        # return pd.read_json(response.content)
 
    @staticmethod
    def _get(url, headers=None, params=None):
        """ Handles the guts of making a request and associated error
        handling."""
        response = requests.get(url, headers=headers, params=params)
        try:
            response.raise_for_status()
        except requests.exceptions.HTTPError as e:
            raise e
        return response


if __name___ == '__main__':

    API_KEY = os.getenv('VIRTUAL_CROSSING_KEY')

    vc = VirtualCrossingAPI(key=API_KEY)

    geodate = pd.read_csv('./data/nuforc/processed/geodate.tsv', sep='\t', header=None)
    geodate.columns = ['loc', 'date']

    errors = []
    weather_list = []

    for i, row in geodate.iterrows():

        print(i)

        try:
            api_output = vc.get_weather(row['loc'], row['date'], row['date'])
            loc_weather = json_normalize(api_output['days'])
            del api_output['days']
            del api_output['stations']
            loc_id = json_normalize(api_output)
            df = pd.concat([loc_id, loc_weather], axis=1)
            df['loc'] = row['loc']

            weather_list.append(df)
        
        except Exception as e:
            print('ERROR')
            errors.append([row['loc'], row['date']])
        
    final = pd.concat(weather_list)
    final.reset_index()

    final.to_csv('./data/weather/raw/weather_dump.tsv', sep='\t', header=True,
                 encoding='utf-8', index=None)
