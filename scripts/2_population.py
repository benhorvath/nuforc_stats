#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
"""

# 2020: https://fred.stlouisfed.org/release/tables?rid=118&eid=259194&od=2019-01-01#

import pandas as pd

if __name___ == '__main__':

    YEARS = range(2000, 2021, 1)
    base_url = 'https://fred.stlouisfed.org/release/tables?rid=118&eid=259194&od='

    pop_list = []

    for year in YEARS:

        year = str(year)

        print(year)

        url = ('%s%s-01-01' % (base_url, year))
        scrape = pd.read_html(url)
        tbl = scrape[0]

        # Flatten multiIndex
        tbl.columns = [' '.join(col).strip() for col in tbl.columns.values]

        pop = tbl.filter(['Unnamed: 1_level_0 Name', 
                          'Thousands of Persons %s' % year])
        pop.columns = ['state', 'population']
        pop['year'] = year

        pop_list.append(pop)

        df = pd.concat(pop_list)
        df.reset_index()

        df.to_csv('./data/population.tsv', sep='\t', header=True,
                  encoding='utf-8', index=None)