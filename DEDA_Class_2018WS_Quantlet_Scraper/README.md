[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="888" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **DEDA_Class_2018WS_Quantlet_Scraper** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: 'DEDA_Class_2018WS_Quantlet_Scraper'

Published in: 'Digital Economy and Decision Analytics (WS 18/19)'

Description: 'Scraping tool to gather raw code from all available .R scripts in
              the Quantlet repository.'

Keywords: 'Web Scraping, Tom Foolery'

Author: 'Alex Truesdale'

Submitted: 'Sun. Feb 26 2019 by Alex Truesdale'

Datafile: '1) quantlet_links.txt
           2) raw_code_urls.txt
           3) code_aggregate.R'

See also: 'code_output/*.R'

```

### PYTHON Code
```python

# -*- coding: utf-8 -*-

"""
Created Feb 2, 2019
@author: Alex Truesdale

Python tool to scrape .R code from github repo contents.
"""

from requests_html import HTMLSession
from bs4 import BeautifulSoup
import time
import html
import sys
import csv
import re
import os

# Introduction.
print('=========================================================')
print("\n This script scrapes all .R code Quantlets to develop \n \
a sample of R code to train Justin Engelmann's super \n \
duper LSTM NN for reproducing natural lang. nonsense. \n")
print('=========================================================')
print()
time.sleep(7)

# Initilise HMTL requests session.
print("------ opening up HTML session for scrapin' \n")
session = HTMLSession()
time.sleep(1.5)

# Open source file as .csv and read in lines.
print("------ reading in links to .R Quantlets on GitHub")
with open('quantlet_links.txt') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter = ',')
    urls = [line[0] for line in csv_reader]

time.sleep(1.5)

print("------ replacing URL components to access raw code hosted online")
time.sleep(1.5)

# Define URL replacement pairings.
replacements = {
    'https://github.com': 'https://raw.githubusercontent.com',
    'blob/': ''
}

# Compile RegEx pattern for 'or' replacement statement; test on dummy URL.
pattern = re.compile('|'.join(replacements.keys()))
text = pattern.sub(lambda x: replacements[x.group(0)], 'https://github.com/QuantLet/ARR/blob/master/ARRcormer/ARRcormer.R')

# Initilise aggregate list container; loop through source URLs, find absolute links for .R files.
print("------ double checking for unique R-only files \n")
time.sleep(1)

links_aggregate = []
for i, url in enumerate(urls):
    page = session.get(url)
    print(' {}: '.format(i) + url, end = '\r')
    links = [pattern.sub(lambda x: replacements[x.group(0)], page) for page in page.html.absolute_links if page.endswith('.R')]
    if len(links) > 0:
        links_aggregate.extend(links)

# Redefine links_aggregate as set of unique URLs.
links_aggregate = [link for link in set(links_aggregate)]
print()
print("------ count unique files: {}".format(len(links_aggregate)))
time.sleep(1.5)

# Save raw code links as .csv file.
print()
print("------ save rwa code urls at 'raw_code_urls.txt'")
time.sleep(1.5)

with open('raw_code_urls.txt', 'w') as raw_urls_out:
    for item in links_aggregate:
        raw_urls_out.write(item + ',' + '\n')

# Create dictionary pairing URLs to their raw contents.
print("------ pairing urls to code content... (ignore ERROR notifications) \n")
time.sleep(1.5)

contents_dictionary = {}
for i, link in enumerate(links_aggregate):
    get_result = session.get(link)

    # Try except block to skip over (and identify) files with encoding problems
    # 6 of ~1500... negligable.
    try:
        code = BeautifulSoup(get_result.html.html, 'html.parser')
        contents_dictionary.update({link: code.prettify()})
    except:
        print('ERROR:', link)

time.sleep(1.5)
print()
print("------ count files: still {} \n".format(len(contents_dictionary)))
time.sleep(1.5)

# Define aggregate_code_path; loop through pairwise dict. and write individual files
# Append code to aggregate file.

print("------ writing individual files alongside aggregate file... \n")
time.sleep(1.5)

aggregate_code_path = 'code_aggregate.R'
for file, contents in contents_dictionary.items():
    filename = file.split('/')[-1]
    print(' saving file: ' + filename, end = '\r')
    time.sleep(.15)

    with open(('code_output/' + filename), 'w') as single_file:
        single_file.write(contents)

    with open(aggregate_code_path, 'a') as aggregate:
        aggregate.write(contents)
        aggregate.write('\n\n')

time.sleep(1.5)

```

automatically created on 2019-03-01