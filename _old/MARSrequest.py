#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "mc",
    "dataset": "cams_reanalysis",
    "date": "2009-01-01/to/2010-12-31",
    "expver": "eac4",
    "levtype": "sfc",
    "param": "72.210/73.210/74.210/165.128/166.128/167.128/168.128",
    "stream": "oper",
    "time": "00:00:00/03:00:00/06:00:00/09:00:00/12:00:00/15:00:00/18:00:00/21:00:00",
    "type": "an",
    'grid': "0.1/0.1",
    'area': "35.466783/33.862331/35.542587/33.916113",
    "target": "Beirut_09_10",
})
#find bounding box at: https://boundingbox.klokantech.com/

#Lebanon: 'area': "34.8826/33.048/36.625/34.6924",

# server.retrieve({
    # "class": "mc",
    # "dataset": "cams_reanalysis",
    # "date": "2009-01-01/to/2010-12-31",
    # "expver": "eac4",
    # "levtype": "sfc",
    # "param": "72.210/73.210/74.210/165.128/166.128/167.128/168.128",
    # "stream": "oper",
    # "time": "00:00:00/03:00:00/06:00:00/09:00:00/12:00:00/15:00:00/18:00:00/21:00:00",
    # "type": "an",
    # 'grid': "0.1/0.1",
    # 'format': "netcdf",
    # 'area': "35.466783/33.862331/35.542587/33.916113",
    # "target": "Beirut_08_09",
# })