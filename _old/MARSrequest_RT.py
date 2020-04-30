#!/usr/bin/env python
from ecmwfapi import ECMWFDataServer
server = ECMWFDataServer()
server.retrieve({
    "class": "mc",
    "dataset": "cams_nrealtime",
    "date": "2019-12-01/to/2019-12-31",
    "expver": "0001",
    "levtype": "sfc",
    "param": "72.210/73.210/74.210",
    "step": "0",
    "stream": "oper",
    "time": "00:00:00/12:00:00",
    "type": "fc",
    "target": "output",
})
#from near realtime