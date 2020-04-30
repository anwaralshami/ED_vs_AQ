cdo outputtab,name,date,time,value Beirut_09_10_Grid > Beirut_09_10.tsv
cdo -r -f nc -t ecmwf copy Beirut_09_10 Beirut_09_10_Grid 