#!/usr/bin/python

####################
## PSCF program
## ~getmetfiles.py~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

###_* import libraries
import time
import csv
import os
import subprocess

###_* user inputs

Input_file = 'userinputs/input.txt'
datadir = 'C:/hysplit4/metdata' 

###_ . data archive and filename format

###_  : EDAS (N. America)
base = 'ftp://arlftp.arlhq.noaa.gov/pub/archives/edas40'
fileform = 'edas.%b%y.%03d'
daysperfile = 15

###_  : GDAS (Global)
# base = 'ftp://arlftp.arlhq.noaa.gov/pub/archives/gdas1/'
# fileform = 'gdas1.%b%y.w%d'
# daysperfile = 7

###_ . curl or wget
###_  : curl
# getfn = 'curl -C - -O'

###_  : wget
getfn = 'wget -c'

###_* parse input file

def astime(x):
    return time.strptime(x,'%m/%d/%y %H:%M:%S')
def asname(x,fileform=fileform,daysperfile=daysperfile):
    archive,monthyear,week = fileform.split('.')
    return '.'.join([archive,
                     time.strftime(monthyear,x).lower(),
                     week % (x[2]/(daysperfile+1)+1)])
def unique(x,key=None):
    return sorted(list(set(x)),key=key)
def keyfn(x):
    parts = x.split('.')
    return time.strptime(parts[1],'%b%y')[:2]+(int(parts[2].replace('w','')),)

start_times = unique([astime(x[3]) for i,x in
                      enumerate(csv.reader(open(Input_file,'r'),delimiter='\t'))
                      if i > 0]) ## skip header
files = unique(map(asname,start_times),key=keyfn)
files.reverse()

###_* retrieve files

here = os.getcwd()
os.chdir(datadir)
for x in files:
    if os.path.exists(x):
        continue
    print x
    subprocess.call(' '.join([getfn,os.path.join(base,x)]),shell=True)
os.chdir(here)

