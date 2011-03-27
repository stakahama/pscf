####################
## PSCF program
## ~runHYSPLIT.py~
## $Rev$
## Sept. 2009
## Satoshi Takahama (stakahama@ucsd.edu)
####################

import os
import time
import operator
import subprocess

## ============== paths/files ======================
Input_file = 'userinputs/input.txt'
Exec_file = './hymodelt.exe'
Meteo_path = './metdata/'
Output_path = './trajectories/'
Output_base = 'tdump-'
Control = 'CONTROL'

## =============== process ======================
def filekey(filename):
    x = filename.split('.')
    return strptime(x[1],'%b%y')[:2] + (int(x[2][1]),)
Meteo_files = sorted(os.listdir(Meteo_path), key=filekey)
Nmet = len(Meteo_files)
Nloc = 1

## ============== function defs ======================
def transform(x,dk=['Start_loc','Start_time','Run_hours',
                    'Vert_coord','Model_top']):
    return dict(zip(dk,[' '.join(x[:3])] + x[3:]))
def label(tm,loc):
    return ('%s-%s' % (tm,str(int(round(float(loc.split()[2]))))))
def time2time(x,fmt='%m/%d/%y %H:%M:%S'):
    return map(lambda x: str(x + 100)[-2:],time.strptime(x,fmt)[:4])
def strip(s,r=[('\r',''),('\n','')]):
    return reduce(lambda x,y: x.replace(y[0],y[1]),r,s)

## ============== read data ======================
infile = [transform(strip(x).split('\t'))
          for i,x in enumerate(open(Input_file,'r')) if i > 0]

## ============  run HYSPLIT ======================
for row in infile[:3]:
    try: os.remove(Control)
    except: pass

    Output_file = ('%s%s' %
                   (Output_base,label('_'.join(time2time(row['Start_time'])),
                                      row['Start_loc'])))
    f = open(Control,'w')
    f.write('\n'.join([' '.join(time2time(row['Start_time'])),
                       str(Nloc),
                       row['Start_loc'],
                       row['Run_hours'],
                       row['Vert_coord'],
                       row['Model_top'],
                       Nmet] +
                      list(reduce(operator.add,zip([Meteo_path]*Nmet,
                                                   Meteo_files))) +
                      [Output_path,Output_file]))
    f.close()
    subprocess.call(Exec_file)

