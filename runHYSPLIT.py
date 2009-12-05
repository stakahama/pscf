import os
import time

## ============== paths/files ======================
Input_file = 'userinputs/input.txt'
Exec_file = './hymodelt.exe'
Meteo_path = './metdata/'
Output_path = './trajectories/'
Output_base = 'tdump-'
Control = 'CONTROL'

## =============== process ======================
Meteo_files = [f for f in os.listdir(Meteo_path)]
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
                      zip([Meteo_path]*Nmet,Meteo_files) +
                      [Output_path,Output_file]))
    f.close()
    os.system('%s' % str(Exec_file))

