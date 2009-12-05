import os
from functional import *
from operator import add
def replace(s,d):
    return ('pdftk %s cat %d output %s compress' %
            (s,d,s.replace('.pdf','pg%d.pdf'% d)))

map(os.system,
    [replace(x,y) for x in ('unique.pdf','identity.pdf') for y in (1,2)])

