#!/usr/bin/python

from random import randint, choice
import requests
from time import sleep

kitchens = ['GL', 'ML', 'NY']

LINES = 20000
now = 1391114661

for i in range(LINES):
    r = requests.get('http://localhost:8081/?mode=streg&kitchen=%s%d&quantity=%f' % (choice(kitchens), randint(2,8), float(randint(2,12)) / 2)) 

    print r.text
    
    sleep(7)
    
    #print "%d;%s%d;%d" % (now, choice(kitchens), randint(2, 8), (randint(2, 12) / 2) )

    now += randint(1, 10)
