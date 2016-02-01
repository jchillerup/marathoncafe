import sys, os, requests, time

fp = open(sys.argv[1], 'r')

ts_offset = -1
timer = 0

SPEED_FACTOR = 10

for line in fp.readlines():
    kitchen, quantity, timestamp = line.rstrip().split(';')

    if ts_offset == -1:
        ts_offset = int(timestamp)

    while (int(timestamp) - ts_offset) > timer:
        timer += 1
        time.sleep(1/SPEED_FACTOR)

        print(timer)
        
    url = 'http://localhost:8081/?mode=streg&kitchen=%s&quantity=%s' % (kitchen, quantity)
    requests.get(url)
    print(url)
