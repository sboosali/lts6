#!/usr/bin/python

import websocket
import time

ws = websocket.create_connection("ws://localhost:8888/websocket")
i = 1

while True:
    print "[sending]..."
    ws.send(str(i))
    print "[sent]"

    print "[receiving...]"
    s = ws.recv()
    print "[received] '%s'" % s
    j = int(s)

    i = i + j
    time.sleep(1)

ws.close()
