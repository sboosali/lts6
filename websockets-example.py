#!/usr/bin/python

from websocket import create_connection
ws = create_connection("ws://localhost:8888/websocket")
print "[sending] 'Hello, World'..."
ws.send("Hello, World")
print "[sent]"
print "[receiving...]"
result = ws.recv()
print "[received] '%s'" % result
ws.close()
