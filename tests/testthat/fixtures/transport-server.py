from http.server import BaseHTTPRequestHandler, HTTPServer
import os
import socketserver
import json
import sys

class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass
    def do_GET(self):
        if self.path.endswith('/data/JSESSION'):
            self.send_response(405)
            self.end_headers()
            return
        if self.path.endswith('/archive'):
            body = bytes(range(256)) * 1024
            kind = 'application/octet-stream'
        else:
            body = json.dumps({'method': self.command, 'path': self.path,
                               'authorization': self.headers.get('Authorization'),
                               'cookie': self.headers.get('Cookie')}).encode()
            kind = 'application/json'
        self.send_response(200)
        self.send_header('Content-Type', kind)
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)
    def do_POST(self):
        body = self.rfile.read(int(self.headers.get('Content-Length', 0)))
        if self.path.endswith('/data/JSESSION'):
            body = b'synthetic-session'
        self.send_response(200)
        self.send_header('Content-Type', self.headers.get('Content-Type', 'text/plain'))
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

class LoopbackServer(HTTPServer):
    # HTTPServer.server_bind() resolves socket.getfqdn(host), a reverse-DNS
    # lookup that can stall for a long time on macOS CI runners before the
    # port is reported. A loopback fixture needs no server name lookup.
    def server_bind(self):
        socketserver.TCPServer.server_bind(self)
        self.server_name = '127.0.0.1'
        self.server_port = self.server_address[1]

server = LoopbackServer(('127.0.0.1', 0), Handler)
# Publish the port atomically so a reader never sees a partial write.
with open(sys.argv[1] + '.tmp', 'w') as ready:
    ready.write(str(server.server_port))
os.replace(sys.argv[1] + '.tmp', sys.argv[1])
server.serve_forever()
