from http.server import BaseHTTPRequestHandler, HTTPServer
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

server = HTTPServer(('127.0.0.1', 0), Handler)
with open(sys.argv[1], 'w') as ready:
    ready.write(str(server.server_port))
server.serve_forever()
