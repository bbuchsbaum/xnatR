"""Loopback-only REST fixture, never used with real credentials or records."""
from http.server import BaseHTTPRequestHandler, HTTPServer
from urllib.parse import urlsplit, parse_qs
import json
import sys
import io
import zipfile
import os
import socket
import time

with open(sys.argv[2]) as source:
    tables = json.load(source)
legacy = sys.argv[3] == 'legacy'

class Handler(BaseHTTPRequestHandler):
    def log_message(self, *args):
        pass
    def do_GET(self):
        parsed = urlsplit(self.path)
        path = parsed.path.removeprefix('/xnat/')
        query = parse_qs(parsed.query)
        if path not in tables or not self.headers.get('Authorization'):
            self.send_response(404 if path not in tables else 401)
            self.end_headers()
            return
        if len(sys.argv) > 4:
            with open(sys.argv[4]) as control_file:
                control = json.load(control_file)
            if path in control.get('catalog_changes', {}):
                rows = control['catalog_changes'][path]
            else:
                rows = tables[path]
        else:
            control = {}
            rows = tables[path]
        if query.get('format') == ['zip']:
            log_path = sys.argv[4] + '.downloads'
            with open(log_path, 'a') as log:
                log.write(path + '\n')
            if control.get('mode') == 'slow':
                time.sleep(1)
            output = io.BytesIO()
            with zipfile.ZipFile(output, 'w', zipfile.ZIP_STORED) as archive:
                for row in rows:
                    relative = row['URI'].split('/files/', 1)[1]
                    archive.writestr('wrapper/' + relative, b'x' * int(row['Size']))
                if control.get('mode') == 'extra':
                    archive.writestr('wrapper/extra.dcm', b'x')
                if control.get('mode') == 'unsafe':
                    archive.writestr('../escape', b'x')
            body = output.getvalue()
            if control.get('mode') == 'crc':
                body = body.replace(b'x' * 10, b'y' + b'x' * 9, 1)
            truncate = control.get('mode') == 'truncate' and control.get('target', '') in path
            self.send_response(200)
            self.send_header('Content-Type', 'application/zip')
            self.send_header('Content-Length', str(len(body)))
            self.end_headers()
            if control.get('mode') == 'change_after':
                changed = [dict(row) for row in rows]
                changed[0]['Size'] = str(int(changed[0]['Size']) + 1)
                with open(sys.argv[4], 'w') as control_file:
                    json.dump({'catalog_changes': {path: changed}}, control_file)
            self.wfile.write(body[:len(body)//2] if truncate else body)
            if truncate:
                self.wfile.flush()
                self.connection.shutdown(socket.SHUT_RDWR)
                self.connection.close()
            return
        limit = query.get('limit', ['*'])[0]
        result = {'Result': rows}
        if path.endswith('/files'):
            # Scan-resource files are a non-paged listing. The documented
            # response has title=Files but does not include totalRecords.
            result['title'] = 'Files'
        elif legacy or limit == '*':
            result['totalRecords'] = str(len(rows))
        else:
            offset = int(query.get('offset', ['0'])[0])
            # Cap independently of the requested page size; no totalRecords.
            result['Result'] = rows[offset:offset + min(int(limit), 1)]
        body = json.dumps({'ResultSet': result}).encode()
        self.send_response(200)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

server = HTTPServer(('127.0.0.1', 0), Handler)
with open(sys.argv[1], 'w') as ready:
    ready.write(str(server.server_port))
server.serve_forever()
