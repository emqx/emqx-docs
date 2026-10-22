from pathlib import Path
from html.parser import HTMLParser
from urllib.parse import urlsplit,unquote
import argparse

parser = argparse.ArgumentParser(description='Validate rendered migration fixture links and anchors')
parser.add_argument('--dist', type=Path, required=True)
root = parser.parse_args().dist.resolve()
class Page(HTMLParser):
 def __init__(self,text):
  super().__init__();self.ids=set();self.links=[];self.depth=0;self.feed(text)
 def handle_starttag(self,tag,attrs):
  a=dict(attrs)
  if 'id' in a:self.ids.add(a['id'])
  if tag=='div':
   if self.depth:self.depth+=1
   elif 'vp-doc' in a.get('class','').split():self.depth=1
  if self.depth:
   for k in ['href','src']:
    if k in a:self.links.append(a[k])
 def handle_endtag(self,tag):
  if tag=='div' and self.depth:self.depth-=1

pages={p:Page(p.read_text()) for p in (root/'en/emqx/v6.3').rglob('*.html')}
if len(pages) != 6:
 raise ValueError(f"Expected 6 rendered fixture pages, found {len(pages)}")
checked=0
for p,data in pages.items():
 for link in data.links:
  u=urlsplit(link)
  path=unquote(u.path)
  if u.scheme or u.netloc:continue
  q=(root/path.lstrip('/')) if path.startswith('/') else (p.parent/path if path else p)
  q=q.resolve()
  if q.is_dir():q=q/'index.html'
  if not q.is_relative_to(root) or not q.is_file():
   raise ValueError(f"Missing or out-of-root destination in {p}: {link} -> {q}")
  if u.fragment and q.suffix=='.html':
   ids=pages[q].ids if q in pages else Page(q.read_text()).ids
   if unquote(u.fragment) not in ids:
    raise ValueError(f"Missing fragment in {p}: {link}")
  checked+=1
print(f'{len(pages)} rendered pages; {checked} content links/images/fragments verified')
