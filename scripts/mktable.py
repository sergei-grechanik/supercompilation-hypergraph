#!/usr/bin/env python

import sys
import argparse
import xml.etree.ElementTree as ET
from collections import defaultdict

argp = argparse.ArgumentParser("Transform report.xml into a human-readable table")
argp.add_argument('files', nargs='+')
argp.add_argument('-f', nargs=1, default=["user"])

args = argp.parse_args()

fields = args.f[0].split(",")

table = defaultdict(lambda : defaultdict(list))
optsset = set()

for f in args.files:
    tree = ET.parse(f)
    for run in tree.findall("run"):
        table[run.find("test").text][run.find("options").text].append(run)
        optsset.add(run.find("options").text)

print("test\t" + 
  "\t".join("\t".join("\"" + o + " (" + f + ")\"" for f in fields) for o in optsset))

warnings = []

def warn(run):
    warnings.append("Instability warning: " + run.find("full-command").text)

def mkcell(runs, f):
    if runs:
        codes = list(int(r.find("exit-code").text) for r in runs)
        if any(c == 0 for c in codes) and any(c == 1 for c in codes):
            warn(runs[0])
        if all(c != 0 for c in codes):
            return "fail"
        else:
            l = [float(r.find(f).text) for r in runs if int(r.find("exit-code").text) == 0]
            return str(sum(l)/len(l))
    else:
        return "?"

for t, runs in table.iteritems():
    print(t + "\t" + 
          "\t".join("\t".join(mkcell(runs[o], f) for f in fields) for o in optsset))
    
for w in warnings:
    print w

