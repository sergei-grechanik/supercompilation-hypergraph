#!/usr/bin/env python

import sys
import os.path
import argparse
import xml.etree.ElementTree as ET
from collections import defaultdict

argp = argparse.ArgumentParser(description="Transform report.xml into a human-readable table")
argp.add_argument('file', nargs='+')
argp.add_argument('-f', nargs=1, default=["user"], help="fields")
argp.add_argument('--all', '-a', action="store_true")

args = argp.parse_args()

fields = args.f[0].split(",")

table = defaultdict(lambda : defaultdict(list))
optsset = set()

for f in args.file:
    tree = ET.parse(f)
    for run in tree.findall("run"):
	opts = run.find("options").text
	if opts:
        	opt = (run.find("base-command").text or "") + " " + opts
	else:
		opt = (run.find("base-command").text or "")
        table[run.find("test").text][opt].append(run)
        optsset.add(opt)

prefix = os.path.commonprefix(optsset)
preflen = len(prefix)

output = [["test"] + ["\"" + o[preflen:] + "\"" for o in optsset]]

warnings = []

def warn(run):
    warnings.append("Instability warning: " + run.find("full-command").text)

def mkcell(runs, f):
    if runs:
        codes = list(int(r.find("exit-code").text) for r in runs)
        if any(c == 0 for c in codes) and any(c == 1 for c in codes):
            warn(runs[0])
            
        if args.all:
            l = [float(r.find(f).text) for r in runs if r.find(f) is not None]
            return str(sum(l)/len(l)) if l else "fail"
        elif all(c != 0 for c in codes):
            return "fail"
        else:
            l = [float(r.find(f).text) for r in runs if int(r.find("exit-code").text) == 0]
            return str(sum(l)/len(l))
    else:
        return "?"

for t, runs in table.iteritems():
    for f in fields:
        output.append([t + " (" + f + ")"] + 
                      [mkcell(runs[o], f) for o in optsset])
    
lens = [max(len(l[i]) for l in output) for i in xrange(0, len(optsset) + 1)]

print "Base: " + prefix

for l in output:
    print " ".join(c + " " * (m - len(c)) for c, m in zip(l, lens))
    
    
for w in warnings:
    print w

