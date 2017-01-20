#!/usr/bin/env python

import argparse, csv, collections, os
import csv
from bs4 import BeautifulSoup


parser = argparse.ArgumentParser(description="""Sanitizes a macsquant outputfile into a csv with the same name and makes a "pheno" file to take the names of the sample names""")
    
parser.add_argument("infile", type=str,
                    help="")

args = parser.parse_args()



###############
#Testing
###############

# class Empty():
#     def __init__(self):
#         pass

# args = Empty()
    
# args.infile = "/Users/cs/Desktop/2015-06-06-FP_with_flow2/cm2015-06-07.024.xls"
# args.outfile = "/Users/cs/Desktop/2015-06-06-FP_with_flow2/FP_comp_with_flow2.csv"


####


# dat = collections.defaultdict(lambda : {} )

sampleNames = []

with open(args.infile) as f:
    with open( os.path.splitext(args.infile)[0] + ".csv", "w") as out:
        writer = csv.writer(out)
        page = BeautifulSoup(f)
        rows = page.find("table")
        # writes the header note that the bs4 next method doesn't consume the
        # iterator for some reason
        header = next(rows, None)
        header = [i.get_text() for i in header]
        header = [i.encode("utf8") for i in header]
        header.append( "file".encode("utf8"))
        if not '%-#' in header:
            raise KeyError(" '%-#' not found in the header. The script won't work")
        # Find where the annotation for a new sample lives
        idx = header.index("%-#")
        path_idx = header.index("Path")
        writer.writerow(header)
        print("done")
        for row in rows:
            tds = row.find_all("td")
            if len(tds) > 0:
                values = [i.get_text() for i in tds]
                values = [i.encode("utf8") for i in values]
                if values[idx] == "---":
                    fcs = values[path_idx]
                    sampleNames.append(fcs)
                values.append(fcs)
                writer.writerow(values)

outFile2 = os.path.splitext(args.infile)[0] + "_pheno.csv"
with open(outFile2, "w") as out:
    out.write("file\n")
    for i in sampleNames:
        out.write(i + "\n")
