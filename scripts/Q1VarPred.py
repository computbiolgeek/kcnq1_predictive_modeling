# -*- coding: utf-8 -*-
"""
Created on Mon Jun 12 16:22:59 2017
@summary: This script was written to be adapted for the Q1VarPred web server.
          It takes a list of KCNQ1 variants, one per line, in the format of
          HGVS notation, and outputs prediction of functional impact for each
          variant.
@author: Bian Li
@contact: bian.li@vanderbilt.edu
"""

from argparse import ArgumentParser
import pandas as pd

# parse command line options and arguments
parser = ArgumentParser()
parser.add_argument('--query', help = 'a file containing query variants, \
                    one per line, in the format HGVS notation, e.g. V207M')
parser.add_argument("--output", help = "output filename")
args = parser.parse_args()

# dataset
ds = pd.read_csv('all_predictions_Q1VarPred.csv')

# list of query variants
query_file = open(args.query, 'rt')

# retrieve the label and localPPV for each query
output = open(args.output, 'wt')
output.write('{:7} {:13} {:11}\n'.format('Variant', 'Label', 'Probability'))
for query in query_file.readlines():
  query = query.rstrip()
  wt = query[:1]
  var = query[-1:]
  if wt == var:
    print('Wild type residue is the same as the variant residue, invalid input ' + query + '!')
    output.write('{:7} {}\n'.format(query, 'Invalid input'))
    continue
  resi = int(query[1:-1])
  if wt != ds.ix[resi - 1, 'Wild.type']:
    print('The wild type at position ' + str(resi) + ' did not match the canonical residue type!' )
    output.write('{:7} {}\n'.format(query, 'Incorrect wild type residue'))
    continue
  label, probability = ds.ix[resi - 1, [var + '.label', var + '.localPPV']]
  output.write('{:7} {:13} {:<4.3f}\n'.format(query, label, probability))
output.close()