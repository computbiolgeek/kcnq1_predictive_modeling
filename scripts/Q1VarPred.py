#!/usr/bin/env python3

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
import csv

def get_prediction(wt, var, resi):
    '''
    Get the prediction for the given variant.

    Parameter
    ---------
    wt : str
        one-letter amino acid code
    
    var : str
        one-letter amino acid code

    resi : int
        residue ID

    Returns
    -------
    '''
    # dataset of all predictions
    ds = pd.read_csv('../data/all_predictions_Q1VarPred.csv')

    if wt == var:
        message = 'Wild type residue is the same as the variant residue, invalid input ' + wt + str(resi) + var + '!'
        output_line = [resi, one2three(wt), wt, one2three(var), var, message]
    elif wt != ds.ix[resi - 1, 'Wild.type']:
        message = 'The wild type at position ' + str(resi) + ' did not match the canonical residue type, invalid input ' + wt + str(resi) + var + '!'
        output_line = [resi, one2three(wt), wt, one2three(var), var, message]
    else:
        label, probability = ds.ix[resi - 1, [var + '.label', var + '.localPPV']]
        output_line = [resi, one2three(wt), wt, one2three(var), var, label, '{:<4.3f}'.format(probability)]
    return output_line


def one2three(input_code):
    one2three = {
        'A': 'ALA',
        'R': 'ARG',
        'N': 'ASN',
        'D': 'ASP',
        'C': 'CYS',
        'E': 'GLU',
        'Q': 'GLN',
        'G': 'GLY',
        'H': 'HIS',
        'I': 'ILE',
        'L': 'LEU',
        'K': 'LYS',
        'M': 'MET',
        'F': 'PHE',
        'P': 'PRO',
        'S': 'SER',
        'T': 'THR',
        'W': 'TRP',
        'Y': 'TYR',
        'V': 'VAL'
    }

    # make sure that input is valid
    assert len(input_code) == 1, 'input_code must have extactly one letter'
    assert input_code in one2three.keys(), 'given input_code was not recognized'

    # return the one-letter code
    input_code = input_code.upper()
    return one2three[input_code]


def main():
    '''
    '''
    # parse command line options and arguments
    parser = ArgumentParser()
    parser.add_argument('--query', help = 'a file containing query variants, \
                        one per line, in the format HGVS notation, e.g. V207M, or \
                        comma/whitespace-separated variants on the same line.')
    parser.add_argument("--output", help = "output filename")
    args = parser.parse_args()

    # list of query variants
    query_file = open(args.query, 'rt')

    # retrieve the label and localPPV for each query
    output_lines = []
    output_lines.append(['Residue ID', 'Wild type (three letter code)', 'Wild type (one letter code)', 'Variant (three letter code)', 'Variant (one letter code)', 'Label', 'Probability'])

    # retrieve prediction for each variant
    with open(args.query, 'rt') as query_file:
        for query_line in query_file.readlines():
            query_line = query_line.strip()
            # parse the query line
            if ',' in query_line:
                variants = [v.strip() for v in query_line.split(',')]
            else:
                variants = query_line.split()
            for v in variants:
                wt = v[:1].upper()
                var = v[-1:].upper()
                resi = int(v[1:-1])
                output_line = get_prediction(wt, var, resi)
                output_lines.append(output_line)

    # write predictions
    with open(args.output, 'wt') as output_file:
        writer = csv.writer(output_file)
        for l in output_lines:
            writer.writerow(l)


if __name__ == '__main__':
    main()
