#!/usr/bin/env Rscript

# load libraries
library(argparser)

args.parser <- arg_parser(description = "This script takes a list of PDB chain IDs and curls their 
                   corresponding UniProt sequences.")
args.parser <- add_argument(parser = args.parser, arg = "--input", help = "input file containing a 
                            list of PDB chain IDs, for example 2bl2A")
# args.parser <- add_argument(parser = args.parser, arg = "--output", help = "file where to write the 
#                             mapping of PDB chain IDs to UniProt IDs.")
args <- parse_args(parser = args.parser)

# load the pdb_chain_uniprot.tsv file
db <- read.table(file = "pdb_chain_uniprot.tsv", sep = "\t", header = TRUE, stringsAsFactors = FALSE)
is_duplicated <- duplicated(db[1:2])
db <- db[!is_duplicated,]
# set row names for fast subsetting
rownames(db) <- paste(db[[1]], db[[2]], sep = "")

# load the list of PDB chain IDs
chain_ids <- read.table(file = args$input, header = FALSE, stringsAsFactors = FALSE)
# get UniProt IDs
uniprot_ids <- db[as.character(chain_ids[[1]]), ]

# curl UniProt sequence for each PDB chain
for(chain in rownames(uniprot_ids)) {
  uniprot_id <- uniprot_ids[chain, 3]
  uniprot_url <- paste("http://www.uniprot.org/uniprot/", uniprot_id, ".fasta", sep = "")
  download.file(url = uniprot_url, destfile = paste(chain, "_uniprot.fasta", sep = ""), method = "curl", quiet = FALSE)
}
