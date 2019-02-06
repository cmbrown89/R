#!/usr/bin/env Rscript


# GOAL: Accept txt file with barcode column first and sample names second column
# WHY: IDEMP needs reverse complements of barcodes

# First capture arguments from command line
args = commandArgs(trailingOnly = TRUE)

# First check for number of arguments
if(length(args) == 0){
  stop("USAGE: Provide a tab-delimited file with the barcodes in the first column and the corresponding sample names in the second column.\n\n How to run this script: Rscript --vanilla revcomp.R FILE")
}

# Read in file
barcode = read.delim(args[1], header = T)
#barcode = f

# Check that txt file has 2 columns 
if(dim(barcode)[2] != 2){
  stop("ERROR: You need to give me a file with the barcodes in the first column and the corresponding sample names in the second column")
}

# Barcodes will never have any numbers in them, so check that first col doesn't have nums
if(length(grep("\\d", barcode[,1])) != 0){
  stop("ERROR: Barcode column must be first, SampleID column second")
}


# Check for presence of seqinr package
if(library(seqinr, logical.return = T) != TRUE){
  print("Package seqinr is required, installing now.")
  install.packages("seqinr")
}

# Read in necesary package
library(seqinr) 

# Convert barcode col to characters 
barcode[,1] = as.character(barcode[,1])

# Get reverse complement of barcodes 
# This was figured out by Albert Barberan
barcode$Barcode_revcomp = sapply(barcode[,1], function(x){toupper(c2s(rev(comp(s2c(x)))))})

# Build dataframe
barcode_revcomp = data.frame("Barcode" = barcode$Barcode_revcomp, "SampleID" = barcode[,2])

# Write dataframe to table 
write.table(barcode_revcomp, "reformatted_barcodes.txt", sep = "\t", quote = F)

# Print success message
print("Reversed barcodes are in reformatted_barcodes.txt")


