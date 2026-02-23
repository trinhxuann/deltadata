# Get full taxonomic information from ITIS

Get full taxonomic information from ITIS

## Usage

``` r
getTaxonomyFromItis(taxonNames, verbose = TRUE)
```

## Arguments

- taxonNames:

  A vector of species names of their Taxonomic Species Number (TSN).
  Supports different ranks as input. See details.

- verbose:

  Logical. Defaults to TRUE. Should progress notifications be printed in
  the console?

## Value

A data frame

## Details

Only taxonomic rank information above the inputted value will be
provided, e.g., if you provide a value at the phylum level, only the
kingdom-associated levels will be returned. Additionally, there may be
different TSNs associated with an input value, one being valid and the
other not. Both will be returned. Users should take care to check the
'validity' column for additional filtering criteria.

## Examples

``` r
if (FALSE) { # \dontrun{
taxa <- c("Chinese Mitten Crab", "Liparis", "Oncorhynchus tshawytscha")

getTaxonomyFromItis(taxa)
} # }
```
