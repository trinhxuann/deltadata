# Save the data tables pulled from an Access database as .csv files

Save the data tables pulled from an Access database as .csv files

## Usage

``` r
saveRelationalTables(relationalTables, folderPath, fileNames = NULL)
```

## Arguments

- relationalTables:

  A list of the relational tables, ideally taken from
  [`bridgeAccess`](https://trinhxuann.github.io/deltadata/reference/bridgeAccess.md).

- folderPath:

  Path to the folder where the csv files will be saved.

- fileNames:

  A vector of file names (not file path). If `NULL`, will pull the names
  from the provided `relationalTables` argument. Defaults to `NULL`

## Value

Each table will be written. A metadata table will be returned indicating
success of the file writes.

## Details

This is a simple wrapper function that will save the relational tables
pulled from an Access database. The function was developed specifically
for the surveys within the Native Fish Program from CDFW, requiring the
UTF-8 encoding.

## Examples

``` r
if (FALSE) { # \dontrun{
slsTables <- bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
tables = c("Catch", "FishCodes"))

# Saving to the temporary directory
saveRelationalTables(slsTables, folderPath = tempdir())
} # }
```
