# Pulling CDEC gage metadata

A function to pull the metadata table associated with a CDEC gage.

## Usage

``` r
pullMetadataCDEC(cdecGage, maxAttempt = 3, timeout = 60, verbose = TRUE)
```

## Arguments

- cdecGage:

  Name of the CDEC gage, a singular value

- maxAttempt:

  Number of times to retry a scrape. Defaults to 3.

- timeout:

  Seconds before a connection is terminated. Defaults to 60.

- verbose:

  Should the function annotate its progress? Defaults to TRUE.

## Value

A list containing the location and sensor metadata of a CDEC station.

## Examples

``` r
if (FALSE) { # \dontrun{
pullMetadataCDEC("MAL")
} # }
```
