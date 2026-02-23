# Pull CDEC gage lat/lon

Pull CDEC gage lat/lon

## Usage

``` r
pullCoordinates(cdecGage, maxAttempt = 3, timeout = 60)
```

## Arguments

- cdecGage:

  Name of the gage of interest, as a character.

- maxAttempt:

  Number of attempts to retry the same pull

- timeout:

  Max duration to wait for a download, in seconds

## Value

A data frame containing the station name, lat, and lon.

## Examples

``` r
if (FALSE) { # \dontrun{
pullCoordinates("MAL")
} # }
```
