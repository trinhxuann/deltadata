# Grab metadata from an EDI package webpage

Grab metadata from an EDI package webpage

## Usage

``` r
getMetadataEdi(url, version = "newest", all = FALSE)
```

## Arguments

- url:

  URL to the EDI data package landing page

- version:

  Defaults to pulling the newest version. Specify a number if you are
  interested in a specific version

- all:

  Defaults to FALSE. If TRUE, will return the XML file of the metadata
  itself. If FALSE, provides only an opinionated set of parameters

## Value

One of the following depending on all:

- list:

  A list containing a data.frame of opinionated metadata, the package
  title, and the publication date of the package

- xml:

  The EML XML file

## Details

Only an opinionated set of metadata parameters will be returned by
default, read from the EML XML metadata file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Data package of the CDFW IEP SLS Survey
getMetadataEdi("https://portal.edirepository.org/nis/mapbrowse?packageid=edi.534.9")
} # }
```
