# Changelog

## deltadata 0.1.0

First major release of the package, focusing on making the various CDEC
functions more robust.

#### New functions

- [`batchCDEC()`](https://github.com/trinhxuann/deltadata/reference/batchCDEC.md):
  New function to handle downloading very large datasets from CDEC by
  automatically paginating requests.

#### New features

- [`pullCDEC()`](https://github.com/trinhxuann/deltadata/reference/pullCDEC.md):
  1.  allow automatic retries of a server request.
  2.  ability to request a sparser duration if the specified duration
      information is missing (`fallbackDuration` argument).
  3.  decrease interactive nature of the function, will now simply
      return the metadata table if arguments are missing.
  4.  added pagination logic (see batchCDEC).
- [`calcNearestCDEC()`](https://github.com/trinhxuann/deltadata/reference/calcNearestCDEC.md):
  officially incorporated the ability to specify nearest station (`n`
  argument).
  1.  can specify specific sensor of interest via `sensor`.
  2.  changed default distance calculation function to increase speed of
      operation, `method` argument.
  3.  output now consistently return a flattened data frame, keeping
      each sampling location format intact.
- [`pullMetadataCDEC()`](https://github.com/trinhxuann/deltadata/reference/pullMetadataCDEC.md):
  no longer relies on pullCDEC() to grab the metadata table.
  1.  now returns the location table and the sensor metadata table, as a
      list output

#### Updates

- `deltadata:::cdecMetadata` and `deltadata:::cdecStation` updated.

#### Deprecation

- [`calcNthNearestCDEC()`](https://github.com/trinhxuann/deltadata/reference/calcNthNearestCDEC.md)
  is now deprecated and will be removed in a future version. Please use
  [`calcNearestCDEC()`](https://github.com/trinhxuann/deltadata/reference/calcNearestCDEC.md)
  instead.
