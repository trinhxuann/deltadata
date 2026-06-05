# deltadata 0.1.1

### Bug fixes

*   Fixed a bug in which 32-bit R would not correctly launch from 64-bit R (#11)
*   `bridgeAccess()` no longer uses `download.file()`. Solves SSL and corrupted file issues (#3)

# deltadata 0.1.0

First major release of the package, focusing on making the various CDEC functions more robust.

### New functions 

*   `batchCDEC()`: New function to handle downloading very large datasets from CDEC by automatically paginating requests.

### New features

*   `pullCDEC()`: 
      1. allow automatic retries of a server request. 
      2. ability to request a sparser duration if the specified duration information is missing (`fallbackDuration` argument).
      3. decrease interactive nature of the function, will now simply return the metadata table if arguments are missing.
      4. added pagination logic (see batchCDEC).
*   `calcNearestCDEC()`: officially incorporated the ability to specify nearest station (`n` argument).
      1. can specify specific sensor of interest via `sensor`. 
      2. changed default distance calculation function to increase speed of operation, `method` argument. 
      3. output now consistently return a flattened data frame, keeping each sampling location format intact. 
*   `pullMetadataCDEC()`: no longer relies on pullCDEC() to grab the metadata table. 
      1. now returns the location table and the sensor metadata table, as a list output

### Updates

*   `deltadata:::cdecMetadata` and `deltadata:::cdecStation` updated.

### Deprecation

*   `calcNthNearestCDEC()` is now deprecated and will be removed in a future version. Please use `calcNearestCDEC()` instead.


