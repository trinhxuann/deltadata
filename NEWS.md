# deltadata 0.2.0

### New functions

*   `getCNRA()`: download data files from the CNRA data repository. Follows the workflow og `getEDI()`

### New features

*   `batcCDEC()`: support downloading data directly to the harddrive to be more robust against download failures, via `cacheDir` argument
*   `calcNearestCDEC()`: supports hydrological distance via `distMethod` argument (before only Euclidean). This argument is supported in functions that utilizes this function, e.g., `pullCDEC()` and `batchCDEC()`.
*   `getEDI()`: now supports the need for an Access Token to use the EDI API

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


