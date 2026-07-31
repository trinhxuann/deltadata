# This script creates the waterMask.tif that is included with the package for
# `calcNearestCDEC()`

# Download DEM. Will use DWR's LiDAR DEM file
# https://data.cnra.ca.gov/dataset/san-francisco-bay-and-sacramento-san-joaquin-delta-dem-for-modeling-version-4-3
# File: dem_bay_delta_10m_20250312.tif

demURI <- "https://data.cnra.ca.gov/dataset/f902e012-7d8d-429c-8a1a-2bf5b4312532/resource/9f931e1a-b009-4e91-bde1-795e36536922/download/dem_bay_delta_10m_20250312.zip"
downloadDEM(url = demURI, destDir = "inst/extdata/", asMask = T)
