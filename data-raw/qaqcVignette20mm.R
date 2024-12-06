# This script creates the 20 mm example data tables for the QAQC vignette

library(deltadata)

# --- Download ---
# Relational tables

# For now, will work from a local copy that I have just downloaded manually from
# the FTP website; IT is looking into why the CDFW FTP website is closed off
# from those outside the Department.

# Ideally, this is done programatically but cannot currently do this unless
# architectures are the same
tableNames20mm <- c("MSysAccessStorage", "MSysAccessXML", "MSysACEs",
                "MSysComplexColumns", "MSysNavPaneGroupCategories",
                "MSysNavPaneGroups", "MSysNavPaneGroupToObjects",
                "MSysNavPaneObjectIDs", "MSysObjects", "MSysQueries",
                "MSysRelationships", "MSysResources", "20mmStations",
                "FishCodes", "FishLength", "FishSample", "Gear",
                "GearCodesLkp", "MeterCorrections", "SampleCode", "Station",
                "Survey", "Tow", "ZooCodes", "ZooCount", "ZooSample")

tables20mm <- bridgeAccess(file = "C:\\Users\\TXNguyen\\Downloads\\20mm_New.zip",
                       tables = c("Station", "Survey", "Tow", "Gear", "20mmStations"))

# Relationship table
schema20mm <- bridgeAccess(
  "C:\\Users\\TXNguyen\\Downloads\\20mm_New.zip",
  tables = c("MSysRelationships"), retry = T
)[[1]]

