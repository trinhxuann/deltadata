# Connect to an Access database

Create the connection to an Access database and pull the requested
tables. This function will attempt to account for mismatched
architectures (R vs Microsoft Access) but will take longer to execute if
so.

## Usage

``` r
bridgeAccess(file, tables = "check", path32 = "default", ...)
```

## Arguments

- file:

  File path to the Access database file. Can be a path to a hard drive
  or a URL.

- tables:

  A vector of table names to determine which relational tables to pull.
  This can be left blank to provide a list of options. If a system table
  is provided, you may need to provide read permission before it will
  work. This has to be done in the Access DB itself. Open the file,
  select "Enable Content" if prompted, `Ctrl + G`, paste in the
  "Immediate" window:
  `CurrentProject.Connection.Execute "GRANT SELECT ON MSysRelationships TO Admin;"`,
  and run the command by pressing `Enter` before exiting Access
  database.

- path32:

  File path to your 32 bit R executable, `Rscript.exe`. Only needed if
  you're using 32-bit Office.

- ...:

  Additional arguments to be passed onto `connectAccess()`. Used to pass
  on a specific driver if the default Access driver does not work, a
  user name, or password.

## Value

A list of relational tables read from the Access database connection.

## Examples

``` r
if (FALSE) { # \dontrun{
bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip")

bridgeAccess("https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
tables = c("Catch", "FishCodes", "Lengths", "Meter Corrections",
"SLS Stations", "Tow Info", "Water Info"))
} # }
```
