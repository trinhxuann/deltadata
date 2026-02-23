# Applies the joins in accordance to a relationship schema.

This function pulls the system table within an Access database that
dictates the join order and types. It will then attempt to apply those
joining rules to the provided tables. Users can specify which tables
they would like to join through the tables that they provide the
function. Ensure that the names of the tables provided matches those
required in the relationship table.

## Usage

``` r
schemaJoin(schema, data)
```

## Arguments

- schema:

  A schema table that mirrors the structure of an Access schema.

- data:

  A list of data tables to be joined

## Value

A data frame joined according to the relationship schema.

## Details

The join type in the relationship table from Access can be modified
directly if required. Simply add a `joinType` column to the relationship
table and specify one of four options: `full_join`, `inner_join`,
`left_join`, or `right_join`. The `data` argument can theoretically take
in any data frame. This allows for manipulations of the relational
tables if such operations are necessary before joining. Within the
schema table itself, `szReferencedObject` refers to the relational table
with the primary key, while `szObject` refers to the relational table
with the foreign key.

## Note

The tables in the 20mm dataset aren't sequentially linked. There are
tables that are linked together first before being linked back to the
original. How do I solve this?

## Examples

``` r
if (FALSE) { # \dontrun{
slsTables <- bridgeAccess(
"https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
tables = c("Catch", "FishCodes", "Lengths", "Meter Corrections",
"SLS Stations", "Tow Info", "Water Info")
)

schema <- bridgeAccess(
"https://filelib.wildlife.ca.gov/Public/Delta%20Smelt/SLS.zip",
tables = c("MSysRelationships"))[[1]]

schemaJoin(schema, slsTables)
} # }
```
