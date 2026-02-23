# Mock schema data frame
mock_schema <- data.frame(
  szRelationship = c("Rel1", "Rel2"),
  szObject = c("TableB", "TableC"),
  szReferencedObject = c("TableA", "TableB"),
  szColumn = c("FK_A", "FK_B"),
  szReferencedColumn = c("ID_A", "ID_B"),
  grbit = c(1, 16777216), # inner join, left join
  stringsAsFactors = FALSE
)

# Mock data tables
table_a <- data.frame(ID_A = 1:3, A_data = letters[1:3])
table_b <- data.frame(ID_B = 1:3, FK_A = 1:3, B_data = LETTERS[1:3])
table_c <- data.frame(ID_C = 1:2, FK_B = 1:2, C_data = paste0("c", 1:2))

mock_data_list <- list(
  TableA = table_a,
  TableB = table_b,
  TableC = table_c
)

test_that("translateSchema processes schema correctly", {
  # Using the mock_schema defined above
  translated <- deltadata:::translateSchema(mock_schema)

  # Check column names
  expect_true(all(c("foreignKeys", "primaryKeys", "foreignTable", "primaryTable", "joinType", "joinFunction") %in% names(translated)))

  # Check join types
  expect_equal(translated$joinType, c("inner_join", "left_join"))

  # Check that joinFunction is a list of functions
  expect_type(translated$joinFunction, "list")
  expect_true(all(sapply(translated$joinFunction, is.function)))
})

test_that("orderSchema orders the schema correctly for joining", {
  # Using a more complex schema for ordering
  schema_for_ordering <- data.frame(
    szRelationship = c("Rel1", "Rel2", "Rel3"),
    szObject = c("TableB", "TableC", "TableD"),
    szReferencedObject = c("TableA", "TableB", "TableC"),
    szColumn = c("ID_A", "ID_B", "ID_C"),
    szReferencedColumn = c("ID", "ID", "ID"),
    grbit = c(1, 1, 1),
    stringsAsFactors = FALSE
  )

  # Translate it first to get the correct column names
  translated_schema <- deltadata:::translateSchema(schema_for_ordering)

  # Test ordering
  ordered <- deltadata:::orderSchema(translated_schema, providedTables = c("TableA", "TableB", "TableC", "TableD"))

  # The order should be a valid join order starting from TableA
  expect_equal(ordered$primaryTable[1], "TableA")
  expect_setequal(ordered$primaryTable, c("TableA", "TableB", "TableC"))
  expect_setequal(ordered$foreignTable, c("TableB", "TableC", "TableD"))
})

test_that("schemaJoin joins tables correctly based on schema", {
  # Using the mock schema and data list from above

  joined_data <- schemaJoin(mock_schema, mock_data_list)

  # Check final dimensions
  expect_equal(nrow(joined_data), 3)
  expect_equal(ncol(joined_data), 6)
  expect_true(all(c("ID_A", "A_data", "ID_B", "B_data", "ID_C", "C_data") %in% names(joined_data)))

  # Check the NAs from the left join
  expect_true(is.na(joined_data$ID_C[3]))
  expect_true(is.na(joined_data$C_data[3]))

  # Check a value
  expect_equal(joined_data$A_data[1], "a")
  expect_equal(joined_data$B_data[2], "B")
  expect_equal(joined_data$C_data[1], "c1")
})
