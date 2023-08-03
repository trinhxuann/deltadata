Args <- commandArgs(T)

file <- Args[1]
rBit <- Args[3]
officeBit <- Args[4]
out <- Args[5]
retry <- Args[6]

tables <- Args[7:length(Args)]

con <- deltadata:::connectAccess(file)

deltadata:::extractTables(con = con,
                          tables = tables,
                          rBit = rBit,
                          officeBit = officeBit,
                          out = out,
                          retry = retry)

