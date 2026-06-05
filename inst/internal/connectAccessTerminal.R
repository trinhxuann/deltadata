# use print() to print this if debugging from 64-bit R, e.g., print(Args)
Args <- commandArgs(T)

if (length(Args) < 1) {
  stop("Error: No parameter file provided.")
}

param_file <- Args[1]
if (!file.exists(param_file)) {
  stop("Error: Parameter file not found at: ", param_file)
}

# Load the parameters
params <- readRDS(param_file)

# Extract arguments
file <- params$file
rBit <- params$rBit
officeBit <- params$officeBit
out <- params$out
retry <- params$retry
tables <- params$tables
extraArgs <- params$extraArgs

# Robustly establish connection, incorporating any '...' arguments passed from the parent process
con <- do.call(deltadata:::connectAccess, c(list(path = file), extraArgs))

# Extract the requested tables
deltadata:::extractTables(
  con = con,
  tables = tables,
  rBit = rBit,
  officeBit = officeBit,
  out = out,
  retry = retry
)
