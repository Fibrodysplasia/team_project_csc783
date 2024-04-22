# Install RSQLite if not already installed
if (!requireNamespace("RSQLite", quietly = TRUE)) {
  install.packages("RSQLite")
}

# Load the RSQLite package
library(RSQLite)

# Establish a connection to the SQLite database
db <- dbConnect(RSQLite::SQLite(), dbname = "Data/fires_raw.sqlite")

# Query the database, might take a second
query_result <- dbGetQuery(db, "SELECT * FROM Fires")

# The query_result is now a dataframe with the contents of 'Fires'
head(query_result)  # Verify this output with query_result.txt in Scripts

# Save the dataframe to disk as an R data file
saveRDS(query_result, file = "Data/fires_df.rds")

# Disconnect from the database
dbDisconnect(db)
