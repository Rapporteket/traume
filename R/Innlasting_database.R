library(RMariaDB)
library(DBI)

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "db",
                 user = "root",
                 password = "root",
                 dbname = "traume"
                 )
mydata <- iris


dbWriteTable(
  con,
  name = "Test",
  value = mydata,
  overwrite = TRUE,
  row.names = FALSE
)

Sys.setenv(MYSQL_HOST = "db")

query <- "
SELECT
  *
FROM
  Test;
"

data <- rapbase::loadRegData("traume", query)

