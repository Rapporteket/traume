library(RMariaDB)
library(DBI)

con <- dbConnect(RMariaDB::MariaDB(),
                 host = "db",
                 user = "root",
                 password = "root",
                 dbname = "traume"
                 )

mydata <- read.csv2("traume/datasett.csv")

dbWriteTable(
  con,
  name = "data",
  value = mydata,
  overwrite = TRUE,
  row.names = FALSE
)

Sys.setenv(MYSQL_HOST = "db")

query <- "
SELECT
  *
FROM
  data;
"

data <- rapbase::loadRegData("traume", query)

