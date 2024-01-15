if(!require("RPostgres")){ # check for package existence 
  install.packages("RPostgres")
}
library("RPostgres")

con <- dbConnect(RPostgres::Postgres(), 
                 dbname = "postgres", 
                 host = "127.0.0.1", 
                 port = 5432, 
                 user = "postgres", 
                 password = '===')

setwd('C:/Users/Public/sql')
dbWriteTable(con, "new_banks_total_table", read.csv("banks_total_export.csv"))

result <- dbGetQuery(con, "SELECT COUNT(*) FROM new_banks_total_table")
print(result)

dbDisconnect(con)
