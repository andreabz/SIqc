#' SQLite DB for testing
#'
#' A database containing two tables:
#' \describe{
#'    \item{plan}{stores planned tasks for the laboratory QC;}
#'    \item{results}{stores results for the QC tests, simulating a laboratory LIMS.}
#' }
#'
#' @format a SQLite DB
#'
#' @name sqlite_test
#' @docType data
#' @keywords data
#' @import hms
#' @importFrom RSQLite SQLite
#' @importFrom here here
#' @importFrom DBI dbConnect dbWriteTable dbGetQuery dbExecute
# sqlite3 ~/Smartworking/07_laboratorio/SIqc/SIqc/data/sqlite_test.db
# CREATE TABLE plan(id integer NOT NULL,
#                   metodo text NOT NULL,
#                   attivita text NOT NULL,
#                   anno integer NOT NULL,
#                   mese_previsto text NOT NULL,
#                   data_effettiva text,
#                   operatore_previsto text NOT NULL,
#                   operatore_effettivo text,
#                   matrice text NOT NULL,
#                   esito integer);
conn <- DBI::dbConnect(RSQLite::SQLite(), "./data/sqlite_test.db", extended_types = TRUE)
plan_data <- read.csv2(here::here("data/registro.csv")) |> prepare_tasks_summary()
DBI::dbExecute(conn, "CREATE TABLE plan(
                id integer PRIMARY KEY AUTOINCREMENT,
                metodo text NOT NULL,
                attivita text NOT NULL,
                anno integer NOT NULL,
                mese_previsto text NOT NULL,
                data_effettiva date,
                operatore_previsto text NOT NULL,
                operatore_effettivo text,
                matrice text NOT NULL,
                esito integer
                );")
DBI::dbWriteTable(conn, "plan", plan_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM plan;")

