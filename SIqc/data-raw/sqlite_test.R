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
conn <- DBI::dbConnect(RSQLite::SQLite(), "./data/sqlite_test.db", extended_types = TRUE)

#### plan table ----
plan_data <- read.csv2(here::here("data/registro.csv"))
DBI::dbExecute(conn, "CREATE TABLE plan(
                id_plan integer PRIMARY KEY AUTOINCREMENT,
                id_metodo integer NOT NULL,
                id_attivita integer NOT NULL,
                id_anno integer NOT NULL,
                id_mese text NOT NULL,
                id_matrice integer NOT NULL,
                data_effettiva date,
                operatore_previsto text NOT NULL,
                operatore_effettivo text,
                esito integer
                );")
DBI::dbWriteTable(conn, "plan", plan_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM plan;")

#### attivita table ----
attivita_data <- data.frame(attivita = c("ripetibilità", "giustezza", "pt"))
DBI::dbExecute(conn, "CREATE TABLE attivita(
                id_attivita integer PRIMARY KEY AUTOINCREMENT,
                attivita text NOT NULL
                );")
DBI::dbWriteTable(conn, "attivita", attivita_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM attivita;")

#### mese table ----
mese_data <- data.frame(mese = c("gennaio", "febbraio", "marzo",
               "aprile", "maggio", "giugno",
               "luglio", "agosto", "settembre",
               "ottobre", "novembre", "dicembre",
               "non previsto"))
DBI::dbExecute(conn, "CREATE TABLE mese(
                id_mese integer PRIMARY KEY AUTOINCREMENT,
                mese text NOT NULL
                );")
DBI::dbWriteTable(conn, "mese", mese_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM mese;")

#### anno table ----
anno_data <- data.frame(anno = c(2020:2054))
DBI::dbExecute(conn, "CREATE TABLE anno(
                id_anno integer PRIMARY KEY AUTOINCREMENT,
                anno integer NOT NULL
                );")
DBI::dbWriteTable(conn, "anno", anno_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM anno;")

#### metodo table ----
metodo_data <- data.frame(metodo = paste0("C6", sprintf("%02d", 1:99)))
DBI::dbExecute(conn, "CREATE TABLE metodo(
                id_metodo integer PRIMARY KEY AUTOINCREMENT,
                metodo text NOT NULL
                );")
DBI::dbWriteTable(conn, "metodo", metodo_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM metodo;")

#### campione table ----
campione_data <- data.frame(id_campione = c(5635, 5679, 7597, 10090, 5636,
                                           5680, 10091, 563500, 567900, 1009000,
                                           7598, 759700, 759701),
                           id_metodo = rep(1, 13))
DBI::dbExecute(conn, "CREATE TABLE campione(
                id_campione integer PRIMARY KEY,
                id_metodo integer NOT NULL
                );")
DBI::dbWriteTable(conn, "campione", campione_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM campione;")

#### tipo table ----
tipo_data <- data.frame(tipo = c("reale", "simulato (spike)",
                                 "matrice certificata", "pt", "altro"))
DBI::dbExecute(conn, "CREATE TABLE tipo(
                id_tipo integer PRIMARY KEY AUTOINCREMENT,
                tipo text NOT NULL
                );")
DBI::dbWriteTable(conn, "tipo", tipo_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM tipo;")

#### matrice table ----
matrice_data <- data.frame(matrice =
                c("acqua incognita",
                  "acque",
                  "acque d.c.u.",
                  "acque d.c.u. pozzi (sotterranee ad uso potabile)",
                  "acque di condotta fognaria",
                  "acque di mare",
                  "acque di piezometro",
                  "acque di pioggia",
                  "acque di scarico",
                  "acque di transizione",
                  "acque dolci di piscina",
                  "acque marine di piscina",
                  "acque minerali",
                  "acque sotterranee",
                  "acque superficiali",
                  "acque superficiali ad uso potabile",
                  "acque termali",
                  "alimenti",
                  "aria ambiente",
                  "aria emissioni convogliate",
                  "bigiotteria",
                  "biota",
                  "carboni",
                  "cemento",
                  "ceneri",
                  "colla",
                  "cosmetici",
                  "derivati petroliferi",
                  "estratto",
                  "fall out",
                  "fanghi dei depuratori",
                  "fibra tessile",
                  "gas interstiziali",
                  "incensi",
                  "inerti",
                  "matrice liquida",
                  "matrice solida",
                  "nastro adesivo",
                  "percolato di discarica",
                  "pietre",
                  "piumini",
                  "prodotti per l'igiene personale",
                  "prodotto petrolifero",
                  "reticelle lampade a gas",
                  "rifiuti",
                  "rifiuto arpal terre",
                  "scarichi ospedalieri",
                  "sedimenti",
                  "soluzioni dialisi",
                  "sorgente radioattiva liquida",
                  "sorgente radioattiva solida",
                  "sorgenti radioattive",
                  "stupefacenti",
                  "superfici",
                  "terreno",
                  "varie")
)

DBI::dbExecute(conn, "CREATE TABLE matrice(
                id_matrice integer PRIMARY KEY AUTOINCREMENT,
                matrice text NOT NULL
                );")
DBI::dbWriteTable(conn, "matrice", matrice_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM matrice;")

#### registro table ----
DBI::dbExecute(conn, "CREATE TABLE registro(
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

DBI::dbExecute(conn, "INSERT INTO registro
                       SELECT metodo,
                              attivita,
                              anno,
                              mese AS mese_previsto,
                              data_effettiva,
                              operatore_previsto,
                              operatore_effettivo,
                              matrice,
                              esito FROM plan
                         INNER JOIN metodo ON plan.id_metodo = metodo.id_metodo
                         INNER JOIN attivita ON plan.id_attivita = attivita.id_attivita
                         INNER JOIN anno ON plan.id_anno = anno.id_anno
                         INNER JOIN mese ON plan.id_mese = mese.id_mese
                         INNER JOIN matrice ON plan.id_matrice = matrice.id_matrice;")
DBI::dbGetQuery(conn, "SELECT * FROM registro;")

#### risultati table ----
risultati_data <- read.csv(here::here("data/risultati.csv"))
DBI::dbExecute(conn, "CREATE TABLE risultati(
                id_risultati integer PRIMARY KEY AUTOINCREMENT,
                id_plan integer NOT NULL,
                id_tipo integer NOT NULL,
                id_matrice integer NOT NULL,
                data_effettiva date NOT NULL,
                id_campione integer NOT NULL,
                operatore_effettivo text NOT NULL,
                parametro text NOT NULL,
                udm text NOT NULL,
                valore real NOT NULL,
                incertezza real,
                argomento text NOT NULL
                );")
DBI::dbWriteTable(conn, "risultati", risultati_data, append = TRUE)
DBI::dbGetQuery(conn, "SELECT * FROM risultati;")

#### TODO ####
## usare id_plan per unire risultati e plan
## prendere data_effettiva e operatore_effettivo da risultati e metterlo in plan
####

DBI::dbDisconnect(conn)
