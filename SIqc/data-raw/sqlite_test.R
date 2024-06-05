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
risultati_data <- read.csv(here::here("data/risultati.csv"))
DBI::dbWriteTable(conn, "risultati_tmp", risultati_data, append = TRUE)
campioni <- DBI::dbGetQuery(conn, "SELECT DISTINCT campione FROM risultati_tmp")
campione_data <- data.frame(campione = campioni,
                            id_metodo = rep(1, 13))
DBI::dbExecute(conn, "CREATE TABLE campione(
                id_campione integer PRIMARY KEY AUTOINCREMENT,
                campione text NOT NULL,
                id_metodo integer NOT NULL REFERENCES metodo(id_metodo)
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

#### plan table ----
plan_data <- read.csv2(here::here("data/registro.csv"))
DBI::dbWriteTable(conn, "plan_tmp", plan_data, append = TRUE)
DBI::dbExecute(conn, "CREATE TABLE plan(
                id_plan integer PRIMARY KEY AUTOINCREMENT,
                id_metodo integer NOT NULL REFERENCES metodo(id_metodo),
                id_attivita integer NOT NULL REFERENCES attivita(id_attivita),
                id_anno integer NOT NULL REFERENCES anno(id_anno),
                id_mese text NOT NULL REFERENCES mese(id_mese),
                id_matrice integer NOT NULL REFERENCES matrice(id_matrice),
                id_campione1 integer REFERENCES campione(id_campione),
                id_campione2 integer REFERENCES campione(id_campione),
                operatore_previsto text NOT NULL,
                esito integer
                );")
DBI::dbExecute(conn,
               "INSERT INTO plan(
                id_metodo,
                id_attivita,
                id_anno,
                id_mese,
                id_matrice,
                id_campione1,
                id_campione2,
                operatore_previsto,
                esito)
                SELECT
                  plan_tmp.id_metodo,
                  plan_tmp.id_attivita,
                  plan_tmp.id_anno,
                  plan_tmp.id_mese,
                  plan_tmp.id_matrice,
                  a.id_campione AS id_campione1,
                  b.id_campione AS id_campione2,
                  plan_tmp.operatore_previsto,
                  plan_tmp.esito
                 FROM plan_tmp
                  LEFT JOIN (SELECT id_campione, campione FROM campione) AS a
                    ON plan_tmp.campione1 = a.campione
                  LEFT JOIN (SELECT id_campione, campione FROM campione) AS b
                    ON plan_tmp.campione2 = b.campione;")
DBI::dbExecute(conn, "DROP TABLE plan_tmp;")

#### risultati table ----
DBI::dbExecute(conn, "CREATE TABLE risultati(
                id_risultati integer PRIMARY KEY AUTOINCREMENT,
                id_plan integer NOT NULL REFERENCES plan(id_plan),
                id_tipo integer NOT NULL REFERENCES tipo(id_tipo),
                id_matrice integer NOT NULL REFERENCES matrice(id_matrice),
                id_campione integer NOT NULL REFERENCES campione(id_campione),
                data_effettiva date NOT NULL,
                operatore_effettivo text NOT NULL,
                parametro text NOT NULL,
                udm text NOT NULL,
                valore real NOT NULL,
                incertezza real,
                argomento text NOT NULL
                );")

DBI::dbExecute(conn,
               "INSERT INTO risultati(
                  id_plan,
                  id_tipo,
                  id_matrice,
                  id_campione,
                  data_effettiva,
                  operatore_effettivo,
                  parametro,
                  udm,
                  valore,
                  incertezza,
                  argomento)
                SELECT
                  risultati_tmp.id_plan,
                  risultati_tmp.id_tipo,
                  matrice.id_matrice,
                  campione.id_campione,
                  risultati_tmp.data_effettiva,
                  risultati_tmp.operatore_effettivo,
                  risultati_tmp.parametro,
                  risultati_tmp.udm,
                  risultati_tmp.valore,
                  risultati_tmp.incertezza,
                  risultati_tmp.argomento
                FROM risultati_tmp
                  INNER JOIN matrice ON risultati_tmp.matrice = matrice.matrice
                  INNER JOIN campione ON risultati_tmp.campione = campione.campione;")


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
                              res.data_effettiva,
                              operatore_previsto,
                              res.operatore_effettivo,
                              matrice,
                              esito FROM plan
                         INNER JOIN metodo ON plan.id_metodo = metodo.id_metodo
                         INNER JOIN attivita ON plan.id_attivita = attivita.id_attivita
                         INNER JOIN anno ON plan.id_anno = anno.id_anno
                         INNER JOIN mese ON plan.id_mese = mese.id_mese
                         INNER JOIN matrice ON plan.id_matrice = matrice.id_matrice
                         LEFT JOIN (
                          SELECT DISTINCT id_plan, data_effettiva, operatore_effettivo FROM risultati
                          ) AS res
                          ON plan.id_plan = res.id_plan;")
DBI::dbGetQuery(conn, "SELECT * FROM registro;")

# get planned tasks ----
DBI::dbGetQuery(conn, "SELECT metodo,
                              attivita,
                              anno,
                              mese AS mese_previsto,
                              res.data_effettiva,
                              operatore_previsto,
                              res.operatore_effettivo,
                              matrice,
                              esito FROM plan
                         INNER JOIN metodo ON plan.id_metodo = metodo.id_metodo
                         INNER JOIN attivita ON plan.id_attivita = attivita.id_attivita
                         INNER JOIN anno ON plan.id_anno = anno.id_anno
                         INNER JOIN mese ON plan.id_mese = mese.id_mese
                         INNER JOIN matrice ON plan.id_matrice = matrice.id_matrice
                         LEFT JOIN (
                          SELECT DISTINCT id_plan, data_effettiva, operatore_effettivo FROM risultati
                          ) AS res
                          ON plan.id_plan = res.id_plan;")

# get planned task and sample id ----
DBI::dbGetQuery(conn, "SELECT metodo,
                              attivita,
                              anno,
                              mese AS mese_previsto,
                              operatore_previsto,
                              a.campione AS campione1,
                              b.campione AS campione2,
                              matrice,
                              esito FROM plan
                         INNER JOIN metodo ON plan.id_metodo = metodo.id_metodo
                         INNER JOIN attivita ON plan.id_attivita = attivita.id_attivita
                         INNER JOIN anno ON plan.id_anno = anno.id_anno
                         INNER JOIN mese ON plan.id_mese = mese.id_mese
                         INNER JOIN matrice ON plan.id_matrice = matrice.id_matrice
                         LEFT JOIN (
                          SELECT id_campione, campione FROM campione
                          ) AS a
                          ON plan.id_campione1 = a.id_campione
                         LEFT JOIN (
                          SELECT id_campione, campione FROM campione
                          ) AS b
                          ON plan.id_campione2 = b.id_campione;")

# get planned tasks and results ----
DBI::dbGetQuery(conn, "SELECT metodo,
                              attivita,
                              anno,
                              mese AS mese_previsto,
                              operatore_previsto,
                              a.parametro AS parametro,
                              a.valore AS campione1,
                              b.valore AS campione2,
                              matrice,
                              esito FROM plan
                         INNER JOIN metodo ON plan.id_metodo = metodo.id_metodo
                         INNER JOIN attivita ON plan.id_attivita = attivita.id_attivita
                         INNER JOIN anno ON plan.id_anno = anno.id_anno
                         INNER JOIN mese ON plan.id_mese = mese.id_mese
                         INNER JOIN matrice ON plan.id_matrice = matrice.id_matrice
                         LEFT JOIN (
                          SELECT id_campione, parametro, valore FROM risultati
                          ) AS a
                          ON plan.id_campione1 = a.id_campione
                         LEFT JOIN (
                          SELECT id_campione, parametro, valore FROM risultati
                          ) AS b
                          ON plan.id_campione2 = b.id_campione
                          AND a.parametro = b.parametro;")

DBI::dbExecute(conn, "DROP TABLE risultati_tmp")

#### TODO ####
## usare id_plan per unire risultati e plan
## prendere data_effettiva e operatore_effettivo da risultati e metterlo in plan
####

DBI::dbDisconnect(conn)
