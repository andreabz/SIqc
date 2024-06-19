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


##### Loading data for the tables ----
pianificazione_csv <- read.csv2(here::here("data/pianificazione.csv"))
risultato_csv <- read.csv2(here::here("data/risultato.csv"))
ripetibilita_csv <- read.csv2(here::here("data/ripetibilita.csv"))
esito_csv <- read.csv2(here::here("data/esito.csv"))
parametro_csv <- read.csv2(here::here("data/parametro.csv"))
attivita_csv <- read.csv2(here::here("data/attivita.csv"))
anno_csv <- read.csv2(here::here("data/anno.csv"))
mese_csv <- read.csv2(here::here("data/mese.csv"))
tipo_campione_csv <- read.csv2(here::here("data/tipo_campione.csv"))
matrice_csv <- read.csv2(here::here("data/matrice.csv"))
campione_csv <- read.csv2(here::here("data/campione.csv"))
metodo_csv <- read.csv2(here::here("data/metodo.csv"))
unita_misura_csv <- read.csv2(here::here("data/unita_misura.csv"))
giudizio_csv <- read.csv2(here::here("data/giudizio.csv"))
pianificazione_campione_csv <- read.csv2(here::here("data/pianificazione_campione.csv"))

#### pianificazione table ----
DBI::dbExecute(conn, "CREATE TABLE pianificazione(
                pianificazione_id integer PRIMARY KEY AUTOINCREMENT,
                metodo_id integer NOT NULL REFERENCES metodo(metodo_id),
                attivita_id integer NOT NULL REFERENCES attivita(attivita_id),
                anno_id integer NOT NULL REFERENCES anno(anno_id),
                mese_id text NOT NULL REFERENCES mese(mese_id),
                tipo_campione_id integer NOT NULL REFERENCES tipo_campione(tipo_campione_id),
                operatore_previsto text NOT NULL,
                azione text NOT NULL
                );")
DBI::dbWriteTable(conn, "pianificazione", pianificazione_csv, append = TRUE)

#### risultato table ----
DBI::dbExecute(conn, "CREATE TABLE risultato(
                risultato_id integer PRIMARY KEY AUTOINCREMENT,
                campione_id integer NOT NULL REFERENCES campione(campione_id),
                matrice_id integer NOT NULL REFERENCES matrice(matrice_id),
                parametro_id integer NOT NULL REFERENCES parametro(parametro_id),
                unita_misura_id integer NOT NULL REFERENCES unita_misura(unita_misura_id),
                data_effettiva date NOT NULL,
                operatore_effettivo text NOT NULL,
                valore real NOT NULL,
                incertezza real,
                codice_argomento text NOT NULL
                );")
DBI::dbWriteTable(conn, "risultato", risultato_csv, append = TRUE)

#### ripetibilita table ----
DBI::dbExecute(conn, "CREATE TABLE ripetibilita(
                ripetibilita_id integer PRIMARY KEY AUTOINCREMENT,
                campione1_id integer NOT NULL REFERENCES campione(campione_id),
                campione2_id integer NOT NULL REFERENCES campione(campione_id),
                pianificazione_id integer NOT NULL REFERENCES pianificazione(pianificazione_id),
                parametro_id integer NOT NULL REFERENCES parametro(parametro_id),
                esito_id integer NOT NULL REFERENCES esito(esito_id),
                differenza real,
                requisito real,
                differenza_su_requisito real
                );")
DBI::dbWriteTable(conn, "ripetibilita", ripetibilita_csv, append = TRUE)

#### esito table ----
DBI::dbExecute(conn, "CREATE TABLE esito(
                esito_id integer PRIMARY KEY AUTOINCREMENT,
                esito text NOT NULL
                );")
DBI::dbWriteTable(conn, "esito", esito_csv, append = TRUE)

#### parametro table ----
DBI::dbExecute(conn, "CREATE TABLE parametro(
                parametro_id integer PRIMARY KEY AUTOINCREMENT,
                parametro text NOT NULL
                );")
DBI::dbWriteTable(conn, "parametro", parametro_csv, append = TRUE)

#### attivita table ----
DBI::dbExecute(conn, "CREATE TABLE attivita(
                attivita_id integer PRIMARY KEY AUTOINCREMENT,
                attivita text NOT NULL
                );")
DBI::dbWriteTable(conn, "attivita", attivita_csv, append = TRUE)

#### anno table ----
DBI::dbExecute(conn, "CREATE TABLE anno(
                anno_id integer PRIMARY KEY AUTOINCREMENT,
                anno integer NOT NULL
                );")
DBI::dbWriteTable(conn, "anno", anno_csv, append = TRUE)

#### mese table ----
DBI::dbExecute(conn, "CREATE TABLE mese(
                mese_id integer PRIMARY KEY AUTOINCREMENT,
                mese text NOT NULL
                );")
DBI::dbWriteTable(conn, "mese", mese_csv, append = TRUE)

#### pianificazione_campione table ----
DBI::dbExecute(conn, "CREATE TABLE pianificazione_campione(
                pianificazione_campione_id integer PRIMARY KEY AUTOINCREMENT,
                pianificazione_id integer NOT NULL REFERENCES pianificazione(pianificazione_id),
                campione_id integer REFERENCES campione(campione_id)
                );")
DBI::dbWriteTable(conn, "pianificazione_campione", pianificazione_campione_csv, append = TRUE)

#### tipo_campione table ----
DBI::dbExecute(conn, "CREATE TABLE tipo_campione(
                tipo_campione_id integer PRIMARY KEY AUTOINCREMENT,
                tipo_campione text NOT NULL
                );")
DBI::dbWriteTable(conn, "tipo_campione", tipo_campione_csv, append = TRUE)

#### matrice table ----
DBI::dbExecute(conn, "CREATE TABLE matrice(
                matrice_id integer PRIMARY KEY AUTOINCREMENT,
                matrice text NOT NULL
                );")
DBI::dbWriteTable(conn, "matrice", matrice_csv, append = TRUE)

#### campione table ----
DBI::dbExecute(conn, "CREATE TABLE campione(
                campione_id integer PRIMARY KEY AUTOINCREMENT,
                campione text NOT NULL
                );")
DBI::dbWriteTable(conn, "campione", campione_csv, append = TRUE)

#### metodo table ----
DBI::dbExecute(conn, "CREATE TABLE metodo(
                metodo_id integer PRIMARY KEY AUTOINCREMENT,
                metodo text NOT NULL
                );")
DBI::dbWriteTable(conn, "metodo", metodo_csv, append = TRUE)

#### unita_misura table ----
DBI::dbExecute(conn, "CREATE TABLE unita_misura(
                unita_misura_id integer PRIMARY KEY AUTOINCREMENT,
                unita_misura text NOT NULL
                );")
DBI::dbWriteTable(conn, "unita_misura", unita_misura_csv, append = TRUE)

#### giudizio table ----
DBI::dbExecute(conn, "CREATE TABLE giudizio(
                giudizio_id integer PRIMARY KEY AUTOINCREMENT,
                pianificazione_id integer REFERENCES pianificazione(pianificazione_id),
                esito_id integer REFERENCES esito(esito_id),
                commento text
                );")
DBI::dbWriteTable(conn, "giudizio", giudizio_csv, append = TRUE)

#### ripetibilita_tmp table ----
DBI::dbExecute(conn, "CREATE TABLE ripetibilita_tmp(
                ripetibilita_id integer PRIMARY KEY,
                campione1 text NOT NULL REFERENCES campione(campione),
                campione2 text NOT NULL REFERENCES campione(campione),
                pianificazione_id integer NOT NULL REFERENCES pianificazione(pianificazione_id),
                parametro text NOT NULL REFERENCES parametro(parametro),
                esito text NOT NULL REFERENCES esito(esito),
                differenza real,
                requisito real,
                differenza_su_requisito real
                );")

#### giudizio_tmp table ----
DBI::dbExecute(conn, "CREATE TABLE giudizio_tmp(
                giudizio_id integer PRIMARY KEY AUTOINCREMENT,
                pianificazione_id integer REFERENCES pianificazione(pianificazione_id),
                esito text REFERENCES esito(esito),
                commento text
                );")

DBI::dbGetQuery(conn, "SELECT
                        metodo,
                        attivita,
                        anno,
                        mese AS mese_previsto,
                        data_effettiva,
                        operatore_previsto,
                        operatore_effettivo,
                        matrice,esito,
                        azione AS azioni
                      FROM pianificazione AS pz
                      INNER JOIN metodo
                        ON pz.metodo_id = metodo.metodo_id
                      INNER JOIN attivita
                        ON pz.attivita_id = attivita.attivita_id
                      INNER JOIN anno
                        ON pz.anno_id = anno.anno_id
                      INNER JOIN mese
                        ON pz.mese_id = mese.mese_id
                      LEFT JOIN
                        (SELECT DISTINCT
                          pc.pianificazione_id,
                          risultato.data_effettiva,
                          risultato.operatore_effettivo,
                          risultato.matrice_id
                         FROM risultato
                        INNER JOIN pianificazione_campione AS pc
                          ON risultato.campione_id = pc.campione_id) as eff
                      ON pz.pianificazione_id = eff.pianificazione_id
                      LEFT JOIN matrice
                        ON eff.matrice_id = matrice.matrice_id
                      LEFT JOIN giudizio
                        ON pz.pianificazione_id = giudizio.pianificazione_id
                      LEFT JOIN esito
                        ON giudizio.esito_id = esito.esito_id")


DBI::dbGetQuery(conn,
                "SELECT
                  rip.pianificazione_id,
                  par.parametro,
                  udm.unita_misura,
                  res1.valore AS campione1,
                  res2.valore AS campione2,
                  differenza,
                  requisito,
                  esito
                FROM ripetibilita AS rip
                INNER JOIN esito AS ex
                  ON rip.esito_id = ex.esito_id
                INNER JOIN parametro AS par
                  ON rip.parametro_id = par.parametro_id
                LEFT JOIN risultato AS res1
                  ON res1.campione_id = rip.campione1_id
                  AND res1.parametro_id = rip.parametro_id
                LEFT JOIN risultato AS res2
                  ON res2.campione_id = rip.campione2_id
                  AND res2.parametro_id = rip.parametro_id
                INNER JOIN unita_misura AS udm
                  ON res1.unita_misura_id = udm.unita_misura_id
                WHERE
                  (
                    (res1.campione_id = 3 AND res2.campione_id = 11)
                    OR (res2.campione_id = 3 AND res1.campione_id = 11)
                  )
                  AND rip.pianificazione_id = 1;")

DBI::dbGetQuery(conn,
                "SELECT
                              a.parametro_id,
                              a.unita_misura_id,
                              a.valore AS campione1,
                              b.valore AS campione2
                             FROM risultato AS a
                            LEFT JOIN (
                              SELECT campione_id, parametro_id, valore FROM risultato
                            ) AS b
                            ON a.parametro_id = b.parametro_id
                            WHERE a.campione_id = 12 AND b.campione_id = 8;")



#### TODO ####
## usare id_plan per unire risultati e plan
## prendere data_effettiva e operatore_effettivo da risultati e metterlo in plan
####

DBI::dbDisconnect(conn)
