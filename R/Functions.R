#' Funktion zum Einlesen von Daten aus verschiedenen Dateiformaten.
#'
#' Nimmt den Pfad der Datei entgegen und liest die Datei ein.
#'
#' @param file_path Pfad zur Datei.
#' @return Gibt die eingelesen Tabelle zurück.
#' @examples
#' # Basic example
#' table <- read_data_file("C:/Users/name/Desktop/data/Data.csv")
#' reads the data from the specified path.
#' @export
read_data_file <- function(file_path) {

  file_extension <- tools::file_ext(file_path)

  data <- switch(tolower(file_extension),
                 "csv" = read.csv(file_path),
                 "tsv" = .handle_tsv(file_path),
                 "xls" = .handle_excel(file_path),
                 "xlsx" = .handle_excel(file_path),
                 "json" = .handle_json(file_path),
                 "rds" = .handle_rds(file_path),
                 stop("Unsupported file format: ", file_extension))
  return(data)
}

#' Funktion zum schreiben von Daten in verschiedenen Dateiformaten.
#'
#' Nimmt einen Pfad und eine Tabelle entgegen und schreibt die Tabelle in die Datei.
#'
#' @param data Tabelle die geschrieben werden soll.
#' @param file_path Pfad zur Datei.
#' @return erstellt eine Date bei dem angegebenen Pfad, im angegebenen Format.
#' @examples
#' # Basic example
#' write_data_file(data = data, file_path = "C:/Users/name/Desktop/data/Data.csv")
#' writes the data table to the specified path in the specified format.
#' @export
write_data_file <- function(data, file_path) {

  file_extension <- tools::file_ext(file_path)

  switch(tolower(file_extension),
         "csv" = write.csv(data, file_path, row.names = FALSE),
         "tsv" = .write_tsv(data, file_path),
         "xls" = .write_excel(data, file_path),
         "xlsx" = .write_excel(data, file_path),
         "json" = .write_json(data, file_path),
         "rds" = .write_rds(data, file_path),
         stop("Unsupported file format: ", file_extension))

  message("Data successfully written to ", file_path)
}

# The Methods below are used as private functions and should not be called directly.
# That is why i dont document them.

.handle_tsv <- function(file_path) {
  data <- read.table(file_path, header = TRUE, sep = "\t")
  return(data)
}

.write_tsv <- function(data, file_path) {
  write.table(data, file_path, sep = "\t", row.names = FALSE, col.names = TRUE)
}

.handle_excel <- function(file_path) {
  library(readxl)
  data <- readxl::read_excel(file_path)
  class(data) <- "data.frame"
  return(data)
}

.write_excel <- function(data, file_path) {
  library(openxlsx)
  openxlsx::write.xlsx(data, file_path)
}

.handle_json <- function(file_path) {
  library(jsonlite)
  data <- fromJSON(file_path)
  return(data)
}

.write_json <- function(data, file_path) {
  library(jsonlite)
  jsonlite::write_json(data, file_path)
}

.handle_rds <- function(file_path) {
  data <- readRDS(file_path)
  return(data)
}

.write_rds <- function(data, file_path) {
  saveRDS(data, file_path)
}
