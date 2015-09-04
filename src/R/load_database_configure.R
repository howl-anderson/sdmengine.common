#!/usr/bin/env Rscript

#' load_database_configure
#' @param workshop_dir dir of workshop
#' @return a list of configure
#'
#' @export
load_database_configure <- function(workshop_dir) {
    # load configure from file
    configure_file_path <- file.path(workshop_dir,
                                     "configure",
                                     "database.yaml")
    configure <- yaml.load_file(configure_file_path)

    database_engine <- as.character(configure[["engine"]])
    db_host <- as.character(configure[['host']])
    database_name <- as.character(configure[['database']])
    user <- as.character(configure[['user']])
    password <- as.character(configure[['password']])

    configure <- list()
    configure[["engine"]] <- database_engine
    configure[["host"]] <- db_host
    configure[['database']] <- database_name
    configure[['user']] <- user
    configure[['password']] <- password

    return(configure)
}
