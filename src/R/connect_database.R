#!/usr/bin/env Rscript

#' connection_database
#' @param workshop_dir Directory dir of workshop
#' @return Database connection object
#' @export
connect_database <- function(workshop_dir) {
    engine_list <- c('MySQL', 'PostgreSQL')

    db_config <- load_database_configure(workshop_dir)
    engine <- db_config$engine
    db_host <- db_config$host
    db_name <- db_config$database
    user <- db_config$user
    password <- db_config$password

    if (! engine %in% engine_list) {
        stop(sprintf('Database engine only support: %s.',  str(engine_list)))
    }

    package_name <- paste('R', engine, sep='')
    load_result <- library(package_name, character.only=TRUE, logical.return=TRUE)
    if (! load_result) {
        stop('Load database package failed, please check if the database package is installed.')
    }

    db_driver <- dbDriver(engine)
    db_object <- dbConnect(db_driver,
                           host=db_host,
                           username=user,
                           password=password,
                           dbname=db_name)

    return(db_object)
}
