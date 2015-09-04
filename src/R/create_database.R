#!/usr/bin/env Rscript

#' create_database
#'
#' create database for user according to configure file.
#' if only the user have database permission to create database.
#'
#' @param workshop_dir Directory dir of workshop
#' @return NULL
#' @export
create_database <- function(workshop_dir) {
    engine_list <- c('MySQL', 'PostgreSQL')

    db_config <- load_database_configure(workshop_dir)
    engine <- db_config$engine
    db_host <- db_config$host
    db_name <- db_config$database
    user <- db_config$user
    password <- db_config$password

    if (! engine %in% engine_list) {
        message <- sprintf('Database engine only support: %s.',
                           str(engine_list))
        stop(message)
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
                           dbname=NULL)

    sql_string_base <- 'CREATE DATABASE `%s` CHARACTER SET utf8 COLLATE utf8_general_ci;'
    sql_string <- sprintf(sql_string_base,
                          db_name)

    db_result <- dbSendQuery(db_object, sql_string)
    dbClearResult(db_result)

    dbDisconnect(db_object)

    return(NULL)
}
