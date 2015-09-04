#!/usr/bin/env Rscript

#' import_sql
#'
#' Import SQL into database with a given SQL file.
#'
#' @param workshop Directory of workshop
#' @param sql_file The path of SQL file
#' @export
#' @return Boolean
import_sql <- function(workshop, sql_file) {
    db_session <- connect_database(workshop)
    lines <- readLines(sql_file)

    sql_string <- ""
    for (line in lines) {
        line <- str_trim(line)

        # skip it if it's a comment
        if (str_sub(line, 0, 2) == '--' || line == '') {
            next
        }

        # add this line to the current segment
        sql_string <- paste0(sql_string, line)
        # if it has a semicolon at the end, it's the end of the query
        if (substr(line, str_length(line), str_length(line)) == ';')
        {
            # execute the query
            res <- dbSendQuery(db_session, sql_string)
            dbClearResult(res)
            # reset temp variable to empty
            sql_string <- ""
        }
    }
    dbDisconnect(db_session)
}

