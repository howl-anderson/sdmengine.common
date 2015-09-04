#!/usr/bin/env Rscript

#' load_species_configure
#' @param workshop_dir Dir of workshop
#' @param species_name Name of species which user want to load configure
#' @return a list of configure
#'
#' @export
load_species_setting <- function(workshop_dir, species_name) {
    # load configure from file
    configure_file_path <- file.path(workshop_dir,
                                     "species",
                                     species_name,
                                     "setting.yaml")

    if (! file.exists(configure_file_path)) {
        background <- NULL
        presence <- NULL
    } else {
        configure <- yaml.load_file(configure_file_path)

        background <- as.character(configure[["background"]])
        presence <- as.character(configure[['presence']])
    }

    configure <- list()
    configure[["background"]] <- background
    configure[["presence"]] <- presence

    return(configure)
}
