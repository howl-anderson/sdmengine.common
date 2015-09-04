#!/usr/bin/env Rscript

clean_configure <- function (app_list, default_list) {
    default_list_names <- names(default_list)
    app_list_names <- names(app_list)
    result_list <- list()
    for (name in default_list_names) {
        if (name %in% app_list_names) {
            app_value <- app_list[[name]]
            result_list[[name]] <- app_value
        } else {
            default_value <- default_list[[name]]
            result_list[[name]] <- default_value
        }
    }

    return(result_list)
}


#' load_configure_file
#'
#' Load configure file into list
#'
#' @param workshop_dir Dir of workshop
#' @return A list of configure
#'
#' @export
load_configure_file <- function(workshop_dir) {
    # load configure from file
    configure_file_path <- file.path(workshop_dir,
                                     "configure",
                                     "configure.yaml")
    configure <- yaml.load_file(configure_file_path)

    # load default configure from file
    default_configure_file_path <- file.path(system.file(package='sdmengine.common'),
                                             '/configure/default-configure.yaml')
    default_configure <- yaml.load_file(default_configure_file_path)

    # clean configure variable
    configure <- clean_configure(configure, default_configure)

    # read configure and translate to R type configure
    # <--

    # engine item
    modelling_engine <- as.character(configure[["engine"]])

    # CPU item
    cpu <- configure[["cpu"]]

    # species_name item
    if (typeof(configure[["species_name"]]) == "logical") {
        if (configure[["species_name"]] == TRUE) {
            species_dir <- file.path(workshop_dir, "species")
            species_list <- list.dirs(species_dir,
                                      full.names=FALSE,
                                      recursive=FALSE)
            species_name <- species_list
        } else {
            msg <- "FALSE is a illegal value for species_name item."
            stop(msg)
        }
    } else {
        species_name <- as.character(configure[["species_name"]])
    }

    # run_times item
    run_times <- as.integer(configure[["run_times"]])

    # predict item
    if (typeof(configure[["predict"]]) == "logical") {
        if (configure[["predict"]] == TRUE) {
            environment_dir <- file.path(workshop_dir, "environment")
            environment_list <- list.dirs(environment_dir,
                                          full.names=FALSE,
                                          recursive=FALSE)
            # remove base
            environment_list <- environment_list[environment_list != "base"]
            predict_environment_set <- environment_list
        } else {
            predict_environment_set <- NULL
        }
    } else {
        predict_environment_set <- configure[["predict"]]
    }
    if (length(predict_environment_set) == 0) {
        predict_environment_set <- NULL
    }

    # environment_layer item
    if (typeof(configure[["environment_layer"]]) == "logical") {
        if (configure[["environment_layer"]]) {
            base_environment_set_path <- file.path(workshop_dir,
                                                   "base_environment")
            files <- list.files(path=base_environment_set_path,
                                pattern="*.bil$",
                                full.names=FALSE)
            name_data = strsplit(files, split=".", fixed=TRUE)
            name_list = lapply(X=name_data,
                               FUN=function(file_name){return(file_name[1])})
            environment_layer <- unlist(name_list)
        } else {
            msg <- "FALSE is a illegal value for environment_layer item."
            stop(msg)
        }
    } else {
        environment_layer <- configure[["environment_layer"]]
    }

    # backgroud_point item
    backgroud.point <- as.integer(configure[["backgroud_point"]])
    if (length(backgroud.point) == 0) {
        backgroud.point = NULL
    }

    # presence_point item
    presence.point <- as.integer(configure[["presence_point"]])
    if (length(presence.point) == 0) {
        presence.point = NULL
    }

    # algorithms item
    algorithms.list <- configure[["algorithms"]]
    # -->

    # assemble R type configure list
    # <--
    configure <- list()
    configure[["engine"]] <- modelling_engine
    configure[["cpu"]] <- cpu
    configure[["species_name"]] <- species_name
    configure[["run_times"]] <- run_times
    if (is.null(predict_environment_set)) {
        configure["predict"] <- list(NULL)
    } else {
        configure[["predict"]] <- predict_environment_set
    }
    configure[["environment_layer"]] <- environment_layer
    if (is.null(backgroud.point)) {
        configure["backgroud_point"] <- list(NULL)
    } else {
        configure[["backgroud_point"]] <- backgroud.point
    }
    if (is.null(presence.point)) {
        configure["presence_point"] <- list(NULL)
    } else {
        configure[["presence_point"]] <- presence.point
    }
    configure[["algorithms"]] <- algorithms.list
    # -->

    return(configure)
}
