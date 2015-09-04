#!/usr/bin/env Rscript

#' get_cpu_cores
#' @param workshop_dir Directory of workshop
#' @return CPU cores that can be used
#'
#' @export
get_cpu_cores <- function(workshop_dir) {
    configure <- load_configure_file(workshop_dir)
    cpu_setting <- configure$cpu

    if (is.null(cpu_setting)) {
        # default reserved one CPU core
        cpu_setting <- -1
    }

    if (cpu_setting > 0) {
        cpu_number <- cpu_setting
    } else {
        cpu_number <- detectCores() - abs(cpu_setting)
        if (cpu_number <= 0) {
            stop('Error: CPU reserved too much, there are no CPU to use.')
        }
    }

    return(cpu_number)
}
