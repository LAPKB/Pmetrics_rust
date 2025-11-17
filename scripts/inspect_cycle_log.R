suppressPackageStartupMessages({
    library(jsonlite)
})

pm_print_str_limited <- function(obj, max_level = 2L, vec_len = 6L, max_lines = 80L) {
    output <- capture.output(str(obj, max.level = max_level, vec.len = vec_len, give.attr = FALSE))
    pm_cat_limited(output, max_lines)
}

pm_print_vector_preview <- function(values, max_items = 20L) {
    if (length(values) == 0) {
        cat("(none)\n")
        return(invisible(NULL))
    }
    shown <- values[seq_len(min(length(values), max_items))]
    suffix <- if (length(values) > max_items) sprintf(" ... (%d total)", length(values)) else ""
    cat(paste(shown, collapse = ", "), suffix, "\n", sep = "")
    invisible(NULL)
}

pm_cat_limited <- function(lines, max_lines = 80L) {
    if (length(lines) > max_lines) {
        omitted <- length(lines) - max_lines
        lines <- c(lines[seq_len(max_lines)], sprintf("... truncated %d additional lines ...", omitted))
    }
    cat(paste(lines, collapse = "\n"), "\n", sep = "")
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
    stop("Usage: Rscript inspect_cycle_log.R <rds-file>", call. = FALSE)
}
source_file <- args[[1]]
if (!file.exists(source_file)) {
    stop(sprintf("File not found: %s", source_file), call. = FALSE)
}
blob <- readRDS(source_file)
cycle_log <- blob$result$result$cycle_log
if (is.null(cycle_log)) {
    stop("cycle_log is NULL in the provided result", call. = FALSE)
}
cat("Top-level cycle_log structure:\n")
pm_print_str_limited(cycle_log)
entries <- cycle_log$cycles
if (is.null(entries)) {
    if (is.list(cycle_log)) {
        entries <- cycle_log
    } else {
        stop("cycle_log is not a list", call. = FALSE)
    }
}
cat(sprintf("Entry container class: %s\n", paste(class(entries), collapse = ", ")))
if (is.data.frame(entries)) {
    cat(sprintf("Data frame rows: %d, cols: %d\n", nrow(entries), ncol(entries)))
    cat("Column names:\n")
    pm_print_vector_preview(names(entries))
    cat("First row overview:\n")
    pm_print_str_limited(as.list(entries[1, , drop = FALSE]))
} else if (is.list(entries) && length(entries) > 0) {
    cat(sprintf("Entry count: %d\n", length(entries)))
    cat("First entry overview:\n")
    pm_print_str_limited(entries[[1]])
}
