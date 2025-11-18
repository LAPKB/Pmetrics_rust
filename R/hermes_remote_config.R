if (!exists("%||%", mode = "function")) {
    `%||%` <- function(lhs, rhs) {
        if (is.null(lhs)) {
            rhs
        } else if (is.character(lhs) && length(lhs) == 1 && !nzchar(lhs)) {
            rhs
        } else {
            lhs
        }
    }
}

pm_remote_default_settings <- function() {
    list(
        profile_name = "bke-example",
        base_url = "http://localhost:8080",
        queue = "heavy-jobs",
        poll_interval_sec = 2,
        timeout_sec = 3600,
        verify_tls = TRUE,
        api_key_alias = "hermes-bke"
    )
}

pm_options_user_dir <- function() {
    dplyr::case_when(
        getOS() %in% c(1, 3) ~ fs::path_expand("~/.PMopts"),
        getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts"),
        TRUE ~ fs::path_expand("~/.PMopts")
    )
}

pm_options_user_file <- function() {
    file.path(pm_options_user_dir(), "PMoptions.json")
}

#' Configure Hermes remote execution for Pmetrics
#'
#' This helper stores the connection details (URL, queue, TLS policy) and
#' optionally persists an API key reference for Hermes so that remote fits can
#' be submitted without re-entering credentials every session.
#'
#' The configuration is saved in the same preferences directory used by other
#' Pmetrics options (typically `~/.PMopts` on macOS/Linux or `%APPDATA%/PMopts`
#' on Windows). API keys are written to the system keyring under the service
#' name "Pmetrics Hermes"; when the keyring is unavailable you can set the
#' `PM_HERMES_API_KEY` environment variable as a fallback before submitting a
#' remote job.
#'
#' @param base_url Hermes base URL, e.g. `https://hermes.example.com`.
#' @param api_key Optional API key to store securely. Use `NULL` to skip
#'   storage and rely on `PM_HERMES_API_KEY` instead.
#' @param queue Target Hermes queue (defaults to `"heavy-jobs"`).
#' @param poll_interval Seconds between status polls once a job is submitted.
#' @param timeout Request timeout in seconds for Hermes HTTP calls.
#' @param allow_insecure Set to `TRUE` only for local testing with self-signed
#'   certificates. Production deployments should always verify TLS.
#' @param profile Named profile to create or update (defaults to `"default"`).
#' @param api_key_alias Optional keyring alias to associate with the profile.
#'   Defaults to `"hermes-"` + `profile`.
#' @param set_active When `TRUE`, mark this profile as the active remote target.
#'
#' @return The profile definition (invisibly).
#' @export
pm_remote_configure <- function(
    base_url,
    api_key = NULL,
    queue = "heavy-jobs",
    poll_interval = 5,
    timeout = 3600,
    allow_insecure = FALSE,
    profile = "default",
    api_key_alias = NULL,
    set_active = TRUE) {
    if (missing(base_url) || !is.character(base_url) || length(base_url) != 1) {
        stop("base_url must be a single string", call. = FALSE)
    }

    base_url <- pm_remote_normalize_url(base_url)
    queue <- pm_remote_validate_queue(queue)
    poll_interval <- pm_remote_validate_numeric(poll_interval, name = "poll_interval", min_value = 1)
    timeout <- pm_remote_validate_numeric(timeout, name = "timeout", min_value = 30)
    profile <- pm_remote_validate_profile(profile)

    if (!is.null(api_key_alias)) {
        if (!is.character(api_key_alias) || length(api_key_alias) != 1 || !nzchar(api_key_alias)) {
            stop("api_key_alias must be a non-empty string", call. = FALSE)
        }
        api_key_alias <- trimws(api_key_alias)
    } else {
        api_key_alias <- paste0("hermes-", profile)
    }
    profile_entry <- list(
        profile_name = profile,
        base_url = base_url,
        queue = queue,
        poll_interval_sec = poll_interval,
        timeout_sec = timeout,
        verify_tls = !isTRUE(allow_insecure),
        api_key_alias = api_key_alias
    )

    opts <- pm_remote_read_options()
    if (identical(opts, -1)) {
        opts <- jsonlite::read_json(
            paste(system.file("options", package = "Pmetrics"), "PMoptions.json", sep = "/"),
            simplifyVector = TRUE
        )
    }
    if (!is.list(opts) || length(opts) == 0) {
        opts <- list()
    }

    opts$remote <- profile_entry
    if (isTRUE(set_active)) {
        opts$backend <- "remote"
    }

    dir.create(dirname(pm_options_user_file()), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(opts, pm_options_user_file(), pretty = TRUE, auto_unbox = TRUE)

    if (!is.null(api_key) && nzchar(api_key)) {
        pm_remote_store_api_key(api_key_alias, api_key)
    }

    invisible(profile_entry)
}

pm_remote_validate_queue <- function(queue) {
    if (!is.character(queue) || length(queue) != 1 || !nzchar(queue)) {
        stop("queue must be a non-empty string", call. = FALSE)
    }
    trimws(queue)
}

pm_remote_validate_numeric <- function(value, name, min_value) {
    if (!is.numeric(value) || length(value) != 1 || is.na(value)) {
        stop(sprintf("%s must be numeric", name), call. = FALSE)
    }
    value <- as.numeric(value)
    if (value < min_value) {
        stop(sprintf("%s must be >= %s", name, min_value), call. = FALSE)
    }
    value
}

pm_remote_validate_profile <- function(profile) {
    if (!is.character(profile) || length(profile) != 1 || !nzchar(profile)) {
        stop("profile must be a non-empty string", call. = FALSE)
    }
    profile <- trimws(profile)
    profile
}

pm_remote_normalize_url <- function(url) {
    url <- trimws(url)
    if (!nzchar(url)) {
        stop("base_url cannot be empty", call. = FALSE)
    }
    if (!grepl("^https?://", url, ignore.case = TRUE)) {
        stop("base_url must start with http:// or https://", call. = FALSE)
    }
    sub("/+$", "", url)
}

if (!exists("%||%", mode = "function")) {
    `%||%` <- function(lhs, rhs) {
        if (is.null(lhs)) {
            rhs
        } else if (is.character(lhs) && length(lhs) == 1 && !nzchar(lhs)) {
            rhs
        } else {
            lhs
        }
    }

    pm_remote_default_settings <- function() {
        list(
            profile_name = "bke-example",
            base_url = "http://localhost:8080",
            queue = "heavy-jobs",
            poll_interval_sec = 2,
            timeout_sec = 3600,
            verify_tls = TRUE,
            api_key_alias = "hermes-bke"
        )
    }

    pm_options_user_dir <- function() {
        dplyr::case_when(
            getOS() %in% c(1, 3) ~ fs::path_expand("~/.PMopts"),
            getOS() == 2 ~ file.path(Sys.getenv("APPDATA"), "PMopts"),
            TRUE ~ fs::path_expand("~/.PMopts")
        )
    }

    pm_options_user_file <- function() {
        file.path(pm_options_user_dir(), "PMoptions.json")
    }
}

pm_remote_profile_config <- function(profile = NULL, config = NULL) {
    if (!is.null(config)) {
        remote <- config
    } else {
        opts <- pm_remote_read_options()
        if (!identical(opts, -1) && is.list(opts)) {
            remote <- opts$remote
        } else {
            remote <- NULL
        }
    }

    defaults <- pm_remote_default_settings()
    if (is.null(remote) || !is.list(remote)) {
        remote <- defaults
    } else {
        remote <- utils::modifyList(defaults, remote)
    }

    if (!is.null(profile) && nzchar(profile)) {
        remote$profile_name <- profile
        remote$api_key_alias <- paste0("hermes-", profile)
    }

    remote$base_url <- remote$base_url %||% ""
    remote$queue <- remote$queue %||% "heavy-jobs"
    remote$poll_interval_sec <- remote$poll_interval_sec %||% 5
    remote$timeout_sec <- remote$timeout_sec %||% 3600
    remote$verify_tls <- if (is.null(remote$verify_tls)) TRUE else isTRUE(remote$verify_tls)
    remote$api_key_alias <- remote$api_key_alias %||% paste0("hermes-", remote$profile_name %||% "default")

    remote
}

pm_remote_read_options <- function() {
    user_file <- pm_options_user_file()
    if (fs::file_exists(user_file)) {
        return(jsonlite::read_json(user_file, simplifyVector = TRUE))
    }

    default_file <- file.path(system.file("options", package = "Pmetrics"), "PMoptions.json")
    if (fs::file_exists(default_file)) {
        return(jsonlite::read_json(default_file, simplifyVector = TRUE))
    }

    list()
}

pm_remote_store_api_key <- function(alias, api_key) {
    if (!nzchar(api_key)) {
        return(invisible(FALSE))
    }
    if (!pm_remote_keyring_enabled()) {
        warning(
            "Keyring disabled; set PM_HERMES_API_KEY in your environment before running remote fits.",
            call. = FALSE
        )
        return(invisible(FALSE))
    }

    tryCatch(
        {
            keyring::key_set_with_value(
                service = pm_remote_key_service(),
                username = alias,
                password = api_key
            )
            TRUE
        },
        error = function(e) {
            stop(sprintf("Failed to store API key: %s", e$message), call. = FALSE)
        }
    )
}

pm_remote_get_api_key <- function(alias, required = TRUE) {
    env_key <- Sys.getenv("PM_HERMES_API_KEY", unset = NA_character_)
    if (!is.na(env_key) && nzchar(env_key)) {
        return(env_key)
    }

    if (!pm_remote_keyring_enabled()) {
        if (isTRUE(required)) {
            stop(
                paste(
                    "No Hermes API key found.",
                    "Set PM_HERMES_API_KEY or run pm_remote_configure(..., api_key = 'your_key')."
                ),
                call. = FALSE
            )
        }
        return(NULL)
    }

    tryCatch(
        keyring::key_get(service = pm_remote_key_service(), username = alias),
        error = function(e) {
            if (isTRUE(required)) {
                stop(
                    sprintf(
                        "Failed to read API key from keyring (profile '%s'): %s",
                        alias,
                        e$message
                    ),
                    call. = FALSE
                )
            }
            NULL
        }
    )
}

pm_remote_key_service <- function() {
    "Pmetrics Hermes"
}

pm_remote_keyring_enabled <- function() {
    !identical(Sys.getenv("PM_REMOTE_DISABLE_KEYRING", unset = "0"), "1")
}

pm_remote_detect_os <- function() {
    sys <- tryCatch(Sys.info()[["sysname"]], error = function(...) NA_character_)
    if (identical(sys, "Darwin")) {
        return(1L)
    }
    if (identical(sys, "Windows")) {
        return(2L)
    }
    if (identical(sys, "Linux")) {
        return(3L)
    }
    0L
}
