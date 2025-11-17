pm_remote_validate_profile_config <- function(profile) {
  required <- c("base_url", "queue", "api_key_alias", "timeout_sec", "poll_interval_sec")
  missing <- required[!required %in% names(profile)]
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Profile '%s' is missing required fields: %s",
        profile$profile_name %||% "unknown",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!nzchar(profile$base_url)) {
    stop("Remote base URL is empty. Re-run pm_remote_configure().", call. = FALSE)
  }
  invisible(profile)
}

pm_remote_request <- function(profile, path, method = "GET", body = NULL, timeout = NULL) {
  pm_remote_validate_profile_config(profile)
  url <- pm_remote_join_url(profile$base_url, path)
  req <- httr2::request(url)
  req <- httr2::req_method(req, method)
  req <- httr2::req_headers(req, Accept = "application/json")

  api_key <- pm_remote_get_api_key(profile$api_key_alias, required = FALSE)
  if (!is.null(api_key)) {
    req <- httr2::req_headers(req, "X-API-Key" = api_key)
  }
  ua <- sprintf("Pmetrics/%s (remote)", utils::packageVersion("Pmetrics"))
  req <- httr2::req_user_agent(req, ua)
  verify <- isTRUE(profile$verify_tls)
  if (!verify) {
    req <- httr2::req_options(req, ssl_verifypeer = 0L, ssl_verifyhost = 0L)
  }
  timeout_val <- if (!is.null(timeout)) timeout else profile$timeout_sec
  req <- httr2::req_timeout(req, timeout_val)
  if (!is.null(body)) {
    req <- httr2::req_body_json(req, data = body, auto_unbox = TRUE, digits = NA)
  }
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (status >= 400) {
    detail <- suppressWarnings(httr2::resp_body_string(resp))
    detail <- trimws(detail)

    if ((status == 401 || status == 403) && is.null(api_key)) {
      stop(
        paste(
          "Hermes requires an API key for this endpoint.",
          "Set PM_HERMES_API_KEY or run pm_remote_configure(..., api_key = 'your_key')."
        ),
        call. = FALSE
      )
    }

    stop(
      sprintf("Hermes request failed (%s): %s", status, detail),
      call. = FALSE
    )
  }
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

pm_remote_enqueue <- function(payload, profile = NULL, config = NULL) {
  if (!is.list(payload) || !all(c("model", "data", "settings") %in% names(payload))) {
    stop("payload must contain model, data, and settings fields", call. = FALSE)
  }
  prof <- pm_remote_profile_config(profile, config) # nolint
  response <- pm_remote_request(
    prof,
    path = sprintf("%s/enqueue", prof$queue),
    method = "POST",
    body = list(payload = payload)
  )
  list(job_id = response$id, queue = response$queue, profile = prof$profile_name)
}

pm_remote_job_status <- function(job_id, profile = NULL, config = NULL) {
  if (!nzchar(job_id)) {
    stop("job_id must be provided", call. = FALSE)
  }
  prof <- pm_remote_profile_config(profile, config)
  pm_remote_request(
    prof,
    path = sprintf("%s/%s/status", prof$queue, job_id),
    method = "GET"
  )
}

pm_remote_fetch_result <- function(job_id, profile = NULL, config = NULL) {
  if (!nzchar(job_id)) {
    stop("job_id must be provided", call. = FALSE)
  }
  prof <- pm_remote_profile_config(profile, config)
  pm_remote_request(
    prof,
    path = sprintf("%s/%s/result", prof$queue, job_id),
    method = "GET",
    timeout = max(prof$timeout_sec, 120)
  )
}

pm_remote_build_payload <- function(model_txt, data_csv, settings = NULL, settings_json = NULL) {
  if (!is.character(model_txt) || length(model_txt) != 1 || !nzchar(model_txt)) {
    stop("model_txt must be a single string", call. = FALSE)
  }
  if (!is.character(data_csv) || length(data_csv) != 1 || !nzchar(data_csv)) {
    stop("data_csv must be a single string", call. = FALSE)
  }
  if (!is.null(settings_json) && !is.null(settings)) {
    stop("Specify either settings or settings_json, not both", call. = FALSE)
  }
  settings_blob <- if (!is.null(settings_json)) {
    pm_remote_validate_settings_json(settings_json)
  } else if (!is.null(settings)) {
    jsonlite::toJSON(settings, auto_unbox = TRUE, null = "null", digits = NA)
  } else {
    stop("settings or settings_json must be supplied", call. = FALSE)
  }

  list(
    model = model_txt,
    data = data_csv,
    settings = settings_blob
  )
}

pm_remote_validate_settings_json <- function(value) {
  if (!is.character(value) || length(value) != 1 || !nzchar(value)) {
    stop("settings_json must be a JSON string", call. = FALSE)
  }
  tryCatch(
    {
      jsonlite::fromJSON(value)
      value
    },
    error = function(e) {
      stop(sprintf("settings_json is not valid JSON: %s", e$message), call. = FALSE)
    }
  )
}

pm_remote_wait_for_job <- function(job_id, profile = NULL, config = NULL, poll_interval = NULL, timeout = NULL) {
  if (!nzchar(job_id)) {
    stop("job_id must be provided", call. = FALSE)
  }
  prof <- pm_remote_profile_config(profile, config)
  pm_remote_validate_profile_config(prof)

  interval <- poll_interval
  if (is.null(interval) || !is.finite(interval)) {
    interval <- prof$poll_interval_sec
  }
  if (is.null(interval) || !is.finite(interval)) {
    interval <- 5
  }
  interval <- max(0.5, as.numeric(interval))

  timeout_limit <- timeout
  if (is.null(timeout_limit) || !is.finite(timeout_limit)) {
    timeout_limit <- prof$timeout_sec
  }
  deadline <- if (!is.null(timeout_limit) && is.finite(timeout_limit)) {
    Sys.time() + timeout_limit
  } else {
    Inf
  }

  history <- list()
  last_status <- NULL

  repeat {
    status <- pm_remote_job_status(job_id, config = prof)
    status$polled_at <- format(Sys.time(), tz = "UTC", usetz = TRUE)
    history[[length(history) + 1]] <- status

    message <- status$status
    if (is.null(message) || !nzchar(message)) {
      message <- "(status unavailable)"
    }
    progress <- status$progress
    if (!identical(message, last_status)) {
      progress_msg <- if (!is.null(progress) && length(progress) == 1 && is.finite(progress)) {
        sprintf(" (%d%%)", as.integer(progress))
      } else {
        ""
      }
      cli::cli_inform(c("i" = sprintf("Hermes job %s: %s%s", job_id, message, progress_msg)))
      last_status <- message
    }

    if (!is.null(status$error) && nzchar(status$error)) {
      stop(sprintf("Hermes job %s failed: %s", job_id, status$error), call. = FALSE)
    }

    if (identical(message, "Job completed successfully")) {
      return(list(status = status, history = history, profile = prof))
    }

    if (is.finite(deadline) && Sys.time() > deadline) {
      stop(sprintf("Timed out waiting for Hermes job %s to complete.", job_id), call. = FALSE)
    }

    Sys.sleep(interval)
  }
}

pm_remote_join_url <- function(base_url, path) {
  base <- sub("/+$", "", base_url)
  tail <- sub("^/+", "", path)
  paste(base, tail, sep = "/")
}

`%||%` <- function(lhs, rhs) {
  if (is.null(lhs) || (is.character(lhs) && length(lhs) == 1 && !nzchar(lhs))) {
    rhs
  } else {
    lhs
  }
}
