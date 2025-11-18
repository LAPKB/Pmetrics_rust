#' @title Get Pmetrics User Options
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Get user options for Pmetrics
#' @details
#' This function will get user options for Pmetrics. It will look for a *PMoptions.json* file
#' in a hidden folder outside of the Pmetrics package. If that does not exist,
#' it will look for a default options file in the package options folder. See [setPMoptions] for
#' details on where the options file is stored and how to set options.
#'
#' @param opt The option to retrieve.  If omitted, all option values will be returned.
#' @param warn Warn if options file doesn't exist. Default `TRUE`.
#' @param quiet Suppress warning messages. Default `FALSE`.
#' @return A list with the current options.
#' @author Michael Neely
#' @export

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

pm_options_default_file <- function() {
  file.path(system.file("options", package = "Pmetrics"), "PMoptions.json")
}

pm_options_load_defaults <- function() {
  defaults_path <- pm_options_default_file()
  if (!file.exists(defaults_path)) {
    return(list())
  }
  jsonlite::read_json(defaults_path, simplifyVector = TRUE)
}

pm_options_merge_defaults <- function(user_settings) {
  defaults <- pm_options_load_defaults()
  if (length(defaults) == 0) {
    return(user_settings)
  }
  if (is.null(user_settings) || length(user_settings) == 0) {
    return(defaults)
  }
  utils::modifyList(defaults, user_settings)
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

pm_options_store_remote_key <- function(alias, api_key) {
  handler <- get0("pm_remote_store_api_key", mode = "function")
  if (is.null(handler)) {
    stop("Remote key storage helper is unavailable", call. = FALSE)
  }
  handler(alias, api_key)
}

pm_options_persist_settings <- function(settings, path) {
  jsonlite::write_json(settings, path, pretty = TRUE, auto_unbox = TRUE)
}

pm_options_numeric_or_default <- function(value, fallback, min_value = NULL) {
  numeric_value <- suppressWarnings(as.numeric(value))
  if (length(numeric_value) != 1 || is.na(numeric_value) || !is.finite(numeric_value)) {
    numeric_value <- fallback
  }
  if (!is.null(min_value) && numeric_value < min_value) {
    numeric_value <- min_value
  }
  numeric_value
}

pm_options_remote_validators <- function() {
  list(
    normalize_url = get0("pm_remote_normalize_url", mode = "function"),
    validate_queue = get0("pm_remote_validate_queue", mode = "function"),
    validate_numeric = get0("pm_remote_validate_numeric", mode = "function"),
    validate_profile = get0("pm_remote_validate_profile", mode = "function"),
    profile_config = get0("pm_remote_validate_profile_config", mode = "function")
  )
}

pm_options_normalize_remote_override <- function(remote) {
  if (is.null(remote) || !is.list(remote) || length(remote) == 0) {
    return(NULL)
  }

  normalized <- list()
  normalized$profile_name <- remote$profile_name %||% remote$profile
  normalized$base_url <- remote$base_url %||% remote$url
  normalized$queue <- remote$queue
  normalized$poll_interval_sec <- remote$poll_interval_sec %||% remote$poll_interval
  normalized$timeout_sec <- remote$timeout_sec %||% remote$timeout

  if (!is.null(remote$verify_tls)) {
    normalized$verify_tls <- isTRUE(remote$verify_tls)
  }
  if (!is.null(remote$allow_insecure)) {
    normalized$verify_tls <- !isTRUE(remote$allow_insecure)
  }

  normalized$api_key_alias <- remote$api_key_alias %||% remote$alias
  compact_normalized <- Filter(function(x) !is.null(x), normalized)
  if (length(compact_normalized) == 0) {
    return(NULL)
  }
  compact_normalized
}

pm_options_validate_remote <- function(values, defaults, require_base_url = FALSE) {
  candidate <- defaults
  if (!is.null(values) && length(values) > 0) {
    candidate <- utils::modifyList(candidate, values)
  }

  candidate$profile_name <- candidate$profile_name %||% defaults$profile_name %||% "default"
  candidate$api_key_alias <- candidate$api_key_alias %||% defaults$api_key_alias %||% paste0("hermes-", candidate$profile_name)
  candidate$queue <- candidate$queue %||% defaults$queue
  candidate$poll_interval_sec <- pm_options_numeric_or_default(candidate$poll_interval_sec, defaults$poll_interval_sec, min_value = 1)
  candidate$timeout_sec <- pm_options_numeric_or_default(candidate$timeout_sec, defaults$timeout_sec, min_value = 30)
  candidate$verify_tls <- isTRUE(candidate$verify_tls)

  if (!nzchar(candidate$base_url %||% "")) {
    if (isTRUE(require_base_url)) {
      stop("Hermes base URL is required when using the remote backend.", call. = FALSE)
    }
    candidate$base_url <- candidate$base_url %||% ""
    candidate$queue <- trimws(candidate$queue)
    candidate$api_key_alias <- trimws(candidate$api_key_alias)
    if (!nzchar(candidate$api_key_alias)) {
      candidate$api_key_alias <- paste0("hermes-", candidate$profile_name)
    }
    return(candidate)
  }

  validators <- pm_options_remote_validators()
  if (any(vapply(validators, is.null, logical(1)))) {
    stop("Hermes remote helpers are unavailable; reinstall Pmetrics to enable remote fits.", call. = FALSE)
  }

  candidate$profile_name <- validators$validate_profile(candidate$profile_name)
  candidate$base_url <- validators$normalize_url(candidate$base_url)
  candidate$queue <- validators$validate_queue(candidate$queue)
  candidate$poll_interval_sec <- validators$validate_numeric(candidate$poll_interval_sec, name = "poll_interval", min_value = 1)
  candidate$timeout_sec <- validators$validate_numeric(candidate$timeout_sec, name = "timeout", min_value = 30)
  candidate$verify_tls <- isTRUE(candidate$verify_tls)
  candidate$api_key_alias <- trimws(candidate$api_key_alias)
  if (!nzchar(candidate$api_key_alias)) {
    candidate$api_key_alias <- paste0("hermes-", candidate$profile_name)
  }
  validators$profile_config(candidate)
  candidate
}

getPMoptions <- function(opt, warn = TRUE, quiet = FALSE) {
  # check for existing options
  opt_dir <- pm_options_user_dir()

  if (dir.exists(opt_dir)) { # external options file exists
    PMoptionsFile <- file.path(opt_dir, "PMoptions.json")
  } else { # external options file does not exist
    PMoptionsFile <- pm_options_default_file()
  }


  # if it doesn't exist, warn and exit
  if (!file.exists(PMoptionsFile)) {
    if (warn && !quiet) cli::cli_inform("Run {.help setPMoptions} to create a Pmetrics options file.")
    return(invisible(-1))
  }

  # read the options file
  PMopts_raw <- jsonlite::read_json(path = PMoptionsFile, simplifyVector = TRUE)
  PMopts <- pm_options_merge_defaults(PMopts_raw)
  if (missing(opt)) {
    return(PMopts)
  } else {
    index <- which(names(PMopts) == opt)
    if (length(index) == 0) {
      return(NULL)
    } else {
      return(PMopts[[index]])
    }
  }
}

#' @title Set Pmetrics User Options
#' @description
#' `r lifecycle::badge("stable")`
#'
#' Set user options for Pmetrics
#' @details
#' When you call this function with the default `launch.app = TRUE`, it will start
#' a Shiny app to set options for the Pmetrics package.
#' Also, when the Pmetrics package is first loaded with `library(Pmetrics)`,
#' this function will be called with `launch.app = TRUE` to read saved options from
#' a *PMoptions.json* file stored in a folder outside
#' of the Pmetrics package, so that your options will persist when Pmetrics is updated.
#'
#' @param launch.app Launch the app to set options. Default `TRUE`.
#' @param backend Optional backend override when calling programmatically. Use
#'   `"remote"` to select Hermes by default.
#' @param remote Optional named list of Hermes remote settings (e.g., base URL,
#'   queue, poll/poll_interval_sec, timeout/timeout_sec, verify_tls,
#'   profile_name, api_key_alias). When supplied, the settings are validated and
#'   written without launching the UI.
#' @param remote_api_key Optional Hermes API key to store in the system keychain
#'   when updating remote settings programmatically.
#' @return The user preferences file will be updated.  This will persist from session to session
#' and if stored in the external location, through Pmetrics versions.
#' @author Michael Neely
#' @export

setPMoptions <- function(launch.app = TRUE, backend = NULL, remote = NULL, remote_api_key = NULL) {
  opt_dir <- pm_options_user_dir()

  fs::dir_create(opt_dir) # ensure directory exists
  PMoptionsUserFile <- pm_options_user_file()

  # If file doesn't exist in user space, copy default
  if (!fs::file_exists(PMoptionsUserFile)) {
    PMoptionsFile <- pm_options_default_file()
    fs::file_copy(PMoptionsFile, PMoptionsUserFile, overwrite = TRUE)
  }

  settings <- tryCatch(
    jsonlite::read_json(PMoptionsUserFile, simplifyVector = TRUE),
    error = function(e) list()
  )
  settings <- pm_options_merge_defaults(settings)

  remote_defaults <- pm_remote_default_settings()
  remote_settings <- remote_defaults
  if (!is.null(settings$remote)) {
    remote_settings <- utils::modifyList(remote_defaults, settings$remote)
  }

  remote_override <- pm_options_normalize_remote_override(remote)
  overrides_requested <- !is.null(remote_override) || !is.null(backend) || !is.null(remote_api_key)

  if (overrides_requested) {
    target_backend <- backend %||% settings$backend %||% "rust"
    candidate <- remote_settings
    if (!is.null(remote_override)) {
      candidate <- utils::modifyList(candidate, remote_override)
    }
    override_names <- names(remote_override %||% list())
    require_base_url <- identical(target_backend, "remote") || ("base_url" %in% override_names)
    candidate <- pm_options_validate_remote(candidate, remote_defaults, require_base_url = require_base_url)
    remote_settings <- candidate
    settings$remote <- remote_settings
    if (!is.null(backend)) {
      settings$backend <- backend
    }
    pm_options_persist_settings(settings, PMoptionsUserFile)

    if (!is.null(remote_api_key) && nzchar(remote_api_key)) {
      alias <- remote_settings$api_key_alias %||% remote_defaults$api_key_alias
      pm_options_store_remote_key(alias, remote_api_key)
    }

    if (!isTRUE(launch.app)) {
      return(invisible(settings))
    }
  }

  settings$remote_base_url <- remote_settings$base_url
  settings$remote_queue <- remote_settings$queue
  settings$remote_poll_interval <- remote_settings$poll_interval_sec
  settings$remote_timeout <- remote_settings$timeout_sec
  settings$remote_allow_insecure <- !isTRUE(remote_settings$verify_tls)
  settings$remote_profile_name <- remote_settings$profile_name
  settings$remote_api_key_alias <- remote_settings$api_key_alias

  app <- shiny::shinyApp(

    # --- UI ---
    ui = bslib::page_fluid(
      theme = bslib::bs_theme(bootswatch = "flatly"),
      title = "Pmetrics Options",
      shiny::tags$details(
        shiny::tags$summary("📁 Data File Reading"),
        shiny::selectInput("sep", "Field separator",
          choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
          selected = ","
        ),
        shiny::selectInput("dec", "Decimal mark",
          choices = c(Period = ".", Comma = ","),
          selected = "."
        )
      ),
      # Formatting options
      shiny::tags$details(
        shiny::tags$summary("📏 Formatting Options"),
        shiny::numericInput("digits", "Number of digits to display",
          value = 3, min = 0, max = 10, step = 1
        )
      ),
      # Fit options
      shiny::tags$details(
        shiny::tags$summary("🔍 Fit Options"),
        shiny::selectInput("backend", "Default backend",
          choices = c("Rust" = "rust", "Hermes Remote" = "remote"),
          selected = settings$backend %||% "rust"
        ),
        shiny::markdown("Select Hermes Remote to run fits via the Hermes service."),
        shiny::div(
          class = "mt-3 p-3 border rounded bg-light",
          shiny::tags$strong("Hermes Remote Settings"),
          shiny::helpText("These values are used whenever the backend is set to Hermes Remote."),
          shiny::textInput("remote_profile_name", "Profile name",
            value = remote_settings$profile_name %||% "default"
          ),
          shiny::textInput("remote_base_url", "Hermes base URL",
            placeholder = "https://hermes.example.com",
            value = remote_settings$base_url
          ),
          shiny::textInput("remote_queue", "Queue name", value = remote_settings$queue),
          shiny::numericInput("remote_poll_interval", "Poll interval (seconds)",
            value = remote_settings$poll_interval_sec, min = 1, step = 1
          ),
          shiny::numericInput("remote_timeout", "Request timeout (seconds)",
            value = remote_settings$timeout_sec, min = 30, step = 30
          ),
          shiny::checkboxInput("remote_allow_insecure", "Disable TLS verification (local testing only)",
            value = !remote_settings$verify_tls
          ),
          shiny::textInput("remote_api_key_alias", "API key alias",
            value = remote_settings$api_key_alias
          ),
          shiny::passwordInput("remote_api_key", "Hermes API key", value = ""),
          shiny::helpText("API keys are saved to the system keychain. Leave blank to keep the stored key.")
        )
      ),
      shiny::tags$details(
        shiny::tags$summary("📊 Prediction Error Metrics"),
        shiny::br(),
        shiny::checkboxInput("show_metrics", "Display error metrics on obs-pred plots with linear regression", TRUE),
        shiny::selectInput("bias_method", "Bias Method",
          choices = c(
            "Mean absolute error (MAE)" = "mae",
            "Mean weighted error (MWE)" = "mwe"
          ),
          selected = "mwe"
        ),
        shiny::selectInput("imp_method", "Imprecision Method",
          choices = c(
            "Mean squared error (MSE)" = "mse",
            "Mean weighted squared error (MWSE)" = "mwse",
            "Root mean squared error (RMSE)" = "rmse",
            "Mean, bias-adjusted, squared error (MBASE)" = "mbase",
            "Mean, bias-adjusted, weighted, squared error (MBAWSE)" = "mbawse",
            "Root mean, bias-adjusted, weighted, squared error (RMBAWSE)" = "rmbawse"
          ),
          selected = "rmbawse"
        ),
        shiny::checkboxInput("use_percent", "Use percent for error metrics", value = TRUE),
        shiny::selectInput("ic_method", "Information Criterion Method",
          choices = c(
            "Akaike Information Criterion (AIC)" = "aic",
            "Bayesian Information Criterion (BIC)" = "bic"
          ),
          selected = "aic"
        )
      ),
      shiny::tags$details(
        shiny::tags$summary("📝 Report Generation"),
        shiny::selectInput("report_template", "Default report template",
          choices = c("plotly", "ggplot2"),
          selected = "plotly"
        )
      ),
      shiny::br(),
      shiny::div(
        class = "d-flex gap-2",
        shiny::actionButton("save", "Save"),
        shiny::actionButton("exit", "Exit"),
      ),
      shiny::br(),
      shiny::br(),
      shiny::verbatimTextOutput("settings_location"),
      shiny::br(),
      shiny::actionButton("open_file", "Open Options File",
        icon = shiny::icon("folder-open"), class = "btn-primary"
      )
    ),

    # --- Server ---
    server = function(input, output, session) {
      # Load settings from external file
      settings <- tryCatch(
        {
          jsonlite::fromJSON(PMoptionsUserFile)
        },
        error = function(e) NULL
      )

      # update this list every time a new option is added
      input_types <- list(
        sep = shiny::updateSelectInput,
        dec = shiny::updateSelectInput,
        show_metrics = shiny::updateCheckboxInput,
        digits = shiny::updateNumericInput,
        bias_method = shiny::updateSelectInput,
        imp_method = shiny::updateSelectInput,
        use_percent = shiny::updateCheckboxInput,
        ic_method = shiny::updateSelectInput,
        report_template = shiny::updateSelectInput,
        backend = shiny::updateSelectInput,
        remote_base_url = shiny::updateTextInput,
        remote_queue = shiny::updateTextInput,
        remote_poll_interval = shiny::updateNumericInput,
        remote_timeout = shiny::updateNumericInput,
        remote_allow_insecure = shiny::updateCheckboxInput,
        remote_profile_name = shiny::updateTextInput,
        remote_api_key_alias = shiny::updateTextInput
      )


      # Apply updates
      purrr::imap(settings, function(val, name) {
        updater <- input_types[[name]]
        formals_names <- names(formals(updater))
        arg_name <- intersect(formals_names, c("value", "selected"))
        arg_name <- arg_name[1]

        if (!is.null(updater) && length(arg_name) == 1 && nzchar(arg_name)) {
          args <- list(session = session, inputId = name)
          args[[arg_name]] <- stringr::str_remove(val, "^percent_") # remove 'percent_' prefix if present
          do.call(updater, args)
        }
      })





      # Display path to user settings file
      output$settings_location <- shiny::renderText({
        glue::glue("Options file path:\n{PMoptionsUserFile}")
      })


      ### Action button handlers

      # Save updated settings
      shiny::observeEvent(input$save, {
        percent_prefix <- c("", "percent_")[1 + as.numeric(input$use_percent)]
        remote_payload <- list(
          profile_name = input$remote_profile_name %||% remote_settings$profile_name %||% "default",
          base_url = input$remote_base_url %||% "",
          queue = input$remote_queue %||% "heavy-jobs",
          poll_interval_sec = as.numeric(input$remote_poll_interval %||% remote_defaults$poll_interval_sec),
          timeout_sec = as.numeric(input$remote_timeout %||% remote_defaults$timeout_sec),
          verify_tls = !isTRUE(input$remote_allow_insecure),
          api_key_alias = input$remote_api_key_alias %||% remote_settings$api_key_alias %||% remote_defaults$api_key_alias
        )

        require_remote <- identical(input$backend, "remote") || nzchar(remote_payload$base_url)
        remote_payload <- tryCatch(
          pm_options_validate_remote(remote_payload, remote_defaults, require_base_url = require_remote),
          error = function(e) {
            shiny::showNotification(e$message, type = "error", duration = 6)
            return(NULL)
          }
        )
        if (is.null(remote_payload)) {
          return(invisible(NULL))
        }

        settings <- list(
          sep = input$sep,
          dec = input$dec,
          digits = input$digits,
          show_metrics = input$show_metrics,
          bias_method = glue::glue(percent_prefix, input$bias_method),
          imp_method = glue::glue(percent_prefix, input$imp_method),
          ic_method = input$ic_method,
          report_template = input$report_template,
          backend = input$backend,
          remote = remote_payload
        )

        save_status <- tryCatch(pm_options_persist_settings(settings, PMoptionsUserFile),
          error = function(e) {
            shiny::showNotification(
              paste("Error saving settings:", e$message),
              type = "error", duration = 5
            )
            return(FALSE)
          }
        )
        if (identical(save_status, FALSE)) {
          return(invisible(NULL))
        }
        shiny::showNotification(
          "Settings saved",
          type = "message", duration = 3
        )

        if (!is.null(input$remote_api_key) && nzchar(input$remote_api_key)) {
          tryCatch(
            {
              pm_options_store_remote_key(remote_payload$api_key_alias, input$remote_api_key)
              shiny::showNotification("API key stored in keychain", type = "message", duration = 3)
            },
            error = function(e) {
              shiny::showNotification(
                paste("Failed to store API key:", e$message),
                type = "error", duration = 5
              )
            }
          )
        }
      })

      # Exit the app
      shiny::observeEvent(input$exit, {
        shiny::stopApp()
      })

      # Open the options file in the default application
      shiny::observeEvent(input$open_file, {
        system(glue::glue("open {PMoptionsUserFile}"))
      })
    } # end server
  ) # end shinyApp


  # Launch the app without trying to launch another browser
  if (launch.app) {
    shiny::runApp(app, launch.browser = TRUE)
  }

  return(invisible(NULL))
} # end of PM_options function
