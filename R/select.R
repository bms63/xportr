#' Select Required Variables
#'
#' Subsets a given data frame so it contains only those variables stated in a
#' given variable level metadata.
#'
#' @param .df A dataframe with columns that can be <NEED TO COMPLETE>
#' @param metadata An appropriate metadata object that has <NEED TO COMPLETE>
#' @param domain Appropriate CDISC dataset name, e.g. ADAE, DM. Used to subset the
#'   metadata object. If none is passed, then name of the dataset passed `.df`
#'   will be used.
#' @param verbose The action the function takes when a variable in the `metadata`
#'   is not found in the `.df`. Options are 'stop', 'warn', 'message', and 'none'.
#'
#' @return Data frame containing only those variables given in the metadata.
#' @family metadata functions
#' @seealso [xportr_df_label()], [xportr_format()], [xportr_label()], and
#'   [xportr_length()]
#' @export
#'
#' @examples
#' adsl <- data.frame(
#'   USUBJID = c(1001, 1002, 1003),
#'   SITEID = c(001, 002, 003),
#'   AGE = c(63, 35, 27),
#'   SEX = c("M", "F", "M"),
#'   SEX_DEC = c("MALE", "FEMALE", "MALE")
#' )
#'
#' metadata_adsl <- data.frame(
#'   dataset = "adsl",
#'   variable = c("USUBJID", "SITEID", "AGE", "SEX")
#' )
#'
#' adsl <- xportr_select(adsl, metadata = metadata_adsl)
#'
#' dm <- data.frame(
#'   USUBJID = c(1001, 1002, 1003),
#'   SITEID = c(001, 002, 003),
#'   AGE = c(63, 35, 27)
#' )
#'
#' metadata_dm <- data.frame(
#'   dataset = "dm",
#'   variable = c("USUBJID", "SUBJID", "AGE", "SEX")
#' )
#'
#' dm <- xportr_select(dm, metadata = metadata_dm, verbose = "warn")
#'
#' dm <- xportr_select(dm, metadata = metadata_dm, verbose = "stop")

xportr_select <- function(.df,
                          metadata,
                          domain = NULL,
                          verbose = getOption("xportr.select_verbose", "none")) {

  domain_name <- getOption("xportr.domain_name")
  variable_name <- getOption("xportr.variable_name")

  ## Common section to detect domain from argument or pipes

  df_arg <- tryCatch(as_name(enexpr(.df)), error = function(err) NULL)
  domain <- get_domain(.df, df_arg, domain)
  if (!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain

  ## End of common section

  if (inherits(metadata, "Metacore")) {
    metadata <- metadata$var_spec
  }

  if (domain_name %in% names(metadata)) {
    metadata <- metadata %>%
      dplyr::filter(!!sym(domain_name) == domain)
  }

  dfvars <- names(.df)
  metavars <- metadata[[variable_name]]

  if (all(metavars %in% dfvars) == FALSE) {
    miss_vars <- metavars[which(! metavars %in% dfvars)]
    select_log(miss_vars, verbose)
    cat("\n")
  }

  drop_vars <- dfvars[which(! dfvars %in% metavars)]
  if (length(drop_vars) > 0) {
    .df <- .df %>%
      select(-all_of(drop_vars))

    cli_alert_info("The following variable(s) have been dropped from `.df`:")
    #cat(paste0(drop_vars, collapse = "\n"))
    cli_text("Variables: {drop_vars}.")
    cat("\n")
  } else {
    cli_alert_info("No variables have been dropped from `.df`.")
    cat("\n")
  }

  .df
}

