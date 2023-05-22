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
#' @param verbose The action the function takes when a variable in the `metacore`
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
#' metacore_adsl <- data.frame(
#'   dataset = "adsl",
#'   variable = c("USUBJID", "SITEID", "AGE", "SEX")
#' )
#'
#' adsl <- xportr_select(adsl, metadata = metacore_adsl)
#'
#' dm <- data.frame(
#'   USUBJID = c(1001, 1002, 1003),
#'   SITEID = c(001, 002, 003),
#'   AGE = c(63, 35, 27)
#' )
#'
#' metacore_dm <- data.frame(
#'   dataset = "adsl",
#'   variable = c("USUBJID", "SUBJID", "AGE", "SEX")
#' )
#'
#' dm <- xportr_select(dm, metadata = metacore_dm, verbose = "warn")
#'
#' dm <- xportr_select(dm, metadata = metacore_dm, verbose = "stop")

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
}

