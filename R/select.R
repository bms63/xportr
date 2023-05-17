#' Select Required Variables
#'
#' Subsets a given data frame so it contains only those variables stated in a
#' given variable level metadata.
#'
#' @param .df A data frame of CDISC standard.
#' @param metacore A data frame containing variable level metadata.
#' @param domain A character value to subset the `metacore`. If `NULL` (default),
#'   uses `.df` value as a subset condition.
#' @param verbose The action the function takes when a variable in the `metacore`
#'   is not found in the `.df`. Options are 'stop', 'warn', 'message', and 'none'.
#'
#' @return Data frame containing only those variables given in the metadata.
#' @family metadata functions
#' @seealso [xportr_df_label()], [xportr_format()], [xportr_label()], and
#'   [xportr_length()]
#' @export
