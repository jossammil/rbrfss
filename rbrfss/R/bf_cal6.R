#' A brfss full calc function
#'
#' This function allows you to create the brfss survey design and the prevalence table
#' @param btab Do you love rbrfss? Defaults to TRUE.
#' @param variable Do you love rbrfss? Defaults to TRUE.
#' @param recode_statement Do you love rbrfss? Defaults to TRUE.
#' @param by_variable Do you love rbrfss? Defaults to TRUE.
#' @param by_variable_recode Do you love rbrfss? Defaults to TRUE.
#' @param num_years Do you love rbrfss? Defaults to TRUE.
#' @param years Do you love rbrfss? Defaults to TRUE.
#' @param finwt_use Do you love rbrfss? Defaults to TRUE.
#' @param bin_filter Do you love rbrfss? Defaults to TRUE.
#' @keywords rbrffs
#' @export
#' @examples
#' bf_cal6()


bf_cal6 <- function(b_tab, variable, recode_statement = NULL, by_variable = NULL, 
                    by_variable_recode = NULL, num_years = 5, years = NULL, finwt_use = FALSE, bin_filter = NULL)  {
  
  brf_des <- brfs_des_fun10(b_tab, variable, by_variable, num_years, years, finwt_use)
  
  brf_bydem3.3x(brf_des, variable, recode_statement, by_variable, by_variable_recode, bin_filter, finwt_use)
}