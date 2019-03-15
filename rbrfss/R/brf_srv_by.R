#' A brfss brf_srv_by calc function
#'
#' This function allows you to create the brfss survey design and the prevalence table
#' @param brf_srvyr_design A brfss srvyr design object.
#' @param variable The main variable of interest for summary calculations (entered as a character).
#' @param recode_statement A character text in CAR recode format for recoding the variable parameter. 
#' @param by_variable A secondary variable defining a subet for summary calculation (entered as character). 
#' @param by_variable_recode A character text in CAR recode format for recoding the by_variable parameter.
#' @param finwt_use If TRUE, uses a combined final weight calculated from aggregating years in weighting process. Defaults to FALSE.
#' @param bin_filter A character match for quickly keeping/filtering a single level of the primary variable. Must match recode label.
#' @keywords rbrffs
#' @export
#' @examples
#' brf_srv_by()



brf_srv_by <- function(brf_srvyr_design, variable, recode_statement = NULL, 
                          by_variable = NULL, by_variable_recode = NULL, 
                          bin_filter = NULL, finwt_use = FALSE) {
  
  variable1a <- as.name(variable)
  variable1a <- srvyr::enquo(variable1a)
  
  if (!is.null(by_variable)) {
    by_variablea <- as.name(by_variable)
    by_variablea <- srvyr::enquo(by_variablea)
  }
  
  minyr <- substr(min(brf_srvyr_design$variables$yr),3,4)
  maxyr <- substr(max(brf_srvyr_design$variables$yr),3,4)
  variableyr1 <- paste0(minyr, "to", maxyr)
  
  xi <- brf_srvyr_design %>%
  {if (!is.null(recode_statement)) 
    srvyr::mutate(., label = factor(car::recode(!! variable1a, recode_statement))) else
      srvyr::mutate(., label = factor(!! variable1a))} %>%
    #mutate(label = factor(recode(!! variable1a, recode_statement))) %>%
      {if (!is.null(by_variable) & !is.null(by_variable_recode))
        srvyr::mutate(., !! by_variablea := factor(car::recode(!! by_variablea, by_variable_recode)))
        else .} %>% 
    srvyr::filter(!is.na(label)) %>% 
    {if (!is.null(by_variable)) 
      srvyr::filter(., !is.na(!! by_variablea))
      else .} %>%
      {if (!is.null(by_variable) && by_variable == "fips")
        srvyr::filter(., !is.na(fips), !(fips %in% c(28179, 28888, 28999, 28777)))
        else .} %>%
        {if (!is.null(by_variable) && by_variable == "fips")
          srvyr::group_by(., xstate, !! by_variablea, label)
          else if (is.null(by_variable) && finwt_use == TRUE)
            srvyr::group_by(., xstate, label)
          else if (!is.null(by_variable) && finwt_use == TRUE)
            srvyr::group_by(., xstate, !! by_variablea, label)
          else if (!is.null(by_variable) && by_variable != "fips" && finwt_use == FALSE)
            srvyr::group_by(., xstate, yr, !! by_variablea, label)
          else (srvyr::group_by(., xstate, yr, label))} %>%
    srvyr::summarize(Per = srvyr::survey_mean(vartype = c("ci", "se"), na.rm = T),
                     n = srvyr::unweighted(n()),
                     EstN = srvyr::survey_total(vartype = c("ci", "se"))) %>%
                     {if (!is.null(by_variable) && by_variable == "fips" && finwt_use == FALSE)
                       srvyr::group_by(., !! by_variablea)
                       else if (!is.null(by_variable) && by_variable != "fips" && finwt_use == TRUE)
                         srvyr::group_by(., !! by_variablea)
                       else if (is.null(by_variable) && finwt_use == TRUE)
                         srvyr::group_by(., label)
                       else (srvyr::group_by(., yr))} %>%
    srvyr::mutate(ntotal = sum(n), 
                  variable = as.character(variable1a)[2]) %>%
                  {if (!is.null(bin_filter)) filter(., label == bin_filter) else .} %>%
    srvyr::mutate(Per_A = ifelse(ntotal < 50, NA, round(Per * 100, 1)),
                  Per_A_MOE = ifelse(is.na(Per_A), NA, round((Per_upp - Per) * 100, 1))) %>%
                  {if (!is.null(by_variable)) srvyr::mutate(., ByVar = as.character(by_variablea)[2]) else .} %>%
                  {if (!is.null(by_variable) && by_variable == "fips")
                    srvyr::mutate(., yr = !! variableyr1)
                    else if (finwt_use == TRUE)
                      srvyr::mutate(., yr = !! variableyr1)
                    else .} %>% 
                    {if (!is.null(by_variable)) srvyr::rename(., ByGroup = !! by_variablea) else .} %>% 
                    {if (!is.null(by_variable)) srvyr::select(., xstate, yr, ByVar, ByGroup, variable, label, Per_A, Per_A_MOE, n, ntotal, Per, Per_low, Per_upp, Per_se, EstN, EstN_low, EstN_upp, EstN_se) 
                      else srvyr::select(., xstate, yr, variable, label, Per_A, Per_A_MOE, n, ntotal, Per, Per_low, Per_upp, Per_se, EstN, EstN_low, EstN_upp, EstN_se)}
  return(xi)
}
