#' A brfss bydem3.3x calc function
#'
#' This function allows you to create the brfss survey design and the prevalence table
#' @param btab Do you love rbrfss? Defaults to TRUE.
#' @param variable Do you love rbrfss? Defaults to TRUE.
#' @param recode_statement Do you love rbrfss? Defaults to TRUE.
#' @param by_variable1 Do you love rbrfss? Defaults to TRUE.
#' @param by_variable1_recode Do you love rbrfss? Defaults to TRUE.
#' @param bin_filter Do you love rbrfss? Defaults to TRUE.
#' @param finwt_use Do you love rbrfss? Defaults to TRUE.
#' @keywords rbrffs
#' @export
#' @examples
#' brf_bydem3.3x()



brf_bydem3.3x <- function(table, variable, recode_statement = NULL, 
                          by_variable1 = NULL, by_variable1_recode = NULL, 
                          bin_filter = NULL, finwt_use = FALSE) {
  
  variable1a <- as.name(variable)
  variable1a <- srvyr::enquo(variable1a)
  
  if (!is.null(by_variable1)) {
    by_variable1a <- as.name(by_variable1)
    by_variable1a <- srvyr::enquo(by_variable1a)
  }
  
  minyr <- substr(min(table$variables$yr),3,4)
  maxyr <- substr(max(table$variables$yr),3,4)
  variableyr1 <- paste0(minyr, "-", maxyr)
  
  xi <- table %>%
  {if (!is.null(recode_statement)) 
    srvyr::mutate(., label = factor(car::recode(!! variable1a, recode_statement))) else
      srvyr::mutate(., label = factor(!! variable1a))} %>%
    #mutate(label = factor(recode(!! variable1a, recode_statement))) %>%
      {if (!is.null(by_variable1) & !is.null(by_variable1_recode))
        srvyr::mutate(., !! by_variable1a := factor(car::recode(!! by_variable1a, by_variable1_recode)))
        else .} %>% 
    srvyr::filter(!is.na(label)) %>% 
    {if (!is.null(by_variable1)) 
      srvyr::filter(., !is.na(!! by_variable1a))
      else .} %>%
      {if (!is.null(by_variable1) && by_variable1 == "fips")
        srvyr::filter(., !is.na(fips), !(fips %in% c(28179, 28888, 28999, 28777)))
        else .} %>%
        {if (!is.null(by_variable1) && by_variable1 == "fips")
          srvyr::group_by(., xstate, !! by_variable1a, label)
          else if (is.null(by_variable1) && finwt_use == TRUE)
            srvyr::group_by(., xstate, label)
          else if (!is.null(by_variable1) && finwt_use == TRUE)
            srvyr::group_by(., xstate, !! by_variable1a, label)
          else if (!is.null(by_variable1) && by_variable1 != "fips" && finwt_use == FALSE)
            srvyr::group_by(., xstate, yr, !! by_variable1a, label)
          else (srvyr::group_by(., xstate, yr, label))} %>%
    srvyr::summarize(Per = srvyr::survey_mean(vartype = c("ci", "se"), na.rm = T),
                     n = srvyr::unweighted(n()),
                     EstN = srvyr::survey_total(vartype = c("ci", "se"))) %>%
                     {if (!is.null(by_variable1) && by_variable1 == "fips" && finwt_use == FALSE)
                       srvyr::group_by(., !! by_variable1a)
                       else if (!is.null(by_variable1) && by_variable1 != "fips" && finwt_use == TRUE)
                         srvyr::group_by(., !! by_variable1a)
                       else if (is.null(by_variable1) && finwt_use == TRUE)
                         srvyr::group_by(., label)
                       else (srvyr::group_by(., yr))} %>%
    srvyr::mutate(ntotal = sum(n), 
                  variable = as.character(variable1a)[2]) %>%
                  {if (!is.null(bin_filter)) filter(., label == bin_filter) else .} %>%
    srvyr::mutate(Per_A = ifelse(ntotal < 50, NA, round(Per * 100, 1)),
                  Per_A_MOE = ifelse(is.na(Per_A), NA, round((Per_upp - Per) * 100, 1))) %>%
                  {if (!is.null(by_variable1)) srvyr::mutate(., ByVar = as.character(by_variable1a)[2]) else .} %>%
                  {if (!is.null(by_variable1) && by_variable1 == "fips")
                    srvyr::mutate(., yr = !! variableyr1)
                    else if (finwt_use == TRUE)
                      srvyr::mutate(., yr = !! variableyr1)
                    else .} %>% 
                    {if (!is.null(by_variable1)) srvyr::rename(., ByGroup = !! by_variable1a) else .} %>% 
                    {if (!is.null(by_variable1)) srvyr::select(., xstate, yr, ByVar, ByGroup, variable, label, Per_A, Per_A_MOE, n, ntotal, Per, Per_low, Per_upp, Per_se, EstN, EstN_low, EstN_upp, EstN_se) 
                      else srvyr::select(., xstate, yr, variable, label, Per_A, Per_A_MOE, n, ntotal, Per, Per_low, Per_upp, Per_se, EstN, EstN_low, EstN_upp, EstN_se)}
  return(xi)
}
