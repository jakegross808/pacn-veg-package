
#' Combine Strata low & high
#'
#' @param data An Understory tibble/dataframe with Species Point Data
#'
#' @return The Understory tibble/dataframe with one stratum "combined"
#' @export
#'
#' @examples
#' \dontrun{
#' data <- FilterPACNVeg("Understory")
#' understory_with_one_stratum <- CombineStrata(data)
#' }
UnderCombineStrata <- function(data) {
  if(!"Point" %in% colnames(data)){
    stop("Invalid data table. Point data is missing")
  }

  data_strat_comb <- data %>%
    #Stratum removed from grouping variable
    dplyr::select(-Stratum) %>%
    unique() %>%
    dplyr::mutate(Stratum = "High and Low")
  return(data_strat_comb)
}


#' Calculate Total Native & Nonnative Cover in Understory
#'
#' @inheritParams FilterPACNVeg
#'
#' @return Summary table of total Percent Cover by Nativity
#' @export
#'
#' @examples
#' \dontrun{
#' data <- FilterPACNVeg("Understory")
#' Native_Cover_Summary_table <- UnderNativityCoverTotal(data)
#' }
UnderNativityCoverTotal <- function(combine_strata = FALSE, park, sample_frame, community, year, cycle, plot_type, silent = FALSE) {

  raw_data <- FilterPACNVeg("Understory", park, sample_frame, community, year, cycle, plot_type, is_qa_plot = FALSE, silent = silent)
  if (combine_strata == TRUE) {
    raw_data <- UnderCombineStrata(raw_data)
  }

  # Calculate Total Native & Nonnative Cover by stratum
  Nat_Cov <- raw_data %>%
    dplyr::group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number, Point, Stratum, Nativity) %>%
    dplyr::summarise(Hits_All_Nat = dplyr::n(), .groups = 'drop')  %>%
    # Don't count record if point had no hits: (e.g. Point is NA )
    dplyr::mutate(Hits_All_Nat = replace(Hits_All_Nat, is.na(Nativity), 0)) %>%
    # group by plot (i.e. remove Point from grouping variable)
    dplyr::group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, Stratum, Nativity) %>%
    #Total hits at each point for each strata for entire plot
    # (can be > 300 points or >100% because more than one native species can be present per point)
    dplyr::summarise(tot_pct_cov = (sum(Hits_All_Nat)) / 300 * 100, .groups = 'drop')

  return(Nat_Cov)
}

