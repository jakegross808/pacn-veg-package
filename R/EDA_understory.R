
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
UnderNativityCoverTotal <- function(combine_strata = FALSE, paired_change = FALSE, park, sample_frame, community, year, cycle, plot_type, silent = FALSE) {

  raw_data <- FilterPACNVeg("Understory", park, sample_frame, community, year, cycle, plot_type, is_qa_plot = FALSE, silent = silent)
  if (combine_strata == TRUE) {
    raw_data <- UnderCombineStrata(raw_data)
  }

  # Calculate Total Native & Nonnative Cover by stratum
  Nat_Cov <- raw_data %>%
    # Drop point records if point had no hits: (drop if 'Code == NA')
    tidyr::drop_na(Code)  %>%
    dplyr::group_by(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Point, Stratum, Nativity) %>%
    # Count Species at each cover point (for Native & Non-native within each Strata):
    dplyr::summarise(Hits_All_Nat = dplyr::n())  %>%
    # group by plot (remove Point from grouping variable)
    dplyr::group_by(Unit_Code, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Stratum, Nativity) %>%
    #Total hits at each point for each strata for entire plot
    # (can be > 300 points or >100% because more than one native species can be present per point)
    dplyr::summarise(tot_pct_cov = (sum(Hits_All_Nat)) / 300 * 100, .groups = 'drop')

  if (paired_change == TRUE) {
    paired_plots <- RemoveSingleVisits(raw_data)
    p <- paired_plots$Plot_Number
    Nat_Cov <- Nat_Cov %>%
      dplyr::filter(Plot_Number %in% p) %>%
      #group_by("Unit_Code", "Sampling_Frame","Plot_Number","Nativity") %>%
      tidyr::complete(tidyr::nesting(S_Cycle, Unit_Code, Sampling_Frame, Plot_Number, Strata, Nativity),
               tidyr::fill = list(tot_pct_cov = 0)) %>%
      pivot_wider(names_from = S_Cycle, values_from = tot_pct_cov) %>%
      mutate(tot_pct_cov_chg = round(`2` - `1`, 2))

  }

  return(Nat_Cov)
}


