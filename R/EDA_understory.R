
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
    ## Drop point records if point had no hits: (drop if 'Code == NA')
    tidyr::drop_na(Code)  %>%
    # Count Species at each cover point (for Native & Non-native within each Strata):
    dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Point, Stratum, Nativity, Year, Cycle) %>%
    dplyr::summarise(Hits_All_Nat = dplyr::n(), .groups = 'drop')  %>%
    # group hits by plot (remove Point from grouping variable)
    dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity, Year, Cycle) %>%
    # Total hits at each point for each strata for entire plot
    #   (can be > 300 points or >100% because more than one native species can be present per point)
    dplyr::summarise(tot_pct_cov = (sum(Hits_All_Nat)) / 300 * 100, .groups = 'drop') %>%
    # Insert "0" for cover if category does not exsist (for example no hits for non-natives in High Stratum)
    tidyr::complete(tidyr::nesting(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle), Stratum, Nativity,
                    fill = list(tot_pct_cov = 0)) %>%
    # Arrange table so that difference in cover between cycles can be calculated easily (example - cycle 1 value for
    #   cover is followed by cycle 2 value for cover).
    dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity) %>%
    dplyr::arrange(Cycle, .by_group = TRUE) %>%
    dplyr::ungroup()

  if (paired_change == TRUE) {
    paired_plots <- RemoveSingleVisits(raw_data)
    p <- paired_plots$Plot_Number
    Nat_Cov <- Nat_Cov %>%
      # remove plots that were only sampled once (this removes all rotationals and possibly some fixed plots if only sampled once)
      dplyr::filter(Plot_Number %in% p) %>%
      dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Stratum, Nativity) %>%
      # Calculate the change in cover per cycle
      dplyr::mutate(chg_per_cycle = tot_pct_cov - dplyr::lag(tot_pct_cov, order_by = Cycle))

  }

  return(Nat_Cov)
}


