
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
    stop("Invalid data table. Point data is missing")}
  else {
  data_strat_comb <- data %>%
    #Stratum removed from grouping variable
    select(-Stratum) %>%
    unique() %>%
    mutate(Stratum = "High and Low")
  return(data_strat_comb)
}}


#' Calculate Total Native & Nonnative Cover in Understory
#'
#' @param data An Understory tibble/dataframe with Nativity Point Data
#'
#' @return Summary table of total Percent Cover by Nativity
#' @export
#'
#' @examples
#' \dontrun{
#' data <- FilterPACNVeg("Understory")
#' Native_Cover_Summary_table <- UnderNativityCoverTotal(data)
#' }
UnderNativityCoverTotal <- function(park, sample_frame, community, year, cycle, plot_type) {

  raw_data <- FilterPACNVeg("Understory", park, sample_frame, community, year, cycle, plot_type)
  # Calculate Total Native & Nonnative Cover by stratum
  Nat_Cov <- raw_data %>%
    group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number, Point, Stratum, Nativity) %>%
    summarise(Hits_All_Nat = n(), .groups = 'drop')  %>%
    # Don't count record if point had no hits: (e.g. Point is NA )
    mutate(Hits_All_Nat = replace(Hits_All_Nat, is.na(Nativity), 0)) %>%
    # group by plot (i.e. remove Point from grouping variable)
    group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, Stratum, Nativity) %>%
    #Total hits at each point for each strata for entire plot
    # (can be > 300 points or >100% because more than one native species can be present per point)
    summarise(tot_pct_cov = (sum(Hits_All_Nat)) / 300 * 100, .groups = 'drop')

  return(Nat_Cov)
}

# library(tidyverse)
# undstr <- veg$Understory
# fun1 <- undstr %>%
#   group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Number, Point, Stratum, Nativity) %>%
#   summarize(Hits_All_Nat = n(), .groups = 'drop')  %>%
#   # Don't count record if point had no hits: (e.g. Point is NA )
#   mutate(Hits_All_Nat = replace(Hits_All_Nat, is.na(Nativity), 0)) %>%
#   # group by plot (i.e. remove Point from grouping variable)
#   group_by(Cycle, Unit_Code, Sampling_Frame, Plot_Number, Stratum, Nativity) %>%
#   #Total hits at each point for each strata for entire plot
#   # (can be > 300 points or >100% because more than one native species can be present per point)
#   summarise(tot_pct_cov = (sum(Hits_All_Nat)) / 300 * 100, .groups = 'drop')
