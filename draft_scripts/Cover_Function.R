Cover_test <- function(plant_grouping, paired_change = FALSE) {

  understory <- FilterPACNVeg("Understory", park = "HAVO", is_qa_plot = FALSE)

  understory2 <- understory %>%
    mutate(Plot_Number = as.factor(Plot_Number),
           Stratum = replace_na(Stratum, "No_Veg"))

  base_vars <- c("Unit_Code", "Sampling_Frame", "Cycle", "Year",
                "Plot_Type", "Plot_Number", "Point", "Stratum")

  if (plant_grouping == "All") {
    new_vars <- c()
  }

  if (plant_grouping == "Nativity") {
    new_vars <- c("Nativity")
  }

  if (plant_grouping == "Life_Form") {
    new_vars <- c("Nativity", "Life_Form")
  }

  if (plant_grouping == "Species") {
    new_vars <- c("Nativity", "Life_Form", "Code", "Scientific_Name")
  }

  all_vars <- c(base_vars, new_vars)
  all_vars_minus_point <- all_vars[all_vars != "Point"]

  understory3 <- understory2 %>%
    dplyr::group_by(dplyr::across(all_vars)) %>%
    dplyr::summarise(Hits = dplyr::n(), .groups = 'drop') %>%
    # group hits by plot (remove Point from grouping variable)
    dplyr::group_by(dplyr::across(all_vars_minus_point)) %>%
    # Total hits at each point for each strata for entire plot
    #   (can be > 300 points or >100% because more than one 'Hit' can be present per point-strata)
    dplyr::summarise(Cover = (sum(Hits)) / 300 * 100, .groups = 'drop')


  if (paired_change == FALSE) {
    return(understory3)

  }

  if (paired_change == TRUE) {

    #understory4 <- pacnvegetation:::RemoveSingleVisits(understory3)
    understory4 <- understory3 %>%
      filter(Plot_Type == "Fixed")

    var_nest1 <- c("Unit_Code", "Sampling_Frame", "Cycle", "Year", "Plot_Type", "Plot_Number")
    var_nest2 <- c("Stratum", new_vars)
    arrange_remove <- c("Cycle", "Year", "Point")
    arrange_vars <- all_vars[!all_vars %in% arrange_remove]
    max_cycle <- understory4 %>% pull(Cycle) %>% max()
    max_cycle_lable <- as_label(max_cycle)
    max_cycle_lable <- paste0("Cycle", max_cycle_lable, "vs1")


    understory4 <- understory4 %>%
      # Insert "0" for cover if category does not exist (for example no hits for non-natives in High Stratum)
      #tidyr::complete(tidyr::nesting(!!!syms(base_vars_minus_point)),
      #              tidyr::nesting(!!!syms(new_vars))) %>% # This should work now!
      tidyr::complete(tidyr::nesting(!!!syms(var_nest1)),
                      tidyr::nesting(!!!syms(var_nest2)),
                      fill = list(Cover = 0)) %>% # This should work now!
      # Arrange table so that difference in cover between cycles can be calculated easily (example - cycle 1 value for
      #   cover is followed by cycle 2 value for cover).
      dplyr::group_by(dplyr::across(arrange_vars)) %>%
      dplyr::arrange(Cycle, Year, .by_group = TRUE) %>%
      # Calculate the change in cover per cycle
      dplyr::mutate(Chg_Prior = Cover - dplyr::lag(Cover, order_by = Cycle)) %>%
      dplyr::mutate(Years_Prior = Year - dplyr::lag(Year, order_by = Cycle)) %>%
      dplyr::mutate(Chg_Per_Year = Chg_Prior / Years_Prior) %>%
      dplyr::mutate(!!max_cycle_lable := Cover - dplyr::lag(Cover, order_by = Cycle,
                                                    n = max_cycle-1)) %>%
      ungroup()

    return(understory4)

    }

}



# # To run test use:
# library(pacnvegetation)
# library(tidyverse)
#
# LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
#                          "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20211208.mdb",
#                          "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
#             cache = TRUE, force_refresh = FALSE)
#
# test_All <- Cover_test(plant_grouping = "All")
# test_Nat2 <- Cover_test(plant_grouping = "Nativity")
# test_LF2 <- Cover_test(plant_grouping = "Life_Form")
# test_SP <- Cover_test(plant_grouping = "Species")
