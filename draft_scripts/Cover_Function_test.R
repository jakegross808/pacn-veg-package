Cover_test <- function(plant_grouping) {

  understory <- FilterPACNVeg("Understory", sample_frame = "Olaa", is_qa_plot = FALSE)

  understory2 <- understory %>%
    mutate(Cycle = as.factor(Cycle),
           Plot_Number = as.factor(Plot_Number),
           Stratum = replace_na(Stratum, "No_Veg"),
           Nativity = replace_na(Nativity, "No_Veg"))#,
           #Life_Form = replace_na(Life_Form, "No_Veg"),
           #Code = replace_na(Code, "No_Veg"),
           #Scientific_Name = replace_na(Scientific_Name, "No_Veg"))

  all_vars <- c("Unit_Code", "Sampling_Frame", "Cycle", "Year",
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

  all_vars <- c(all_vars, new_vars)
  all_vars_minus_point <- all_vars[all_vars != "Point"]

  arrange_remove <- c("Cycle", "Year", "Point")
  arrange_vars <- all_vars[!all_vars %in% arrange_remove]

  understory3 <- understory2 %>%
    dplyr::group_by(dplyr::across(all_vars)) %>%
    dplyr::summarise(Hits = dplyr::n(), .groups = 'drop') %>%
    # group hits by plot (remove Point from grouping variable)
    dplyr::group_by(dplyr::across(all_vars_minus_point)) %>%
    # Total hits at each point for each strata for entire plot
    #   (can be > 300 points or >100% because more than one 'Hit' can be present per point-strata)
    dplyr::summarise(Cover = (sum(Hits)) / 300 * 100, .groups = 'drop')

  understory4 <- understory3 %>%
    # Insert "0" for cover if category does not exist (for example no hits for non-natives in High Stratum)
    tidyr::complete(tidyr::nesting(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle, Stratum),
                    #tidyr::nesting(Nativity, Life_Form)) %>% # This works
                    #tidyr::nesting(Nativity)) %>% # This also works
                    tidyr::nesting(syms(!!!new_vars))) %>% # This doesn't work
    # Arrange table so that difference in cover between cycles can be calculated easily (example - cycle 1 value for
    #   cover is followed by cycle 2 value for cover).
    dplyr::group_by(dplyr::across(arrange_vars)) %>%
    dplyr::arrange(Cycle, Year, .by_group = TRUE) %>%
    dplyr::ungroup()

  return(understory4)
}


#To run test use:
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
