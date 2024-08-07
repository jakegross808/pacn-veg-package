#' Returns masterlist of all datasheets to determine where a species was recorded
#'
#' @description  'qc_sp_datasheets()' returns all datasheets (Cover,
#' Small Trees, Shrub-belt, Large Trees) so that the location where a species
#' was recorded can be pinpointed.
#'
#' All filters are optional. To ignore a filter, omit it or set it to NA.
#'
#' @inheritParams FilterPACNVeg
#'
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#' sp_records_from_plot_46 <- qc_sp_datasheets(sample_frame = "Olaa", plot_number = 46, species_code = "MYRLES")
#'
#'
#' }
qc_sp_datasheets <- function(park, sample_frame, community, year, cycle, plot_type, plot_number, species_code, sci_name, nativity, is_qa_plot = FALSE, silent = TRUE) {

  # load species tables...

  # Can load presence table in also at some point...
  #filter_presence <- FilterPACNVeg("Presence", park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, plot_number = plot_number, species_code = species_code, sci_name = sci_name, nativity = nativity, is_qa_plot = is_qa_plot, silent = silent)
  filter_sm_woody <- FilterPACNVeg("SmWoody", park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, plot_number = plot_number, species_code = species_code, sci_name = sci_name, nativity = nativity, is_qa_plot = is_qa_plot, silent = silent)
  filter_understory <- FilterPACNVeg("Understory", park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, plot_number = plot_number, species_code = species_code, sci_name = sci_name, nativity = nativity, is_qa_plot = is_qa_plot, silent = silent)
  filter_lg_trees <- FilterPACNVeg("LgTrees", park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, plot_number = plot_number, species_code = species_code, sci_name = sci_name, nativity = nativity, is_qa_plot = is_qa_plot, silent = silent)

  # prepare small woody species info for row bind...
  sp_sm_woody <- filter_sm_woody %>%
    tidyr::drop_na(Code, Scientific_Name) %>%
    dplyr::mutate(Datasheet = dplyr::case_when(Sample_Area == "Quad 4" ~ "Small Trees",
                                               Sample_Area == "Transect 3" ~ "Shrub Belt",
                                               TRUE ~ "Other")) %>%
    dplyr::mutate(Point = "") %>%
    dplyr::select(Sampling_Frame, Cycle, Plot_Number, Code, Scientific_Name, Life_Form, Nativity, Datasheet, Point, Status, Comments, Certified, Verified)

  # prepare understory species info for row bind...
  sp_understory <- filter_understory %>%
    tidyr::drop_na(Code, Scientific_Name) %>%
    dplyr::mutate(Datasheet = "Cover") %>%
    #dplyr::mutate(Status = dplyr::case_when(Dead == TRUE ~ "Dead",
    #                          Dead == FALSE ~ "Live")) %>%
    #Dead may have been removed from understory (4/19/2022) changing to [Status == "Live"] because no cover points should be dead.
    dplyr::mutate(Status = "Live") %>%
    dplyr::mutate(Comments = "") %>% # understory does not have comments so add blank
    dplyr::mutate(Point = as.character(Point)) %>%
    dplyr::select(Sampling_Frame, Cycle, Plot_Number, Code, Scientific_Name, Life_Form, Nativity, Datasheet, Point, Status, Comments, Certified, Verified)

  # prepare large tree species info for row bind...
  sp_lg_trees <- filter_lg_trees %>%
    tidyr::drop_na(Code, Scientific_Name) %>%
    dplyr::mutate(Datasheet = "Large Trees") %>%
    dplyr::mutate(Comments = "") %>% # filter for Large Trees does not currently pull comments, so add blank
    dplyr::mutate(Point = Quad) %>%
    dplyr::select(Sampling_Frame, Cycle, Plot_Number, Code, Scientific_Name, Life_Form, Nativity, Datasheet, Point, Status, Comments, Certified, Verified)

  # row bind
  sp_datasheets <- dplyr::bind_rows(sp_understory, sp_sm_woody, sp_lg_trees)

  return(sp_datasheets)
}

#' Check for completeness of species presence data by plot.
#'
#' @description  'qc_presence_complete()' compares species recorded on the
#' Presence datasheet to the species recorded on other datasheets (Cover,
#' Small Trees, Shrub-belt, Large Trees). Species that may have been missed
#' on the Presence datasheet are listed.
#'
#' All filters are optional. To ignore a filter, omit it or set it to NA.
#'
#' @param all_records TRUE = Return all records (e.g. every cover hit), FALSE = Return 1st record of species per plot
#'
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#'
#' species_missed <- qc_presence_complete()
#'
#' HAVO_species_missed <- qc_presence_complete(park = "HAVO")
#'
#' }
qc_presence_complete <- function(all_records = TRUE, park, sample_frame, community, year, cycle, plot_type, plot_number, species_code, sci_name, nativity, is_qa_plot = FALSE, silent = TRUE) {

  # sp_datasheets
  sp_datasheets <- pacnvegetation::qc_sp_datasheets(park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, plot_number = plot_number, species_code = species_code , sci_name = sci_name, nativity = nativity, is_qa_plot = is_qa_plot, silent = silent)

  filter_presence <- FilterPACNVeg("Presence", park = park, sample_frame = sample_frame, community = community, year = year, cycle = cycle, plot_type = plot_type, plot_number = plot_number, species_code = species_code, sci_name = sci_name, nativity = nativity, is_qa_plot = is_qa_plot, silent = silent)

  # filter out species that are found on both "Presence" and other datasheets
  not_on_presence <- sp_datasheets %>%
    dplyr::anti_join(filter_presence, by = c("Sampling_Frame", "Cycle", "Plot_Number", "Code"))

  # create and populate column "Genus_level" for species that were only identified to Genus level
  genus_sp_not_on_presence <- not_on_presence %>%
    dplyr::mutate(Genus_Sp = dplyr::case_when(
      stringr::str_detect(Code, "SPP$") ~ Scientific_Name,
      stringr::str_detect(Code, "SP\\.") ~ Scientific_Name,
      TRUE ~ "")) %>%
    dplyr::mutate(Genus_level = stringr::word(Genus_Sp, 1))

  # create and populate same "Genus_level" column for all Presence data, including those identified to species
  genus.Presence <- filter_presence %>%
    dplyr::mutate(Genus_level = stringr::word(Scientific_Name, 1))

  # if any "Genus Sp." on other datasheet matches same Genus on Presence data, it gets removed.
  not_on_presence_sp <- genus_sp_not_on_presence %>%
    dplyr::anti_join(genus.Presence, by = c("Sampling_Frame", "Cycle", "Plot_Number", "Genus_level"))

  # Two options: show all records (e.g. each understory hit), or show 1 record per plot+species
  if (all_records == TRUE) {
    qc_presence <- not_on_presence_sp %>%
      dplyr::select(-Genus_level, -Genus_Sp, -Certified) %>%
      dplyr::arrange(Cycle, Sampling_Frame, Plot_Number)
  }

  if (all_records == FALSE) {
    qc_presence <- not_on_presence_sp %>%
      dplyr::distinct(Cycle, Sampling_Frame, Plot_Number, Code, .keep_all = TRUE) %>%
      dplyr::select(-Genus_level, -Genus_Sp, -Certified) %>%
      dplyr::arrange(Cycle, Sampling_Frame, Plot_Number)
  }

  return(qc_presence)
}
