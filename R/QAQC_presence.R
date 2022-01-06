
#' Check completeness of species presence data throughout plot
#'
#' @details All filters are optional. To ignore a filter, omit it or set it to NA.
#'
#' @inheritParams FilterPACNVeg
#' @param all.records Return all records, or just 1st record for a species in a plot
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' all_plots_presence <- chkPresence()
#' all_hawaii_plots <- chkPresence(park = c("KALA", "KAHO", "HAVO", "HALE"))
#' }
chkPresence <- function(all.records = TRUE, park, sample_frame, community, year, cycle, plot_type, silent = FALSE) {

  # load species tables...
  Filter_Presence <- FilterPACNVeg("Presence", sample_frame, community, year, cycle, plot_type, is_qa_plot, silent = silent)
  Filter_SmWoody <- FilterPACNVeg("SmWoody", park, sample_frame, community, year, cycle, plot_type, is_qa_plot, silent = silent)
  Filter_Understory <- FilterPACNVeg("Understory", park, sample_frame, community, year, cycle, plot_type, is_qa_plot, silent = silent)
  Filter_LgTrees <- FilterPACNVeg("LgTrees", park, sample_frame, community, year, cycle, plot_type, is_qa_plot, silent = silent)

  # prepare small woody species info for row bind...
  sp.SmWoody <- Filter_SmWoody %>%
    drop_na(Code, Scientific_Name) %>%
    mutate(Datasheet = case_when(Transect == "Quad 4" ~ "Small Trees",
                                 Transect == "Transect 3" ~ "Shrub Belt",
                                 TRUE ~ "Other")) %>%
    mutate(Point = NA) %>%
    select(Sampling_Frame, Cycle, Plot_Number, Code, Scientific_Name, Life_Form, Nativity, Datasheet, Point, Status, Comments, Certified, Verified)

  # prepare understory species info for row bind...
  sp.Understory <- Filter_Understory %>%
    drop_na(Code, Scientific_Name) %>%
    mutate(Datasheet = "Cover") %>%
    mutate(Status = case_when(Dead == TRUE ~ "Dead",
                              Dead == FALSE ~ "Live")) %>%
    mutate(Comments = NA) %>%
    mutate(Point = as.character(Point)) %>%
    select(Sampling_Frame, Cycle, Plot_Number, Code, Scientific_Name, Life_Form, Nativity, Datasheet, Point, Status, Comments, Certified, Verified)

  # prepare large tree species info for row bind...
  sp.LgTrees <- Filter_LgTrees %>%
    drop_na(Code, Scientific_Name) %>%
    mutate(Datasheet = "Large Trees") %>%
    mutate(Comments = NA) %>%
    mutate(Point = Quad) %>%
    select(Sampling_Frame, Cycle, Plot_Number, Code, Scientific_Name, Life_Form, Nativity, Datasheet, Point, Status, Comments, Certified, Verified)

  # row bind
  sp.Datasheets <- bind_rows(sp.Understory, sp.SmWoody, sp.LgTrees)

  # filter out species that are found on both "Presence" and other datasheets
  not.on.presence <- sp.Datasheets %>%
    anti_join(Filter_Presence, by = c("Sampling_Frame", "Cycle", "Plot_Number", "Code"))

  # create and populate column "Genus_level" for species that were only identified to Genus level
  genus.sp.not.on.presence <- not.on.presence %>%
    mutate(Genus_Sp = case_when(
      str_detect(Code, "SPP$") ~ Scientific_Name,
      str_detect(Code, "SP\\.") ~ Scientific_Name,
      TRUE ~ "")) %>%
    mutate(Genus_level = word(Genus_Sp, 1))

  # create and populate same "Genus_level" column for all Presence data, including those identified to species
  genus.Presence <- Filter_Presence %>%
    mutate(Genus_level = word(Scientific_Name, 1))

  # if any "Genus Sp." on other datasheet matches same Genus on Presence data, it gets removed.
  not.on.presence.sp <- genus.sp.not.on.presence %>%
    anti_join(genus.Presence, by = c("Sampling_Frame", "Cycle", "Plot_Number", "Genus")) %>%
    arrange(Cycle, Sampling_Frame)

  # Two options: show all records (e.g. each understory hit), or show 1 record per plot+species
  if (all.records == TRUE) {
    qaqc_presence <- not.on.presence.sp %>%
      select(-Genus, -Genus_Sp, -Certified) %>%
      arrange(Cycle, Sampling_Frame, Plot_Number)
  }

  if (all.records == FALSE) {
    qaqc_presence <- not.on.presence.sp %>%
      distinct(Cycle, Sampling_Frame, Plot_Number, Code, .keep_all = TRUE) %>%
      select(-Genus, -Genus_Sp, -Certified) %>%
      arrange(Cycle, Sampling_Frame, Plot_Number)
  }

  return(qaqc_presence)
}
