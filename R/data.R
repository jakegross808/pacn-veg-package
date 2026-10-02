#' PACN vegetation place names and sampling-frame abbreviations
#'
#' Lookup table of unit codes, park names, islands, plant communities, and
#' formal (diacritical) sampling-frame names used across PACN vegetation
#' protocols.
#'
#' @format A tibble with one row per sampling frame and columns:
#' \describe{
#'   \item{Unit_Code}{Four-letter park unit code.}
#'   \item{Park_Name}{Full park name.}
#'   \item{State_Territory}{State or territory.}
#'   \item{Island}{Island name.}
#'   \item{Community}{Plant community.}
#'   \item{abbr_Community}{Community abbreviation.}
#'   \item{Sampling_Frame}{Sampling frame name (ASCII).}
#'   \item{abbr_Sampling_Frame}{Two-letter sampling-frame abbreviation.}
#'   \item{Formal_Sampling_Frame}{Sampling frame name with Hawaiian diacriticals.}
#' }
#' @source PACN Inventory & Monitoring program.
"PACN_veg_names"

#' Plot event metadata with management units
#'
#' Event-level attributes for PACN vegetation plots, including spatial
#' coordinates and assigned management unit / zone.
#'
#' @format A tibble with one row per plot event. Columns include Unit_Code,
#'   Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, coordinates, and
#'   management-unit fields.
#' @source PACN Inventory & Monitoring program.
"events_extra_xy_mgmt"
