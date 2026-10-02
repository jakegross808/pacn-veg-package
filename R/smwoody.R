# ______________________________________________________________________________
# ---- prep_smwoody() ----
# Clean SmWoody and return an analysis-ready, zero-filled density-prep table.
#
#   x <- prep_smwoody(SmWoody)          # x IS a tibble; pipes normally
#   nonconforming(x)                    # log of every conversion / drop
#
# Repairs (each emits a warning + is logged):
#   * drop blank records (LF_Sm_Woody NA)
#   * life-form conversions: Fern->Small Tree Fern; Tree/Unknown->by DBH bin
#   * Sample_Area typo "QuAd 4"->"Quad 4"
#   * schema cleaning: non-forest Rooting->NA; SNAG Live->Dead; small-tree
#     stray Length->NA  (forest NA-rooting is left as-is for manual review)
#   * subplot reassignment to the protocol-correct Sample_Area
#   * residual unplaceable rows are dropped
# ______________________________________________________________________________

# ______________________________________________________________________________
# ---- prep_smwoody() : clean + zero-fill SmWoody ----
#
#   x <- prep_smwoody(SmWoody)          # x IS a tibble; pipes normally
#   nonconforming(x)                    # log of every conversion / repair / drop
#
# Package deps (declared in DESCRIPTION Imports): dplyr, tidyr, purrr, rlang,
# tibble. All calls are fully namespaced; @importFrom tags on the roxygen block
# below register the pipe/operators used.
# ______________________________________________________________________________

# ---- ...static schema constants ----------------------------------------------
LEGIT_LF   <- c("Seedling", "Small Tree", "Shrub", "Vine", "Small Tree Fern")
NON_FOREST <- c("Subalpine Shrubland", "Coastal Strand")
FOREST_COMM <- c("Wet Forest", "Limestone Forest", "Mangrove Wetland")
ROOTING_ALL <- c("R1", "R2", "R3", "R4", "R5")

#' @noRd
comm_group_of <- function(community) {
  dplyr::case_when(
    community %in% FOREST_COMM        ~ "Forest",
    community == "Coastal Strand"     ~ "Coastal Strand",
    community == "Subalpine Shrubland"~ "Subalpine Shrubland",
    TRUE                              ~ NA_character_
  )
}

# Protocol-correct Sample_Area(s) by life form x community group.
#   Forest:         Small Tree -> Quad 4; others -> Transect 3
#   Coastal Strand: ALL life forms -> both Transect 1 and Transect 3
#   Subalpine:      others -> Transect 3; Small Tree -> INVALID (measured with
#                   large trees, not in SmWoody) -> returns character(0)
# character(0) signals "this life form does not exist in this community" so the
# caller drops the record rather than reassigning it.
#' @noRd
correct_subplots <- function(life_form, comm_group) {
  if (comm_group == "Coastal Strand") return(c("Transect 1", "Transect 3"))
  if (life_form == "Small Tree") {
    if (comm_group == "Subalpine Shrubland") return(character(0))  # invalid
    return("Quad 4")                                              # forest
  }
  "Transect 3"                                    # seedling/shrub/vine/fern
}

# ---- ...nonconforming() reader -----------------------------------------------
#' Retrieve the nonconforming-records log from a prep_smwoody() result
#'
#' Returns the log of every record that [prep_smwoody()] converted, repaired, or
#' dropped while cleaning the data. Each row is an original (pre-fix) record plus
#' a `disposition` column describing the action taken and why.
#'
#' @param x A tibble returned by [prep_smwoody()]. The log lives in its
#'   `"nonconforming"` attribute.
#'
#' @return A tibble of nonconforming source records with a `disposition` column.
#'   Returns an empty tibble (with a `disposition` column) if nothing was flagged.
#' @export
#'
#' @details The log is stored as an attribute, which some dplyr operations strip.
#'   Call `nonconforming()` on the object returned directly by [prep_smwoody()],
#'   before further wrangling; otherwise it errors rather than silently returning
#'   nothing.
#'
#' @seealso [prep_smwoody()]
#'
#' @examples
#' \dontrun{
#' sw_prepped <- prep_smwoody(FilterPACNVeg(data_name = "SmWoody"))
#' nonconforming(sw_prepped)
#' dplyr::count(nonconforming(sw_prepped), disposition)
#' }
nonconforming <- function(x) {
  log <- attr(x, "nonconforming")
  if (is.null(log)) {
    stop("No `nonconforming` attribute found. It was likely dropped by a ",
         "downstream operation -- call nonconforming() on the object returned ",
         "directly by prep_smwoody(), before further wrangling.")
  }
  log
}

# ==============================================================================
#' Prepare SmWoody data: clean, validate, and zero-fill for density analysis
#'
#' Cleans the raw SmWoody (small woody / regeneration) data to conform to the
#' PACN protocol schema, then returns a fully zero-filled table with one row per
#' plot x Sample_Area x species x size/category combination, ready for density
#' and frequency analyses. Every legitimate category combination is present for
#' every species in the plot's Sampling_Frame (absent combinations get
#' `Count = 0`).
#'
#' Records that violate the schema are handled in one of three ways, each logged
#' and reported via a `warning()`:
#' \itemize{
#'   \item \strong{Converted} - life-form relabels: `Fern` -> `Small Tree Fern`;
#'     `Tree`/`Unknown` -> `Seedling`/`Small Tree` by DBH bin
#'     (`Tree` with missing DBH -> `Small Tree`; `Unknown` with missing DBH ->
#'     `Shrub`).
#'   \item \strong{Repaired} - single-fault fixes: `Sample_Area` typo
#'     `"QuAd 4"` -> `"Quad 4"`; non-forest `Rooting` -> `NA`; live `SNAG` ->
#'     `Dead`; missing DBH assigned a default when it is the only fault
#'     (`Small Tree` -> `"1<5"`, `Small Tree Fern` -> `"<10"`); records in a
#'     protocol-wrong `Sample_Area` reassigned to the correct subplot
#'     (Coastal Strand ambiguous cases go to `Transect 1` by convention).
#'   \item \strong{Dropped} - unrecoverable records: blank rows
#'     (`LF_Sm_Woody` is `NA`); life forms not sampled in a community (e.g.
#'     `Small Tree` in Subalpine Shrubland, which is measured with large trees);
#'     and combinations with two or more broken/impossible fields.
#' }
#'
#' The full log of every conversion, repair, and drop is attached to the result
#' as a `"nonconforming"` attribute and retrieved with [nonconforming()]. Use it
#' to locate and fix source-database errors.
#'
#' Protocol schema enforced by the zero-fill grid:
#' \tabular{lllll}{
#'   \strong{Life form} \tab \strong{DBH} \tab \strong{Length} \tab \strong{Sample_Area (Forest / Coastal Strand / Subalpine)} \cr
#'   Seedling        \tab `<1`          \tab L1-L3               \tab Transect 3 / T1 + T3 / Transect 3 \cr
#'   Small Tree      \tab `1<5`, `5<10` \tab NA (not measured)   \tab Quad 4     / T1 + T3 / -- (not sampled) \cr
#'   Shrub           \tab NA            \tab L1-L3 (+L0 in Coastal Strand) \tab Transect 3 / T1 + T3 / Transect 3 \cr
#'   Vine            \tab NA            \tab L1-L3               \tab Transect 3 / T1 + T3 / Transect 3 \cr
#'   Small Tree Fern \tab `<10`         \tab L0-L3               \tab Transect 3 / T1 + T3 / Transect 3 \cr
#' }
#' `Rooting` is `R1`-`R5` in forest communities and `NA` in non-forest
#' (Subalpine Shrubland, Coastal Strand). `Status` is `Live` or `Dead` for all
#' life forms (`SNAG` is forced to `Dead`).
#'
#' @param data A SmWoody data frame, e.g. from
#'   `FilterPACNVeg(data_name = "SmWoody")`. Must contain the columns
#'   `Unit_Code`, `Sampling_Frame`, `Community`, `Cycle`, `Plot_Number`,
#'   `Sample_Area`, `LF_Sm_Woody`, `DBH`, `Length`, `Status`, `Rooting`, `Code`,
#'   `Scientific_Name`, `Nativity`, `Life_Form`, and `Count`.
#' @param life_forms Character vector of life forms to retain, a subset of
#'   `c("Seedling", "Small Tree", "Shrub", "Vine", "Small Tree Fern")`. Defaults
#'   to all five. Filtering is applied to each record's \emph{final} life form
#'   (after conversion), so requesting `"Small Tree Fern"` correctly includes
#'   records originally labelled `"Fern"`.
#'
#' @return A tibble with one row per plot x Sample_Area x species x category
#'   combination, including `Count` (0 where the combination was not observed)
#'   and a `"nonconforming"` attribute (see [nonconforming()]).
#' @export
#'
#' @seealso [nonconforming()] to retrieve the log of cleaned/dropped records.
#'
#' @importFrom dplyr filter mutate case_when if_else distinct across all_of
#' @importFrom dplyr left_join anti_join group_by summarise ungroup rowwise
#' @importFrom dplyr bind_rows arrange select first any_of n
#' @importFrom tidyr expand_grid crossing replace_na
#' @importFrom purrr pmap compact
#'
#' @examples
#' \dontrun{
#' # All five life forms
#' sw_prepped <- prep_smwoody(FilterPACNVeg(data_name = "SmWoody"))
#'
#' # A single life form
#' seedlings <- prep_smwoody(FilterPACNVeg(data_name = "SmWoody"),
#'                           life_forms = "Seedling")
#'
#' # Review what was changed or dropped, then fix at the source
#' nonconforming(sw_prepped)
#' dplyr::count(nonconforming(sw_prepped), disposition)
#' }
prep_smwoody <- function(data,
                         life_forms = LEGIT_LF) {

  life_forms <- match.arg(life_forms, LEGIT_LF, several.ok = TRUE)
  log <- list()                       # accumulate nonconformer records
  logit <- function(df, disposition) {
    if (nrow(df) == 0) return(invisible())
    log[[length(log) + 1]] <<- dplyr::mutate(df, disposition = disposition)
  }

  # ---- ...Step 1: resolve final life form, THEN restrict to requested -------
  # Conversions change LF_Sm_Woody, so they must be computed before filtering
  # (a raw 'Fern' only becomes 'Small Tree Fern' after conversion). We compute
  # the final life form on all rows, then keep only requested life_forms, so
  # that every subsequent warning/log entry pertains to the returned dataset.
  #
  # Blank records (LF_Sm_Woody NA, Count NA) are dropped up front regardless.
  blanks <- data |> dplyr::filter(is.na(LF_Sm_Woody))

  d0 <- data |>
    dplyr::filter(!is.na(LF_Sm_Woody)) |>
    dplyr::mutate(
      .lf_final = dplyr::case_when(
        LF_Sm_Woody == "Fern" ~ "Small Tree Fern",
        LF_Sm_Woody == "Tree"    & DBH == "<1"              ~ "Seedling",
        LF_Sm_Woody == "Tree"    & DBH %in% c("1<5","5<10") ~ "Small Tree",
        LF_Sm_Woody == "Tree"    & is.na(DBH)               ~ "Small Tree",
        LF_Sm_Woody == "Unknown" & DBH == "<1"              ~ "Seedling",
        LF_Sm_Woody == "Unknown" & DBH %in% c("1<5","5<10") ~ "Small Tree",
        LF_Sm_Woody == "Unknown" & is.na(DBH)               ~ "Shrub",
        TRUE ~ LF_Sm_Woody
      )
    )

  # Keep only records whose FINAL life form is requested.
  d0 <- d0 |> dplyr::filter(.lf_final %in% life_forms)

  # Now log/warn -- only for conversions that survive into the requested output.
  conv_fern <- d0 |> dplyr::filter(LF_Sm_Woody == "Fern")
  if (nrow(conv_fern) > 0) {
    warning(nrow(conv_fern), " 'Fern' -> 'Small Tree Fern'. ",
            "Use nonconforming() to inspect.", call. = FALSE)
    logit(conv_fern, "converted: Fern -> Small Tree Fern")
  }
  reclass <- d0 |> dplyr::filter(LF_Sm_Woody %in% c("Tree", "Unknown"))
  if (nrow(reclass) > 0) {
    warning(nrow(reclass), " 'Tree'/'Unknown' reclassified by DBH bin. ",
            "Use nonconforming() to inspect.", call. = FALSE)
    logit(reclass, "converted: Tree/Unknown -> by DBH bin")
  }
  # Blank drop is reported only when it is relevant... it always is (blanks have
  # no life form so they can't be filtered by request); report unconditionally.
  if (nrow(blanks) > 0) {
    warning(nrow(blanks), " blank records (LF_Sm_Woody NA) dropped. ",
            "Use nonconforming() to inspect.", call. = FALSE)
    logit(blanks, "dropped: blank record (LF_Sm_Woody NA)")
  }

  # Commit the final life form.
  d <- d0 |>
    dplyr::mutate(LF_Sm_Woody = .lf_final) |>
    dplyr::select(-.lf_final)

  # ---- ...Step 3: Sample_Area typo fix --------------------------------------
  typo <- d |> dplyr::filter(Sample_Area == "QuAd 4")
  if (nrow(typo) > 0) {
    warning(nrow(typo), " Sample_Area 'QuAd 4' -> 'Quad 4'. ",
            "Use nonconforming() to inspect.", call. = FALSE)
    logit(typo, "corrected: Sample_Area QuAd 4 -> Quad 4")
  }
  d <- d |> dplyr::mutate(Sample_Area = dplyr::if_else(Sample_Area == "QuAd 4",
                                                       "Quad 4", Sample_Area))

  # ---- ...Step 4: schema cleaning (Rooting / Status / Length) ----------------
  d <- d |>
    dplyr::mutate(
      Rooting = dplyr::if_else(Community %in% NON_FOREST, NA_character_, Rooting),
      Status  = dplyr::if_else(Code == "SNAG" & Status == "Live", "Dead", Status),
      Length  = dplyr::if_else(DBH %in% c("1<5", "5<10"), NA_character_, Length)
    )

  # ---- ...Step 4b: repair missing DBH (single-fault only) --------------------
  # A record that exists was probably measured; repair when DBH is the SOLE
  # fault, drop (leave for the unplaceable bucket) when 2+ fields are broken.
  #   Small Tree,      DBH NA, Length NA (correct)  -> DBH = "1<5"   [repair]
  #   Small Tree,      DBH NA, Length present       -> leave (2 faults -> dropped later)
  #   Small Tree Fern, DBH NA, Length present (ok)  -> DBH = "<10"   [repair]
  #   Small Tree Fern, DBH NA, Length NA            -> leave (2 faults -> dropped later)
  # Note: Step 4 already ran, but its Length-strip only touches DBH 1<5/5<10, so
  # these NA-DBH rows still carry their original Length here.
  fix_st_dbh <- d |>
    dplyr::filter(LF_Sm_Woody == "Small Tree", is.na(DBH), is.na(Length))
  if (nrow(fix_st_dbh) > 0) {
    warning(nrow(fix_st_dbh), " Small Tree with missing DBH -> assigned '1<5' ",
            "(default; fix at source). Use nonconforming() to inspect.",
            call. = FALSE)
    logit(fix_st_dbh, "repaired: Small Tree missing DBH -> '1<5'")
  }
  fix_stf_dbh <- d |>
    dplyr::filter(LF_Sm_Woody == "Small Tree Fern", is.na(DBH), !is.na(Length))
  if (nrow(fix_stf_dbh) > 0) {
    warning(nrow(fix_stf_dbh), " Small Tree Fern with missing DBH -> assigned ",
            "'<10' (only valid bin). Use nonconforming() to inspect.",
            call. = FALSE)
    logit(fix_stf_dbh, "repaired: Small Tree Fern missing DBH -> '<10'")
  }

  d <- d |>
    dplyr::mutate(
      DBH = dplyr::case_when(
        LF_Sm_Woody == "Small Tree"      & is.na(DBH) & is.na(Length)  ~ "1<5",
        LF_Sm_Woody == "Small Tree Fern" & is.na(DBH) & !is.na(Length) ~ "<10",
        TRUE ~ DBH
      )
    )
  # Compute each row's correct subplot set.
  #   * empty set (character(0)) -> life form invalid in this community -> DROP
  #   * current Sample_Area not in set -> reassign (Coastal Strand -> Transect 1
  #     by convention; else the single correct subplot)
  d <- d |>
    dplyr::mutate(Comm_Group = comm_group_of(Community)) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      .correct = list(correct_subplots(LF_Sm_Woody, Comm_Group)),
      .invalid = length(.correct) == 0,
      .ok      = !.invalid && Sample_Area %in% .correct,
      .target  = dplyr::case_when(
        .invalid                        ~ NA_character_,
        Comm_Group == "Coastal Strand"  ~ "Transect 1",
        TRUE                            ~ dplyr::first(.correct)
      )
    ) |>
    dplyr::ungroup()

  # Invalid life form x community (e.g. Small Tree in Subalpine) -> drop + log.
  invalid_lf <- d |> dplyr::filter(.invalid)
  if (nrow(invalid_lf) > 0) {
    warning(nrow(invalid_lf), " records dropped: life form not sampled in this ",
            "community (e.g. Small Tree in Subalpine Shrubland -> measured with ",
            "large trees). Use nonconforming() to inspect.", call. = FALSE)
    logit(invalid_lf |> dplyr::select(-.correct, -.invalid, -.ok, -.target),
          "dropped: life form not sampled in this community")
  }
  d <- d |> dplyr::filter(!.invalid)

  # Wrong (but valid-elsewhere) subplot -> reassign + log.
  reassigned <- d |> dplyr::filter(!.ok)
  if (nrow(reassigned) > 0) {
    warning(nrow(reassigned), " records reassigned to protocol-correct ",
            "Sample_Area. Use nonconforming() to inspect.", call. = FALSE)
    logit(reassigned |> dplyr::select(-.correct, -.invalid, -.ok, -.target),
          "reassigned: wrong Sample_Area -> protocol subplot")
  }
  d <- d |>
    dplyr::mutate(Sample_Area = dplyr::if_else(.ok, Sample_Area, .target)) |>
    dplyr::select(-.correct, -.invalid, -.ok, -.target)

  # ---- ...Step 6: (life-form restriction already applied in Step 1) ----------

  # ---- ...Step 7: build per-life-form valid category grid --------------------
  # Level sets differ by life form; L0 is fern-always / shrub-only-in-CoastalStrand.
  lf_grid <- function(lf, comm_group) {
    dbh <- switch(lf,
                  "Seedling"        = "<1",
                  "Small Tree"      = c("1<5", "5<10"),
                  "Small Tree Fern" = "<10",
                  "Shrub"           = NA_character_,
                  "Vine"            = NA_character_
    )
    length_levels <- switch(lf,
                            "Seedling"        = c("L1", "L2", "L3"),
                            "Small Tree"      = NA_character_,               # not measured
                            "Small Tree Fern" = c("L0", "L1", "L2", "L3"),
                            "Vine"            = c("L1", "L2", "L3"),
                            "Shrub"           = if (comm_group == "Coastal Strand")
                              c("L0", "L1", "L2", "L3") else c("L1", "L2", "L3")
    )
    rooting <- if (comm_group %in% c("Coastal Strand", "Subalpine Shrubland"))
      NA_character_ else ROOTING_ALL
    tidyr::expand_grid(
      LF_Sm_Woody = lf, DBH = dbh, Length = length_levels,
      Status = c("Live", "Dead"), Rooting = rooting
    )
  }

  # Plot scaffold WITHOUT Sample_Area -- the protocol-correct subplot(s) are
  # generated per block below, not inherited from the data.
  plot_key       <- c("Unit_Code", "Sampling_Frame", "Cycle", "Plot_Number", "Sample_Area")
  plot_key_nosa  <- setdiff(plot_key, "Sample_Area")
  plots <- d |>
    dplyr::distinct(dplyr::across(dplyr::all_of(plot_key_nosa)), Community) |>
    dplyr::mutate(Comm_Group = comm_group_of(Community))

  # Species pool per Sampling_Frame x life form (keyed on Code; SNAG kept).
  species_pool <- d |>
    dplyr::distinct(Sampling_Frame, LF_Sm_Woody, Code,
                    Scientific_Name, Nativity, Life_Form)

  # Assemble the full grid one (life form x community group) block at a time,
  # then stack. Each block:
  #   plots of that community group
  #     x species of that life form seen in the plot's Sampling_Frame
  #     x that life form's valid categories for that community group
  #     x the protocol-correct Sample_Area(s) for that life form x community.
  # Blocks where the life form is invalid for the community (empty subplot set,
  # e.g. Small Tree x Subalpine) are skipped entirely.
  cat_cols <- c("DBH", "Length", "Status", "Rooting")

  build_block <- function(lf, cgroup) {
    subplots <- correct_subplots(lf, cgroup)
    if (length(subplots) == 0) return(NULL)     # invalid lf x community -> no grid

    block_plots <- plots |>
      dplyr::filter(Comm_Group == cgroup)
    if (nrow(block_plots) == 0) return(NULL)

    block_species <- species_pool |>
      dplyr::filter(LF_Sm_Woody == lf) |>
      dplyr::select(-LF_Sm_Woody)           # re-added below as a column

    categories <- lf_grid(lf, cgroup) |>
      dplyr::select(-LF_Sm_Woody)           # single lf; re-added below as a column

    block_plots |>
      dplyr::left_join(block_species, by = "Sampling_Frame",
                       relationship = "many-to-many") |>
      dplyr::filter(!is.na(Code)) |>        # plots w/ no species of this lf -> none
      tidyr::crossing(categories) |>
      tidyr::crossing(Sample_Area = subplots) |>   # generate protocol subplot(s)
      dplyr::mutate(LF_Sm_Woody = lf)
  }

  blocks <- tidyr::expand_grid(
    lf     = life_forms,
    cgroup = c("Forest", "Coastal Strand", "Subalpine Shrubland")
  )

  full_grid <- purrr::pmap(blocks, function(lf, cgroup) build_block(lf, cgroup)) |>
    purrr::compact() |>
    dplyr::bind_rows() |>
    dplyr::select(dplyr::any_of(c(plot_key, "Community", "Comm_Group",
                                  "Code", "Scientific_Name", "Nativity", "Life_Form", "LF_Sm_Woody",
                                  cat_cols)))

  # ---- ...Step 8: observed counts + zero-fill --------------------------------
  key_cols <- c(plot_key, "LF_Sm_Woody", "Code", "DBH", "Length", "Status", "Rooting")

  observed <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop")

  # Rows in observed with no grid slot = residual unplaceable -> DROP + log.
  unplaceable <- observed |>
    dplyr::anti_join(full_grid, by = key_cols)
  if (nrow(unplaceable) > 0) {
    warning(nrow(unplaceable), " unplaceable observed rows dropped ",
            "(residual category violations). Use nonconforming() to inspect.",
            call. = FALSE)
    logit(unplaceable, "dropped: unplaceable category combination")
  }

  completed <- full_grid |>
    dplyr::left_join(observed, by = key_cols) |>
    tidyr::replace_na(list(Count = 0L)) |>
    dplyr::arrange(Unit_Code, Sampling_Frame, Cycle, Plot_Number, Sample_Area,
                   LF_Sm_Woody, Code, DBH, Length, Status, Rooting)

  # ---- ...Step 9: attach log + return ---------------------------------------
  log_tbl <- if (length(log) > 0) dplyr::bind_rows(log) else
    dplyr::tibble(disposition = character(0))
  attr(completed, "nonconforming") <- log_tbl
  completed
}

#' Stem density (stems/ha) from a prep_smwoody() result
#'
#' Converts zero-filled small-woody counts into stem densities (stems per
#' hectare) per plot, species (or species group), size/stage class, and status.
#' Sampled-area denominators follow the PACN protocol and vary by community group
#' and life form.
#'
#' Size class is DBH-based only: tree life forms use the DBH bin (`Seedling` /
#' `Small Tree` / `Medium Tree`); non-tree life forms (Shrub, Vine, Small Tree
#' Fern) have no DBH axis, so their `Size_Class` is the life form itself. Length
#' class is a separate, orthogonal column (`Length_Class`) holding L-classes for
#' any life form that records them: seedlings (L1-L3) and shrub/vine/fern
#' (L0-L3); `NA` for small trees.
#'
#' Sampled areas (m^2):
#' \tabular{lll}{
#'   \strong{Community group} \tab \strong{Seedling/Shrub/Vine/STFern (Transect 3)} \tab \strong{Small Tree (Quad 4)} \cr
#'   Forest              \tab 100 \tab 250 \cr
#'   Coastal Strand      \tab 40 per subplot / 80 per plot \tab 40 / 80 \cr
#'   Subalpine Shrubland \tab 100 \tab -- (not sampled) \cr
#' }
#'
#' @param prepped A tibble returned by [prep_smwoody()].
#' @param species A character vector of species `Code`s to include, or `NULL`
#'   (default) for all species in `prepped`. For functional groups, filter
#'   `prepped` yourself before calling (e.g. `dplyr::filter(x, Nativity ==
#'   "Native", Life_Form == "Tree")`) or pass the resulting `Code`s here.
#' @param group_label Short label describing the species selection, carried in
#'   the output `Group` column. Defaults to `"all species"`.
#' @param coastal How to treat Coastal Strand's two subplots (Transect 1 and
#'   Transect 3): `"per_plot"` (default) sums both and divides by 80 m^2;
#'   `"per_subplot"` keeps them separate, each divided by 40 m^2.
#' @param status_split If `TRUE` (default), Live and Dead are separate rows;
#'   if `FALSE`, they are combined.
#' @param aggregate Which sub-classes to sum over (density is unaffected by these
#'   since area does not depend on them):
#'   `"none"` (default) retains both `Rooting` and `Length_Class`;
#'   `"rooting"` sums over rooting classes; `"length"` sums over length classes
#'   (collapsing shrub/vine/fern to one row per life form and seedlings across
#'   L1-L3, while tree DBH size classes stay split); `"both"` sums over both.
#'
#' @return A tibble with columns identifying the plot, `Group`, `LF_Sm_Woody`,
#'   `Size_Class`, `Status` (if split), `Count`, `Area_m2`, and `Density_ha`.
#' @export
#'
#' @seealso [prep_smwoody()]
#'
#' @importFrom dplyr filter mutate case_when if_else group_by summarise ungroup
#' @importFrom dplyr left_join across all_of relocate arrange n
#'
#' @examples
#' \dontrun{
#' x <- prep_smwoody(FilterPACNVeg(data_name = "SmWoody"))
#'
#' # One species
#' d_metpol <- smwoody_density(x, species = "METPOL1", group_label = "METPOL1")
#'
#' # A functional group: all native trees
#' native_tree_codes <- x |>
#'   dplyr::filter(Nativity == "Native", Life_Form == "Tree") |>
#'   dplyr::distinct(Code) |> dplyr::pull(Code)
#' d_nat <- smwoody_density(x, species = native_tree_codes,
#'                          group_label = "native trees")
#' }
smwoody_density <- function(prepped,
                            species = NULL,
                            group_label = "all species",
                            coastal = c("per_plot", "per_subplot"),
                            status_split = TRUE,
                            aggregate = c("none", "rooting", "length", "both")) {

  coastal   <- match.arg(coastal)
  aggregate <- match.arg(aggregate)

  # ---- Area lookup (m^2) by community group x subplot group --------------------
  # Subplot_Group: "Tree" = Small Tree (Quad 4); "NonTree" = everything counted
  # in Transect 3 (seedlings, shrubs, vines, ferns).
  area_lookup <- dplyr::tibble(
    Comm_Group = c("Forest", "Forest",
                   "Subalpine Shrubland",
                   "Coastal Strand", "Coastal Strand"),
    Subplot_Group = c("NonTree", "Tree",
                      "NonTree",
                      "NonTree", "Tree"),
    Area_per_plot    = c(100, 250, 100, 80, 80),
    Area_per_subplot = c(100, 250, 100, 40, 40)
  )

  # ---- Filter to requested species --------------------------------------------
  d <- prepped
  if (!is.null(species)) {
    d <- dplyr::filter(d, Code %in% species)
  }

  # ---- Derive size class, length class, subplot group -------------------------
  # Size_Class: DBH-based for trees; life form for shrub/vine/fern (no DBH axis).
  # Length_Class: standalone length dimension (seedlings L1-L3, shrub/vine/fern
  #   L0-L3); NA for small trees. Orthogonal to Size_Class.
  d <- d |>
    dplyr::mutate(
      Size_Class = dplyr::case_when(
        LF_Sm_Woody == "Seedling"                     ~ "Seedling",
        LF_Sm_Woody == "Small Tree" & DBH == "1<5"    ~ "Small Tree",
        LF_Sm_Woody == "Small Tree" & DBH == "5<10"   ~ "Medium Tree",
        LF_Sm_Woody %in% c("Shrub", "Vine", "Small Tree Fern") ~ LF_Sm_Woody,
        TRUE ~ NA_character_
      ),
      Length_Class  = Length,     # NA for small trees; L-classes otherwise
      Subplot_Group = dplyr::if_else(LF_Sm_Woody == "Small Tree",
                                     "Tree", "NonTree")
    )

  # ---- Grouping keys -----------------------------------------------------------
  plot_key <- c("Unit_Code", "Sampling_Frame", "Cycle", "Year", "Plot_Number")
  plot_key <- intersect(plot_key, names(d))

  # Coastal per_plot: collapse Sample_Area (sum T1+T3). Otherwise keep it.
  keep_sample_area <- (coastal == "per_subplot")

  # Retain Rooting and/or Seedling_Length unless aggregated away.
  keep_rooting <- !(aggregate %in% c("rooting", "both"))
  keep_length  <- !(aggregate %in% c("length",  "both"))

  group_cols <- c(plot_key,
                  if (keep_sample_area) "Sample_Area",
                  "Comm_Group", "LF_Sm_Woody", "Size_Class", "Subplot_Group",
                  if (keep_length)  "Length_Class",
                  if (keep_rooting) "Rooting",
                  if (status_split) "Status")

  # ---- Aggregate counts, attach area, compute density -------------------------
  dens <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
    dplyr::left_join(area_lookup, by = c("Comm_Group", "Subplot_Group")) |>
    dplyr::mutate(
      Area_m2    = if (keep_sample_area) Area_per_subplot else Area_per_plot,
      Group      = group_label,
      Density_ha = Count / Area_m2 * 10000
    ) |>
    dplyr::select(-Area_per_plot, -Area_per_subplot) |>
    dplyr::relocate(Group) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)))

  # ---- Validation --------------------------------------------------------------
  stopifnot(!any(is.na(dens$Area_m2)))          # every row got an area
  stopifnot(all(is.finite(dens$Density_ha)))    # finite densities

  dens
}
