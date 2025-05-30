#------------------------------------------------------------------------------.

library(pacnvegetation)

library(tidyverse)
library(magrittr)

# if need to install packages while on network
#options(download.file.method = "wininet")

# Load Data ----

LoadPACNVeg(ftpc_params = "pacnveg",
            eips_paths = c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20220503.mdb",
                           "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2021_established_invasives_20221010_20230119.mdb",
                           "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2022_established_invasives_20230118_20230119.mdb"),
            cache = TRUE,
            expire_interval_days = 30,
            force_refresh = FALSE)

# ----Clidemia in NPSA----
spp_chg_und <- summarize_understory(combine_strata = TRUE,
                                    plant_grouping = "Species",
                                    paired_change = TRUE,
                                    park = "NPSA")

clihir_chg_und <- spp_chg_und %>%
  filter(Scientific_Name == "Clidemia hirta")

presence <- clihir_chg_und %>%
  dplyr::group_by(Year, Sampling_Frame) %>%
  dplyr::mutate(sp_present = dplyr::case_when(
    Cover > 0 ~ TRUE,
    Cover <= 0 ~ FALSE)) %>%
  summarise(plots_present = sum(sp_present),
            total_plots = n(),
            presence = plots_present/total_plots)

# Nativity discrete scale Colors:
year_filter <- "TRUE"
param <- "Chg_Prior"
param
sym(param)

nativity_colors <- c("Native" = "#1b9e77",
                     "No_Veg" = "grey",
                     "Non-Native" = "#d95f02",
                     "Unknown" = "#7570b3")

# add stats
clihir_chg_und_stats <- add_stats(clihir_chg_und, Unit_Code, Sampling_Frame,
                                  Cycle, Year, Stratum, Nativity, Scientific_Name)

# sample size calculation for text (output is on graph caption)
sample_size <- clihir_chg_und_stats %>%
  dplyr::filter(NPLOTS != 0) %>%
  dplyr::filter(Parameter == param) %>%
  dplyr::select(Sampling_Frame, Year, NPLOTS) %>%
  dplyr::distinct() %>%
  dplyr::group_by(Sampling_Frame) %>%
  dplyr::mutate(count_cycles = match(Year, unique(Year))) %>%
  dplyr::mutate(SF = paste0("; ", Sampling_Frame, ": ")) %>%
  dplyr::mutate(SF = dplyr::case_when(count_cycles == 1 ~ SF,
                                      TRUE ~ "")) %>%
  dplyr::mutate(Text = paste0(SF, Year, " [n = ", NPLOTS, "]")) %>%
  dplyr::pull(Text) %>%
  paste(collapse = ", ") %>%
  stringr::str_sub(3) %>%
  stringr::str_replace_all(", ;", ";")
sample_size

#........BAR YEARLY MEANS
label_param <- stringr::str_replace_all(param, "_", " ")

plot <- clihir_chg_und_stats %>%
  dplyr::mutate(SF_no_space = stringr::str_replace_all(Sampling_Frame, " ", "_")) %>%
  dplyr::filter(NPLOTS != 0) %>%
  dplyr::filter(Parameter == param) %>%
  ggplot2::ggplot(ggplot2::aes(x = Year, y = MEAN, fill = Nativity)) +
  ggplot2::geom_col(position = ggplot2::position_dodge()) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin=L, ymax=R), width=.2,
                         position=ggplot2::position_dodge(.9)) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::labs(y = paste(label_param, "(% Cover)")) +
  #ggh4x package allows nested facets:
  #ggplot2::facet_grid(Stratum ~ SF_no_space + Nativity,
  #                    labeller = ggplot2::label_parsed,
  #                    scales = "free_x") +
  ggplot2::facet_grid(Scientific_Name ~ Sampling_Frame) +
  ggplot2::scale_fill_manual(values = nativity_colors, limits = force) +
  ggplot2::xlab("Year") +
  ggplot2::theme(legend.position="none") +
  ggplot2::labs(caption = sample_size) +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle = 90, hjust = 0, vjust = 0.5))

plot

# Get Map:
data_table <- clihir_chg_und
paired_change <- TRUE
interactive <- TRUE

# Get max value for plotting data
#toplot.max <- data_table %>%
#  dplyr::ungroup() %>%
#  dplyr::select(dplyr::starts_with(c("Cover")))
#toplot.max <- max(c(abs(max(toplot.max, na.rm = TRUE)), abs(min(toplot.max, na.rm = TRUE))))

input <- data_table %>%
  filter(Sampling_Frame == "Tutuila") %>%
  drop_na(sym(param)) %>%
  dplyr::arrange(-(!!rlang::sym(param)))

#input$ramp_colors <- grDevices::colorRampPalette(list("#d11141", "#f37735", "#C8E52A", "#00b159"))(length(unique(param_cols)))

param_cols <- input %>%
  select(param) %>%
  unique()

param_cols$new_cols <- grDevices::colorRampPalette(list("#d11141", "#f37735", "#C8E52A"))(length(param_cols[[rlang::sym(param)]]))

param_cols$new_cols <- grDevices::colorRampPalette(list("#d11141", "#f37735", "#C8E52A", "#C8E52A", "#23BE21","#038001"   ))(length(param_cols[[rlang::sym(param)]]))

param_cols$new_cols[param_cols$Cover == 0] <- "#00b159"

plot(NULL, xlim=c(0,length(param_cols$new_cols)), ylim=c(0,1),
     xlab="", ylab="", xaxt="n", yaxt="n")
rect(0:(length(param_cols$new_cols)-1), 0, 1:length(param_cols$new_cols), 1, col=param_cols$new_cols)

input <- input %>%
  left_join(param_cols, by = param)

input <- dplyr::mutate(input, key = paste0(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number, Year, Cycle)) #crosstalk recommends using unique key
input <- crosstalk::SharedData$new(input, group = "ctg", key = ~key)
input_table <- input$data()

if (param == "Cover") {
  plotly_title <- paste(clihir_chg_und$Scientific_Name[1])
  }else{
    plotly_title <- paste(clihir_chg_und$Scientific_Name[1])
}

plt3 <- input %>%
  plotly::plot_ly(x = ~Plot_Number,
                  y = ~get(param),
                  hoverinfo = "text",
                  type = "bar",
                  marker = list(color = ~new_cols),
                  hovertext = ~paste0('</br> Plot: ', Plot_Number,
                                '</br> Species: ', Scientific_Name,
                                '</br> Time Period: Cycle ', Cycle, " - Cycle ", (Cycle-1),
                                '</br> % cover difference: ', round(Chg_Prior, 1), "%"),
                  showlegend = TRUE,
                  name = "Plot") %>%
  plotly::layout(title = plotly_title,
                 xaxis = list(categoryorder = "trace",
                              title = "Plot Number"),
                 yaxis = list(title = '% cover difference')) %>%
  plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick")

# If 'year_filter == TRUE' then add filter checkbox:
if (year_filter) {
  box_filter <- crosstalk::filter_checkbox("html_year_id", "Year", input, ~Year, inline = TRUE)

  plt3 <- crosstalk::bscols(
    widths = c(12, 12),
    plt3, box_filter)
}

plt3

map_input_dataset <- input_table %>%
  mutate(Plot_Number = as.integer(Plot_Number))

map3 <- MapCoverTotal3chg(sample_frame = "Tutuila", map_dataset = map_input_dataset, crosstalk = TRUE, crosstalk_group = "ctg")
map3
crosstalk::bscols(widths = c(6, NA), plt3, map3)
