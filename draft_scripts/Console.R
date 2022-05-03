
library(pacnvegetation)
library(tidyverse)
library(tidytext)

LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20220428.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2021_established_invasives_20220422.mdb"),
            cache = TRUE, force_refresh = FALSE)

names(FilterPACNVeg())
#-----------------------
chk <- process_photos(AGOL_Layer = "EIPS",
                      gdb_name = "EIPS_OL_ER_20220502.gdb",
                      gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                      gdb_layer = "EIPS_OL_ER_20220502",
                      return_table = TRUE)
chk1 <- chk %>%
  # remove subjects that are not photo points
  filter(!Subject_EIPS == "Staff" & !Subject_EIPS == "Other") %>%
  separate(Subject_EIPS, sep = "_", into = c("distance", "direction"), remove = FALSE) %>%
  group_by(Sampling_Frame, Site_numb, Site_Type, distance) %>%
  summarise(n_direct = n_distinct(direction)) %>%
  filter(n_direct != 3 & Site_Type == "Fixed" |
           n_direct != 2 & Site_Type == "Rotational" )

process_photos(AGOL_Layer = "EIPS",
                      gdb_name = "EIPS_OL_ER_20220502.gdb",
                      gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                      gdb_layer = "EIPS_OL_ER_20220502",
                      return_table = FALSE)

transects <- FilterPACNVeg("EIPS_data")

l <- transects %>%
  filter(Sampling_Frame == "Mauna Loa" |
           Sampling_Frame == "Kahuku") %>%
  distinct(Cycle, Sampling_Frame, Transect_Number, Transect_Type) %>%
  mutate(Transect_Number = as.numeric(Transect_Number))
  count(Cycle, Sampling_Frame)

AMME_transects <- FilterPACNVeg("EIPS_data") %>%
  filter(Sampling_Frame == "Muchot")

l <- AMME_transects %>%
  group_by(Cycle, Transect_Number, Segment) %>%
  summarise(All_Segments = n())

AMME_transects %>%
  group_by(Cycle, Scientific_Name, Code, Life_Form, Nativity) %>%
  summarise(total_segments = n())

canopy <- FilterPACNVeg("Canopy")

canopy <- canopy %>%
  arrange(Cycle, Sampling_Frame, Plot_Number) #%>%
  summarise(n = n())

small_woody <- FilterPACNVeg("SmWoody") %>%
  #filter(Sampling_Frame == "Haleakala") %>%
  filter(Cycle == 2)


species_missed <- qc_presence_complete()
species_missed <- qc_presence_complete(all_records = FALSE)

FilterPACNVeg("LgTrees") %>%
  filter(Caudex_Length != 999) %>%
  filter(Life_Form == "Tree Fern") %>%
  ggplot(aes(x=Caudex_Length)) +
  geom_histogram(color="black", fill="white") +
  facet_grid(. ~ Sampling_Frame)

large_trees %>%
  #filter(Status == "Dead") %>%
  filter(Life_Form == "Tree Fern") #%>%
  count(Foliar)


shrubs <- FilterPACNVeg("SmWoody")

chk <- process_photos(AGOL_Layer = "EIPS",
               gdb_name = "EIPS_OL_ER_20220502.gdb",
               gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
               gdb_layer = "EIPS_OL_ER_20220502",
               return_table = TRUE)


?process_photos
process_photos(AGOL_Layer = "EIPS",
               gdb_name = "EIPS_Olaa_Nahuku_20220323_1.gdb",
               gdb_location = "C:/Users/JJGross/Documents/RData/PROJECTS/pacnvegetation/geodatabase",
               gdb_layer = "EIPS_Olaa_Nahuku_20220323",
               return_table = FALSE)


LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20210818.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_1_2021_20220120.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/EIPS_Databases/2021_established_invasives_2_20210129.mdb"),
            cache = TRUE, force_refresh = FALSE)


plt <- UnderNativityCover.plot.nat_v_non(sample_frame = "Haleakala",
                                         combine_strata = TRUE,
                                         crosstalk = TRUE,
                                         interactive = TRUE)
plt




test_sum_und <- summarize_understory(combine_strata = TRUE,
                                     plant_grouping = "Nativity",
                                     sample_frame = "Haleakala")

last_fixed <- test_sum_und %>%
  select(Unit_Code, Sampling_Frame, Cycle, Year, Plot_Type, Plot_Number)


remove_prior_fixed <- test_sum_und %>%
  dplyr::group_by(Unit_Code, Sampling_Frame, Plot_Type, Plot_Number) %>%
  dplyr::mutate(last_cycle = max(Cycle)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(last_visit = dplyr::case_when(Cycle == last_cycle ~ TRUE,
                TRUE ~ FALSE)) #%>%
  #dplyr::filter(Cycle == last_cycle)


dplyr::filter(dplyr::case_when(Plot_Type == "Fixed"))

plt <- UnderNativityCover.plot.nat_v_non(sample_frame = "Haleakala",
                                         combine_strata = TRUE,
                                         crosstalk = TRUE,
                                         interactive = TRUE)
plt

ps <- highlight_key(data)

year_filter <- crosstalk::filter_checkbox("Cycle", "Monitoring Cycle", data, ~Cycle)

crosstalk::bscols(
  widths = c(11, 1),
  plt, year_filter
)

plt2 <- plt %>%
  plotly::highlight_key(~Plot_Number) %>%
  plotly::highlight(on = "plotly_hover", off = "plotly_doubleclick")
plt2
  plotly::add_segments(x = 0, xend = 150, y = 150, yend = 0,
                       showlegend = TRUE,
                       name = "1:1",
                       line = list(color = "gray"))
plt2


plt <- UnderNativityCover.plot.nat_v_non(sample_frame = "Haleakala",
                                         combine_strata = TRUE,
                                         paired_change = TRUE,
                                         paired_cycle = 1,
                                         crosstalk = TRUE,
                                         interactive = TRUE)
plt

tot_data_noCT_ratio <- tot_data_noCT %>%
  #mutate(nat_ratio = dplyr::case_when(Native_Cover_Total_pct > NonNative_Cover_Total_pct ~ Native_Cover_Total_pct/(Native_Cover_Total_pct+NonNative_Cover_Total_pct),
  #                                      Native_Cover_Total_pct < NonNative_Cover_Total_pct ~ NonNative_Cover_Total_pct/(NonNative_Cover_Total_pct+Native_Cover_Total_pct)*-1,
  #                                      TRUE ~ 0)) %>%
  mutate(nat_ratio = Native_Cover_Total_pct / (NonNative_Cover_Total_pct + Native_Cover_Total_pct) * 100) %>%
  mutate(tot_cover = Native_Cover_Total_pct + NonNative_Cover_Total_pct)

#pal <- grDevices::colorRampPalette(c("red", "orange", "orange", "yellow", "yellow", "green"))(length(unique(tot_data_noCT_ratio$nat_ratio)))
pal <- grDevices::colorRampPalette(c("red", "orange", "yellow", "green"))(length(unique(tot_data_noCT_ratio$nat_ratio)))

plt <- plotly::plot_ly(colors = pal) %>%
  plotly::add_segments(x = 0, xend = 144, y = 0, yend = 144,
                       showlegend = TRUE,
                       name = "1:1",
                       line = list(color = "gray")) %>%
  plotly::add_markers(data = tot_data_noCT_ratio,
                    x = ~ Native_Cover_Total_pct,
                    y = ~ NonNative_Cover_Total_pct,
                    hoverinfo = "text",
                    color = ~ nat_ratio,
                    #colors = pal,
                    #size = ~ tot_cover,
                    type = "scatter",
                    #mode = "markers",
                    marker = list(line = list(color = "black"), width = 2, size = ~ tot_cover*.1),
                    text = ~paste('</br> Plot: ', Plot_Number,
                                  '</br> Native cover: ', round(Native_Cover_Total_pct, 1),
                                  '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1)),
                                  #'</br> Native cover: ', round(Native_Cover_Total_pct, 1)),
                    showlegend = TRUE,
                    name = "Plot") %>%#) %>%
  plotly::highlight(on = "plotly_hover") %>%
  plotly::layout(xaxis = list(title = "Native cover"), #, range = lims
                 yaxis = list(title = "Non-native cover")) %>% #, range = lims
  plotly::colorbar(title = "% Native", limits = c(0,100))
plt

plt <- plotly::plot_ly(data = tot_data_noCT_ratio,
                       x = ~ Native_Cover_Total_pct,
                       y = ~ NonNative_Cover_Total_pct,
                       hoverinfo = "text",
                       color = ~ nat_ratio,
                       colors = pal,
                       size = ~ tot_cover,
                       type = "scatter",
                       mode = "markers",
                       marker = list(line = list(color = "black"), width = 2),
                       text = ~paste('</br> Plot: ', Plot_Number,
                                     '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1),
                                     '</br> Native cover: ', round(Native_Cover_Total_pct, 1))) %>%
  plotly::highlight(on = "plotly_hover") %>%
  plotly::add_segments(x = 0, xend = 100, y = 0, yend = 100) %>%
  plotly::layout(xaxis = list(title = "Native cover"), #, range = lims
                 yaxis = list(title = "Non-native cover"), #, range = lims
                 showlegend = T) %>%
  plotly::colorbar(title = "% Native", limits = c(0,100))
plt

# Get monitoring cycles
cycles <- FilterPACNVeg("Events_extra_xy", sample_frame = params$sample_frame) %>%
  dplyr::select(Cycle) %>%
  unique() %>%
  dplyr::arrange(Cycle)
cycles <- cycles[["Cycle"]]

grp <- "cov_change"
tot_grp <- "tot_change"

plt <- UnderNativityCover.plot.nat_v_non(sample_frame = params$sample_frame,
                                         cycle = max(cycles),
                                         paired_cycle = min(cycles),
                                         paired_change = TRUE,
                                         combine_strata = TRUE,
                                         crosstalk = TRUE,
                                         crosstalk_group = grp,
                                         interactive = TRUE)
plt

tot_data_noCT <- UnderNativityCover(combine_strata = TRUE, paired_change = FALSE,
                                    crosstalk = FALSE, sample_frame = "Haleakala")


tot_data <- UnderNativityCover(combine_strata = TRUE, paired_change = FALSE,
                             crosstalk = TRUE, sample_frame = "Haleakala")


# tot_data_noCT_diff <- tot_data_noCT %>%
#   mutate(diff_abline = dplyr::case_when(Native_Cover_Total_pct > NonNative_Cover_Total_pct ~ abs(Native_Cover_Total_pct-NonNative_Cover_Total_pct),
#                                         Native_Cover_Total_pct < NonNative_Cover_Total_pct ~ abs(NonNative_Cover_Total_pct-Native_Cover_Total_pct)*-1,
#                                         TRUE ~ 0)) %>%
#   mutate(diff_abline = dplyr::case_when(NonNative_Cover_Total_pct == 0 ~ 100,
#                                         Native_Cover_Total_pct == 0 ~ -100,
#                                         TRUE ~ diff_abline))

tot_data_noCT_ratio <- tot_data_noCT %>%
  #mutate(nat_ratio = dplyr::case_when(Native_Cover_Total_pct > NonNative_Cover_Total_pct ~ Native_Cover_Total_pct/(Native_Cover_Total_pct+NonNative_Cover_Total_pct),
  #                                      Native_Cover_Total_pct < NonNative_Cover_Total_pct ~ NonNative_Cover_Total_pct/(NonNative_Cover_Total_pct+Native_Cover_Total_pct)*-1,
  #                                      TRUE ~ 0)) %>%
  mutate(nat_ratio = Native_Cover_Total_pct / NonNative_Cover_Total_pct + Native_Cover_Total_pct) %>%
  mutate(tot_cover = Native_Cover_Total_pct + NonNative_Cover_Total_pct)

#breaks <- c(-1, -0.5, 0, 0.5, 1)
breaks <- c(-1, -0.5, 0, 0.5, 1)

test <- ggplot(tot_data_noCT_ratio, aes(x = Native_Cover_Total_pct, y = NonNative_Cover_Total_pct,
                                        color = nat_ratio, size = tot_cover,
                                        text=sprintf("Plot: %s<br>Year: %s", Plot_Number, Year))) + # Set up text for plotly hover info
  geom_point() +
  geom_abline(intercept = 0, slope=1, color="blue") +
  scale_color_gradientn(colors = c("red", "yellow", "green"), limits = c(-1,1),
                        values = scales::rescale(c(-1, -0.5, 0.7, 1))) +
  labs(colour = "Native Ratio",
       size = "Total Veg Cover (%)",
       x = "Native Cover",
       y = "Non-native Cover") +
  #scale_shape_identity() +
  theme_bw()

test

test <- ggplot(tot_data_noCT_ratio, aes(x = Native_Cover_Total_pct, y = NonNative_Cover_Total_pct,
                                        fill = nat_ratio, size = tot_cover,
                                        text=sprintf("Plot: %s<br>Year: %s", Plot_Number, Year))) + # Set up text for plotly hover info
  geom_point(shape = 21, color = "black") +
  geom_abline(intercept = 0, slope=1, color="blue") +
  scale_fill_gradientn(colors  = c("red", "yellow", "green"), limits = c(-1,1),
                        values = scales::rescale(c(-1, -0.5, 0.7, 1))) +
  labs(fill = "Native Ratio",
       size = "Total Veg Cover (%)",
       x = "Native Cover",
       y = "Non-native Cover") +
  theme_bw()

test

test2 <- test %>%
  plotly::ggplotly(tooltip= c("text")) #%>%
test2

pal <- grDevices::colorRampPalette(c("red", "orange", "orange", "yellow", "yellow", "green"))(length(unique(tot_data_noCT_ratio$nat_ratio)))

plt <- plotly::plot_ly(data = tot_data_noCT_ratio,
        x = ~ Native_Cover_Total_pct,
        y = ~ NonNative_Cover_Total_pct,
        hoverinfo = "text",
        color = ~ nat_ratio,
        colors = pal,
        size = ~ tot_cover,
        type = "scatter",
        mode = "markers",
        marker = list(line = list(color = "black"), width = 2),
        text = ~paste('</br> Plot: ', Plot_Number,
                      '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1),
                      '</br> Native cover: ', round(Native_Cover_Total_pct, 1))) %>%
  plotly::highlight(on = "plotly_hover") %>%
  plotly::layout(xaxis = list(title = "Native cover"), #, range = lims
                 yaxis = list(title = "Non-native cover"), #, range = lims
                 showlegend = FALSE)
plt


pal


  plotly::plot_ly(data = tot_data_noCT_ratio,
                  x = ~ Native_Cover_Total_pct,
                  y = ~ NonNative_Cover_Total_pct,
                  #hoverinfo = "text",
                  color = ~ nat_ratio,
                  colors = pal,
                  size = ~ tot_cover,
                  type = "scatter",
                  mode = "markers",
                  marker = list(line = list(color = "black"), width = 2),
                  text = ~paste('</br> Plot: ', Plot_Number,
                                '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1),
                                '</br> Native cover: ', round(Native_Cover_Total_pct, 1)))
plt



str(tot_data)

plotly::plotly_data(tot_data)

tot_data$data

library("RColorBrewer")
display.brewer.all(type = 'div')

tot_plt <- tot_data_noCT_ratio %>%
  plotly::plot_ly() %>%
  plotly::add_segments(y = 0, yend = 200,
                       x = 0, xend = 200) %>%
  plotly::add_trace(x = ~ Native_Cover_Total_pct,
                    y = ~ NonNative_Cover_Total_pct,
                    hoverinfo = "text",
                    type = "scatter",
                    mode = "markers",
                    showlegend = FALSE,
                    marker = color ~ diff_abline,
                    text = ~paste('</br> Plot: ', Plot_Number,
                                  '</br> Non-native cover: ', round(NonNative_Cover_Total_pct, 1),
                                  '</br> Native cover: ', round(Native_Cover_Total_pct, 1))) %>%
  # plotly::add_text(data = data,
  #                  x = ~ Native_Cover_Change_pct,
  #                  y = ~ NonNative_Cover_Change_pct,text = ~Plot_Number, textposition = "top right") %>%
  plotly::highlight(on = "plotly_hover") %>%
  plotly::layout(xaxis = list(title = "Native cover"),
                 yaxis = list(title = "Non-native cover"),
                 showlegend = FALSE)
tot_plt

tot_plt <- UnderNativityCover.plot.nat_v_non(sample_frame = params$sample_frame,
                                             cycle = max(cycles),
                                             paired_change = FALSE,
                                             combine_strata = TRUE,
                                             crosstalk = TRUE,
                                             crosstalk_group = tot_grp,
                                             interactive = TRUE)
tot_plt

map <- MapCoverChange(crosstalk = TRUE, crosstalk_group = grp, sample_frame = params$sample_frame, cycle = max(cycles), paired_cycle = min(cycles))

bscols(plt, map)





MapPACNVeg2(sample_frame = "Haleakala")



MapCoverChange(sample_frame = "Haleakala", cycle = 2)


pacnvegetation:::pchIcons(pch = rep(22, nrow(cover_data)),
         width = 30,
         height = 30,
         bg = colorspace::darken(cover_data$color),
         col = cover_data$color, 0.3)

MapCoverChange(combine_strata = TRUE, park = "WAPA", cycle = 2, paired_cycle = 1)

look <- summarize_understory(combine_strata = TRUE, plant_grouping = "Nativity", paired_change = TRUE, sample_frame = "Olaa")
look_old <- UnderNativityCover(combine_strata = TRUE, paired_change = TRUE, sample_frame = "Olaa", cycle = 3)


look <- FilterPACNVeg("Understory", park = "HAVO", cycle = 2)
look <- FilterPACNVeg("Understory", sample_frame = "Haleakala", cycle = 2)

chk <- look %>%
  #filter(Plot_Number == 10) %>%
  filter(Stratum == "Low") %>%
  group_by(Sampling_Frame, Cycle, Dead, Scientific_Name, Code, Nativity) %>%
  summarise(hits = n())



v_cover_plot_bar_nativity(sample_frame = "Haleakala", paired_change = FALSE, param = "Cover")


v_cover_plot_bar_nativity(sample_frame = "Haleakala", paired_change = TRUE, param = "Chg_Per_Year")

v_cover_plot_bar_nativity(sample_frame = "Nahuku/East Rift", paired_change = TRUE, param = "Chg_Per_Year")



haleakala_nativity <- summarize_understory(sample_frame = "Haleakala",
                               paired_change = FALSE,
                               plant_grouping = "Nativity")

haleakala_nativity_stats <- add_stats(haleakala_nativity)

haleakala_nativity_paired <- summarize_understory(sample_frame = "Haleakala", paired_change = FALSE, plant_grouping = "Nativity", combine_strata = TRUE)

