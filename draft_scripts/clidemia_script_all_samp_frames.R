#------------------------------------------------------------------------------.

library(pacnvegetation)

library(tidyverse)
library(magrittr)
library(gghighlight)
library(elucidate)

# if need to install packages while on network
#options(download.file.method = "wininet")

#--- 1. Read latest cache ----

# Write/Read csv from pacnvegetation package:
pacnveg_cache_path <- "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/R_WritePACNVeg"

# Read
path_file_info <- file.info(list.files(pacnveg_cache_path, full.names = T))
latest_folder <- rownames(path_file_info)[which.max(path_file_info$mtime)]

LoadPACNVeg(data_path = latest_folder,
            data_source = "file")

#--- 2. Proportion native v non-native

nat_und <- summarize_understory(combine_strata = TRUE,
                                    plant_grouping = "Nativity",
                                    paired_change = FALSE)


nat_und_ratio <- nat_und %>%
  pivot_wider(names_from = Nativity, values_from = Cover) %>%
  mutate(vegetated = 100 - `NA`) %>%
  mutate(Total_Veg_Cover = (Native+`Non-Native`)) %>%
  mutate(Native_Ratio = Native/Total_Veg_Cover) %>%
  arrange(-Native_Ratio)

nat_und_ratio$Sampling_Frame <- fct_reorder(nat_und_ratio$Sampling_Frame,
                                            desc(nat_und_ratio$Native_Ratio))

levels(nat_und_ratio$Sampling_Frame)
threshold <- 0.50

# Native Ratio boxplot
nat_und_ratio %>%
  filter(Cycle == 2) %>%
  group_by("Unit_Code", "Sampling_Frame", "Cycle", "Year") %>%
  ggplot() +
  geom_boxplot(aes(x=Sampling_Frame, y=Native_Ratio)) +
  geom_hline(yintercept = .50, linetype="dashed", color = "black") +
  theme(axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1)) +
  xlab("Sampling Frame") + ylab("Native Ratio (understory)") +
  geom_text(aes(1.5,threshold,label = "50%", vjust = -0.5))

# Native Ratio x total veg point chart
# nat_und_ratio %>%
#   filter(Cycle == 2) %>%
#   group_by(Unit_Code, Sampling_Frame, Cycle, Year) %>%
#   summarise(median_Native_Ratio = median(Native_Ratio),
#             median_Total_Veg_Cover  = median(Total_Veg_Cover )) %>%
#   ggplot() +
#   geom_point(aes(x=Sampling_Frame, y=median_Native_Ratio, size = median_Total_Veg_Cover)) +
#   geom_hline(yintercept = .50, linetype="dashed", color = "black") +
#   theme(axis.text.x = element_text(size = 12, angle = 90, vjust = 0.5, hjust=1)) +
#   xlab("Sampling Frame") + ylab("Native Ratio (understory)") +
#   geom_text(aes(1.5,threshold,label = "50%", vjust = -0.5))

nat_und_ratio %>%
  #filter(Sampling_Frame == "Olaa") #%>%
  plot_stat_error(y = Native_Ratio, x = Cycle,
  #geom = "point",
  p_size = 3,
  add_lines = TRUE,
  dodge_width = 0,
  alpha = 0.6,
  stat = "mean",
  print_stats = TRUE,
  facet_var = Sampling_Frame,
  )




v_cover_bar_stats(combine_strata = TRUE)


#--- 3. Native/Non-native change
nat_chg_und <- summarize_understory(combine_strata = TRUE,
                                    plant_grouping = "Nativity",
                                    paired_change = TRUE)

nat_chg_und <- nat_chg_und %>%
  filter(Cycle > 1)

max_y<- round(max(nat_chg_und$Chg_Prior, na.rm = TRUE), digits = -1)
max_y
min_y<- round(min(nat_chg_und$Chg_Prior, na.rm = TRUE), digits = -1)
min_y

get_p <- function(chg_vector) {
  the_test <- t.test(chg_vector, mu = 0)
  the_p <- the_test$p.value
  the_p
}

nat_chg_und_grouped <- nat_chg_und %>%
  group_by(Cycle, Year, Sampling_Frame, Nativity) %>%
  filter(!is.na(Chg_Prior)) %>%
  summarize(out_median = median(Chg_Prior, na.rm = TRUE),
            n_plots = n(),
            p_val = get_p(Chg_Prior)) %>%
  mutate(high_low = case_when(out_median > 0 ~ "high",
                              out_median <= 0 ~ "low",
                              .default = NA
  )) %>%
  mutate(Sampling_Frame2 = paste0(Sampling_Frame, " (", n_plots, ")"))

native_chg_und_cyc2 <- nat_chg_und %>%
  left_join(nat_chg_und_grouped) %>%
  #ungroup() %>%
  filter(Nativity == "Native") %>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame2, desc(Chg_Prior), median),
             y=Chg_Prior,
             fill = high_low)) +
  geom_boxplot() +
  #stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="blue", fill="black") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Cycle 2-1 (2015-19 vs. 2010-14)") +
  xlab("Sampling Frame") + ylab("NATIVE - Change in % cover") +
  scale_fill_manual(values=c("#336600", "#FF6633")) +
  theme(legend.position="none") +
  scale_y_continuous(limits = c(min_y, max_y), breaks=seq(min_y,max_y,10)) +
  gghighlight(min(p_val) < 0.05,
              unhighlighted_params = list(color = "#696969", fill = NULL, alpha = 0.15, outlier.alpha = 0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")
#geom_text(aes(1.5,0,label = "0%", vjust = -0.5))
native_chg_und_cyc2

native_chg_und_cyc3 <- nat_chg_und %>%
  left_join(nat_chg_und_grouped) %>%
  ungroup() %>%
  filter(Nativity == "Native") %>%
  filter(Cycle == 3) %>%
  ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame2, desc(Chg_Prior), median),
             y=Chg_Prior,
             fill = high_low)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Cycle 3-2 (2021-23 vs. 2015-19)") +
  xlab("Sampling Frame") +
  #ylab("Change in Native cover per year") +
  scale_fill_manual(values=c("#336600", "#FF6633")) +
  theme(legend.position="none",
        #axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  scale_y_continuous(limits = c(min_y, max_y), breaks=seq(min_y,max_y,10)) +
  gghighlight(min(p_val) < 0.05,
              unhighlighted_params = list(color = "#696969", fill = NULL, alpha = 0.15, outlier.alpha = 0.5)) +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")
native_chg_und_cyc3

#library(gridExtra)
grid.arrange(native_chg_und_cyc2, native_chg_und_cyc3, nrow = 1, widths = c(2.1,1))

alien_chg_und_cyc2 <- nat_chg_und %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot(fill = "red") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  gghighlight(median(Chg_Per_Year, na.rm = TRUE) > 0) +
  ggtitle("Non-Native Understory Cover 2010-14 v. 2015-19") +
  xlab("Sampling Frame") + ylab("Change in cover per year") +
  geom_hline(yintercept = 0, linetype="dashed", color = "black")
#geom_text(aes(1.5,0,label = "0%", vjust = -0.5))
alien_chg_und_cyc2

look <- nat_chg_und %>%
  group_by(Sampling_Frame, Cycle, Nativity) %>%
  summarize(median = median(Chg_Per_Year, na.rm = TRUE))

# SF_native_und_chg_cycle2
native_chg_und <- nat_chg_und %>%
  filter(Nativity == "Native") %>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Native Understory Cover 2010-14 v. 2015-19") +
  xlab("Sampling Frame") + ylab("Change in % cover per year")
native_chg_und

# SF_native_und_chg_cycle2
non_native_chg_und <- nat_chg_und %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Non-Native Understory Cover 2010-14 v. 2015-19") +
  xlab("Sampling Frame") + ylab("Change in % cover per year")
non_native_chg_und


non_native_chg_und <- nat_chg_und %>%
  filter(Nativity == "Native" | Nativity == "Non-Native") #%>%
  filter(Cycle == 2) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Native Understory Cover 2010-14 v. 2015-19") +
  xlab("Sampling Frame") + ylab("Change in % cover per year")
non_native_chg_und

non_native_chg_und <- nat_chg_und %>%
  filter(Nativity == "Non-Native") %>%
  filter(Cycle == 3) %>%
  ggplot(aes(x=fct_reorder(Sampling_Frame, desc(Chg_Per_Year), median), y=Chg_Per_Year)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
non_native_chg_und
# ----Clidemia in NPSA----
spp_chg_und <- summarize_understory(combine_strata = TRUE,
                                    plant_grouping = "Species",
                                    paired_change = TRUE)

keep <- spp_chg_und %>%
  group_by(Year, Sampling_Frame, Scientific_Name) %>%
  summarise(sum_chg = sum(Chg_Per_Year,na.rm = TRUE)) %>%
  filter(sum_chg != 0)

absmax <- function(x) { x[which.max( abs(x) )]}

spp_chg_und_keep <- spp_chg_und %>%
  semi_join(keep)

spp_chg_und_keep2 <- spp_chg_und_keep %>%
  group_by(Cycle, Sampling_Frame, Scientific_Name) %>%
  summarise(max_chg_per_year = absmax(Chg_Per_Year)) %>%
  arrange(-max_chg_per_year)

top20 <- spp_chg_und_keep2 %>%
  group_by(Scientific_Name) %>%
  summarise(max = max(max_chg_per_year)) %>%
  arrange(-max) %>%
  filter(Scientific_Name != "Poa pratensis") %>%
  filter(Scientific_Name != "Fimbristylis cymosa") %>%
  filter(Scientific_Name != "Festuca rubra") %>%
  filter(Scientific_Name != "Cyathea spp.") %>%
  top_n(20) %>%
  pull(Scientific_Name)

spp_chg_und_keep %>%
  left_join(spp_chg_und_keep2) %>%
  mutate(Scientific_Name = forcats::fct_reorder(Scientific_Name, desc(max_chg_per_year))) %>%
  filter(Cycle == 2) %>%
  filter(Scientific_Name %in% top20) %>%
  ggplot() +
  geom_boxplot(aes(x=Sampling_Frame, y=Chg_Per_Year, color = Sampling_Frame)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~Scientific_Name, ncol = 20, scales = "free_x")


spp_chg_und_keep %>%
  left_join(spp_chg_und_keep2) %>%
  mutate(Scientific_Name = forcats::fct_reorder(Scientific_Name, desc(max_chg_per_year))) %>%
  filter(Cycle == 2) %>%
  filter(Scientific_Name %in% top20) %>%
  ggplot() +
  geom_boxplot(aes(x=Sampling_Frame, y=Chg_Per_Year, color = Sampling_Frame)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~Scientific_Name, ncol = 20, scales = "free_x")

spp_chg_und_keep %>%
  left_join(spp_chg_und_keep2) %>%
  mutate(Scientific_Name = forcats::fct_reorder(Scientific_Name, desc(max_chg_per_year))) %>%
  filter(Scientific_Name == "Clidemia hirta") %>%
  #filter(Cycle == 2) %>%
  filter(Scientific_Name %in% top20) %>%
  ggplot() +
  geom_boxplot(aes(x=Year, y=Chg_Per_Year, color = Sampling_Frame)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_wrap(~Unit_Code, ncol = 20, scales = "free_x")


look

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

map4 <- MapCoverTotal3chg(map_dataset = map_input_dataset, crosstalk = TRUE, crosstalk_group = "ctg")
map4
crosstalk::bscols(widths = c(6, NA), plt3, map3)
