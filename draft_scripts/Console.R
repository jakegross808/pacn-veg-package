
library(pacnvegetation)
library(tidyverse)
#library(tidytext)

BA <- function(dbh) {
  pi*(dbh/200)^2
}

BA(10)

LoadPACNVeg("pacnveg", c("C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/established_invasives_BE_master_20220428.mdb",
                         "C:/Users/JJGross/OneDrive - DOI/Documents/Certification_Local/Databases/EIPS/2021_established_invasives_20220422.mdb"),
            cache = TRUE, force_refresh = FALSE)

names(FilterPACNVeg())

spp <- FilterPACNVeg(data_name = "Species_extra", sample_frame = "Mauna Loa", is_qa_plot = FALSE)

# Large Trees - Basal Area ------------------------------------------------


Trees <- FilterPACNVeg(data_name = "LgTrees", sample_frame = "Mauna Loa", is_qa_plot = FALSE)

SmWoody <- FilterPACNVeg(data_name = "SmWoody", sample_frame = "Mauna Loa", is_qa_plot = FALSE)

SmWoody %>%
  distinct(Code)

Trees %>%
  distinct(Code)

SS_SmWoody1 <- SS_SmWoody %>%
  filter(Plot_Number == 3) %>%
  group_by(Status, Code, Cycle) %>%
  summarize(total = sum(Count)) %>%
  pivot_wider(names_from = Cycle, values_from = total)

write_csv(SS_SmWoody1, "SmWoody_Plt3.csv")


SS_Trees_BA3 <- SS_Trees %>%
  mutate(bole_dbh = case_when(is.na(DBH_Bole) & !is.na(DBH) ~ DBH,
                              is.na(DBH_Bole) & is.na(DBH) ~ 0,
                              TRUE ~ DBH_Bole)) %>%
  mutate(Basal_Area_m2_bole = BA(bole_dbh),
         A_BD = BA(DBH_Other)) %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Quad, Status,
           Height, Height_Dead, Boles, DBH, DBH_Other, Vigor, Rooting, Fruit_Flower, Foliar,
           Shrublike_Growth, Resprouts, Measurement_Type, Scientific_Name, Code,
           Life_Form, Nativity, A_BD) %>%
  summarize(A_DBH = sum(Basal_Area_m2_bole),
            bole_check = n()) #%>%
  #filter(Plot_Number == 3)
str(SS_Trees_BA3$Cycle)

SS_Trees_BA4 <- SS_Trees_BA3 %>%
  filter(A_DBH != is.na(A_DBH),
         A_BD != is.na(A_BD))

SS_Trees_BA4 %>%
  mutate(Cycle = as.character(Cycle)) %>%
  pivot_longer(cols = c("A_BD", "A_DBH"), names_to = "Measurement", values_to = "Area") %>%
  ggplot(aes(x = Cycle, y = Area)) +
  geom_violin() +
  facet_grid(rows = vars(Measurement))

SS_Trees_dbh <- SS_Trees_BA4 %>%
  group_by(Unit_Code, Community, Sampling_Frame, Year, Cycle, Plot_Type, Plot_Number, Quad, Status,
           Height, Height_Dead, Boles, DBH, DBH_Other, Vigor, Rooting, Fruit_Flower, Foliar,
           Shrublike_Growth, Resprouts, Measurement_Type, Scientific_Name, Code,
           Life_Form, Nativity, A_BD)
  pivot_wider(names_from = Cycle, values_from = A_DBH)

SS_Trees_BA4
dbh1 <- SS_Trees_BA4 %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Cycle == 1) %>%
  pull(A_DBH) %>%
  var()
dbh2 <- SS_Trees_BA4 %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Cycle == 2) %>%
  pull(A_DBH) %>%
  var()

bd1 <- SS_Trees_BA4 %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Cycle == 1) %>%
  pull(A_BD) %>%
  var()
bd2 <- SS_Trees_BA4 %>%
  filter(Plot_Type == "Fixed") %>%
  filter(Cycle == 2) %>%
  pull(A_BD) %>%
  var()
bd2 - dbh2
bd1 - dbh1
var_bd <- bd2-bd1
var_dbh <- dbh2-dbh1
var_bd/var_dbh

var(A_BD)

SS_Trees_BA3 %>%
  mutate(Cycle = as.character(Cycle)) %>%
  ggplot(aes(x = Cycle, y = A_BD)) +
  geom_boxplot() #+
  #facet_grid(cols = vars())



dbh1 <- SS_Trees_BA3 %>%
  filter(Cycle == 1) %>%
  pull(A_DBH)

mean(dbh1)
var(dbh1)

dbh2 <- SS_Trees_BA3 %>%
  filter(Cycle == 2) %>%
  pull(A_DBH)

mean(dbh2)
var(dbh2)


SS_Trees_Plt3_a <- SS_Trees_Plt3 %>%
  arrange(Quad, Year)

write_csv(SS_Trees_Plt3_a, "Lrg_Tree_Plt3.csv")


bole_checks <- SS_Trees_BA3 %>%
  filter(Boles != bole_check)

BA_vs_DBH <- SS_Trees_BA3 %>%
  filter(!is.na(A_BD),
           Code == "METPOL1")

# Variables
#basal <- BA_vs_DBH$A_BD
#DBH <- BA_vs_DBH$A_DBH
ggplot(BA_vs_DBH, aes(x = A_BD, y = A_DBH)) +
  geom_point()

l <- BA_vs_DBH %>%
  filter(!is.na(Height)) %>%
  mutate(LD_height = case_when(Height_Dead > Height ~ Height_Dead,
                              TRUE ~ Height))

ggplot(l, aes(x = LD_height, y = A_DBH)) +
  geom_point()
ggplot(l, aes(x = LD_height, y = A_BD)) +
  geom_point()

ggplot(BA_vs_DBH, aes(x = A_BD, y = A_DBH)) +
  geom_point()
ggplot(BA_vs_DBH, aes(x = A_BD, y = A_DBH)) +
  geom_point()


# Distribution of CONT variable
library(ggpubr)
ggdensity(BA_vs_DBH, x = "A_BD", fill = "lightgray", title = "BD") +
  #scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(BA_vs_DBH, x = "A_DBH", fill = "lightgray", title = "DBH") +
  #scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

ggdensity(l, x = "LD_height", fill = "lightgray", title = "height") +
  #scale_x_continuous(limits = c(3, 12)) +
  stat_overlay_normal_density(color = "red", linetype = "dashed")

cor.test(BA_vs_DBH$Basal_Area_m2_BD, BA_vs_DBH$Basal_Area_m2_DBH, method = 'pearson')

lm1 <- lm(Basal_Area_m2_BD ~ Basal_Area_m2_DBH, data = BA_vs_DBH)
lm1
summary(lm1)
par(mfrow=c(2,2))

plot(lm1, which=1:4)

ggplot(BA_vs_DBH) +
  geom_qq(aes(sample = Basal_Area_m2_BD))

nq <- 100
p <- (1 : nq) / nq - 0.5 / nq
ggplot() +
  geom_point(aes(x = qnorm(p), y = quantile(BA_vs_DBH$Basal_Area_m2_BD, p)))

ggplot() +
  geom_point(aes(x = qexp(p), y = quantile(diamonds$price, p)))

basal <- BA_vs_DBH$Basal_Area_m2_BD
DBH <- BA_vs_DBH$Basal_Area_m2_DBH
n <- nrow(BA_vs_DBH)
p <- (1 : n) / n - 0.5 / n
ggplot(BA_vs_DBH) +
  geom_point(aes(x = basal, y = sort(pnorm(fheight, m, s))))



understory_nativity <- summarize_understory(plant_grouping = "Nativity", combine_strata = TRUE)

understory_species <- summarize_understory(plant_grouping = "Species", combine_strata = TRUE)

library(tidyverse)

understory_nativity1 <- understory_nativity %>%
  filter(Nativity != "Unknown",
         !is.na(Nativity)) %>%
  filter(Cycle != 3)

understory_species1 <- understory_species %>%
  mutate(Presence = case_when(Cover > 0 ~ 1,
                              TRUE ~ 0)) %>%
  filter(Presence == 1) %>%
  filter(!is.na(Scientific_Name)) %>%
  filter(Cycle != 3) %>%
  filter(Nativity != "Unknown")

understory_nativity1 %>%
  count(Year, Unit_Code)
#library(ggplot2)

write_csv(understory_species1, "Understory_Species.csv")
write_csv(understory_nativity1, "Understory_Nativity.csv")

library(tidyverse)

understory_species1 <- read_csv("Understory_Species.csv")

understory_species2 <- understory_species1 %>%
  group_by(Unit_Code, Sampling_Frame, Cycle, Year, Plot_Type, Plot_Number, Nativity) %>%
  summarize(Richness = sum(Presence))

ggplot(understory_nativity1, aes(x=Cover, fill = Nativity)) +
  geom_histogram() +
  facet_grid(cols = vars(Sampling_Frame),
                         rows = vars(Cycle)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(understory_species1, aes(x=Presence)) +
  geom_histogram()

EIPS <- FilterPACNVeg(data_name = "EIPS_data")



update_photos <- process_photos(AGOL_Layer = "FTPC",
               gdb_name = "FTPC_OL_ER_20220503.gdb",
               gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
               gdb_layer = "FTPC_OL_ER_20220503",
               return_table = TRUE)

update_photos2 <- update_photos[1:5,]

update_photos2$DATA <- as.list(update_photos2$DATA)

apply(X = update_photos2, MARGIN = 1, FUN = watermark, new_folder = "watermark2")



# Fix orientation -------------
library(magick)

plots2update <- "OL_47"
update_photos2 <- update_photos %>%
  filter(samp_plot %in% plots2update)

# northwest corner
nw1 <- "TEST"
nw2 <- "ORIENTATION"
nw <- paste(nw1, nw2, sep = "\n")


p2 <- update_photos2$DATA[[3]]

mp2 <- magick::image_read(p2)
mp2

q.upsidedown <- image_attributes(mp2)

mp2_orient <- magick::image_orient(mp1)


img.marked <- magick::image_annotate(mp2_orient, nw,
                                size = 25,
                                gravity = "northwest",
                                font = "Helvetica",
                                color = "white",
                                strokecolor = "black",
                                weight = 900)
img.marked
# Check photos -----------------------------------------------------------------

# Tables to check
# ............... chk_missing
# ............... dupes



# FTPC Photos ---------

# Load FTPC data
chk <- process_photos(AGOL_Layer = "FTPC",
                      gdb_name = "FTPC_OL_ER_20220503.gdb",
                      gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                      gdb_layer = "FTPC_OL_ER_20220503",
                      return_table = TRUE)



# Get list of missing fixed photos
chk_fixed <- chk %>%
  filter(Site_Type == "Fixed") %>%
  filter(Subject1 != "Staff_Photo") %>%
  filter(Subject1 != "Other") %>%
  group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  complete(nesting(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type), Subject1) %>%
  filter(is.na(n))

# Get list of missing rotational photos
chk_rotational <- chk %>%
  filter(Site_Type == "Rotational") %>%
  filter(Subject1 != "Staff_Photo") %>%
  filter(Subject1 != "Other") %>%
  group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  complete(nesting(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type), Subject1) %>%
  filter(is.na(n))

# Combine fixed and rotational into one table
chk_missing <- bind_rows(chk_fixed, chk_rotational)


# Create a table of duplicate points
# missing photos may be hiding as a mislabeled point,
# if so, it would likely be a duplicate

chk_dupes <- chk %>%
  # Count number of photos per subject
  group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, REL_GLOBALID) %>%
  summarize(n_photos = n()) %>%
  # Count number of points per subject (disregards multiple photos at one point)
  group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  summarize(n_points = n()) %>%
  filter(n_points > 1) %>%
  filter(Subject1 != "Other")

# join with original data to check created date, etc.
dupes <- chk_dupes %>%
  left_join(chk) %>%
  select(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, n_points, Staff_list, created_date, last_edited_date, last_edited_user)

# EIPS Photos ---------

# Load EIPS data
EIPS_chk <- process_photos(AGOL_Layer = "EIPS",
                      gdb_name = "EIPS_OL_ER_20220502.gdb",
                      gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                      gdb_layer = "EIPS_OL_ER_20220502",
                      return_table = TRUE)

# Look for missing points
EIPS_missing <- EIPS_chk %>%
  # remove subjects that are not photo points
  filter(!Subject_EIPS == "Staff" & !Subject_EIPS == "Other") %>%
  separate(Subject_EIPS, sep = "_", into = c("distance", "direction"), remove = FALSE) %>%
  group_by(Sampling_Frame, Site_numb, Site_Type, distance) %>%
  summarise(n_direct = n_distinct(direction)) %>%
  filter(n_direct != 3 & Site_Type == "Fixed" |
           n_direct != 2 & Site_Type == "Rotational" )

EIPS_chk_dupes <- EIPS_chk %>%
  # Count number of photos per subject
  group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, REL_GLOBALID) %>%
  summarize(n_photos = n()) %>%
  # Count number of points per subject (disregards multiple photos at one point)
  group_by(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1) %>%
  summarize(n_points = n()) %>%
  filter(n_points > 1)

# join with original data to check created date, etc.
EIPS_dupes <- EIPS_chk_dupes %>%
  left_join(EIPS_chk) %>%
  select(Samp_Year, Samp_Frame, Sampling_Frame, Site_Number, Site_Type, Subject1, n_points, Staff_list, created_date, last_edited_date, last_edited_user)



# Code to Rotate an upsidedown image -------------

upsidedown <- chk %>%
  filter(Samp_Frame == "OL",
         Site_numb == "EIPS 05",
         Subject_EIPS == "600m_Post")

# apply() function doesn't like blobs so change to list before running apply()
upsidedown$DATA <- as.list(upsidedown$DATA)
# Load photo
library(magick)
image_r <- image_read(upsidedown$DATA[[1]])
image_r
image_r <- image_flip(image_r)
upsidedown$DATA[[1]] <- image_write(image_r)
image_r <- image_read(upsidedown$DATA[[1]])
image_r
# applyr the "watermark" function to each record (ie photo)
apply(X = upsidedown, MARGIN = 1, FUN = watermark, new_folder = "upsidedown")

# --------------------------------

# example of function parameters to process photos

process_photos(AGOL_Layer = "EIPS",
                      gdb_name = "EIPS_OL_ER_20220502.gdb",
                      gdb_location = "C:/Users/JJGross/OneDrive - DOI/Documents/Photo Processing/FTPC_EIPS_Photo_Processing",
                      gdb_layer = "EIPS_OL_ER_20220502",
                      return_table = FALSE)


# ------------------------------------------------------------------------------

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

