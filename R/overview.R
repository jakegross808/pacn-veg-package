#' Create boxplots of understory cover for all Sampling Frames in Dataset
#'
#' @param df Dataframe from summarize_understory()
#' @param y y-variable from dataframe used for boxplot
#' @param nativity default = "Native"; can be changed to "Non-Native"
#' @param cycle default = 2; select one cycle, can be 2 or 3
#' @param no_y_labs default = FALSE; if true, removes y-axis tick marks and labels
#'
#' @return ggplot boxplot
#' @export
#'
#' @examples
#' \dontrun{
#' ncyc3 <- understory_cover_boxplot(nat_chg_und, nativity = "Non-Native", cycle = 3, no_y_labs = TRUE)
#'
#' }

understory_cover_boxplot <- function(df, y = "Chg_Prior", nativity = "Native", cycle = 2, no_y_labs = FALSE) {

  nat_chg_und <- df %>%
    filter(Cycle > 1)

  max_y<- round(max(nat_chg_und$Chg_Prior, na.rm = TRUE), digits = -1)
  min_y<- round(min(nat_chg_und$Chg_Prior, na.rm = TRUE), digits = -1)

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
    mutate(positive_negative = case_when(out_median > 0 ~ "positive",
                                         out_median <= 0 ~ "negative",
                                         .default = NA)) %>%
    mutate(Sampling_Frame2 = paste0(Sampling_Frame, " (", n_plots, ")"))

  if (nativity == "Native") {
    color_scheme <-  c("positive" = "#336600", "negative" = "#FF6633")
  }
  if (nativity == "Non-Native") {
    color_scheme <-  c("positive" = "#FF6633", "negative" = "#336600")
  }

  df_years2 <- df %>%
    filter(Cycle == cycle) %>%
    mutate(Year = as.integer(as.character(Year)))
  max_year2 <- max(df_years2$Year)
  min_year2 <- min(df_years2$Year)
  max_year2.2 <- str_sub(max_year2, -2)

  df_years1 <- df %>%
    filter(Cycle == (as.numeric(cycle) - 1)) %>%
    mutate(Year = as.integer(as.character(Year)))
  max_year1 <- max(df_years1$Year)
  min_year1 <- min(df_years1$Year)
  max_year1.2 <- str_sub(max_year1, -2)

  title <- paste0("Cycle ", cycle, "-", cycle-1, " (", min_year2, "-", max_year2.2," vs. ", min_year1, "-", max_year1.2, ")")

  cover_boxplot <- nat_chg_und %>%
    left_join(nat_chg_und_grouped) %>%
    filter(Nativity == nativity) %>%
    filter(Cycle == cycle) %>%
    ggplot(aes(x=fct_reorder(.na_rm = TRUE, Sampling_Frame2, desc(Chg_Prior), median),
               y=Chg_Prior,
               fill = positive_negative)) +
    geom_boxplot() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggtitle(title) +
    xlab("Sampling Frame") +
    ylab(paste0(nativity, " - Change in % cover")) +
    scale_fill_manual(values = color_scheme) +
    theme(legend.position="none") +
    scale_y_continuous(limits = c(min_y, max_y), breaks=seq(min_y,max_y,10)) +
    gghighlight(min(p_val) < 0.05,
                unhighlighted_params = list(color = "#696969", fill = NULL, alpha = 0.15, outlier.alpha = 0.5)) +
    geom_hline(yintercept = 0, linetype="dashed", color = "black")

  if (no_y_labs == TRUE) {
    cover_boxplot <- cover_boxplot +
      ylab(NULL) +
      theme(legend.position="none",
            #axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())
  }

  return(cover_boxplot)

}

