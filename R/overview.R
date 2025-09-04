# 5/1/2024

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
    dplyr::group_by(Cycle, Year, Sampling_Frame, Nativity) %>%
    filter(!is.na(Chg_Prior)) %>%
    dplyr::summarize(out_median = median(Chg_Prior, na.rm = TRUE),
              n_plots = n(),
              p_val = get_p(Chg_Prior)) %>%
    dplyr::mutate(positive_negative = dplyr::case_when(out_median > 0 ~ "positive",
                                         out_median <= 0 ~ "negative",
                                         .default = NA)) %>%
    dplyr::mutate(Sampling_Frame2 = paste0(Sampling_Frame, " (", n_plots, ")"))

  if (nativity == "Native") {
    color_scheme <-  c("positive" = "#336600", "negative" = "#FF6633")
  }
  if (nativity == "Non-Native") {
    color_scheme <-  c("positive" = "#FF6633", "negative" = "#336600")
  }

  df_years2 <- df %>%
    filter(Cycle == cycle) %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))
  max_year2 <- max(df_years2$Year)
  min_year2 <- min(df_years2$Year)
  max_year2.2 <- stringr::str_sub(max_year2, -2)

  df_years1 <- df %>%
    filter(Cycle == (as.numeric(cycle) - 1)) %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))
  max_year1 <- max(df_years1$Year)
  min_year1 <- min(df_years1$Year)
  max_year1.2 <- stringr::str_sub(max_year1, -2)

  title <- paste0("Cycle ", cycle, "-", cycle-1, " (", min_year2, "-", max_year2.2," vs. ", min_year1, "-", max_year1.2, ")")

  cover_boxplot <- nat_chg_und %>%
    dplyr::left_join(nat_chg_und_grouped) %>%
    filter(Nativity == nativity) %>%
    filter(Cycle == cycle) %>%
    ggplot2::ggplot(aes(x=forcats::fct_reorder(.na_rm = TRUE, Sampling_Frame2, desc(Chg_Prior), median),
               y=Chg_Prior,
               fill = positive_negative)) +
    ggplot2::geom_boxplot() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)) +
    ggplot2::ggtitle(title) +
    ggplot2::xlab("Sampling Frame") +
    ggplot2::ylab(paste0(nativity, " - Change in % cover")) +
    ggplot2::scale_fill_manual(values = color_scheme) +
    ggplot2::theme(legend.position="none") +
    ggplot2::scale_y_continuous(limits = c(min_y, max_y), breaks=seq(min_y,max_y,10)) +
    gghighlight::gghighlight(min(p_val) < 0.05,
                unhighlighted_params = list(color = "#696969", fill = NULL, alpha = 0.15, outlier.alpha = 0.5)) +
    ggplot2::geom_hline(yintercept = 0, linetype="dashed", color = "black")

  if (no_y_labs == TRUE) {
    cover_boxplot <- cover_boxplot +
      ggplot2::ylab(NULL) +
      ggplot2::theme(legend.position="none",
            #axis.title.x=ggplot2::element_blank(),
            axis.title.y=ggplot2::element_blank(),
            axis.text.y=ggplot2::element_blank(),
            axis.ticks.y=ggplot2::element_blank())
  }

  return(cover_boxplot)

}

