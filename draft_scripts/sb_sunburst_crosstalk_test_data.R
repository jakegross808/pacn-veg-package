library(plotly)

list1 <- (dplyr::tibble(
  labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
  parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
  values = c(65, 14, 12, 10, 2, 6, 6, 4, 4),
))

nest1 <- list1 %>%
  tidyr::nest(data = everything()) %>%
  dplyr::mutate(name = "bible")

list2 <- (dplyr::tibble(
  labels = c("Abraham", "Herb", "Homer", "Bart", "Lisa", "Maggie"),
  parents = c("", "Abraham", "Abraham", "Homer", "Homer", "Homer"),
  values = c(100, 25, 75, 25, 25, 25),
))

nest2 <- list2 %>%
  tidyr::nest(data = everything()) %>%
  dplyr::mutate(name = "simpsons")

fam_tree <- dplyr::bind_rows(nest1, nest2)

plotly::plot_ly(fam_tree[[1]][[1]], labels = ~labels, parents = ~parents, values = ~values, type = 'sunburst', branchvalues = 'total')
plotly::plot_ly(fam_tree[[1]][[2]], labels = ~labels, parents = ~parents, values = ~values, type = 'sunburst', branchvalues = 'total')

fam_tree %>%
  dplyr::filter(name == "simpsons") %>%
  dplyr::pull(data) %>%
  purrr::pluck()

fam_tree[[1]][[2]]

create_buttons <- function(df, y_axis_var_names) {
  lapply(
    y_axis_var_names,
    FUN = function(var_name, df) {
      button <- list(
        method = 'restyle',
        args = list('y', list(df[, var_name])),
        label = sprintf('Show %s', var_name)
      )
    },
    df
  )

}

y_axis_var_names <- c('bible', 'simpsons')

p <- plot_ly(fam_tree, x = ~x, y = ~y, mode = "markers", name = "A", visible = T) %>%
  layout(
    title = "Drop down menus - Styling",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        y = 0.7,
        buttons = create_buttons(df, y_axis_var_names)
      )
    ))
p

fam_tree[[1]][[2]]
fam_tree[[1]][[1]]

plotly::plot_ly(fam_tree,
                labels = ~lapply(fam_tree$data, function(x) x[1]),
                parents = ~lapply(fam_tree$data, function(x) x[2]),
                values = ~lapply(fam_tree$data, function(x) x[3]),
                type = 'sunburst',
                branchvalues = 'total')

l
#,
                updatemenus = list(
                  list(
                    y = 0.7,
                    buttons = list(
                      list(method = "restyle",
                           args = list("simpsons", list(fam_tree[[1]])),  # put it in a list
                           label = "Show A"),
                      list(method = "restyle",
                           args = list("bible", list(fam_tree[[2]])),  # put it in a list
                           label = "Show B")))))


fam_tree$name
(unlist(fam_tree, recursive = FALSE), `[`, 1)
lapply(fam_tree$data, function(x) x[3])
lapply(fam_tree, function(x) x[[1]])
sapply(fam_tree, function(x) x[[1]])
lapply(unlist(fam_tree, recursive = FALSE), `[`, 1)

# An example nested list
myNestedList <- list(A = list(`0` = c(`1` = 10, `2` = 20, `3` = 30, `4` = 72),
                              `1` = c(`1` = 15, `2` = 9, `3` = 7)),
                     B = list(`0` = c(A = 11, B = 12, C = 13),
                              `1` = c(X = 14, Y = 15, Z = 16)))

# Run the following and see what you come up with....
lapply(unlist(fam_tree, recursive = FALSE), `[`, 1)
lapply(unlist(fam_tree, recursive = FALSE), `[[`, 1)
sapply(unlist(fam_tree, recursive = FALSE), `[[`, 1)
rapply(fam_tree, f=`[[`, ...=1, how="unlist")
