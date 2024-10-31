# Basic R terminology

# Vectors ----
x <- c(1,2,3)
y <- c(2,4,6)

is.vector(x)
is.data.frame(x)

# A vector can also have just one value
name <- "Jake"

is.vector(name)
is.data.frame(name)

# anything on left side of <- is usually the object
# Vectors will usually show up under "Values" in the Global Environment

# Data Frame ----
df <- data.frame(x,y)

is.data.frame(df)
is.vector(df)

# one column
x
data.frame(x)

# empty dataframe
empty_df <- data.frame()
empty_df

weird <- c("Jake", 1, "Pacific Island Network", FALSE)
is.vector(weird)
str(weird)
typeof(weird)

typeof(x)
typeof(c(TRUE, TRUE, FALSE))

# Use $ to select a column (vector) in dataframe
is.vector(df$x)

# Easy to plot/graph data in dataframe
plot(df)

# Add another vector (column) to dataframe
df$z <- c("A", "B", "C")
df
plot(df)

?str
str(x)
str(df$x)
str(df$z)
str(z)
z
str(df)

is.vector(x)
is.vector(df)
is.data.frame(df)

is.vector(df$x)
is.vector(df$x)

df$z
is.vector(df$z)

library(ggplot2)
#ggplot2::

plot2cont_vars <- ggplot2::ggplot(df, aes(x,y))

plot2cont_vars + geom_label(aes(label = x))
plot2cont_vars + ggplot2::geom_point(color = "red")
plot2cont_vars + geom_smooth(method = lm)

plot_1disc_1cont <- ggplot2::ggplot(df, aes(z,y))

plot_1disc_1cont + geom_col()

# Three variables
ggplot(df, aes(x,y, color = z)) +
  ggplot2::geom_point()


plot_1disc_1cont + ggplot2::geom_boxplot()

df

df10 <- df$x + 10
df10 <- df$x + 10
df10

# Rename our columns?
df
names(df)

# make a copy
names_df <- df
names_df

# Renaming using base R
names(names_df)[names(names_df)=="z"] <- "species"
names(names_df)[names(names_df)=="x"] <- "count"
names(names_df)[names(names_df)=="y"] <- "stems"
names_df

library(tidyverse)
# Renaming using tidyverse
df
names_df2 <- df %>%
  rename(species = z)

names_df2

names_df2 <- df %>%
  rename(species = z,
         count = x,
         stems = y)

plot2 <- names_df2 %>%
  mutate(count = count*3,
         stems = count)
plot2

plot3 <- plot2 %>%
  mutate(count = count-3,
         stems = count)
plot3


new_plot <- function(operator, amount) {
  count <- c(1,2,3)
  stems <- c(2,4,6)
  species <- c("A", "B", "C")
  df <- data.frame(count,stems,species)

  if (operator == "add") {
  df <- df %>%
    mutate(count = count + amount,
           stems = count)
  }

  if (operator == "mult") {
    df <- df %>%
      mutate(count = count * amount,
             stems = count)
  }

  return(df)

}

new_plot(operator = "add", amount = 100)
new_plot(operator = "mult", amount = 1)

user <- "jake"
user <- "kathy"
user <- "Jake"

if (user == "jake") {
  print("Hi Jake")
  }

if (user == "jake") {
  print("Hi Jake")
  } else {
    print("You are not Jake")
}

plot1 <- names_df2
plot1
plot2 <- new_plot(operator = "add", amount = -1)
plot2
plot3 <- new_plot(operator = "mult", amount = 10)
plot3

plot1 <- names_df2 %>%
  mutate(plot = "1")
plot1
plot2 <- new_plot(operator = "add", amount = -1) %>%
  mutate(plot = "2")
plot2
plot3 <- new_plot(operator = "mult", amount = 10) %>%
  mutate(plot = "3")
plot3

dplyr::bind_rows(plot1, plot2, plot3)

df3 <- dplyr::bind_rows(plot1, plot2, plot3)

ggplot(df3, aes(count,stems, color = species)) +
  ggplot2::geom_point()

ggplot(df3, aes(plot,count, color = species)) +
  ggplot2::geom_point()

ggplot(df3, aes(species, count, color = species)) +
  ggplot2::geom_boxplot()

ggplot(df3, aes(plot, count)) +
  ggplot2::geom_boxplot()

ggplot(df3, aes(plot, count)) +
  ggplot2::geom_boxplot(notch = TRUE)

?geom_boxplot

data()
data(cats)
head(cats, 10)

ggplot(cats, aes(x = Sex, y = Bwt)) +
  ggplot2::geom_boxplot(notch = TRUE)

data(iris)
head(iris, 10)

ggplot(iris, aes(x = Species, y = Sepal.Width)) +
  ggplot2::geom_boxplot(notch = TRUE)

