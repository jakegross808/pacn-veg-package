copy <- read.table("clipboard")
Sys.time()
# Project 1: Weighted Dice ----

roll_dice <- function(sides = 6, number_of_die = 2) {
  dice <- sample(1:sides, size = number_of_die, replace = TRUE)
  sum(dice)
}

weighted_dice <- function(sides = 6, number_of_die = 2) {
  dice <- sample(1:sides,
                 size = number_of_die,
                 prob = c(1/8,1/8,1/8,1/8,1/8,3/8),
                 replace = TRUE)
  sum(dice)
}


roll_dice()

qplot

r10 <- replicate(10, roll_dice())
r10.3 <- replicate(10000, roll_dice())
qplot(r10, binwidth = 1)
qplot(r10.3, binwidth = 1)

rw10.3 <- replicate(10000, weighted_dice())
qplot(rw10.3, binwidth = 1)

# Project 2: Playing Cards ----

# a type of "double" is accurate to about 16 significant digits
(sqrt(2)^2) - 2
l <- sqrt(2)^2

now <- Sys.time()
now

book_1s <- unclass(now) - 1395057600
book_2m <- book_1s/60
book_3h <- book_2m/60
book_4d <- book_3h/24
book_5y <- book_4d/365


deck <- read_csv("C:/Users/JJGross/Documents/RData/training/deck.csv")
head(deck)


deck[sample(x = 1:52, size = 1),]

sample(1:52)
deck[ , ]
l<-deck[sample(1:52),]
dim(deck)

deal <- function(cards) {
  cards[1,]

}

shuffle <- function(cards) {
  random <- sample(1:52, size = 52)
  cards[random, ]
}

deck2$value[deck2$face == "ace"]
deck2[deck2$face == "ace",]

deck3 <- deck2
deck3$value[deck3$face == "ace"] <- 14
deck3[deck3$face == "ace",]
