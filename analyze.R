input <- c(0x050303, 0xfff000, 0xf0f0f0);
capture <- "~/sandbox/ws2811-logic/logic.csv"

library(readr)
logic <- read.csv(capture)
logic$Time.s. <- logic$Time.s. * 1000000
logic$duration <- c(diff(logic$Time.s.), NA)
logic <- logic[c(-1),]

bits <- function(integer, nbits) {
  allbits <- sapply(input, function(x) {
    as.integer(intToBits(x))
  })
  
  return(allbits[nbits:0, ])
}

index <- 1:(length(input) * 24)
b <- as.vector(bits(input, 24))
ones <- logic[which(logic$Channel.0 == 1), 'duration']
zeroes <- logic[which(logic$Channel.0 == 0), 'duration']
timings <- data.frame(index=index, value=b, TH=ones, TL=zeroes)
timings$value <- as.factor(timings$value)
timings <- na.omit(timings)

# Timing histogram
library(ggplot2)
#ggplot(timings, aes(x=index, y=duration)) + geom_col
ggplot(timings, aes(TH, fill=value)) + geom_histogram(binwidth=0.01)
ggplot(timings, aes(TL, fill=value)) + geom_histogram(binwidth=0.01)

library(reshape2)
vars <- names(timings) %in% c('value')
chart.data <- melt(timings[!vars], id.vars='index')
chart.data$variable <- relevel(chart.data$variable, 'TL')
ggplot(chart.data, aes(x = index, y = value, fill = variable)) + coord_flip() + geom_col() + scale_x_reverse()

T0H <- timings[which(timings$value == 0),]$TH
T0L <- timings[which(timings$value == 0),]$TL
T1H <- timings[which(timings$value == 1),]$TH
T1L <- timings[which(timings$value == 1),]$TL

cat('T0H: ', min(T0H), ' to ', max(T0H), '\n')
cat('T0L: ', min(T0L), ' to ', max(T0L), '\n')
cat('T1H: ', min(T1H), ' to ', max(T1H), '\n')
cat('T1L: ', min(T1L), ' to ', max(T1L), '\n')
