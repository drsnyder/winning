data = read.csv("data/historical-returns.csv", header=T)
qplot(data$SP500, geom="histogram", binwidth=5,
      main="Histogram for historical S&P 500 yearly returns",
      xlab="Percentage", ylab="Count",
      fill=I("blue"), col=I("black"))

computeReturnsOverTime <- function(v, startingPoint) {
  returnsOverTime <- c(startingPoint)
  for (i in 2:length(v)) {
    returnsOverTime[i] <- returnsOverTime[i-1] + (returnsOverTime[i-1] * (v[i] / 100))
  }
  returnsOverTime
}

start <- data[data$Year>1926,]

start$investOverTime <- computeReturnsOverTime(start$SP500, 1)

ggplot(start, aes(x=Year, y=investOverTime)) + geom_line() +
  scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), 3)) +
  scale_y_continuous(breaks = round(seq(min(data$investOverTime), max(data$investOverTime), 30), 0)) +
  theme(axis.text.x = element_text(angle = 90))


