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

start <- data[data$Year>1974,]

start$investOverTime <- computeReturnsOverTime(start$SP500, 329000)
start = start[!is.na(start$investOverTime),]

ggplot(start, aes(x=Year, y=investOverTime)) + geom_bar(stat="identity") +
 scale_x_continuous(breaks = seq(min(start$Year), max(start$Year), 3)) +
  scale_y_continuous(breaks = round(seq(min(start$investOverTime), max(start$investOverTime), 1000000), 0)) +
  xlab("year") + ylab("value") +
  theme(axis.text.x = element_text(angle = 90))
ggsave("plots/329k-starting-in-70s.png")


