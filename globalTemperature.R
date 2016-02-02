## Plot global temperature anomaly from normal value
## Data derived from JMA http://www.data.jma.go.jp/cpdinfo/temp/list/an_wld.html
##
## Takayuki NUIMURA 2016-02-02


library(ggplot2)
library(reshape2)
library(XML)
library(zoo)


## Error occurs when using CIS network (maybe protocol restriction).
url <- "http://www.data.jma.go.jp/cpdinfo/temp/list/an_wld.html"


## For reading Japanese language table, R GUI interface must be Japanese.
d <- as.data.frame(readHTMLTable(url))
colnames(d) <- c("year", "all", "north", "south")


## Data type conversion
d$time <- strptime(d$year, "%Y年", tz="")
d$year <- as.numeric(format(d$time, "%Y"))
d$all <- as.numeric(as.character(d$all))
d$north <- as.numeric(as.character(d$north))
d$south <- as.numeric(as.character(d$south))


## Calculate moving average
n.mv <- 5
d$all.mv <- rollmean(d$all, n.mv, fill=list(NA, NULL, NA))
d$north.mv <- rollmean(d$north, n.mv, fill=list(NA, NULL, NA))
d$south.mv <- rollmean(d$south, n.mv, fill=list(NA, NULL, NA))


d.mv <- na.omit(d[,c(1,6:8)])
dd.mv <- melt(d.mv, id.vars="year")

dd.mv$time <- strptime(dd.mv$year, "%Y", tz="")


### PLOT
pdf1.filename <- "all_detail.pdf"
pdf2.filename <- "compare_mv.pdf"
xlab <- "年"
ylab <- "1981-2010年平均からの差 (℃)"


g <- ggplot(d, aes(x=time , y=all)) + geom_line(colour=rgb(0.5,0.5,0.5, 0.5), lwd=0.5) + geom_point()
g <- g + theme_classic() + xlab(xlab) + ylab(ylab) + ylim(-1, 1) + xlim(as.POSIXct("1890-01-01"), as.POSIXct("2020-01-01"))

g <- g + geom_hline(yintercept=0, linetype=2)
g <- g + geom_smooth(method="lm", se=F, size=0.4)
g <- g + geom_line(aes(x=time , y=all.mv), colour="red")


ggsave("all_detail.pdf", g, width=8, height=5, family="Japan1GothicBBB")
ggsave("all_detail.png", g, width=8, height=5, family="Japan1GothicBBB")
rm(g)


g <- ggplot(dd.mv, aes(x=time, y= value, colour=variable)) + geom_line()
g <- g + theme_classic() + xlab(xlab) + ylab(ylab) + ylim(-1, 1) + xlim(as.POSIXct("1890-01-01"), as.POSIXct("2020-01-01"))
g <- g + geom_hline(yintercept=0, linetype=2)
g <- g + labs(colour="") + theme(legend.position=c(0.9, 0.2))


ggsave("compare_mv.pdf", g, width=8, height=5, family="Japan1GothicBBB")
ggsave("compare_mv.png", g, width=8, height=5, family="Japan1GothicBBB")
rm(g)
