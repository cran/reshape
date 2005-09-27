# R code generated by weaver 
# from /Users/hadley/Documents/reshape/reshape/inst/doc/introductionw.wvr on 2006-07-13 
# ---------------------------------------------------------
weaver.width <- 6
weaver.height <- 4
setwd("/Users/hadley/Documents/reshape/reshape/inst/doc")
if (file.exists(".introductionw.wcache/cache.rdata")) load(".introductionw.wcache/cache.rdata")
library(xtable)	

##<Block:0x46d1c8>
library(reshape); data(french_fries); options(digits=3)

##<LatexBlock:0x46cebc>
sink(file = ".introductionw.wcache/1dqam.txt")
xtable(head(french_fries))
sink()

##<RBlock:0x46cbc4>
sink(file = ".introductionw.wcache/1dq02.txt")
ff_d <- melt(french_fries, id=1:4)
cast(ff_d, subject ~ time, length)
sink()

##<RBlock:0x46c8cc>
sink(file = ".introductionw.wcache/1dppi.txt")
cast(ff_d, subject ~ time, function(x) 30 - length(x))
sink()

##<RBlock:0x46c5d4>
sink(file = ".introductionw.wcache/1dpey.txt")
cast(ff_d, variable ~ ., function(x) c(min=min(x), max=max(x)))
sink()

##<RBlock:0x46c2dc>
sink(file = ".introductionw.wcache/1dp4e.txt")
cast(ff_d, treatment ~ variable, mean, 
margins=c("grand_col", "grand_row"))
sink()

##<RBlock:0x46bfe4>
sink(file = ".introductionw.wcache/1dotu.txt")
cast(ff_d, treatment + subject ~ variable, mean, 
margins="treatment", subset=subject %in% c(3,10))
sink()

##<GraphicBlock:0x46bcec>
pdf(file = ".introductionw.wcache/1doja.pdf", width=weaver.width, height=weaver.height)
library(lattice)
xyplot(X1 ~ X2 | variable, cast(ff_d, ... ~ rep), aspect="iso")
dev.off()
save(list = ls(all=TRUE), file= ".introductionw.wcache/cache.rdata")