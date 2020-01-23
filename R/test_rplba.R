rm(list = ls())
require(wesanderson)
setwd('/media/yslin/MERLIN/Life/job_apps/Leeds/')
n1 <- 2^10
n2 <- 2^15
n3 <- 2^20

## n <- 2^20
dat1 <- ppda::rplba3(n1)
dat2 <- ppda::rplba3(n2)
dat3 <- ppda::rplba3(n3)
xlim <- range(dat1$RT, dat2$RT, dat3$RT); xlim

crt1 <- dat1[dat1$R==1,"RT"]
ert1 <- dat1[dat1$R==2,"RT"]
crt2 <- dat2[dat2$R==1,"RT"]
ert2 <- dat2[dat2$R==2,"RT"]
crt3 <- dat3[dat3$R==1,"RT"]
ert3 <- dat3[dat3$R==2,"RT"]

scale <- .05 ## bin size
bin <- seq(0, max(xlim)+scale, by = scale); bin
xlim <- c(0, 2)






quantile(crt1)
png("figs/histogram1.png", 1024, 768)
par(mfrow=c(3,2), mar = c(2.5, 5.3, .1, 1), mai = c(.05, 0.5, .1, 0.1), 
    oma = c(2, 2, .2, .2))
hist(crt1, breaks=bin, xlim = xlim, col=rgb(0,0,1,1/4), main="", xaxt="n",
     cex.lab=2, cex.axis=1.5)
hist(ert1, breaks=bin, xlim = xlim, col=rgb(1,0,1,1/4), add = TRUE)

hist(crt1, breaks='fd', xlim = xlim, col=rgb(0,0,1,1/4), main="", xaxt="n",
     cex.lab=2, cex.axis=1.5, ylab="")
hist(ert1, breaks='fd', xlim = xlim, col=rgb(1,0,1,1/4), add = TRUE)


hist(crt2, breaks=bin, xlim = xlim, col=rgb(0,0,1,1/4), main="", xaxt="n",
          cex.lab=2, cex.axis=1.5)
hist(ert2, breaks=bin, xlim = xlim, col=rgb(1,0,1,1/4), add = TRUE)

hist(crt2, breaks='fd', xlim = xlim, col=rgb(0,0,1,1/4), main="", xaxt="n",
     cex.lab=2, cex.axis=1.5, ylab="")
hist(ert2, breaks='fd', xlim = xlim, col=rgb(1,0,1,1/4), add = TRUE)

hist(crt3, breaks=bin, xlim = xlim, col=rgb(0,0,1,1/4), main="",
          cex.lab=2, cex.axis=1.5)
hist(ert3, breaks=bin, xlim = xlim, col=rgb(1,0,1,1/4), add = TRUE)

hist(crt3, breaks='fd', xlim = xlim, col=rgb(0,0,1,1/4), main="", ylab="",
     cex.lab=2, cex.axis=1.5)
hist(ert3, breaks='fd', xlim = xlim, col=rgb(1,0,1,1/4), add = TRUE)
dev.off()



ylab <- 'Frequency'
cexlab <- 2.5
cexa <- 1.5
png("figs/histogram2.png", 1024, 768)
par(mfrow = c(3,2), mar = c(2.5, .1, .1, 1), mai = c(.05, 0.8, .1, 0.1), 
    oma = c(2, 2, .2, .2))
hist(crt1, breaks='fd', xlim = xlim, ylim = c(0, 90), col=rgb(0,0,1,1/4),
     main="", xaxt="n", cex.lab=cexlab, cex.axis=cexa, ylab=ylab)
hist(ert1, breaks='fd', xlim = xlim, ylim = c(0, 90), col=rgb(1,0,1,1/4),
     main="", xaxt="n", yaxt = "n", ylab="")

hist(crt2, breaks='fd', xlim = xlim, ylim = c(0, 1250), col=rgb(0,0,1,1/4),
     main="", xaxt="n", cex.lab=cexlab, cex.axis=cexa, ylab=ylab)
hist(ert2, breaks='fd', xlim = xlim, ylim = c(0, 1250), col=rgb(1,0,1,1/4),
     main="", xaxt="n", yaxt = "n", ylab="")

hist(crt3, breaks='fd', xlim = xlim, ylim = c(0, 9e3), col=rgb(0,0,1,1/4),
     main="", ylab=ylab, cex.lab=cexlab, cex.axis=cexa)
hist(ert3, breaks='fd', xlim = xlim, ylim = c(0, 9e3), col=rgb(1,0,1,1/4),
     main="", cex.lab = cexlab, cex.axis = cexa, ylab="", yaxt = "n")

dev.off()


pal <- wes_palette("GrandBudapest1", 3)
qtils0 <- quantile(crt2, probs = seq(.1, .9, length.out = 5)); qtils0
qtils1 <- quantile(ert2, probs = seq(.1, .9, length.out = 5))


ylab <- 'Frequency'
cexlab <- 2.5
cexa <- 1.5
png("figs/histogram3.png", 1024, 768)
par(mfrow = c(1,2), mar = c(2.5, .1, .1, 1), mai = c(.05, 1, .1, 0.1), 
    oma = c(2, 2, .2, .2))
hist(crt2, breaks='fd', xlim = xlim, ylim = c(0, 1250), col=rgb(0,0,1,1/4),
     main="", cex.lab=cexlab, cex.axis=cexa, ylab=ylab)
abline(v=qtils0, lwd = 2, col = pal)
hist(ert2, breaks='fd', xlim = xlim, ylim = c(0, 1250), col=rgb(1,0,1,1/4),
          main="", cex.lab=cexlab, cex.axis=cexa, yaxt = "n", ylab="")
abline(v=qtils1, lwd = 2, col = pal)
dev.off()




c(.15, .45, .65, .90)
