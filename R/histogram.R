rm(list = ls())
require(ggdmc)
setwd("/media/yslin/MERLIN/Life/job_apps/Leeds/")
tmax <- 2   
h    <- 1e-4    
p.vector <- c(v=1.2, a=1, z=.5, t0=0, s=1)

n <- 100
res0 <- vector(mode="list", length=n)
idx <- vector(mode="list", length=n)
DT100 <- R100 <- numeric(n)
cm100 <- 0 ## Find the maximal time in all the trials
for(i in 1:n) {
    res0[[i]] <- r1d(P=p.vector, tmax=tmax, h=h)
    idx[[i]] <- sum(!is.na(res0[[i]]$Xt));
    dat <- res0[[i]]
    j <- idx[[i]]
    DT100[i] <- max(dat$T[1:j])
    R100[i] <- dat$out[3]
    cm100 <- ifelse( DT100[i] > cm100, DT100[i], cm100 )
}


n <- 1e3
DT1e3 <- R1e3 <- numeric(n)
cm1e3 <- 0
res0 <- vector(mode="list", length=n)
idx <- vector(mode="list", length=n)
for(i in 1:n) {
    res0[[i]] <- r1d(P=p.vector, tmax=tmax, h=h)
    idx[[i]] <- sum(!is.na(res0[[i]]$Xt));
    dat <- res0[[i]]
    j <- idx[[i]]
    DT1e3[i] <- max(dat$T[1:j])
    R1e3[i] <- dat$out[3]
    cm1e3 <- ifelse( DT1e3[i] > cm1e3, DT1e3[i], cm1e3 )
}



n <- 1e4
DT1e4 <- R1e4 <- numeric(n)
cm1e4 <- 0
res0 <- vector(mode="list", length=n)
idx <- vector(mode="list", length=n)
for(i in 1:n) {
    res0[[i]] <- r1d(P=p.vector, tmax=tmax, h=h)
    idx[[i]] <- sum(!is.na(res0[[i]]$Xt));
    dat <- res0[[i]]
    j <- idx[[i]]
    DT1e4[i] <- max(dat$T[1:j])
    R1e4[i] <- dat$out[3]
    cm1e4 <- ifelse( DT1e4[i] > cm1e4, DT1e4[i], cm1e4 )
}

save(DT100, R100, cm100,
     DT1e3, R1e3, cm1e3,
     DT1e4, R1e4, cm1e4, file = "data/histogram.RData")



scale <- .05 ## bin size
bin <- seq(0, max(DT100, DT1e3, DT1e4)+scale, scale); bin

p0 <- hist(DT100[R100==0], breaks=bin, plot=FALSE) ## positive response
p1 <- hist(DT100[R100==1], breaks=bin, plot=FALSE)
p2 <- hist(DT1e3[R1e3==0], breaks=bin, plot=FALSE)
p3 <- hist(DT1e3[R1e3==1], breaks=bin, plot=FALSE)
p4 <- hist(DT1e4[R1e4==0], breaks=bin, plot=FALSE)
p5 <- hist(DT1e4[R1e4==1], breaks=bin, plot=FALSE)

den0 <- p0$counts / 1e2; 
den1 <- p1$counts / 1e2; 
den2 <- p2$counts / 1e3; 
den3 <- p3$counts / 1e3; 
den4 <- p4$counts / 1e4; 
den5 <- p5$counts / 1e4;

xlim <- c(0, max(p0$mids, p2$mids, p4$mids)); xlim
ylim <- c(0, max(den0, den1, den2, den3, den4, den5)); ylim

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")



margin <- c(2.5, 5.3, .1, 1)
fontsize <- 1.5
mainsize <- 2
xlim <- c(0, max(DT100, DT1e3, DT1e4))
## ylim <- c(0, max_count)
grayscale <- gray(0:8 / 8, alpha=1/2)
fontsize <- 3

png("figs/histograms.png", 1024, 768)
par(mfrow = c(3,1),
    mar = margin,
    ## mai = c(.05, 0.05, .1, 0.1)
    oma = c(2, 2, .2, .2))

barplot(den0,
        col = rethinking::col.alpha(cbPalette[1], .30),
        main = "",
        ylim = ylim,
        xlab = "",
        ylab = "Probability",
        cex.lab = fontsize,
        cex.axis = fontsize/2)

barplot(den1,
        col = rethinking::col.alpha(cbPalette[2], .30),
        cex.lab = fontsize,
        cex.axis = fontsize/2,
        add=TRUE)

barplot(den2,
        col = rethinking::col.alpha(cbPalette[1], .30),
        main = "",
        ylim = ylim,
        xlab = "",
        ylab = "Probability",
        cex.lab = fontsize,
        cex.axis = fontsize/2,
        border = NA)

barplot(den3,
        col = rethinking::col.alpha(cbPalette[2], .30),
        cex.lab = fontsize,
        cex.axis = fontsize/2,
        add=TRUE)

barplot(den4,
        col = rethinking::col.alpha(cbPalette[1], .30),
        main = "",
        ylim = ylim,
        xlab = "",
        ylab = "Probability",
        cex.lab = fontsize,
        cex.axis = fontsize/2,
        border = NA)

barplot(den5,
        col = rethinking::col.alpha(cbPalette[2], .30),
        cex.lab = fontsize,
        cex.axis = fontsize/2,
        add=TRUE)
dev.off()


axis(1, at=seq(0, 1.5, .1))
seq(0, 1.5, .1)

