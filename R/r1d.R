rm(list = ls())
require(ggdmc)
setwd("/media/yslin/MERLIN/Life/job_apps/Leeds/")
tmax <- 2   
h    <- 1e-4    
p.vector <- c(v=0, a=1, z=.5, t0=0, s=1)
n <- 10

res0 <- vector(mode="list", length=n)
idx <- vector(mode="list", length=n)
for(i in 1:n) {
    res0[[i]] <- r1d(P=p.vector, tmax=tmax, h=h)
    idx[[i]] <- sum(!is.na(res0[[i]]$Xt)); 
}

current_max <- 0
for(i in 1:n) {
    dat <- res0[[i]]
    j <- idx[[i]]
    tmp <- max(dat$T[1:j])
    current_max <- ifelse( tmp > current_max, tmp, current_max )
}

## gif
fn <- rep(NA, n)
for(i in 1:n) {
    fn[i] <- paste0("figs/rw10_", i, ".png")
    png(filename=fn[i], 800, 600)
    dat <- res0[[i]]
    j <- idx[[i]]
    plot(dat$T[1:j], dat$Xt[1:j], type='l', xlim=c(0, current_max), ylim=c(0, 1), xlab='DT (s)',
         ylab='Evidence', lwd = 3, cex.axis=1.5, cex.lab=1.5)
    abline(h=1, lty='dotted', lwd=2)
    abline(h=0, lty='dotted', lwd=2)
    points(x=0, y=p.vector[3], col='red')
    dev.off()
}



current_max <- 0
res1 <- vector(mode="list", length=n)
idx <- vector(mode="list", length=n)
for(i in 1:n) {
    res1[[i]] <- r1d(P=p.vector, tmax=tmax, h=h)
    idx[[i]] <- sum(!is.na(res1[[i]]$Xt));

    dat <- res1[[i]]
    j <- idx[[i]]
    tmp <- max(dat$T[1:j])
    current_max <- ifelse( tmp > current_max, tmp, current_max )
}


current_max


png(filename='figs/random_walk_1d.png', 800, 600)
dat <- res1[[1]]
ix  <- idx[[1]]
plot(dat$T[1:ix], dat$Xt[1:ix], type='l',
     xlim = c(0, current_max),
     ylim=c(0, 1), xlab='DT (s)',
     ylab='Evidence', lwd = 4, cex.axis=1.5, cex.lab=1.5)
abline(h=1, lty='dotted', lwd=2)
abline(h=0, lty='dotted', lwd=2)
points(x=0, y=p.vector[3], col='red')
for(i in 2:n) {
    dat <- res1[[i]]
    ix  <- idx[[i]]
    lines(dat$T[1:ix], dat$Xt[1:ix])
}
dev.off()

