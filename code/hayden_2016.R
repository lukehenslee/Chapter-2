# function for simulating acoustic transmitter detection probability of a
# fish moving through a line of receivers.  Function optionally produces plot
# of each simulated fish track.

receiverLineDetSim <-function (vel = 1, delayRng = c(120, 360), burstDur = 5, recSpc = 1000, maxDist = 2000,   	rngFun, outerLim = c(0, 0), nsim = 1000, showPlot = FALSE) 
{
  if (any(!is.function(rngFun))) 
    stop("Error: argument 'rngFun' must be a function...\n see ?receiverLineDetSim\n check: is.function(rngFun)")
  if (any(is.na(recSpc))) 
    recSpc <- 0
  xLim <- c(0, sum(recSpc) + sum(outerLim))
  recLoc <- c(outerLim[1], outerLim[1] + cumsum(recSpc))
  yLim <- c(-maxDist, maxDist)
  nTrns <- floor((diff(yLim)/vel)/delayRng[1])
  del <- matrix(runif(nTrns * nsim, delayRng[1], delayRng[2]), 
                nrow = nsim, ncol = nTrns)
  del <- del + burstDur
  trans <- t(apply(del, 1, cumsum))
  trans <- trans - matrix(runif(nsim, trans[, nTrns/2], trans[, (nTrns/2) + 1]), nrow = nsim, ncol 	= nTrns)
  fsh.x <- matrix(runif(nsim, xLim[1], xLim[2]), nrow = nsim, ncol = nTrns)
  fsh.y <- matrix(trans * vel, nrow = nsim, ncol = nTrns)
  if (showPlot) {
    plot(NA, xlim = xLim, ylim = yLim, asp = c(1, 1), xlab = "Distance (in meters) along 		receiver line", 
         ylab = "Distance (in meters) along fish path")
    for (i in 1:nsim) {
      lines(fsh.x[i, ], fsh.y[i, ], col = "grey")
      points(fsh.x[i, ], fsh.y[i, ], pch = 20, cex = 0.8)
    }
    points(recLoc, rep(0, length(recLoc)), pch = 21, bg = "red", 
           cex = 1.2)
    legend("topleft", legend = c("receiver", "sim. fish path", "tag transmit"), pch = c(21, 	124, 20), col = c("black",  "grey", "black"), pt.bg = c("red", NA, NA), pt.cex = 	c(1.2, 1, 0.8))
  }
  for (i in 1:length(recLoc)) {
    if (i == 1) {
      succ <- detP <- distM <- vector("list", length(recLoc))
      nDets <- matrix(NA, nrow = nsim, ncol = length(recLoc))
    }
    distM[[i]] <- sqrt((fsh.x - recLoc[i])^2 + (fsh.y)^2)
    detP[[i]] <- matrix(rngFun(distM[[i]]), nrow = nsim)
    succ[[i]] <- matrix(rbinom(length(detP[[i]]), 1, detP[[i]]), nrow = nsim)
    nDets[, i] <- rowSums(succ[[i]])
  }
  maxDet <- apply(nDets, 1, max)
  detProb <- mean(maxDet > 1)
  return(data.frame(detProb = detProb))
}

# function to calculate the probability of detecting a transmitter transmission at a
# given distance.  Uses a logistic function to model detection range curve.

rngFun <- function(dm, b = c(beta0, beta1)){
  p <- 1 / (1 + exp(-(b[1] + b[2]*dm)))
  return(p)
}
# assign slope and intercept
beta0 <- 5.5 # intercept of logistic detection range curve
beta1 <- -1/120 # slope of logistic detection range curve

# plot detection range curve for distances from 0 to 2000 meters to check.
plot(rngFun(0:2000), type = 'l', xlab = 'transmitter-receiver distance (m)', 	ylab = 'detection probability', las = 1)
# simulate 1000 fish crossing a 10 receiver line with receivers spaced at 1000 m, max # distance between fish and receiver line = 1000 m, outer limits for first and last 
# receivers = 500 m, constant fish movement rate = 1 m*s-1, random transmitter delay
# ranging from 60 to 180 seconds, and duration of transmitter signal = 5 seconds. 
# Produce plot of simulated track for each fish.  
receiverLineDetSim(vel = 1, delayRng = c(60,180), burstDur = 5.0, recSpc = 	rep(1000,9), maxDist = 1000, rngFun = rngFun, outerLim = 	c(500, 	500), nsim=10, showPlot = TRUE)

# Another example of simulating the probability of detecting a fish moving 
# through a receiver line.  This example uses empirically determined detection range # curve instead of a logistic detection range curve.
# create data frame with observed probability of detecting a tag transmission (p) at
# each distance (x)
edr <- data.frame( 
  x=c(0,363,444,530,636,714,794,889,920),  # tag-receiver distance
  p=c(1,1,0.96,0.71,0.67,0.75,0.88,0.21,0))   # detection prob

# create a function to return the detection probability based on distance and linear
# interpolation within edr data frame (i.e., estimate p at given x by "connecting the # dots") 
edrf <- function(dm, my.edr=edr) {
  p <- approx(x=my.edr$x,y=my.edr$p,xout=dm, rule=2)$y
  return(p)
}
# preview empirical detection range curve
plot(edrf(0:2000),type="l",ylab="probability of detecting each coded 	burst", xlab="distance between receiver and transmitter, meters")
# use empirical curve (edrf) in simulation, show optional plot
receiverLineDetSim(rngFun=edrf, nsim=10, showPlot=TRUE)
