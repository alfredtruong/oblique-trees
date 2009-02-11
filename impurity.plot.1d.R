################### plot tick gini graph ################### 
impurity.plot.1d <- function(
	split.impurity=c("deviance","gini"),
	info=iris$Sepal.Length,
	classes=iris$Species) 
{
	impurity.grow <- function(
		class.freq)				#table with class frequencies
	#DESCRIPTION
	#	given summarized figures of class frequencies and a split criterion, the impurity measure of such a split is given
	#OUTPUT		impurity			numeric impurity value of node with such a composition of examples
	{
		#calculate the "impurity" measure on a leaf be it via gini/deviance when given class probabilities at a leaf
		n <- sum(class.freq)
		class.probs <- class.freq/n
		#evaluate impurity
		if (split.impurity=="deviance") {
			#deviance
			#change any class.probs from 0 to 1 to make 0*log(0)=0 by default
			class.probs[class.probs==0] <- 1
			impurity <- -2*sum(class.probs*log(class.probs))
		} else if (split.impurity=="gini") {
			#gini
			impurity <- 1-sum(class.probs^2)
		}

		return(impurity)
	}

	split.impurity <- match.arg(split.impurity)
	#points <- seq(	from=ticks[1],
	#		to=ticks[length(ticks)],
	#		length.out=length(ticks)*detail
	#)
	#points <- points[-1]	#has nothing in the left child
	knots <- sort(unique(info))
	midpoints <- (knots[-1]+knots[-length(knots)])/2
	impurities <- NULL
	for (i in 1:length(midpoints)) {
		left.bin <- info < midpoints[i]
		impurities[i] <-	(	impurity.grow(	table(classes[left.bin])
						)*sum(left.bin)+
						impurity.grow(	table(classes[!left.bin])
						)*sum(!left.bin)
					)/length(left.bin)		
	}

	#plot these points to the plot
	plot(	x=midpoints,
		y=impurities,
		type="n",
		ylab="Split impurity",
		xlab="Data points"
	)
	#add rugs for each class
	for (i in 1:length(levels(classes))) {
		rug(jitter(info[classes==levels(classes)[i]]),col=i)
	}
	#fit a step function to impurities data
	plot(stepfun(x=knots,y=c(impurities[1],impurities,impurities[length(impurities)]),f=0),add=TRUE,do.points=FALSE)
	#fit a spline function to impurities data
#	lines(	spline(	x=points,
#			y=impurities
#		)
#	)
	#reinitialise the axes
	par(new=TRUE)
	plot(	x=info,
		y=1:length(info),
		col=as.numeric(classes),
		type="n",
		axes=FALSE,
#		pch=".",
#		cex=0.5,
		xlab="",
		ylab="")
	par(new=FALSE)
	#overlay original data onto existing plot
	points(	x=info,
		y=1:length(info),
		col=as.numeric(classes),
#		pch=".",
		cex=0.5)
}


#use impurity.plot()
library(MASS)
impurity.plot.1d()

#counts number of superclass splits of interest
generally.positioned.points <- 3; dimensions <- 1; temp <- 0; for (i in 0:dimensions) { temp <- temp + choose(generally.positioned.points-1,i) }; temp <- temp-1; temp









################ iris
library(MASS)
x11()
pairs(iris[,-5],col=as.numeric(iris[,5]))
x11()
par(mfrow=c(2,2))
for (i in 1:4) {
	impurity.plot.1d(
		split.impurity="gini",
		info=iris[,i],
		classes=iris$Species) 
}
################ crabs
library(MASS)
x11()
crabs.data <- predict(princomp(crabs[,-(1:3)]))
pairs(crabs.data,col=rep(1:4,each=50))
x11()
par(mfrow=c(3,3))
for (i in 1:5) {
	impurity.plot.1d(
		split.impurity="gini",
		info=crabs.data[,i],
		classes=factor(rep(1:4,each=50))) 
}


library(MASS)
x11()
crabs.data <- predict(princomp(crabs[,-(1:3)]))
plot(crabs.data[,2:3],col=rep(1:4,each=50))
x11()
for (i in 2:3) {
	x11();
	impurity.plot.1d(
		split.impurity="gini",
		info=crabs.data[,i],
		classes=factor(rep(1:4,each=50))) 
}


