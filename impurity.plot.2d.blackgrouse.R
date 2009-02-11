impurity.plot.2d <- function(
	split.impurity=c("deviance","gini"),
	info=crabs.data[,2:3],
	classes=factor(rep(1:4,each=50)),
	grid.detail=20)
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

	two.dimensional.impurity <- function(
		info,
		classes,
		x.point,
		y.point)
	{
#browser()
		#for every point other than info.index
		impurity <- Inf
		for (point.of.interest in 1:dim(info)[1]) {
#			print(point.of.interest)
			#0=a+bx+cy
			#	fixed point = info[info.index,]
			#	point of interest = info[point.of.interest,]
#info=info[c(1,51,101,151),]
#classes=classes[c(1,51,101,151)]
#plot(info,type="n")
#text(info,label=row.names(info))
#point.of.interest=1
#point.of.interest=point.of.interest+1
			#coefficient of intercept
			A <- x.point*info[point.of.interest,2]-y.point*info[point.of.interest,1]
			#coefficient of x
			B <- y.point-info[point.of.interest,2]
			#coefficient of y
			C <- info[point.of.interest,1]-x.point
#plot(info,type="n")
#text(info,label=row.names(info),col=as.numeric(classes))
#C.temp=C+0.0001
#abline(a=-A/C.temp,b=-B/C.temp)
			#vector of Trues and Falses for points across this hyperplane (anticlockwise)
			if (x.point>info[point.of.interest,1]) {
				child.left.T.F <- 0 <= zapsmall(A+B*info[,1]+C*info[,2])
			} else {
				child.left.T.F <- 0 >= zapsmall(A+B*info[,1]+C*info[,2])
			}
#print(child.left.T.F[child.left.T.F])
			#calculate impurity value
			temp.impurity <- (	impurity.grow(	table(classes[child.left.T.F]))		*	sum(child.left.T.F)+
						impurity.grow(	table(classes[!child.left.T.F]))	*	sum(!child.left.T.F)
						)/length(child.left.T.F)
			if (!is.na(temp.impurity)) {
				impurity <- 	min(impurity,temp.impurity)
			}
		}
		return(impurity)
	}

	split.impurity <- match.arg(split.impurity)

	#make my grid of points, hopefully these points wont coincide with observations, if it does, this code below WONT work
	x.grid <- seq(	from=min(info[,1]),
			to=max(info[,1]),
			length.out=grid.detail)

	y.grid <- seq(	from=min(info[,2]),
			to=max(info[,2]),
			length.out=grid.detail)

	z.grid <- matrix(	0,
				ncol=grid.detail,
				nrow=grid.detail)

	#find impurities at grid points
	for (x.index in 1:grid.detail) {
		print(paste("x =",x.index))	#just so I can see where the calculation is at
		for (y.index in 1:grid.detail) {
#			print(paste("x =",x.index,", y =",y.index))	#just so I can see where the calculation is at
			z.grid[x.index,y.index] <- two.dimensional.impurity(
							info=info,
							classes=classes,
							x.point=x.grid[x.index],
							y.point=y.grid[y.index])
		}
	}
	save(x.grid,y.grid,z.grid,file="persp.R")
}

library(MASS)
crabs.data <- predict(princomp(crabs[,-(1:3)]))
impurity.plot.2d(
	info=crabs.data[,2:3],
	classes=factor(rep(1:4,each=50)),
	grid.detail=100)

