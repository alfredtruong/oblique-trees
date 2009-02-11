rm(list=ls())
library(MASS)
library(tree)
library(nnet)
	crabs.pca=princomp(crabs[,4:8])
	crabs.pca.predict=predict(crabs.pca)
	pairs(crabs.pca.predict,col=rep(1:4,each=50))
	crabs.data=crabs.pca.predict[,2:3]
	windows(); plot(crabs.data,type="n");text(crabs.data,col=rep(1:4,each=50),labels=rep(1:4,each=50),cex=0.5)
	crabs.data=data.frame(crabs.data,factor(rep(1:4,each=50)))
	names(crabs.data)=c("X1","X2","C")

#CRABS AXIS PARALLEL
	#grow the tree
	crabs.ax.tree <- tree(C~.,crabs.data)

	#prune the tree to a nice size
	K <- prune.tree(crabs.ax.tree)$k

	#plot tree pruned to K[i]
	postscript(file="example_of_a_tree_plot_tree.ps",width=4,height=4.5,horizontal=FALSE,paper="special")
		par(mar=c(1,1,1,1))
		crabs.ax.tree.pruned <- prune.tree(crabs.ax.tree,k=K[3])
		plot(crabs.ax.tree.pruned)
		text(crabs.ax.tree.pruned)
	dev.off()


	#plot the decision boundaries
	postscript(file="example_of_a_tree_decision_boundaries.ps",width=4,height=4.5,horizontal=FALSE,paper="special")
		par(mar=c(5.1,4.1,4.1,2.1))
		plot(crabs.data[,1:2],type="n")
		text(crabs.data[,1:2],labels=crabs.data[,3],col=as.numeric(crabs.data[,3]),cex=.5)
		#title(main="Feature Space")
		#crabs.t=tree(g~.,crabs.data)
		lines(x=c(-.370909,-.370909),y=c(-10,10))
		lines(x=c(-10,-.370909),y=c(-.495977,-.495977))
		#lines(x=c(-1.13092,-1.13092),y=c(-10,-.495977))
		lines(x=c(-.370909,10),y=c(.451786,.451786))
		lines(x=c(.4492,.4492),y=c(.451786,-10))
		#lines(x=c(.149344,.149344),y=c(.451786,-10))
	dev.off()
