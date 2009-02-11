#CLEAN START
	rm(list=ls())
	library(MASS)
	library(tree)

#GROWS AXIS PARALLEL
	#get data
	dat <- read.csv("fig_rooted_subtrees.csv")

	#grow tree
	dat.tree.named.nodes <- tree(	formula = C~.,
				data = dat)


	#rename tree splits to make it easy to read
	dat.tree.named.nodes <- dat.tree.named.nodes
	dat.tree.named.nodes$frame$splits[,1] <- row.names(dat.tree.named.nodes$frame)	#rename split name
	levels(dat.tree.named.nodes$frame$var) <- c("<leaf>","Node ","Node ","Node ")	#rename split pre-fix
	dat.tree.named.nodes$frame$yval <- rep("",times = 15)				#remove leaf predictions

	plot(dat.tree.named.nodes,type="uniform")
	text(dat.tree.named.nodes)


#function to snip tree nodes and to plot the tree
	snip.n.plot <- function(
		tree,
		nodes,
		xy = c(1,1))
	{
		#snip
		snipped.tree <- snip.tree(	tree = tree,
						nodes = nodes)

		#plot
		par(mfg = xy)
		#par(cex=1.5)
		par(mai=c(1,0.5,1,0.5))
		plot(snipped.tree,type="uniform")
		text(snipped.tree,cex=2.5)
		
	}

	#test out snip.n.plot()
	#	x11();snip.n.plot(tree = dat.tree.named.nodes,nodes = 1)

postscript(file="fig_tree_pruning.ps",width=20,height=5,horizontal=FALSE,paper="special")
	#snip tree out all possible trees!
	#x11()
	par(mfrow=c(1,4))

	######################################
	par(mfg = c(1,1))
	par(mai=c(1,0.5,1,0.5))

	#rename tree splits to make it easy to read
	plot(dat.tree.named.nodes,type="uniform")
	text(dat.tree.named.nodes,font=1,cex=2.5)						#set labels to bold
	par(font.main=1)
	par(xpd=NA)
	par(font.sub=1)
	title(	main=	expression(	paste(	T[0],
						" = Maximal Tree"
					)
			),
		cex.main=3,
		sub=	expression(	paste(	T[0],
						" optimal in ",
						k %in% group("[",list(k[0], k[1]),")"),
						"=",
						group("[",list(-infinity, k[1]),")")  		
					)
			),
		cex.sub=3
	)

	######################################
	snip <- snip.n.plot(	tree = dat.tree.named.nodes,
				nodes = c(4,3),
				xy = c(1,2))
	title(	main=	expression(	paste(	T[1],
						" = Prune Nodes 4 and 3 from ",
						T[0]
					)
			),
		cex.main=3,
		sub=	expression(	paste(	T[1],
						" optimal in ",
						k %in% group("[",list(k[1], k[2]),")"),				
					)
			),
		cex.sub=3
	)

	######################################
	snip <- snip.n.plot(	tree = dat.tree.named.nodes,
				nodes = c(3,4,2),
				xy = c(1,3))
	title(	main=	expression(	paste(	T[2],
						" = Prune Node 2 from ",
						T[1]
					)
			),
		cex.main=3,
		sub=	expression(	paste(	T[2],
						" optimal in ",
						k %in% group("[",list(k[2], k[3]),")"),				
					)
			),
		cex.sub=3
	)

	######################################
	par(mfg = c(1,4))
	par(mai=c(1,0.5,1,0.5))

	snipped.tree <- snip.tree(	tree = dat.tree.named.nodes,
					nodes = 1)
	class(snipped.tree) <- "tree"
	plot(snipped.tree,type="uniform")
	title(	main=	expression(	paste(	T[3],
						" = Prune Node 1 from ",
						T[2]
					)
			),
		cex.main=3,
		sub=	expression(	paste(	T[4],
						" optimal in ",
						k %in% group("[",list(k[3], k[4]),")"),				
						"=",
						group("[",list(k[3],infinity),")")  		
					)

			),
		cex.sub=3
	)

dev.off()


