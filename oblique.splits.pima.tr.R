#train trees
a <- oblique.tree(type~.,Pima.tr,oblique.splits="off")									#fine
aa <- oblique.tree(type~.,Pima.tr,oblique.splits="on")									#fine
aaa <- oblique.tree(type~.,Pima.tr,oblique.splits="only")								#fine

#plot settings
CEX=1

##############################################################################################oblique splits off
	#oblique.splits.off.crabs.tree.ps
	b <- a
	b$frame$var[b$frame$var!="<leaf>"]=""
	b$frame$splits[,1] <- character(dim(b$frame)[1])
	plot(a);text(b,cex=CEX);title(main="Axis-parallel splits only",cex=CEX);
x11();
	plot(a);text(a,cex=CEX);title(main="Axis-parallel splits only",cex=CEX);
x11();

##############################################################################################oblique splits on
	#oblique.splits.on.crabs.tree.ps
	bb <- aa
	bb$frame$var[bb$frame$var!="<leaf>"]=""
	bb$frame$splits[,1] <- character(dim(bb$frame)[1])
	plot(aa);text(bb,cex=CEX);title(main="Both axis-parallel and oblique splits",cex=CEX);
x11();
	plot(aa);text(aa,cex=CEX);title(main="Both axis-parallel and oblique splits",cex=CEX);
x11();

##############################################################################################oblique splits only
	#oblique.splits.only.crabs.tree.ps
	bbb <- aaa
	bbb$frame$var[bbb$frame$var!="<leaf>"]=""
	bbb$frame$splits[,1] <- character(dim(bbb$frame)[1])
	plot(aaa);text(bbb,cex=CEX);title(main="Oblique splits only",cex=CEX);
x11();
	plot(aaa);text(aaa,cex=CEX);title(main="Oblique splits only",cex=CEX);
x11();

