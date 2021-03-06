#original dataset
crabs.data <- cbind(factor(rep(1:4,each=50)),crabs[,4:8])
names(crabs.data)[1] <- "g"
a <- oblique.tree(g~.,crabs.data,oblique.splits="off")
aa <- oblique.tree(g~.,crabs.data,oblique.splits="on")
aaa <- oblique.tree(g~.,crabs.data,oblique.splits="only")
plot(a);text(a)
x11();
plot(aa);text(aa)
x11();
plot(aaa);text(aaa)


#make augmented dataset
crabs.pca=princomp(crabs[,4:8])
crabs.pca.predict=predict(crabs.pca)
pairs(crabs.pca.predict,col=rep(1:4,each=50))
crabs.data=crabs.pca.predict[,2:3]
#windows(); plot(crabs.data,type="n");text(crabs.data,col=rep(1:4,each=50),labels=rep(1:4,each=50),cex=0.5)
crabs.data=data.frame(crabs.data,factor(rep(1:4,each=50)))
names(crabs.data)[3]="g"
rm(crabs.pca,crabs.pca.predict)

#train trees
a <- oblique.tree(g~.,crabs.data,oblique.splits="off")
aa <- oblique.tree(g~.,crabs.data,oblique.splits="on")
aaa <- oblique.tree(g~.,crabs.data,oblique.splits="only")

#plot settings
CEX=1

##############################################################################################oblique splits off
	#oblique.splits.off.crabs.tree.ps
	b <- a
	b$frame$var[b$frame$var!="<leaf>"]=""
	b$frame$splits[,1] <- character(dim(b$frame)[1])
	plot(a);text(b,cex=CEX);title(main="Axis-parallel splits only",cex=CEX);
	plot(a);text(a,cex=CEX);title(main="Axis-parallel splits only",cex=CEX);

	#oblique.splits.off.crabs.decision.boundaries.ps
	eqscplot(	x=crabs.data[,1],
			y=crabs.data[,2],
			col=as.numeric(crabs.data$g),
			xlab="Comp.2",
			ylab="Comp.3")
	title(main="Associated decision boundaries",cex=CEX);
	lines(x=c(-.370909,-.370909),y=c(-10,10))
		lines(x=c(-10,-.370909),y=c(-.495977,-.495977))				#L#
			lines(x=c(-1.13092,-1.13092),y=c(-10,-.495977))			#LL#
		lines(x=c(-.370909,10),y=c(.451786,.451786))				#R#
			lines(x=c(.4492,.4492),y=c(.451786,-10))			#RL#
				lines(x=c(.149344,.149344),y=c(.451786,-10))		#RLL#
					lines(x=c(.336782,.336782),y=c(.451786,-10))	#RLLR#
			lines(x=c(-.370909,10),y=c(.80047,.80047))			#RR#

##############################################################################################oblique splits on
	#oblique.splits.on.crabs.tree.ps
	bb <- aa
	bb$frame$var[bb$frame$var!="<leaf>"]=""
	bb$frame$splits[,1] <- character(dim(bb$frame)[1])
	plot(aa);text(bb,cex=CEX);title(main="Both axis-parallel and oblique splits",cex=CEX);
	plot(aa);text(aa,cex=CEX);title(main="Both axis-parallel and oblique splits",cex=CEX);

	#oblique.splits.on.crabs.decision.boundaries.ps
	eqscplot(	x=crabs.data[,1],
			y=crabs.data[,2],
			col=as.numeric(crabs.data$g),
			xlab="Comp.2",
			ylab="Comp.3")
	title(main="Associated decision boundaries",cex=CEX);
		#root#
		line1=aaa$details[[1]]
		abline(-line1[1:2]/line1[3])

			#L#	solve AX=B with two lines, line1 and line2 using 0=a_i+b_i Comp.2 + c_i Comp.3
			line2=c(-aa$details[[2]],1,0)
			B=matrix(-c(line1[1],line2[1]),nrow=2)
			A=matrix(c(line1[-1],line2[-1]),byrow=TRUE,nrow=2)
			X=solve(A)%*%B
			#X contains coordinates of point lying on both line1 and line2
			lines(x=c(X[1],X[1]),y=c(X[2],10))

				#LR#	solve AX=B with two lines, line1 and line4 using 0=a_i+b_i Comp.2 + c_i Comp.3
				line4=c(-aa$details[[4]],0,1)
				B=matrix(-c(line1[1],line4[1]),nrow=2)
				A=matrix(c(line1[-1],line4[-1]),byrow=TRUE,nrow=2)
				X=solve(A)%*%B
				#X contains coordinates of point lying on both line1 and line4
				lines(x=c(-line2[1],X[1]),y=c(X[2],X[2]))

			#R#	solve AX=B with two lines, line1 and line7 using 0=a_i+b_i Comp.2 + c_i Comp.3
			line7=c(-aa$details[[7]],1,0)
			B=matrix(-c(line1[1],line7[1]),nrow=2)
			A=matrix(c(line1[-1],line7[-1]),byrow=TRUE,nrow=2)
			X=solve(A)%*%B
			#X contains coordinates of point lying on both line1 and line7
			lines(x=c(X[1],X[1]),y=c(X[2],-10))

				#RL#	solve AX=B with two lines, line1 and line8 using 0=a_i+b_i Comp.2 + c_i Comp.3
				line8=c(-aa$details[[8]],1,0)
				B=matrix(-c(line1[1],line8[1]),nrow=2)
				A=matrix(c(line1[-1],line8[-1]),byrow=TRUE,nrow=2)
				X=solve(A)%*%B
				#X contains coordinates of point lying on both line1 and line8
				lines(x=c(X[1],X[1]),y=c(X[2],-10))

					#RLR#	solve AX=B with two lines, line1 and line10 using 0=a_i+b_i Comp.2 + c_i Comp.3
					line10=c(-aa$details[[10]],1,0)
					B=matrix(-c(line1[1],line10[1]),nrow=2)
					A=matrix(c(line1[-1],line10[-1]),byrow=TRUE,nrow=2)
					X=solve(A)%*%B
					#X contains coordinates of point lying on both line1 and line10
					lines(x=c(X[1],X[1]),y=c(X[2],-10))

				#RR#
				lines(x=c(.4492,10),y=c(.023281,.023281))

##############################################################################################oblique splits only
	#oblique.splits.only.crabs.tree.ps
	bbb <- aaa
	bbb$frame$var[bbb$frame$var!="<leaf>"]=""
	bbb$frame$splits[,1] <- character(dim(bbb$frame)[1])
	plot(aaa);text(bbb,cex=CEX);title(main="Oblique splits only",cex=CEX);
	plot(aaa);text(aaa,cex=CEX);title(main="Oblique splits only",cex=CEX);

	#oblique.splits.only.crabs.decision.boundaries.ps
	eqscplot(	x=crabs.data[,1],
			y=crabs.data[,2],
			col=as.numeric(crabs.data$g),
			xlab="Comp.2",
			ylab="Comp.3")
	title(main="Associated decision boundaries",cex=CEX);
		#root#
		line1=aaa$details[[1]]
		abline(-line1[1:2]/line1[3])

			#L#	solve AX=B with two lines, line1 and line2 using 0=a_i+b_i Comp.2 + c_i Comp.3
			line2=aaa$details[[2]]
			B=matrix(-c(line1[1],line2[1]),nrow=2)
			A=matrix(c(line1[-1],line2[-1]),byrow=TRUE,nrow=2)
			X=solve(A)%*%B
			#X contains coordinates of point lying on both line1 and line2
			lines(x=c(X[1],-10),y=c(X[2],(-line2[1]-line2[2]*-10)/line2[3]))

			#L#	solve AX=B with two lines, line1 and line3 using 0=a_i+b_i Comp.2 + c_i Comp.3
			line3=aaa$details[[5]]
			B=matrix(-c(line1[1],line3[1]),nrow=2)
			A=matrix(c(line1[-1],line3[-1]),byrow=TRUE,nrow=2)
			X=solve(A)%*%B
			#X contains coordinates of point lying on both line1 and line3
			lines(x=c(X[1],-10),y=c(X[2],(-line3[1]-line3[2]*-10)/line3[3]))








