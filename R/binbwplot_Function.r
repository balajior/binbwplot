#Function to plot bwplot having a histogram specifying the
#data distribution with a specific number of bins(specified by the user)

#Start of function
binbwplot <- function(	inData,
						col="grey",
						ylim=NULL,
						breaks=NULL,
						main="BinBwPlot",
						xlab="",
						ylab="Data_Range",
						notch=FALSE)
{
	#Load packages

  #if (!require("lattice")) {
  #  stop('The package lattice was not installed')
  #}
  #else{
  #  library(lattice)
  #}

  if (!requireNamespace("lattice", quietly = TRUE)) {
    stop('The package lattice was not installed')
  } else {
    library(lattice);
  }

	panel.hanoi <- function(x,  y,  horizontal,  breaks="Sturges",  ...){
	#"Sturges" is hist()'s default

		if  (horizontal)  {
		condvar<-  y 	# conditioning ("independent") variable
		datavar<-  x 	# data ("dependent") variable
		}
		else{
			condvar<-  x
			datavar<-  y
		}

		conds<-  sort(unique(condvar))

		# loop through the possible values of the conditioning variable
		for(i in  seq_along(conds)){

			# use base hist(ogram) function to extract some information
			h <- hist(datavar[condvar ==  conds[i]],  plot=F,  breaks)

			# strip outer counts == 0, and corresponding bins
			brks.cnts <- stripOuterZeros(h$breaks,  h$counts)
			brks <- brks.cnts[[1]]
			cnts <- brks.cnts[[2]]

			halfrelfs<-  (cnts/sum(cnts))/2   # i.e. half of the relative frequency
			center<-  i

			# All of the variables passed to panel.rec will usually be vectors, and
			# panel.rect will therefore make multiple rectangles.
			if(horizontal){
				panel.rect(head(brks,  -1),  center - halfrelfs,
							tail(brks,  -1),  center + halfrelfs,  ...)
			}
			else{
				panel.rect(center - halfrelfs,  head(brks,  -1),
							center +  halfrelfs, tail(brks,  -1),  ...)
			}
		}
	}

	#Function to strip counts that are all zero on ends of data,
	#along with the corresponding breaks
	stripOuterZeros <- function(brks,  cnts){
		do.call("stripLeftZeros", stripRightZeros(brks,  cnts))
	}
	stripLeftZeros <- function(brks,  cnts){
		if(cnts[1]  ==  0){
			stripLeftZeros(brks[-1],  cnts[-1])
		}
		else{
			list(brks,  cnts)
		}
	}
	stripRightZeros <- function(brks,  cnts){
		len<-  length(cnts)
		if(cnts[len]  == 0){
			stripRightZeros(brks[-(len+1)],  cnts[-len])
		}
		else{
			list(brks,  cnts)
		}
	}

	mergedOrder <- data.frame(inData)

	colnames(mergedOrder) <- c("MeanValue", "Annotation")
	print(summary(mergedOrder[2]))

	if (is.null(ylim)) {
        ylim <- c(0,1)
	}
	else{
		ylim <- ylim
	}


	if (is.null(breaks)) {
       	breaks <- 10
	}
	else{
		breaks <- breaks
	}


	#Plot binbwplot
	bwplot(	MeanValue ~ Annotation, data=mergedOrder, ylim=ylim,
			main=main, xlab=xlab, ylab=ylab, pch="|", coef=0,
			notch.frac = 0.5, notch=notch,
			panel=function(...){panel.hanoi(col=col, breaks=breaks, ...);
			panel.bwplot(...)})
}

##End of function
