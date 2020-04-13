# Functions required for simulations: 
#--------------------------------
#library(imager)

# plotimage plots a matrix as an image
plotimage=function(mat,lim=NULL,nc=1000,m=NULL,z=FALSE,add=FALSE,cols="rainbow",lc=c(0,0.7),p="l")
{ # mat : matrice a tracer
    
    if(!p%in%c("l","a","al","n","ld","ad","ald","nd")){p="n"}
    if(p=="l"){	axes=FALSE;leg=TRUE;	continuous=TRUE}
    if(p=="a"){	axes=TRUE;leg=FALSE;	continuous=TRUE}
    if(p=="al"){	axes=TRUE;leg=TRUE;	continuous=TRUE}
    if(p=="n"){	axes=FALSE;leg=FALSE	;continuous=TRUE}
    if(p=="ld"){	axes=FALSE;leg=TRUE;	continuous=FALSE}
    if(p=="ad"){	axes=TRUE;leg=FALSE;	continuous=FALSE}
    if(p=="ald"){	axes=TRUE;leg=TRUE;	continuous=FALSE}
    if(p=="nd"){	axes=FALSE;leg=FALSE	;continuous=FALSE}
    alpha.col=1
    #continuous=TRUE
    main=NULL
    imageToPlot=NULL
    nbPixX=dim(mat)[1]
    nbPixY=dim(mat)[2]
    if(is.null(lim)){lim=c(NULL,NULL)}
    min.scale=lim[1]
    max.scale=lim[2]
    start.col=lc[1]
    end.col=lc[2]
    if(is.null(m)){m=matrix(TRUE,dim(mat)[1],dim(mat)[2])}
    if(!z)
    {
        if(is.null(min.scale)){min.scale=min(mat,na.rm=T)}
        if(is.null(max.scale)){max.scale=max(mat,na.rm=T)}
        if(cols=="rainbow")
        {
            vecteurCouleur=rev(rainbow(nc,start=start.col,end=end.col,alpha=alpha.col))
        }
        if(cols=="heat.colors")
        {
            vecteurCouleur=heat.colors(nc, alpha = alpha.col)
        }
        if(cols=="terrain.colors")
        {
            vecteurCouleur=terrain.colors(nc, alpha = alpha.col)
        }
        if(cols=="topo.colors")
        {
            vecteurCouleur=topo.colors(nc, alpha = alpha.col)
        }
        if(cols=="cm.colors")
        {
            vecteurCouleur=cm.colors(nc, alpha = alpha.col)
        }
        if(cols=="gray.scale")
        {
            vecteurCouleur=gray.colors(nc, start = start.col, end = end.col, gamma = 2.2, alpha = alpha.col)
        }
        #if(col.rev){vecteurCouleur=rev(vecteurCouleur)}
        
        pas=(max.scale-min.scale)/nc
        
        if(!add)
        {
            if(leg)
            {
                matrice <- matrix(c(1,2,2,2,2,2),nrow=1,ncol=6,byrow=TRUE)
                layout(matrice)
                par(mar=c(4, 0, 5, 0))
                par(xaxt="n")
                par(yaxt="n")
                par(bty="n")
                
                plot(rep(1,nc),1:nc,col=vecteurCouleur,xlab="",ylab="",cex=4,pch=15)
                
                if(abs(nc*pas)<10){digits=1}else{if(abs(nc*pas)<1){digits=3}else{digits=0}}
                for(k in 0:(nc/100))
                {		
                    text(0.7,k*100,round(min.scale+100*k*pas,digits=digits))
                }
                
                
                
                #par(mar=c(3,2,0,2))
                
                par(xaxt="s")
                par(yaxt="s")
            }
            
            
            
            # if(!continuous)
            # {
            # plot(NULL,xlim=c(0,nbPixX),ylim=c(0,nbPixY),xlab="",ylab="",main=main)
            # }
            
        }
        matR=matB=matG=matrix(0,nbPixX,nbPixY)
        
        for(i in 1:nbPixX)
        {
            for(j in 1:nbPixY)
            {
                tim=mat[i,j]
                
                if(!is.na(tim)&(m[i,j]))
                {
                    if(tim<=min.scale){indicecol=1}
                    if(tim>=max.scale){indicecol=nc}
                    if(tim<max.scale&tim>min.scale){indicecol=round((tim-min.scale)/pas)}
                    if(indicecol>nc){indicecol=nc}
                    if(indicecol<1){indicecol=1}
                    if(!add)
                    {
                        matR[i,j]=col2rgb(vecteurCouleur[indicecol])["red",]
                        matG[i,j]=col2rgb(vecteurCouleur[indicecol])["green",]
                        matB[i,j]=col2rgb(vecteurCouleur[indicecol])["blue",]
                    }
                    else
                    {
                        points(i,j,col=vecteurCouleur[indicecol],pch=20,cex=0.5)
                    }
                    
                    
                }
                
            }
        }
        
        imageToPlot=as.cimg(c(as.vector(unlist(matR)),as.vector(unlist(matG)),as.vector(unlist(matB))),x=nbPixX,y=nbPixY,cc=3)
        if(!add)
        {
            par(xaxt="s")
            par(yaxt="s")
            
            par(mar=c(2,0,3,2))
            plot(imageToPlot,axes=axes,new=add,main=main,interpolate=continuous)
        }	
        
    }
    if(z)
    {
        zones=levels(as.factor(as.vector(mat)))
        couleurs=rainbow(length(zones))
        matR=matB=matG=matrix(0,nbPixX,nbPixY)
        matrice <- matrix(c(1,1,1,1,1,1),nrow=1,ncol=1,byrow=TRUE)
        layout(matrice)
        plot(NULL,xlim=c(0,nbPixX),ylim=c(0,nbPixY),xlab="",ylab="")
        for(i in 1:nbPixX)
        {
            for(j in 1:nbPixY)
            {
                tim=mat[i,j]	
                if(!is.na(tim)&m[i,j])
                {
                    
                    for(k in 1:length(zones))
                    {
                        
                        if(tim==zones[k])
                        {
                            
                            if(!add)
                            {
                                matR[i,j]=as.data.frame(col2rgb(couleurs[k]))["red",]
                                matG[i,j]=as.data.frame(col2rgb(couleurs[k]))["green",]
                                matB[i,j]=as.data.frame(col2rgb(couleurs[k]))["blue",]
                                imageToPlot=as.cimg(c(as.vector(unlist(matR)),as.vector(unlist(matG)),as.vector(unlist(matB))),x=nbPixX,y=nbPixY,cc=3)
                            }	
                            else
                            {
                                # if(!add){plot(NULL,xlim=c(0,nbPixX),ylim=c(0,nbPixY),xlab="",ylab="",main=main)}
                                points(mat[i],mat[j],col=couleurs[k],cex=0.6)
                                matR[i,j]=as.data.frame(col2rgb(couleurs[k]))["red",]
                                matG[i,j]=as.data.frame(col2rgb(couleurs[k]))["green",]
                                matB[i,j]=as.data.frame(col2rgb(couleurs[k]))["blue",]
                                imageToPlot=as.cimg(c(as.vector(unlist(matR)),as.vector(unlist(matG)),as.vector(unlist(matB))),x=nbPixX,y=nbPixY,cc=3)	
                            }
                        }
                    }	
                }
            }
        }
        
        if(!add)
        {
            par(mar=c(1,1,3,1))
            #par(mar=c(0,0,0,0))
            plot(imageToPlot,axes=axes,interpolate=continuous)
            #axis(1)
            #axis(2)
        }		
    }
    return(imageToPlot)
}
# PlotLine allow to add a line in an accumulator
plotLine=function(matRes,x0,y0,x1,y1)
{
    plotLineLow=function(matRes,x0,y0,x1,y1)
    {
        dx=x1-x0
        dy=y1-y0
        yi=1
        if(dy<0){yi=-1;dy=-dy}
        D=2*dy-dx
        y=y0
        for(x in x0:x1)
        {
            matRes[x,y]=1
            if(D>0){y=y+yi;D=D-2*dx}
            D=D+2*dy
        }	
        return(matRes)
    }
    plotLineHigh=function(matRes,x0,y0,x1,y1)
    {
        dx=x1-x0
        dy=y1-y0
        xi=1
        if(dx<0){xi=-1;dx=-dx}
        D=2*dx-dy
        x=x0
        for(y in y0:y1)
        {
            matRes[x,y]=1
            if(D>0){x=x+xi;D=D-2*dy}
            D=D+2*dx
        }	
        return(matRes)
    }
	if(abs(y1-y0)<abs(x1-x0))
	{	
		if(x0>x1){	res=plotLineLow(matRes,x1,y1,x0,y0)}
		if(x0<=x1){res=plotLineLow(matRes,x0,y0,x1,y1)}			
	}
	if(abs(y1-y0)>=abs(x1-x0))
	{	
		if(y0>y1){res=plotLineHigh(matRes,x1,y1,x0,y0)}
		if(y0<=y1){res=plotLineHigh(matRes,x0,y0,x1,y1)}			
	}

	return(res)
}
# genereSegment allows to add a randomly generated segment in an accumulator (using plotLine)
genereSegment=function(tailleSegment,tailleMatrice,tailleMatriceY=NULL,contrainteX=NULL,contrainteY=NULL)
{
	
	if(!is.null(tailleMatriceY)){tailleMatriceX=tailleMatrice;tailleMatriceY=tailleMatriceY}else{tailleMatriceX=tailleMatrice;tailleMatriceY=tailleMatrice}
	matSeg=matrix(0,tailleMatriceX+tailleSegment+2,tailleMatriceY+tailleSegment+2)
	if(is.null(contrainteX))
	{
		x0=round(tailleSegment/2+tailleMatriceX*runif(1))+1
		y0=round(tailleSegment/2+tailleMatriceY*runif(1))+1
	}
	if(!is.null(contrainteX))
	{
			randomI=sample(length(contrainteX),1)
			x0=contrainteX[randomI]
			y0=contrainteY[randomI]
	}
			
	angle=pi*runif(1)-pi/2
	xA=round(x0-floor(tailleSegment/2)*cos(angle))
	xB=round(x0+floor(tailleSegment/2)*cos(angle))
	yA=round(y0-floor(tailleSegment/2)*sin(angle))
	yB=round(y0+floor(tailleSegment/2)*sin(angle))
	mat=plotLine(matRes=matSeg,x0=xA,y0=yA,x1=xB,y1=yB)
	return(mat)
}
# Calculate the p-value for the test (given a maximum)
calculPvalueMax=function(maxObserve,nombreSegment,prob,nbRealisations)
{
	frep=pbinom(maxObserve-1,size=nombreSegment,prob=prob)^nbRealisations
	pval=1-frep
	return(pval)
}
# Calculate the threshold for the test (given a maximum)
calculValeurSeuil=function(nombreSegment,prob,nbRealisations)
{
	return(qbinom((1-0.05)^(1/nbRealisations),size=nombreSegment,prob=prob))
}
distMax=function(Nmax,probaSegment,nbRealisations,nombreSegment)
{
	proba=rep(NA,Nmax)
	for(i in 0:(Nmax-1))
	{
		proba[i+1]=pbinom(i,size=nombreSegment,prob=probaSegment)^nbRealisations-pbinom(i-1,size=nombreSegment,prob=probaSegment)^nbRealisations
	}
	return(proba)
}
		

# Getting an accumulator with various number of segments/sizes of segments
#---------------------------
tailleMatrice=256
nombreSegment=10000 # ou 10000
tailleSegment=100
res=matrix(0,tailleMatrice+tailleSegment+2,tailleMatrice+tailleSegment+2)
for(i in 1:nombreSegment)
{
	matriceSegment=genereSegment(tailleSegment=tailleSegment,tailleMatrice=tailleMatrice)	
	res=res+matriceSegment
}
debutI=tailleSegment/2+1
finI=floor(tailleSegment/2)+1+tailleMatrice
# Figure 3 (to uncomment)
#with imager library
#plotimage(res,p="ld");abline(v=debutI,col="red",lwd=2);	abline(v=finI,col="red",lwd=2);	abline(h=debutI,col="red",lwd=2);abline(h=finI,col="red",lwd=2)	
#abline(v=debut,col="white",lwd=2);abline(v=fin,col="white",lwd=2);abline(h=debut,col="white",lwd=2);abline(h=fin,col="white",lwd=2)
# without imager,use image

res1=res[debutI:finI,debutI:finI]
image(res1,col=topo.colors(6))
# Figure 3 (to uncomment)
#plotimage(res1,p="ld")
#abline(v=marge,col="white",lwd=2);abline(v=fin-marge,col="white",lwd=2);abline(h=marge,col="white",lwd=2);abline(h=fin-marge,col="white",lwd=2)

# Getting Figure 4
#--------------------
nEtude=30
h=hist(as.vector(unlist(res1)),xlim=c(0,nEtude),ylim=c(0,9000),col="light blue",xlab="Counts",ylab="Frequency",main="b. Distribution of accumulator (n=10000)",breaks=nombreSegment)
p=tailleSegment/(dim(res1)[1]^2)
tailleEst=sum(res1)/nombreSegment
pest=tailleEst/(tailleMatrice)^2
vect2=vect3=rep(NA,nEtude+1)
for(i in 0:(nEtude)){vect3[i+1]=dbinom(x=i,size=nombreSegment,prob=pest)*(dim(res1)[1])^2}
for(i in 0:(nEtude)){vect2[i+1]=dbinom(x=i,size=nombreSegment,prob=p)*(dim(res1)[1])^2}
lines(x=0:nEtude,y=vect2[1:(nEtude+1)],col="red",type="l")	  
lines(x=0:nEtude,y=vect3[1:(nEtude+1)],col="dark green",type="l",lwd=2)
	#for(i in 0:(nEtude)){vect2[i+1]=dbinom(x=i,size=nombreSegment,prob=p)*(dim(res2)[1])^2}

marge=floor(tailleSegment/2)
debut=tailleSegment/2+1+marge
fin=floor(tailleSegment/2)+1+tailleMatrice-marge
res2=res[debut:fin,debut:fin]	


# Getting Figure 5
#--------------------
h=hist(as.vector(unlist(res2)),xlim=c(0,nEtude),ylim=c(0,3000),col="light blue",xlab="Counts",ylab="Frequency",main="Distribution of accumulator",breaks=nombreSegment)
vect3=rep(NA,nEtude+1)
tailleEst=sum(res2)/nombreSegment
pest=tailleEst/(dim(res2)[1])^2
vect2=vect3=rep(NA,nEtude+1)
for(i in 0:(nEtude)){vect3[i+1]=dbinom(x=i,size=nombreSegment,prob=pest)*(dim(res2)[1])^2}
lines(x=0:nEtude,y=vect3[1:(nEtude+1)],col="red",type="l")
	
1.5*(quantile(as.vector(unlist(res2))[as.vector(unlist(res2))!=0],0.75)-quantile(as.vector(unlist(res2))[as.vector(unlist(res2))!=0],0.25))

calculValeurSeuil(nombreSegment=nombreSegment,prob=pest,nbRealisations=(dim(res1)[1])^2)
calculPvalueMax(maxObserve=max(unlist(res1)),nombreSegment=nombreSegment,prob=pest,nbRealisations=(dim(res1)[1])^2)
#  1  pour p
#	0.5970869  pour pest

#--------------------------
# Obtaining p-values (not in paper)
#---------------------------
	nbSimul=1000
	tailleMatrice=256
	tailleSegment=10 # en nombre de pixels
	#nombreSegment=tailleMatrice^2
	nombreSegment=100
	marge=floor(tailleSegment/2)
	debut=tailleSegment/2+1+marge
	fin=floor(tailleSegment/2)+1+tailleMatrice-marge
	debutI=tailleSegment/2+1
	finI=floor(tailleSegment/2)+1+tailleMatrice
	probaSegment2=rep(NA,nbSimul)
	probaMaxP1=rep(NA,nbSimul)
	probaMaxP2=rep(NA,nbSimul)
	probaMaxP3=rep(NA,nbSimul)
	maxObserve=rep(NA,nbSimul)
	maxFinal=rep(NA,nbSimul)
	# tailleSegment/(tailleMatrice^2)
# [1] 0.005


	for(k in 1:nbSimul)
	{
			print(k)
			res=matrix(0,tailleMatrice+tailleSegment+2,tailleMatrice+tailleSegment+2)
			for(i in 1:nombreSegment)
			{
				matriceSegment=genereSegment(tailleSegment=tailleSegment,tailleMatrice=tailleMatrice)	
				res=res+matriceSegment
			}
			# trace de l'image
			# plotimage(res,p="ld");abline(v=debutI,col="red",lwd=2);	abline(v=finI,col="red",lwd=2);	abline(h=debutI,col="red",lwd=2);abline(h=finI,col="red",lwd=2)	
			# abline(v=debut,col="white",lwd=2);abline(v=fin,col="white",lwd=2);abline(h=debut,col="white",lwd=2);abline(h=fin,col="white",lwd=2)
			# # donnee restreites à la marge
			resFinal=res[debut:fin,debut:fin]
			maxFinal[k]=max(as.vector(unlist(resFinal)))
			sommeRes=sum(as.vector(unlist(resFinal)))
			tailleObservee=sommeRes/nombreSegment
			probaSegment2[k]=tailleObservee/((dim(resFinal)[1])^2)
			probaMaxP1[k]=calculPvalueMax(maxObserve=maxFinal[k],nombreSegment=nombreSegment,prob=probaSegment2[k],nbRealisations=(dim(resFinal)[1])^2)
			nEtude=7
			vect2=rep(NA,nEtude)
			 # hist(as.vector(unlist(resFinal)),xlim=c(0,nEtude))
			 # for(i in 0:(nEtude)){vect2[i+1]=dbinom(x=i,size=nombreSegment,prob=probaSegment2[k])*(dim(resFinal))[1]^2}
			 # lines(x=0:nEtude,y=vect2[1:(nEtude+1)],col="green",type="l")	  
			 # proTh=distMax(Nmax=nEtude,probaSegment=probaSegment2[k],nbRealisations=(dim(resFinal))[1]^2,nombreSegment=nombreSegment)*(dim(resFinal))[1]^2
			 # lines(x=0:nEtude,y=proTh[1:(nEtude+1)],col="red",type="l")
			# la distribution est validee
	
			
			# donnees sur l'ensemble de la matrice
			resFinal2=res[debutI:finI,debutI:finI]
			maxObserve[k]=max(as.vector(unlist(resFinal2)))
			probaSegment=tailleSegment/(tailleMatrice^2)
			probaMaxP2[k]=calculPvalueMax(maxObserve=maxObserve[k],nombreSegment=nombreSegment,prob=probaSegment,nbRealisations=tailleMatrice^2)
			
			maxFinal2=max(as.vector(unlist(resFinal2)))
			sommeRes2=sum(as.vector(unlist(resFinal2)))
			tailleObservee2=sommeRes2/nombreSegment
			probaSeg=tailleObservee2/((dim(resFinal2)[1])^2)
			probaMaxP3[k]=calculPvalueMax(maxObserve=maxFinal2,nombreSegment=nombreSegment,prob=probaSeg,nbRealisations=tailleMatrice^2)
		
			 # hist(as.vector(unlist(resFinal2)),xlim=c(0,nEtude))
			 # for(i in 0:(nEtude)){vect2[i+1]=dbinom(x=i,size=nombreSegment,prob=probaSegment)*tailleMatrice^2}
			 # lines(x=0:nEtude,y=vect2[1:(nEtude+1)],col="green",type="l")
				# proTh=distMax(Nmax=nEtude,probaSegment=probaSegment,nbRealisations=tailleMatrice^2,nombreSegment=nombreSegment)*tailleMatrice^2
			  # lines(x=0:nEtude,y=proTh[1:(nEtude+1)],col="red",type="l")
		}
		
		
	probRestreinte=100*sum(probaMaxP1<0.05)/length(probaMaxP1)
	#prob=100*sum(probaMaxP2<0.05)/length(probaMaxP2)
	probTotale=100*sum(probaMaxP3<0.05)/length(probaMaxP3)

# }

# Getting the simulation results (to obtain the results in the table, change the parameters)
#-------------------------
	tailleMatrice=256
	nombreSegment=10000
	tailleSegment=25
	nbIter=100
	e0=rep(NA,nbIter)
	e1=rep(NA,nbIter)
	e2=rep(NA,nbIter)
	t1=rep(NA,nbIter)
	t2=rep(NA,nbIter)
	t3=rep(NA,nbIter)
	m=rep(NA,nbIter)
	for(k in 1:nbIter)
	{ print(k)
		res=matrix(0,tailleMatrice+tailleSegment+2,tailleMatrice+tailleSegment+2)
		for(i in 1:nombreSegment)
		{
			matriceSegment=genereSegment(tailleSegment=tailleSegment,tailleMatrice=tailleMatrice)	
			res=res+matriceSegment
		}
		debutI=tailleSegment/2+1
		finI=floor(tailleSegment/2)+1+tailleMatrice
		res1=res[debutI:finI,debutI:finI]
		p=tailleSegment/(dim(res1)[1]^2)
		
		tailleEst=sum(res1)/nombreSegment
		p1=tailleEst/(tailleMatrice)^2

		marge=floor(tailleSegment/2)
		debut=tailleSegment/2+1+marge
		fin=floor(tailleSegment/2)+1+tailleMatrice-marge
		res2=res[debut:fin,debut:fin]	

		tailleEst2=sum(res2)/nombreSegment
		p2=tailleEst2/(dim(res2)[1])^2
		m[k]=max(as.vector(unlist(res1)))
		e0[k]=calculValeurSeuil(nombreSegment=nombreSegment,prob=p,nbRealisations=(dim(res1)[1])^2)
		e1[k]=calculValeurSeuil(nombreSegment=nombreSegment,prob=p1,nbRealisations=(dim(res1)[1])^2)
		e2[k]=calculValeurSeuil(nombreSegment=nombreSegment,prob=p2,nbRealisations=(dim(res2)[1])^2)
		t1[k]=quantile(as.vector(unlist(res1)),0.75)+1.5*(quantile(as.vector(unlist(res1))[as.vector(unlist(res1))],0.75)-quantile(as.vector(unlist(res1))[as.vector(unlist(res1))],0.25))	
		t2[k]=quantile(as.vector(unlist(res2)),0.75)+1.5*(quantile(as.vector(unlist(res2))[as.vector(unlist(res2))],0.75)-quantile(as.vector(unlist(res2))[as.vector(unlist(res2))],0.25))
		#vec=as.vector(unlist(res1))[as.vector(unlist(res1))!=0]
		indmax=which(res1==max(res1),arr.ind=TRUE)
		listIndicesI=(indmax[1]-round(tailleSegment/2)):(indmax[1]+round(tailleSegment/2))
		listIndicesJ=(indmax[2]-round(tailleSegment/2)):(indmax[2]+round(tailleSegment/2))
		listeMatriceTotale=matrix(FALSE,dim(res1)[1],dim(res1)[2])
		for(i in 1:length(listIndicesI))
		{
			for(j in 1:length(listIndicesJ))
			{
				if((listIndicesI[i]-indmax[1])^2+(listIndicesJ[j]-indmax[2])^2<(tailleSegment/2)^2)
				{
			#	print(listIndicesI[i])
			#	print(listIndicesJ[j])
				
					if(listIndicesI[i]<256&listIndicesJ[j]<256)
					{ 
						listeMatriceTotale[listIndicesI[i],listIndicesJ[j]]=TRUE
					}
				
				}
			}
		}
		resCercle=res1[listeMatriceTotale]
		vec=sample(resCercle,max(res1))
		t3[k]=quantile(vec,0.75)+1.5*(quantile(vec,0.75)-quantile(vec,0.25))
		}
	summary(e0)
	summary(e1)
	summary(e2)
	summary(t1)
	summary(t2)
	summary(t3)
	summary(m)