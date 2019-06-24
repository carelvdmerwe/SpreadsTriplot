bipldrawknn <- function(datatrain, datatest, alphin = 0.6, type= "CVA",kin = 7,plotprop = c(T,T,F,T,F,F,T), expin = 1.25,
                        ax.coltri= rep("grey", dim(datatrain)[2]-1), ax.name.coltri = rep("black", dim(datatrain)[2]-1),
                        tickcol = rep("black", dim(datatrain)[2]-1), ticktextcol = rep("black", dim(datatrain)[2]-1),nint=10,
                        pred.print = TRUE,axin=1:(dim(datatrain)[2]-1),hullmultin = c(1.5,0.95) ,  evin = c(1,2),
                        varnamesin = NULL,classnames = NULL,onlylines = FALSE,pchsize = 1,innerline = c(1,2) ,
                        legcex = 0.85, outerline =c(1,3) ,nbpin = 200,...){
  par(xpd=FALSE)
  #use the indicators below to recreate the plots in the paper
  plotouter <- plotprop[1]
  plotinner <-plotprop[2]
  plotbags<-plotprop[3]
  plotcont <- plotprop[4]
  nolines <-plotprop[5] #make sure everyting else is true if this is to be True
  nospots <- plotprop[6]
  nospotstest <- plotprop[7]
  numclasses <- length(unique(datatrain[,1]))
  ev <- evin
  alphsim <- alphin
  hullmult <- hullmultin 
  fillcols <- gray.colors(numclasses, start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL)
  fillcolslines <- rep("black",numclasses)
  pchnofill <- c(0,1,2,5)
  pchsamefill <- c(15,16,17,18)
  pchdifffill <- c(22,21,24,23)
  datatrain.mean <-  apply(datatrain[,-1], 2, function(x) tapply(x, datatrain[,1], mean))
  if(type=="PCA")
  {
    bipl <- PCAbipl_C(datatrain[,-1], scaled.mat = T,  
                      G = indmat(datatrain[, 1]), e.vects = ev, means.plot = F, colours = fillcols, 
                      pch.samples = 15:18, pch.samples.size = 0, n.int=rep(nint,dim(datatrain[,-1])[2]),
                      label = FALSE,  exp.factor = expin,  
                      predictivity.print = TRUE, ax.col = list(ax.col = rep("white",dim(datatrain)[2]-1), 
                      tickmarker.col = rep("white", dim(datatrain)[2]-1), marker.col = rep("white",dim(datatrain)[2]-1)), 
                      ax.name.col = rep("white", dim(datatrain)[2]-1),ax = 1,... )
    xMeans <- apply(datatrain[,-1],2,mean)
    xStdevs <- sqrt(apply(datatrain[,-1],2,var))
    Zbase <- bipl$Z[,1:2]
    Zstar <- scale(datatest[,-1],xMeans,xStdevs)%*%bipl$V[,ev]
    Zstar <- as.data.frame(Zstar)
    colnames(Zstar) <- c("X1","X2")
    Zconstruct <- Zbase
    Zaxesout <- bipl$Z.axes
    axpredout <- bipl$predictivity
  }
  if(type=="CVA")
  {
    bipl <- CVAbipl_C(datatrain[,-1], G =indmat(datatrain[, 1]), weightedCVA = "weighted",n.int=nint, 
                      means.plot = F, colours = fillcols, e.vects = ev,exp.factor = expin,
                      colours.means = fillcols ,pch.samples = 15:18, pch.samples.size =0, label = FALSE,
                      legend.type = c(F, F, F), Tukey.median = FALSE, 
                      ax.col = list(ax.col = rep("white",dim(datatrain)[2]-1), 
                      tickmarker.col = rep("white", dim(datatrain)[2]-1), marker.col = rep("white",dim(datatrain)[2]-1)), 
                      ax.name.col = rep("white", dim(datatrain)[2]-1),ax=1,...) 
    Zbase <- scale(datatrain[,-1], bipl$X.means, scale = FALSE) %*% bipl$Br %*% bipl$rotate.mat %*% bipl$reflect.mat
    Zbase <- as.data.frame(Zbase)
    colnames(Zbase) <- c("X1","X2")
    xMeans <- NA
    xStdevs <- NA
    Zstar <- scale(datatest[,-1], bipl$X.means, scale = FALSE) %*% bipl$Br %*% bipl$rotate.mat %*% bipl$reflect.mat
    Zstar <- as.data.frame(Zstar)
    colnames(Zstar) <- c("X1","X2")
    Zconstruct <- Zbase
    Zaxesout <- bipl$Z.axes
    axpredout <- bipl$axpred
  }
  if(type=="AOD")
  {
    bipl <- PCAbipl_C(datatrain.mean, scaled.mat = T,  
                      G = indmat(as.numeric(rownames(datatrain.mean))), e.vects = ev, means.plot = F, colours = fillcols, 
                      pch.samples = 15:18, pch.samples.size = 0, 
                      label = FALSE, exp.factor = expin,
                      ax.col = list(ax.col = rep("white",dim(datatrain)[2]-1), 
                      tickmarker.col = rep("white", dim(datatrain)[2]-1), marker.col = rep("white",dim(datatrain)[2]-1)), 
                      ax.name.col = rep("white", dim(datatrain)[2]-1),ax = 1, 
                      predictivity.print = TRUE, ... )
    xMeans <- apply(datatrain.mean,2,mean)
    xStdevs <- sqrt(apply(datatrain.mean,2,var))
    Zbase <- scale(datatrain[,-1],xMeans,xStdevs)%*%bipl$V[,ev]
    Zbase <- as.data.frame(Zbase)
    colnames(Zbase) <- c("X1","X2")
    Zstar <- scale(datatest[,-1],xMeans,xStdevs)%*%bipl$V[,ev]
    colnames(Zstar) <- c("X1","X2")
    Zconstruct <- bipl$Z[,1:2]
    Zaxesout <- bipl$Z.axes
    axpredout <- bipl$predictivity
  }
  nbp <- nbpin
  Rangex1 <- c(min(Zconstruct[,1])-abs(min(Zconstruct[,1]))*(expin-1),max(Zconstruct[,1])+abs(max(Zconstruct[,1]))*(expin-1))
  Rangex2 <- c(min(Zconstruct[,2])-abs(min(Zconstruct[,2]))*(expin-1),max(Zconstruct[,2])+abs(max(Zconstruct[,2]))*(expin-1))
  maxrange <- max(Rangex1[2]-Rangex1[1],Rangex2[2] - Rangex2[1])
  if(Rangex1[2]-Rangex1[1]<maxrange) Rangex1 <- c((Rangex1[2]+Rangex1[1])/2-maxrange/2,(Rangex1[2]+Rangex1[1])/2+maxrange/2)
  if(Rangex2[2]-Rangex2[1]<maxrange) Rangex2 <- c((Rangex2[2]+Rangex2[1])/2-maxrange/2,(Rangex2[2]+Rangex2[1])/2+maxrange/2)
  
  PredA <- seq(Rangex1[1],Rangex1[2], length = nbp)
  PredB <- seq(Rangex2[1],Rangex2[2], length = nbp)
  Grid <- expand.grid(X1 = PredA, X2 = PredB)
  
  traindataIND <- datatrain[,1]
  traindataIND[traindataIND==1] <- "p1"
  traindataIND[traindataIND==2] <- "p2"
  traindataIND[traindataIND==3] <- "p3"
  traindataIND[traindataIND==4] <- "p4"
  traindataIND<-cbind(as.data.frame(Zbase[,1:2]),classes=traindataIND)
  
  traindataINDColor <- fillcols
  Model <- train(as.formula("classes ~ ."), data = traindataIND, method = "knn", tuneGrid = data.frame(k = c(kin)))
  Pred <- predict(Model, newdata = Grid)
  if(alphsim!=0)
  {
    if(numclasses == 2) {bagcords_Wnew <-list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,], factor = 1,alph=alphsim)$hull.bag)}
    if(numclasses == 3) {bagcords_Wnew <-list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = 1,alph=alphsim)$hull.bag)}
    if(numclasses == 4) {bagcords_Wnew <-list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,], factor = 1,alph=alphsim)$hull.bag, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = 1,alph=alphsim)$hull.bag,compute.bagplot_C(Zbase[,1:2][datatrain[,1]==4,], factor = 1,alph=alphsim)$hull.bag)}
  }
  if(alphsim==0)
  {
    if(numclasses == 2) {bagcords_Wnew <- list(matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2))}
    if(numclasses == 3) {bagcords_Wnew <- list(matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2))}
    if(numclasses == 4) {bagcords_Wnew <- list(matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2), matrix(c(NA,NA),ncol=2),matrix(c(NA,NA),ncol=2))}
  }
  
  
  if(numclasses == 2) {bagcords_Hull <- list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,],  factor = hullmult[1],alph=hullmult[2])$hull.fullloop)}
  if(numclasses == 3) {bagcords_Hull <- list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,],  factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop)}
  if(numclasses == 4) {bagcords_Hull <- list(compute.bagplot_C(Zbase[,1:2][datatrain[,1]==1,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==2,],  factor = hullmult[1],alph=hullmult[2])$hull.fullloop, compute.bagplot_C(Zbase[,1:2][datatrain[,1]==3,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop,compute.bagplot_C(Zbase[,1:2][datatrain[,1]==4,], factor = hullmult[1],alph=hullmult[2])$hull.fullloop)}
  
  polyorange <- clipcords(numclasses,bagcords_Wnew)
  polygrey <- unioncords(numclasses,bagcords_Hull)
  predGRID <- as.vector(Pred)
  if((length(polyorange)!=0)&(plotinner==T))
  {
    for (h in 1:length(polyorange))
    {
      amberpoints <- pnt.in.poly(Grid,cbind(polyorange[[h]]$x,polyorange[[h]]$y))[,3]
      predGRID[amberpoints==1] <- "orange"
    }
  }
  if(plotouter==T)
  {
    greytemp <- predGRID
    if(length(polygrey)!=0)
    {
      for (h in 1:length(polygrey))
      {
        greypoints <- pnt.in.poly(Grid,cbind(polygrey[[h]]$x,polygrey[[h]]$y))[,3]
        greytemp[greypoints==1] <- "grey"
      }
    }  
    predGRID[greytemp!="grey"] <- "grey" 
  }
  
  Pred15 <- rep(0, length(Pred))
  Pred15[predGRID=="p1"] <- 1
  Pred15[predGRID=="p2"] <- 2
  Pred15[predGRID=="p3"] <- 3
  Pred15[predGRID=="p4"] <- 4
  Pred15[predGRID=="grey"] <- -1
  Pred15[predGRID=="orange"] <- 0
  Pred15 <- matrix(Pred15, length(PredA), length(PredB))
  rownames(Pred15) <- PredA
  colnames(Pred15) <- PredB
  
  if(plotcont==T)
  {
    .filled.contour(PredA, PredB, Pred15, levels=c(-1,0,1:numclasses,numclasses+1,numclasses+2),col=c("white","white",fillcols,"grey","grey"))  
    contour(PredA, PredB, Pred15, levels=c(-1,0,1:numclasses,numclasses+1), labels="", xlab="", ylab="", main=
              "", axes=FALSE,add=TRUE,col="white")
  }

  if(plotcont==T)  
  {
    r.names <- NULL
    usr <- par("usr")
    for (i in 1:dim(datatrain.mean)[[2]]) 
    {
      if(sum(i==axin)==1)
      {
      marker.mat <- bipl$Z.axes[[i]][bipl$Z.axes[[i]][, 4] == 1,1:3] ###
      x.vals <- marker.mat[, 1]
      y.vals <- marker.mat[, 2]
      if (is.null(dimnames(datatrain.mean)[[2]][i])) 
      {
        axis.name <- paste("v", i, sep = "")
      }
      else 
      {
        axis.name <- dimnames(datatrain.mean)[[2]][i]
      }
      r.names[i] <- axis.name
      std.markers <- zapsmall(marker.mat[, 3])
      x.invals <- x.vals[x.vals < usr[2] & x.vals > usr[1] & 
                           y.vals < usr[4] & y.vals > usr[3]]
      if (length(x.invals) < 2) 
      {
        warning(paste("Less than 2 markers on axis ", 
                      i, ". Increase n.int."))
        marker.mat <- bipl$Z.axes[[i]][, 1:3]
        x.vals <- marker.mat[, 1]
        y.vals <- marker.mat[, 2]
        std.markers <- zapsmall(marker.mat[, 3])
        x.invals <- x.vals[x.vals < usr[2] & x.vals > 
                             usr[1] & y.vals < usr[4] & y.vals > usr[3]]
        y.invals <- y.vals[x.vals < usr[2] & x.vals > 
                             usr[1] & y.vals < usr[4] & y.vals > usr[3]]
        tick.labels <- std.markers[x.vals < usr[2] & 
                                     x.vals > usr[1] & y.vals < usr[4] & y.vals > 
                                     usr[3]]
        Draw.line2(x.vals = x.invals, y.vals = y.invals, 
                   marker.vals = tick.labels, line.name = axis.name, 
                   ax.name.size = 0.75, axis.col = ax.coltri[i], 
                   ax.name.col = ax.name.coltri[i], offset = rep(0, length(datatrain.mean)))#, 
                   #pos = pos)
        next
      }
      y.invals <- y.vals[x.vals < usr[2] & x.vals > usr[1] & 
                           y.vals < usr[4] & y.vals > usr[3]]
      tick.labels <- std.markers[x.vals < usr[2] & x.vals > 
                                   usr[1] & y.vals < usr[4] & y.vals > usr[3]]
      uit <- Draw.line2(x.vals = x.invals, y.vals = y.invals, 
                        marker.vals = tick.labels, line.name = axis.name, 
                        ax.name.size = 0.75, axis.col = ax.coltri[i], 
                        ax.name.col =  ax.name.coltri[i], offset = rep(0, length(datatrain.mean)))
      
      gradient <- uit$gradient
      if (onlylines == FALSE){
      for (j in 1:length(x.invals)) Draw.onecmline(x = x.invals[j], 
                                                   y = y.invals[j], grad = -1/gradient, expand = c(1, 1)[1], 
                                                   both.sides = TRUE, col = tickcol[i]) }   #tick mark line
      x.labvals <- x.invals
      y.labvals <- y.invals
      if (onlylines == FALSE){
      for (j in 1:length(x.labvals)) Plot.marker.new(x = x.labvals[j], 
                                                     y = y.labvals[j], grad = -1/gradient, mark = tick.labels[j], 
                                                     expand = c(1, 1)[1], marker.size = 0.6, 
                                                     col = ticktextcol[i], pos.m = NULL, offset.m = 0.0025, 
                                                     side.label = "right") } #label text colour
      
      }
    }
    box(which = "plot", lty = "solid")
  }
  
  
  if(nolines==F)
  {
    if((plotinner==T)|(plotbags==T))
    {
      if(numclasses > 1){
        polygon(bagcords_Wnew[[1]],border = fillcolslines[1], lwd=innerline[1],lty=innerline[2])
        polygon(bagcords_Wnew[[2]],border = fillcolslines[2],lwd=innerline[1],lty=innerline[2])}
      if(numclasses > 2) {polygon(bagcords_Wnew[[3]],border = fillcolslines[3],lwd=innerline[1],lty=innerline[2])}
      if(numclasses > 3) {polygon(bagcords_Wnew[[4]],border = fillcolslines[4],lwd=innerline[1],lty=innerline[2])}
    }
    if(plotouter==T)
    {
      if(numclasses > 1){
        polygon(bagcords_Hull[[1]],border = fillcolslines[1], lwd=outerline[1],lty=outerline[2])
        polygon(bagcords_Hull[[2]],border = fillcolslines[2],lwd=outerline[1],lty=outerline[2])}
      if(numclasses > 2) {polygon(bagcords_Hull[[3]],border = fillcolslines[2],lwd=outerline[1],lty=outerline[2])}
      if(numclasses > 3) {polygon(bagcords_Hull[[4]],border = fillcolslines[4],lwd=outerline[1],lty=outerline[2])}
    }
  }
  
  if(nospots==F)
  {
    for (j in c(1,2,3,4)) 
    {
      Z.class <- Zbase[datatrain[,1] == j, , drop = FALSE]
      if (dim(Z.class)[1] !=0)
      {
        Z.class <- data.frame(Z.class[, 1:2], pch.samples.samp = pchdifffill[j], colr = fillcolslines[j], lty = 1, 
                              pch.samples.size = pchsize, bg=fillcols[j], stringsAsFactors = FALSE)
        for (i in 1:nrow(Z.class)) points(x = Z.class[i, 1], y = Z.class[i, 2], pch = Z.class[i, 3], 
                                          col = Z.class[i, 4], cex = Z.class[i, 6], bg = Z.class[i,  7])
      }
    }
  }
  #stopCluster(cl)
  
  if(nospotstest==F)
  {
    for (j in c(1,2,3,4)) 
    {
      Z.class <- Zstar[datatest[,1] == j, , drop = FALSE]
      if (dim(Z.class)[1] !=0)
      {
        Z.class <- data.frame(Z.class[, 1:2], pch.samples.samp = pchdifffill[j], colr = fillcolslines[j], lty = 1, 
                              pch.samples.size = pchsize, bg=fillcols[j],stringsAsFactors = FALSE)
        for (i in 1:nrow(Z.class)) points(x = Z.class[i, 1], y = Z.class[i, 2], pch = Z.class[i, 3], 
                                          col = Z.class[i, 4], cex = Z.class[i, 6], bg = Z.class[i,  7])
      }
    }
  }
  namesclass <- datatrain[,1]
  namesclass[namesclass==1] <- "A"
  namesclass[namesclass==2] <- "B"
  namesclass[namesclass==3] <- "C"
  namesclass[namesclass==4] <- "D"
  legendtext <- as.character(unique(namesclass))
  count <- 0
  if(is.null(classnames))
  {
  for (i in unique(namesclass))
  {
    count <- count+1
    legendtext[count] <- paste("Class ",legendtext[count],"   ",sep="")
  }
  }
  else
  {
    legendtext <- classnames 
  }
  
  par(xpd=TRUE)
  legend("bottom", legend=legendtext, pt.lwd=0.5, pt.cex=1,lty=1, cex=legcex,pt.bg = fillcols ,pch=pchdifffill ,bg="white",lwd=1,bty="n",horiz=T,inset=c(0,-0.07))
  par(xpd=FALSE)
  
  ##############CORRECTNESS####################
  
  predtest <- as.vector(predict(Model, newdata = Zstar))
  if(length(polyorange)!=0) 
  {
    for (h in 1:length(polyorange))
    {
      pip <- pnt.in.poly(Zstar,cbind(polyorange[[h]]$x,polyorange[[h]]$y))[,3]
      predtest[pip==1] <- NA
    }
  }
  greytemp <- predtest  
  if(length(polygrey)!=0)
  {
    for (h in 1:length(polygrey))
    {
      greypoints <- pnt.in.poly(Zstar,cbind(polygrey[[h]]$x,polygrey[[h]]$y))[,3]
      greytemp[greypoints==1] <- "grey"
    }
  }  
  
  predtest[greytemp!="grey"] <- NA
  predtest[predtest=="p1"] <- 1
  predtest[predtest=="p2"] <- 2
  predtest[predtest=="p3"] <- 3
  predtest[predtest=="p4"] <- 4
  predtest<- as.integer(predtest)
  Correct <- sum(datatest[,1]==predtest, na.rm = TRUE) / length(predtest)
  Incorrect <- sum(!datatest[,1]==predtest, na.rm = TRUE)/ length(predtest)
  NAclass <- sum(is.na(predtest))/ length(predtest)
  ValdateV <- data.frame(Alpha = alphin, Type = type, K = kin, Correct = Correct, Incorrect = Incorrect, NAclass = NAclass)
  tempoutput <- predtest
  
  ##############CORRECTNESS####################
  
  output <- list(ValdateV = ValdateV, Zbase = Zbase,Pred = cbind(PredA, PredB), Zaxes = Zaxesout, model = Model,
                 polygreyout = polygrey, polyorangeout = polyorange, traindataIND= traindataIND, kin=kin, expin=expin,
                 datatrain.mean = datatrain.mean, predtest = predtest, axpred = axpredout, Zstar = Zstar, samplpred = bipl$predictions,
                 bipl = bipl, PredBackGrid = Pred15)
  
  rm(bipl, datatest,   Zbase,Zstar,datatrain,Grid, marker.mat, Pred15,traindataIND, alphsim, amberpoints, axis.name, bagcords_Hull, #bagcords_W,
     bagcords_Wnew,  ev, expin, gradient, greypoints, greytemp, h, hullmult, i, j, maxrange, Model, nbp,
     nospots, nolines, plotbags, plotcont, plotinner, plotouter, polygrey, polyorange, Pred, PredA, PredB, predGRID, r.names, Rangex1, Rangex2, 
     std.markers, tick.labels, traindataINDColor, uit, usr, x.invals, x.labvals, x.vals, y.invals, y.labvals, y.vals, axpredout,
     Zaxesout)
  
  output
}