playground <- function(biplin, linewidin = 10, axin = 1:length(zaxes),plotlines = T, grid = 100, varnamesin = NULL, unifbar = TRUE, fontsizelabs = 0.75){
zaxes <- biplin$Zaxes
Zconstruct <- biplin$Zbase
model <- biplin$Model
polyorange <- biplin$polyorangeout
polygrey <- biplin$polygreyout
datatrain.mean <- biplin$datatrain.mean
fillcols <- gray.colors(dim(biplin$datatrain.mean)[1], start = 0.5, end = 0.9, gamma = 2.2, alpha = NULL) 
expin <- biplin$expin
nbpx <- grid
nbpy <- grid
Model <- train(as.formula("classes ~ ."), data = biplin$traindataIND, method = "knn", tuneGrid = data.frame(k = c(biplin$kin)))
Rangex1 <- c(min(Zconstruct[,1])-abs(min(Zconstruct[,1]))*(expin-1),max(Zconstruct[,1])+abs(max(Zconstruct[,1]))*(expin-1))
Rangex2 <- c(min(Zconstruct[,2])-abs(min(Zconstruct[,2]))*(expin-1),max(Zconstruct[,2])+abs(max(Zconstruct[,2]))*(expin-1))
maxrange <- max(Rangex1[2]-Rangex1[1],Rangex2[2] - Rangex2[1])
if(Rangex1[2]-Rangex1[1]<maxrange) Rangex1 <- c((Rangex1[2]+Rangex1[1])/2-maxrange/2,(Rangex1[2]+Rangex1[1])/2+maxrange/2)
if(Rangex2[2]-Rangex2[1]<maxrange) Rangex2 <- c((Rangex2[2]+Rangex2[1])/2-maxrange/2,(Rangex2[2]+Rangex2[1])/2+maxrange/2)
PredA <- seq(Rangex1[1],Rangex1[2], length = nbpx)
PredB <- seq(Rangex2[1],Rangex2[2], length = nbpy)
Grid <- expand.grid(X1 = PredA, X2 = PredB)
Gridx <- matrix(Grid[,1],ncol=nbpx,byrow=TRUE)
Gridy <- matrix(Grid[,2],ncol=nbpx,byrow=TRUE)

zperpgridy <- list()
zperpgridx <- list()
zgridx <- list()
zgridy <- list()
cperpmat<- list()
inputxy <- list()
perpinputxy <- list()
weightsx<- list()
weightsy<- list()
interceptsperp <- list()


Pred15 <- list()
Pred15Mat <- list()
zperpaxes<- list()
RAGGMat <- list()
zperpgridxyout<- list()
for (i in axin)
{
  
  input <- zaxes[[i]][,c(1,2)]
  m <- (input[dim(input)[1],2]-input[1,2])/(input[dim(input)[1],1]-input[1,1])
  cc <- input[1,2] - m*input[1,1]
  rotategrid <- atan(-1/m)
  zperpgridx[[i]] <- t(apply(cbind(Gridx,Gridy),1,function(x){x[1:(nbpx)]*cos(rotategrid)-x[(nbpx+1):(2*nbpx)]*sin(rotategrid)}))
  zperpgridy[[i]] <- t(apply(cbind(Gridx,Gridy),1,function(x){x[1:(nbpx)]*sin(rotategrid)+x[(nbpx+1):(2*nbpx)]*cos(rotategrid)}))
  interceptsperp[[i]] <- apply(cbind(zperpgridx[[i]][,1],zperpgridy[[i]][,1]),1,function(x){x[2]+(1/m)*x[1]})
  weightsx[[i]] <- apply(as.matrix(interceptsperp[[i]]),1,function(x){(x-cc)/(m+1/m)})
  weightsy[[i]] <- apply(as.matrix(weightsx[[i]]), 1, function(x){m*x+cc})

zperpgridxy <- cbind(as.vector(t(zperpgridx[[i]])), as.vector(t(zperpgridy[[i]])))
zperpgridxyout[[i]] <- zperpgridxy
colnames(zperpgridxy) <- c("X1","X2")
Pred <- predict(Model, newdata = zperpgridxy)
predGRID <- as.vector(Pred)

if((length(polyorange)!=0))
{
  for (h in 1:length(polyorange))
  {
    amberpoints <- pnt.in.poly(zperpgridxy,cbind(polyorange[[h]]$x,polyorange[[h]]$y))[,3]
    predGRID[amberpoints==1] <- "orange"
  }
}
greytemp <- predGRID
  if(length(polygrey)!=0)
  {
    for (h in 1:length(polygrey))
    {
      greypoints <- pnt.in.poly(zperpgridxy,cbind(polygrey[[h]]$x,polygrey[[h]]$y))[,3]
      greytemp[greypoints==1] <- "grey"
    }
  }  
  predGRID[greytemp!="grey"] <- "grey" 

Pred15[[i]] <- rep(0, length(Pred))
Pred15[[i]][predGRID=="p1"] <- 1
Pred15[[i]][predGRID=="p2"] <- 2
Pred15[[i]][predGRID=="p3"] <- 3
Pred15[[i]][predGRID=="p4"] <- 4
Pred15[[i]][predGRID=="grey"] <- -1
Pred15[[i]][predGRID=="orange"] <- 0
Pred15[[i]] <- matrix(Pred15[[i]], ncol = nbpx,byrow=TRUE)
nanaxes <- nbpx - apply(Pred15[[i]],1,function(x){sum(x==-1)})
if (unifbar == FALSE) {nanaxes[nanaxes!=0] <- max(nanaxes)} #verander hier as asse nie wil laat op tel tot 1 toe nie
zperpaxes[[i]]  <-  ifelse(nanaxes == 0, 1, 0)
zperpaxes[[i]]  <- cbind(zperpaxes[[i]], apply(Pred15[[i]],1,function(x){sum(x==0)})/nanaxes)
zperpaxes[[i]]  <- cbind(zperpaxes[[i]], apply(Pred15[[i]],1,function(x){sum(x==1)})/nanaxes)
zperpaxes[[i]]  <- cbind(zperpaxes[[i]], apply(Pred15[[i]],1,function(x){sum(x==2)})/nanaxes)
zperpaxes[[i]]  <- cbind(zperpaxes[[i]], apply(Pred15[[i]],1,function(x){sum(x==3)})/nanaxes)
zperpaxes[[i]]  <- cbind(zperpaxes[[i]], apply(Pred15[[i]],1,function(x){sum(x==4)})/nanaxes)
zperpaxes[[i]][is.na(zperpaxes[[i]])] <- 0 

  r.names <- NULL
  usr <- par("usr")

  marker.mat <- unique(round(zaxes[[i]][zaxes[[i]][, 4] == 1,1:3],10)) 
  x.vals <- marker.mat[, 1] #x-coords vir tick marks
  y.vals <- marker.mat[, 2] #y-coords vir tick marks
  std.markers <- zapsmall(marker.mat[, 3])   #rounds values close to zero to zero
  RAGGMat[[i]] <- cbind( approx(x = x.vals, y = marker.mat[, 3], weightsx[[i]])$y, zperpaxes[[i]])

  if (plotlines == T)  
  {
    
    startind <- TRUE
  for(j in 1:nrow(zperpaxes[[i]]))
  {
    if (zperpaxes[[i]][j,1]!=1 & startind == TRUE) 
      {
      startcoords <- cbind(weightsx[[i]],weightsy[[i]])[max(j-round(2*grid/100,0),1),]
      startind <- FALSE
      }
  }
  endind <- TRUE
  for(j in nrow(zperpaxes[[i]]):1)
  {
    if (zperpaxes[[i]][j,1]!=1 && endind == TRUE) 
    {
      endcoords <- cbind(weightsx[[i]],weightsy[[i]])[min(j+round(2*grid/100,0),nrow(zperpaxes[[i]])),]
      endind <- FALSE
    }
  }
  xstartend <-  c(min(startcoords[1],endcoords[1]), max(startcoords[1],endcoords[1]))
  ystartend <-  c(min(startcoords[2],endcoords[2]), max(startcoords[2],endcoords[2]))
  if(is.null(varnamesin))
  {
  if (is.null(dimnames(datatrain.mean)[[2]][i])) #overrides dimname if there is none
  {
    axis.name <- paste("v", i, sep = "")
  }
  else 
  {
    axis.name <- dimnames(datatrain.mean)[[2]][i]
  } 
  }
  else
  {
    axis.name <- varnamesin[i]
  }
  r.names[i] <- axis.name
  x.invals <- x.vals[x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]& 
                       x.vals <= xstartend[2] & x.vals >= xstartend[1] & y.vals <= ystartend[2] & y.vals >= ystartend[1]]  #these are the pars of the axis that falls within the box
  x.invals <- x.invals[c(1,round(length(x.invals)/4,0), round(length(x.invals)/2,0), round(length(x.invals)*3/4,0),length(x.invals))]
  if (length(x.invals) < 2) 
  {
    warning(paste("Less than 2 markers on axis ", 
                  i, ". Increase n.int."))
    x.invals <-         x.vals[x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3] & 
                               x.vals <= xstartend[2] & x.vals >= xstartend[1] & y.vals <= ystartend[2] & y.vals >= ystartend[1]]
    x.invals <- x.invals[c(1,round(length(x.invals)/4,0), round(length(x.invals)/2,0), round(length(x.invals)*3/4,0),length(x.invals))]
    
    y.invals <-         y.vals[x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]& 
                                 x.vals <= xstartend[2] & x.vals >= xstartend[1] & y.vals <= ystartend[2] & y.vals >= ystartend[1]]
    y.invals <- y.invals[c(1,round(length(y.invals)/4,0), round(length(y.invals)/2,0), round(length(y.invals)*3/4,0),length(y.invals))]
    tick.labels <- std.markers[x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]& 
                                 x.vals <= xstartend[2] & x.vals >= xstartend[1] & y.vals <= ystartend[2] & y.vals >= ystartend[1]]
    tick.labels <- tick.labels[c(1,round(length(x.invals)/4,0), round(length(x.invals)/2,0), round(length(x.invals)*3/4,0),length(x.invals))]

      Draw.line2_CTri(x.vals = x.invals, y.vals = y.invals, 
               marker.vals = tick.labels, line.name = axis.name, 
               ax.name.size = fontsizelabs, axis.col = "black", 
               ax.name.col = "black", offset = rep(0, length(datatrain.mean)),
               weights = zperpaxes[[i]][,1:6], weightsxy = cbind(weightsx[[i]],weightsy[[i]]),linewid = linewidin, weightscol =  c("white","white",fillcols))#, 
    next
    }
  
  y.invals <- y.vals[x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]& 
                               x.vals <= xstartend[2] & x.vals >= xstartend[1] & y.vals <= ystartend[2] & y.vals >= ystartend[1]]
  y.invals <- y.invals[c(1,round(length(y.invals)/4,0), round(length(y.invals)/2,0), round(length(y.invals)*3/4,0),length(y.invals))]
  
  tick.labels <- std.markers[x.vals < usr[2] & x.vals > usr[1] & y.vals < usr[4] & y.vals > usr[3]& 
                               x.vals <= xstartend[2] & x.vals >= xstartend[1] & y.vals <= ystartend[2] & y.vals >= ystartend[1]] #only include ones within box
  tick.labels <- tick.labels[c(1,round(length(tick.labels)/4,0), round(length(tick.labels)/2,0), round(length(tick.labels)*3/4,0),length(tick.labels))]
  

  uit <- Draw.line2_CTri(x.vals = x.invals, y.vals = y.invals, 
                    marker.vals = tick.labels, line.name = axis.name, 
                    ax.name.size = fontsizelabs, axis.col = "black", 
                    ax.name.col =  "black", offset = rep(0, length(datatrain.mean)),
                    weights = zperpaxes[[i]][,1:6], weightsxy =cbind(weightsx[[i]],weightsy[[i]]), weightscol = c("white","white",fillcols), linewid = linewidin)# , 

  gradient <- uit$gradient
  for (j in 1:length(x.invals)) Draw.onecmline(x = x.invals[j], 
                                               y = y.invals[j], grad = -1/gradient, expand = c(1, 1)[1], 
                                               both.sides = TRUE, col = "black")
  x.labvals <- x.invals
  y.labvals <- y.invals
  for (j in 1:length(x.labvals)) Plot.marker.new(x = x.labvals[j], 
                                                 y = y.labvals[j], grad = -1/gradient, mark = tick.labels[j], 
                                                 expand = c(1, 1)[1], marker.size = fontsizelabs,#0.6, 
                                                 col = "black", pos.m = NULL, offset.m = 0.0025, 
                                                 side.label = "right")
  Plot.marker.new_C(x = x.labvals[3], y = y.labvals[3], grad = -1/gradient, mark = paste(axis.name," (",round(biplin$axpred[i]*100,0),"%)",sep=""), 
                                                 expand =  (linewidin+2)*0.3527-3, marker.size =  fontsizelabs,#0.6, 
                                                 col = "black", pos.m = NULL, offset.m =0, 
                                                 side.label = "left")
  }
}

list(RAGGMat,zperpaxes,nanaxes, zperpgridxyout)

}