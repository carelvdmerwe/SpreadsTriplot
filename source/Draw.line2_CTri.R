function (x.vals = NULL, y.vals = NULL, marker.vals = NULL, 
          line.name = NULL, offset = c(0.5, 0.5, 0.5, 0.5), pos = "Orthog", 
          axis.col = "grey", ax.name.col = "black", ax.name.size = 0.65, weights = NA,weightsxy = NA,  weightscol = NA, linewid=25) 
{
    par(lend=1)

    cumull <- cbind(0,weights[,1], weights[,1]+weights[,2],  weights[,1]+weights[,2]+weights[,3],  
                    weights[,1]+weights[,2]+weights[,3]+weights[,4], 
                    weights[,1]+weights[,2]+weights[,3]+weights[,4]+weights[,5])
    keyval <- 2.54*linewid/100
    
    doffset  <- 1*2.54/100
    xf <- c(x.vals[1],x.vals[length(x.vals)])
    yf <- c(y.vals[1],y.vals[length(y.vals)])
    
    usr <- par("usr")
    test <- rep(NA, 4)
    test[1] <- min(x.vals) < usr[1]
    test[2] <- max(x.vals) > usr[2]
    test[3] <- min(y.vals) < usr[3]
    test[4] <- max(y.vals) > usr[4]
    if (any(test)) 
        stop("Points not entirely in graph")
    if (!is.element(pos[1], c("Hor", "Orthog", "Paral"))) 
        stop("Argument pos must be one of 'Hor','Orthog' or 'Paral' ")
    if (pos[1] == "Hor") {
        par(las = 1)
        adjust <- c(0.5, 1, 0.5, 0)
    }
    if (pos[1] == "Orthog") {
        par(las = 2)
        adjust <- c(0, 0, 0, 0)
    }
    if (pos[1] == "Paral") {
        par(las = 0)
        adjust <- c(0.5, 0.5, 0.5, 0.5)
    }
    marker.mat <- cbind(x.vals, y.vals, marker.vals)
    marker.mat <- marker.mat[order(marker.mat[, 1]), ]
    x.vals <- marker.mat[, 1]
    y.vals <- marker.mat[, 2]
        gradient <- (y.vals[1] - y.vals[length(y.vals)])/(x.vals[1] - 
                                                              x.vals[length(x.vals)])
        intercept <- y.vals[1] - gradient * x.vals[1]
    if (!is.null(marker.vals)) 
        marker.vals <- marker.mat[, 3]
    if (y.vals[1] == y.vals[length(y.vals)] & x.vals[1] == x.vals[length(x.vals)]) 
        type.line <- "plot.5"          
    if (y.vals[1] == y.vals[length(y.vals)] & x.vals[1] != x.vals[length(x.vals)]) {
        gradient <- 0
        intercept <- y.vals[1]
        type.line <- "plot.1"       
    }
    if (y.vals[1] != y.vals[length(y.vals)] & x.vals[1] == x.vals[length(x.vals)]) {
        gradient <- Inf
        intercept <- x.vals[1]
        type.line <- "plot.2"          
    }
    if (y.vals[1] != y.vals[length(y.vals)] & x.vals[1] != x.vals[length(x.vals)]) {
        gradient <- (y.vals[1] - y.vals[length(y.vals)])/(x.vals[1] - 
                                                              x.vals[length(x.vals)])
        intercept <- y.vals[1] - gradient * x.vals[1]
        y1.ster <- gradient * usr[1] + intercept
        y2.ster <- gradient * usr[2] + intercept
        x1.ster <- (usr[3] - intercept)/gradient
        x2.ster <- (usr[4] - intercept)/gradient
        if (gradient > 0) 
            type.line <- "plot.3"     
        if (gradient < 0)
            type.line <- "plot.4"         
    }
    
    x.valsw <- weightsxy[,1]
    y.valsw <- gradient * weightsxy[,1] + intercept
    
    plot.1 <- function(marker.mat, line.name = NULL, offset = NULL, 
                       adjust = NULL, axis.col = "black", ax.name.col = "black", 
                       ax.name.size = 0.65) {
        for(i in 2:(length(x.valsw)-1))
        {
            {
                m <- gradient  
                cc <- intercept
                x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                y0 <- c((y.valsw[i-1]+y.valsw[i])/2,(y.valsw[i]+y.valsw[i+1])/2)
                x0m <- x.valsw[i]
                y0m <- y.valsw[i]
                distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                dd <- (doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c(x0m,y0m+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = 0)}
                lines(x = xf, y = yf,  col=axis.col,lwd=1)
            }
        }
        
    }
    plot.2 <- function(marker.mat, line.name = NULL, offset = NULL, 
                       adjust = NULL, axis.col = "black", ax.name.col = "black", 
                       ax.name.size = 0.65) {
        for(i in 2:(length(x.valsw)-1))
        {
            {
                m <- gradient  
                cc <- intercept
                x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                y0 <- c((y.valsw[i-1]+y.valsw[i])/2,(y.valsw[i]+y.valsw[i+1])/2)
                x0m <- x.valsw[i]
                y0m <- y.valsw[i]
                distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                dd <- (doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c(x0m-dd[j],y0m),wx = distx,wy = disty[j],col=weightscol[j],angle = 0)}
                lines(x = xf, y = yf,  col=axis.col,lwd=1)
            }
        }
        
    }
    plot.3 <- function(y1.ster, y2.ster, x1.ster, x2.ster, usr, 
                       marker.mat, line.name = NULL, offset = NULL, adjust = NULL, 
                       axis.col = "black", ax.name.col = "black", ax.name.size = 0.65) {
        if (y1.ster >= usr[3] & y2.ster >= usr[4]) {
            for(i in 2:(length(x.valsw)-1))
            {
                {
                    m <- gradient  
                    cc <- intercept
                    x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                    y0 <- c(x0[1]*m + cc, x0[2]*m + cc)
                    x0m <- x.valsw[i]
                    y0m <- x0m*m + cc
                    distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                    dd <- sqrt(1+m^2)*(doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                    disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                    for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c((y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m), m*(y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m)+cc+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = atan(m)*180/pi)}
                    lines(x = xf, y = yf,  col=axis.col,lwd=1)
                }
            }
            
        }
        if (y1.ster > usr[3] & y2.ster < usr[4]) {
            for(i in 2:(length(x.valsw)-1))
            {
                {
                    m <- gradient  
                    cc <- intercept
                    x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                    y0 <- c(x0[1]*m + cc, x0[2]*m + cc)
                    x0m <- x.valsw[i]
                    y0m <- x0m*m + cc
                    distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                    dd <- sqrt(1+m^2)*(doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                    disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                    for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c((y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m), m*(y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m)+cc+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = atan(m)*180/pi)}
                    lines(x = xf, y = yf,  col=axis.col,lwd=1)
                }
            }
            
        }
        if (y1.ster < usr[3] & y2.ster > usr[4]) {
            for(i in 2:(length(x.valsw)-1))
            {
                {
                    m <- gradient  
                    cc <- intercept
                    x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                    y0 <- c(x0[1]*m + cc, x0[2]*m + cc)
                    x0m <- x.valsw[i]
                    y0m <- x0m*m + cc
                    distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                    dd <- sqrt(1+m^2)*(doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                    disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                    for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c((y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m), m*(y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m)+cc+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = atan(m)*180/pi)}
                    lines(x = xf, y = yf,  col=axis.col,lwd=1)
                }
            }
        }
        if (y1.ster < usr[3] & y2.ster < usr[4]) {
            for(i in 2:(length(x.valsw)-1))
            {
                {
                    m <- gradient  
                    cc <- intercept
                    x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                    y0 <- c(x0[1]*m + cc, x0[2]*m + cc)
                    x0m <- x.valsw[i]
                    y0m <- x0m*m + cc
                    distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                    dd <- sqrt(1+m^2)*(doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                    disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                    for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c((y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m), m*(y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m)+cc+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = atan(m)*180/pi)}
                    lines(x = xf, y = yf,  col=axis.col,lwd=1)
                }
            }
            
        }
    }
    plot.4 <- function(y1.ster, y2.ster, x1.ster, x2.ster, usr, 
                       marker.mat, line.name = NULL, offset = NULL, adjust = NULL, 
                       axis.col = "black", ax.name.col = "black", ax.name.size = 0.65) {
        if (y1.ster > usr[4] & y2.ster > usr[3]) {
            for(i in 2:(length(x.valsw)-1))
            {
                {
                    m <- gradient  
                    cc <- intercept
                    x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                    y0 <- c(x0[1]*m + cc, x0[2]*m + cc)
                    x0m <- x.valsw[i]
                    y0m <- x0m*m + cc
                    distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                    dd <- -sqrt(1+m^2)*(doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                    disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                    for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c((y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m), m*(y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m)+cc+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = atan(m)*180/pi)}
                    lines(x = xf, y = yf,  col=axis.col,lwd=1)
                }
            }
            
        }
        if (y1.ster < usr[4] & y2.ster > usr[3]) {
            for(i in 2:(length(x.valsw)-1))
            {
                {
                    m <- gradient  
                    cc <- intercept
                    x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                    y0 <- c(x0[1]*m + cc, x0[2]*m + cc)
                    x0m <- x.valsw[i]
                    y0m <- x0m*m + cc
                    distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                    dd <- -sqrt(1+m^2)*(doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                    disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                    for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c((y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m), m*(y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m)+cc+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = atan(m)*180/pi)}
                    lines(x = xf, y = yf,  col=axis.col,lwd=1)
                }
            }
            
        }
        if (y1.ster > usr[4] & y2.ster < usr[3]) {
            for(i in 2:(length(x.valsw)-1))
            {
                {
                    m <- gradient  
                    cc <- intercept
                    x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                    y0 <- c(x0[1]*m + cc, x0[2]*m + cc)
                    x0m <- x.valsw[i]
                    y0m <- x0m*m + cc
                    distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                    dd <- -sqrt(1+m^2)*(doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                    disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                    for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c((y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m), m*(y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m)+cc+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = atan(m)*180/pi)}
                    lines(x = xf, y = yf,  col=axis.col,lwd=1)
                }
            }
            
        }
        if (y1.ster < usr[4] & y2.ster < usr[3]) {
            for(i in 2:(length(x.valsw)-1))
            {
                {
                    m <- gradient  
                    cc <- intercept
                    x0 <- c((x.valsw[i-1]+x.valsw[i])/2,(x.valsw[i]+x.valsw[i+1])/2)
                    y0 <- c(x0[1]*m + cc, x0[2]*m + cc)
                    x0m <- x.valsw[i]
                    y0m <- x0m*m + cc
                    distx <- sqrt((x0[2]-x0[1])^2+(y0[2]-y0[1])^2)
                    dd <- -sqrt(1+m^2)*(doffset+(cumull[i,]+weights[i,]/2)*(keyval-(-keyval)))*par("cxy")[2]
                    disty <- (doffset+(weights[i,])*(keyval-(-keyval)))*par("cxy")[2]
                    for(j in 2:6) if(weights[i,j]!=0) {filledrectangle(mid = c((y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m), m*(y0m+(1/m)*x0m - (cc+dd[j]))/(1/m + m)+cc+dd[j]),wx = distx,wy = disty[j],col=weightscol[j],angle = atan(m)*180/pi)}
                    lines(x = xf, y = yf,  col=axis.col,lwd=1)
                }
            }
            
        }
    }
    plot.5 <- function(marker.mat, axis.col = 1) {
        abline(h = marker.mat[1, 2], col = axis.col)
        abline(v = marker.mat[1, 1], col = axis.col)
    }
    switch(type.line, plot.1 = plot.1(marker.mat = marker.mat, 
                                      line.name = line.name, offset = offset, adjust = adjust, 
                                      axis.col = axis.col, ax.name.col = ax.name.col, ax.name.size = ax.name.size), 
           plot.2 = plot.2(marker.mat = marker.mat, line.name = line.name, 
                           offset = offset, adjust = adjust, axis.col = axis.col, 
                           ax.name.col = ax.name.col, ax.name.size = ax.name.size), 
           plot.3 = plot.3(marker.mat = marker.mat, line.name = line.name, 
                           offset = offset, y1.ster = y1.ster, y2.ster = y2.ster, 
                           x1.ster = x1.ster, x2.ster = x2.ster, usr = usr, 
                           adjust = adjust, axis.col = axis.col, ax.name.col = ax.name.col, 
                           ax.name.size = ax.name.size), 
           plot.4 = plot.4(marker.mat = marker.mat, 
                           line.name = line.name, offset = offset, y1.ster = y1.ster, 
                           y2.ster = y2.ster, x1.ster = x1.ster, x2.ster = x2.ster, 
                           usr = usr, adjust = adjust, axis.col = axis.col, 
                           ax.name.col = ax.name.col, ax.name.size = ax.name.size), 
           plot.5 = plot.5(marker.mat = marker.mat), axis.col = axis.col)
    list(gradient = gradient, intercept = intercept)
}
