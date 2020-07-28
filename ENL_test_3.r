rm(list=ls())
gc()
Sys.setenv(LANG = "en")
library("rgl")

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Load a cube to use as a layer

cube <- readRDS("cube.rds")
## plot3d(cube, size = 1, col="grey21", aspect = "iso")
## apply(cube, 2, range)
## dim(cube)

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Create different layers

Layerss <- list(fullLayer = cube[sample(1:nrow(cube), 10000), ],
                midLayer = cube[sample(1:nrow(cube), 5000), ],
                lowLayer = cube[sample(1:nrow(cube), 2500), ],
                poorLayer = cube[sample(1:nrow(cube), 500), ])

Layerss <- lapply(Layerss, function(OO){
    OO <- apply(OO, 2, function(ALA){
        ALA <- (ALA - min(ALA))/
            (max(ALA) - min(ALA))})
    return(OO)})

## lapply(Layerss, apply, 2, range)
## lapply(Layerss, dim)
## 4 layers with the same range but different point densities.

## lapply(Layerss, function(XX){
##     open3d()
##     plot3d(XX, size = 1, col="grey21", aspect = "iso")
##     })

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

## Select how to change he size of the
## original cubic point clouds
## To reduce, Prop = (positive c(0,1))
## To increase, Prop = (negative c(-1,0))

Prop <- 0.50
## Prop <- -0.30

## @@@@@@@@@@@@@@@
## Change the size of the original layers
Prop <- 1 + (Prop)
LayerssR <- lapply(Layerss, function(OO){
    OO <- apply(OO, 2, function(ALA){
        ALA <- (ALA - min(ALA))/
            (Prop - min(ALA))})
    return(OO)})

Layerss <- lapply(Layerss, as.data.frame)
LayerssR <- lapply(LayerssR, as.data.frame)

## original layers
lapply(Layerss, apply, 2, range)
## re-sized layers
lapply(LayerssR, apply, 2, range)


## Their general morphologies are the same,
## but their physical dimensions are different.
plot3d(Layerss[[1]], size = 1, col="grey21", aspect = "iso")
temp <- LayerssR[[1]]
temp[, 1] <- temp[, 1]+1.5
plot3d(temp, size = 1, col="grey21", aspect = "iso", add=T)

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Create the same layout with original and re-sized layers
Layers <- sample(c(1,3,4), 12, replace = TRUE)

Data <- Layerss
LayouT <- lapply(1:length(Layers), function(XX){
    ## XX <- 1
    LayouT <- Layers
    LayouT <- lapply(1:length(LayouT), function(ZZ){
        Displa <- seq(0, 100, by = range(Data[[1]][, 1])[2])
        Displa <- Displa[1:length(LayouT)]
        z <- Displa
        ala <- Data[[LayouT[ZZ]]]
        ala$z <- ala$z+z[ZZ]
        ala})
    LayouT <- do.call("rbind", LayouT)
    return(LayouT)
})
LayouT <- do.call("rbind", LayouT)

Data <- LayerssR
LayouTR <- lapply(1:length(Layers), function(XX){
    ## XX <- 2
    LayouT <- Layers
    LayouT <- lapply(1:length(LayouT), function(ZZ){
        Displa <- seq(0, 100, by = range(Data[[1]][, 1])[2])
        Displa <- Displa[1:length(LayouT)]
        z <- Displa
        ala <- Data[[LayouT[ZZ]]]
        ala$z <- ala$z+z[ZZ]
        ala})
    LayouT <- do.call("rbind", LayouT)
    return(LayouT)
})
LayouTR <- do.call("rbind", LayouTR)


## Their general morphologies are the same,
## but their physical dimensions are different.
plot3d(LayouT, aspect = "iso", col="grey21", size =0.3)
temp <- LayouTR
temp[, 1] <- temp[, 1]+1.5
plot3d(temp, size = 1, col="grey21", aspect = "iso", add=T)

dim(LayouT)
dim(LayouTR)

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## compute ENL
ENL <- function(Data, Res = 0.05){

    DataVox <- VoxR::vox(Data, res = Res)[, 1:3]
    names(DataVox) <- c("x", "y", "z")

    DataVox <- DataVox[order(DataVox$z), ]
    rownames(DataVox) <- 1:nrow(DataVox)

    Isnaturalnumber <- function(x, tol = .Machine$double.eps^0.5)
        x > tol & abs(x - round(x)) < tol

    Breaks <- seq(0, max(DataVox$z), Res)[
        Isnaturalnumber(seq(0, max(DataVox$z), Res))]
    Breakslow <- c(0, Breaks)
    Breaksup <- Breakslow+(1-Res)

    Lay <- lapply(seq_along(Breaksup), function(XX){
        seq(Breakslow[XX], Breaksup[XX], Res)})

    DataVox <- lapply(seq_along(Lay), function(XX){
        DataVoxLay <- DataVox[DataVox$z %in% Lay[[XX]], ]
        DataVoxLay$Layer <- XX
        DataVoxLay})

    upperLayer <- DataVox[[length(DataVox)]]
    restLayer <- DataVox[1:(length(DataVox)-1)]

    TotalVox <- nrow(do.call("rbind", DataVox))

    p <- lapply(restLayer, function(XX){
        nrow(XX)/TotalVox})

    pUpper <- nrow(upperLayer)

    ## a <- seq((length(DataVox)-1), ((length(DataVox)-1)+(1-Res)), Res)
    b <- seq((length(DataVox)-1), max(upperLayer$z), Res)

    pUpper <- pUpper/(length(b)/(1/Res))
    pUpper <- pUpper/TotalVox

    p <- c(do.call("c", p), pUpper)

    ENL2D <- 1/sum(p^2)
    return(ENL2D)
}

ENL(LayouT)
ENL(LayouTR)

## ENL is sensitive to ecosystem scaling
