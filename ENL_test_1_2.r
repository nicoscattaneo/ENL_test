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
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Evaluate ENL sensityvity to point density
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Res = 0.05
NLayers = 20

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
## 4 layers with the same range but different point densities.

## lapply(Layerss, dim)
## lapply(Layerss, function(XX){
##     open3d()
##     plot3d(XX, size = 1, col="grey21", aspect = "iso")
##     })

## Layer voxelization
Layerss <- lapply(Layerss, function(XX){
    lay <- VoxR::vox(XX, res = Res)[, 1:3]
    names(lay) <- c("x", "y", "z")
    return(lay)})

## Compute ENL for 4 different vertical layouts/arrays.
## Each layout is composed of 20 repetitions in the z axis
## of the same cubic layer
ENL2D <- lapply(Layerss, function(Data){
    ## Data <- Layerss[[1]]
    TotalVoxels <- nrow(Data)*NLayers
    p <- nrow(Data)/TotalVoxels
    p <- rep(p, NLayers)
    ENL.2D <- 1/sum(p^2)
    return(ENL.2D)})


## The ENL is the same regardless of the point density of the arrays.
do.call("c", ENL2D)

## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Evaluate ENL sensitivity to vertical layering
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

ENLayering <- function(Data, N){
    ## Data: should be a list similar to "Layerss" object
    ## N: number of different layer distributions to generate.

    Layers <- sample(c(1,3,4), 12, replace = TRUE)
    Layers <- lapply(1:N, function(X){
        sample(Layers, 12)})

    ENL2D <- lapply(seq_along(Layers), function(XX){
        ## XX <- 1
        Distr <- Layers[[XX]]

        TotalVoxels <- sum(
            sapply(seq_along(Distr), function(CC){
                nrow(Data[[Distr[CC]]])}))

        p <- sapply(seq_along(Distr), function(CC){
            nrow(Data[[Distr[[CC]]]])/TotalVoxels})
        ENL.2D <- 1/sum(p^2)
        return(ENL.2D)})

    LayouT <- lapply(1:length(Layers), function(XX){

        LayouT <- Layers[[XX]]
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


    Displa <- seq(0, 100, by = 2)[1:length(LayouT)]

    open3d()
    lapply(seq_along(ENL2D), function(XX){
        temp <- LayouT[[XX]]
        temp[, 1] <- temp[, 1]+Displa[XX]
        plot3d(temp, size = 0.5,
               col="grey21", aspect = "iso",
               axes = F, xlab = "",
               ylab = "", zlab = "",
               main = paste("ENL = ", round(ENL2D[[XX]], 3)),
               add=T)
        paste("ENL = ", round(ENL2D[[XX]], 3))
    })
}

ENLayering(Layerss, 5)

## Different vertical distributions of the same point-cloud layers
## deliver the same ENL value.
