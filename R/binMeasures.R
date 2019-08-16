
binMean <- function(mz,intensity) {
	sum(mz * intensity)/sum(intensity)
}

binMAE <- function(mz,intensity) {
	sum(abs(mz - binMean(mz,intensity)) * intensity) / sum(intensity)
}

binPurity <- function(mz,intensity,dp = 2){
	1 - binMAE(mz,intensity) / (1*10^-dp)
}

binCentrality <- function(mz,intensity,dp = 2){
	1 - abs(binMean(mz,intensity) - round(mz[1],dp))/(1*10^-dp/2)
}