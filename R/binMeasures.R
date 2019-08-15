
binMean <- function(mz,intensity) {
	sum(mz * intensity)/sum(intensity)
}

binSD <- function(mz,intensity) {
	intensity <- ceiling(intensity)
	sqrt(sum(intensity * (mz - binMean(mz,intensity))^2)/sum(intensity - 1))
}

binPurity <- function(mz,intensity,dp = 2){
	1 - binSD(mz,intensity) / (1*10^-dp)
}

binCentrality <- function(mz,intensity,dp = 2){
	1 - abs(binMean(mz,intensity) - round(mz[1],dp))/(1*10^-dp/2)
}