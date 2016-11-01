GreggCarder.data <- function() {
# ******************************************************************
#
#  Opens and reads light data. Includes oxygen and water
#  vapor absorption.
#
#  Light Data
	f.et <- paste(Sys.getenv("R_COPS_DATA_DIR"), "new_etlight.dat", sep = "/")
	f.oz <- paste(Sys.getenv("R_COPS_DATA_DIR"), "new_ozpar.dat", sep = "/")
	f.wv <- paste(Sys.getenv("R_COPS_DATA_DIR"), "new_wvpar.dat", sep = "/")
	tmp.x1 <- read.table(file = f.et, header = TRUE)
	tmp.x2 <- read.table(file = f.oz, header = TRUE)
	tmp.x3 <- read.table(file = f.wv, header = TRUE)
#	tmp.ag <- vector(mode= "numeric", length = 351)
	tmp.ag <- vector(mode= "numeric", length = 581)
	tmp.ag[277 + 30] = 0.0021
	tmp.ag[278 + 30] = 0.0049
	tmp.ag[279 + 30] = 0.0078
	tmp.ag[280 + 30] = 0.0103
	tmp.ag[281 + 30] = 0.0114
	tmp.ag[282 + 30] = 0.0103
	tmp.ag[283 + 30] = 0.0078
	tmp.ag[284 + 30] = 0.0049
	tmp.ag[285 + 30] = 0.0021
	tmp.ag[337 + 30] = 0.0672
	tmp.ag[338 + 30] = 0.8100
	tmp.ag[339 + 30] = 0.6500
	tmp.ag[340 + 30] = 0.5052
	tmp.ag[341 + 30] = 0.3604
	tmp.ag[342 + 30] = 0.3250
	tmp.ag[343 + 30] = 0.2480
	tmp.ag[344 + 30] = 0.1566
	tmp.ag[345 + 30] = 0.0678
	tmp.ag[346 + 30] = 0.0008
	GreggCarder.d <- list(lam = tmp.x2$V1, Fobar = tmp.x1$V2 * 10,
		oza = tmp.x2$V2, ag = tmp.ag, aw = tmp.x3$V2)
	assign("GreggCarder.d", GreggCarder.d, env = .GlobalEnv)
}
