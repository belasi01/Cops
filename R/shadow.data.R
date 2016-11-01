shadow.data <- function() {
	GordonDing.d <- list(
		t = seq(0,90,10),
		table.sun.LuZ = c(2.17,2.17,2.23,2.23,2.29,2.37,2.41,2.45,2.45,2.45),
		table.sun.EuZ = c(3.14,3.14,3.05,2.94,2.80,2.64,2.47,2.33,2.33,2.33),
		table.sky.LuZ = 4.61,
		table.sky.EuZ = 2.70
	)
	assign("GordonDing.d", GordonDing.d, env = .GlobalEnv)
}
