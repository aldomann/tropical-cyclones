# Code to study the PDI vs duration and wind speed lm coeffients
# Author: Alfredo Hernández <aldomann.designs@gmail.com>

# Source base code -----------------------------------------
source("hadisst_base.R")
source("hurdat2_pdi_base.R")

# Get linear regression coefficients -----------------------

get_coefs <- function(hurr.pdi, ssts, status){
	if(status == "ds"){
		hurr.pdi <- hurr.pdi %>%
			filter(max.wind > 33)

	} else if(status == "nds"){
		hurr.pdi <- hurr.pdi %>%
			filter(max.wind <= 33)
	}
	hurr.high.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_low_years(ssts))

	lm.high.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.high.pdi)
	lm.low.y <- lm(log10(storm.pdi) ~ log10(conv_unit(storm.duration, "sec", "hr")), data = hurr.low.pdi)
	lm.high.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.high.pdi)
	lm.low.x <- lm(log10(conv_unit(storm.duration, "sec", "hr")) ~ log10(storm.pdi), data = hurr.low.pdi)

	summary(lm.low.y)
	# summary(lm.low.x)
	# summary(lm.high.y)
	# summary(lm.high.x)
	# paste("low", "y(x)", format(round(coef(lm.low.y)[[1]], 3), nsmall = 3), format(round(coef(lm.low.y)[[2]], 3), nsmall = 3), "\n", "low", "x(y)", format(round(-coef(lm.low.x)[[1]]/coef(lm.low.x)[[2]], 3), nsmall = 3), format(round(1/coef(lm.low.x)[[2]], 3), nsmall = 3), "\n", "high", "y(x)", format(round(coef(lm.high.y)[[1]], 3), nsmall = 3), format(round(coef(lm.high.y)[[2]], 3), nsmall = 3), "\n", "high", "x(y)", format(round(-coef(lm.high.x)[[1]]/coef(lm.high.x)[[2]], 3), nsmall = 3), format(round(1/coef(lm.high.x)[[2]], 3), nsmall = 3))
}

get_coefs(hurr.natl.pdi, ssts.natl, "ds")
get_coefs(hurr.natl.pdi, ssts.natl, "nds")

get_coefs(hurr.epac.pdi, ssts.epac, "ds")
get_coefs(hurr.epac.pdi, ssts.epac, "nds")

# Get linear regression coefficients (wind) ----------------

get_coefs_wind <- function(hurr.pdi, ssts){
	hurr.pdi <- hurr.pdi %>%
		filter(max.wind > 33)
	hurr.high.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_high_years(ssts))
	hurr.low.pdi <- hurr.pdi %>%
		filter(storm.year %in% get_low_years(ssts))

	lm.high.y <- lm(log10(storm.pdi) ~ log10(conv_unit(max.wind, "knot", "m_per_sec")), data = hurr.high.pdi)
	lm.low.y <- lm(log10(storm.pdi) ~ log10(conv_unit(max.wind, "knot", "m_per_sec")), data = hurr.low.pdi)
	lm.high.x <- lm(log10(conv_unit(max.wind, "knot", "m_per_sec")) ~ log10(storm.pdi), data = hurr.high.pdi)
	lm.low.x <- lm(log10(conv_unit(max.wind, "knot", "m_per_sec")) ~ log10(storm.pdi), data = hurr.low.pdi)

	summary(lm.low.y)
	# summary(lm.low.x)
	# summary(lm.high.y)
	# summary(lm.high.x)
	# paste("low", "y(x)", format(round(coef(lm.low.y)[[1]], 3), nsmall = 3), format(round(coef(lm.low.y)[[2]], 3), nsmall = 3), "\n", "low", "x(y)", format(round(-coef(lm.low.x)[[1]]/coef(lm.low.x)[[2]], 3), nsmall = 3), format(round(1/coef(lm.low.x)[[2]], 3), nsmall = 3), "\n", "high", "y(x)", format(round(coef(lm.high.y)[[1]], 3), nsmall = 3), format(round(coef(lm.high.y)[[2]], 3), nsmall = 3), "\n", "high", "x(y)", format(round(-coef(lm.high.x)[[1]]/coef(lm.high.x)[[2]], 3), nsmall = 3), format(round(1/coef(lm.high.x)[[2]], 3), nsmall = 3))
}

get_coefs_wind(hurr.natl.pdi, ssts.natl)
get_coefs_wind(hurr.epac.pdi, ssts.epac)
