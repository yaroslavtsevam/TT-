library("errors")

#ΔC=[π/4 [DBH^2 -(DBH-2w)^2]*h*BCEF]*(1 +R)*CF

EP <- read_delim("papers/ES/Error propagation.csv",";", escape_double = FALSE, trim_ws = TRUE)


errors(EP$`Tree height, m`)=EP$`Tree height error`
errors(EP$`Stem diameter,cm`) = EP$`Stem diameter error`
errors(EP$`Stem radial increment`) = EP$`Stem radial increment error`
errors(EP$BCEF) = EP$`BCEF error`
errors(EP$`R/S`) = EP$`R/S error`

EP$`Stem diameter,cm` = EP$`Stem diameter,cm`/100
EP$`Stem radial increment` = EP$`Stem radial increment`/1000

EP$Biomass2 =   pi*EP$`Tree height, m`*1/3*(EP$`Stem radial increment`)*(EP$`Stem diameter,cm`+EP$`Stem radial increment`) * 0.5*1000

EP$Biomass2_errors = errors(EP$Biomass2)



EP$Biomass =  (pi/4*(EP$`Stem diameter,cm`^2-(EP$`Stem diameter,cm`-2*EP$`Stem radial increment`)^2)*EP$`Tree height, m`*EP$BCEF)*(1 +EP$`R/S`)*0.5

EP$Biomass_errors = errors(EP$Biomass)
