setwd("//media//kswada//MyFiles//R//magazine_prices")

packages <- c("dplyr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)



# ------------------------------------------------------------------------------
# data:  Magazine Prices
#   - Cecchetti (1986) analyzes price changes, with an application to magazines.
#     His analysis is replicated (and criticized) by Willis (1986).
#     Price changes are costly for two reasons:
#        - changing prices induce administrative costs
#        - in a monopolistic competition context, increasing prices will lead to a loss of customers
#     For these two reasons, there is a difference between the optimal price of a good for a given period (p*(nt)) and the actual price (p(nt))
#     A price change will occur only if the gap between the two becomes greater than a given threshold. More formally, the price will change if:
#        - ln (p*(nt) / p(nt)) > hc(nt)    hc(nt):  minimum relative gap between the optimal and the actual price that would result in a price change.
#        - If the price changes, given the infrequency of price changes, the enterprise will set its new price above the optimal price, the relative diffence
#          being equal ot ho(nt)
#   - "length":  the length of period since tha last price change
# ------------------------------------------------------------------------------


data("MagazinePrices", package = "pder")


str(MagazinePrices)


dim(MagazinePrices)


car::some(MagazinePrices)




# ------------------------------------------------------------------------------
# basics:  
# ------------------------------------------------------------------------------


