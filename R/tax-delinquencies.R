# ---------------------------------------------------------------------------- #
#  Import all tax sale data
# ---------------------------------------------------------------------------- #

# annual_sale <-
#   read.socrata("https://datacatalog.cookcountyil.gov/resource/bxb3-vf6m.json")
# scavenger_sale <-
#   read.socrata("https://datacatalog.cookcountyil.gov/resource/i7u4-3rbi.json")

# ---------------------------------------------------------------------------- #
#  Clean PINs
# ---------------------------------------------------------------------------- #

# annual_sale$pin <- gsub("-", "", annual_sale$pin)
# annual_sale$pin <- str_sub(annual_sale$pin, 0, -5) # convert to 10 digit PINs
# scavenger_sale$pin <- gsub("-", "", scavenger_sale$pin)
# scavenger_sale$pin <- str_sub(scavenger_sale$pin, 0, -5) # convert to 10 digit PINs
# tax_sales <- rbind(annual_sale[,c("pin", "tax_sale_year")],
#                    scavenger_sale[c("pin", "tax_sale_year")])

# ---------------------------------------------------------------------------- #
#  Save/Read Rds File
# ---------------------------------------------------------------------------- #


# saveRDS(tax_sales, "data/Rds/tax-sales.Rds")
tax_sales <- readRDS("data/Rds/tax-sales.Rds")

