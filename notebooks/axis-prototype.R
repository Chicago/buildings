library(jsonlite)
library(leaflet)

spPolyDf <- readRDS("combinedGeoJSON.Rds")

spPolyDf$demo.alltime <- unlist(spPolyDf$demoBldg.alltime) +
  unlist(spPolyDf$demoPIN.alltime)
spPolyDf$vacant.alltime <- spPolyDf$vacant311Bldg.alltime + 
  spPolyDf$vacant311PIN.alltime
spPolyDf$tax.alltime <- unlist(spPolyDf$annual_sale.alltime) +
  unlist(spPolyDf$scavenger_sale.alltime)

bad_buildings <- spPolyDf[spPolyDf$demo.alltime > 0,]
bad_buildings$coordindates <- coordinates(bad_buildings)
bad_buildings <- SpatialPointsDataFrame(coords = bad_buildings$coordindates,
                                        bad_buildings@data)
tax_sales <- spPolyDf[spPolyDf$tax.alltime > 0 & spPolyDf$vacant.alltime == 0,]
vacant_calls <- spPolyDf[spPolyDf$vacant.alltime > 0 & spPolyDf$tax.alltime == 0,]
tax_and_vacant <- spPolyDf[spPolyDf$vacant.alltime > 0 & spPolyDf$tax.alltime > 0,]

popupDemo <- paste0(bad_buildings$f_add1, "-", bad_buildings$t_add1, " ", bad_buildings$pre_dir1,
                    " ", bad_buildings$st_name1, " ", bad_buildings$st_type1, "<br>",
                    bad_buildings$pins, "<br>", "Demolition Violations: ", bad_buildings$demo.alltime)
popupTax <- paste0(tax_sales$f_add1, "-", tax_sales$t_add1, " ", tax_sales$pre_dir1,
                   " ", tax_sales$st_name1, " ", tax_sales$st_type1, "<br>",
                   tax_sales$pins, "<br>", "Tax Delinquencies: ", tax_sales$tax.alltime)
popupVacant <- paste0(vacant_calls$f_add1, "-", vacant_calls$t_add1, " ", vacant_calls$pre_dir1,
                      " ", vacant_calls$st_name1, " ", vacant_calls$st_type1, "<br>",
                      vacant_calls$pins, "<br>", "Vacancy Complaints: ", vacant_calls$vacant.alltime)
popupTaxAndVacant <- paste0(tax_and_vacant$f_add1, "-", tax_and_vacant$t_add1, " ", tax_and_vacant$pre_dir1,
                            " ", tax_and_vacant$st_name1, " ", tax_and_vacant$st_type1, "<br>",
                            tax_and_vacant$pins, "<br>", "Vacancy Complaints: ", tax_and_vacant$vacant.alltime,
                            "<br>", "Tax Delinquencies: ", tax_sales$tax.alltime)