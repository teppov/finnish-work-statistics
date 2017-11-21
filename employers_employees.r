library( pxweb )
library( dplyr )
library( tidyr )


### StatFin >> Työmarkkinat >> Työvoimatutkimus >> 004 -- Palkansaajat ja yrittäjät, 15-74-vuotiaat
### http://pxnet2.stat.fi/PXWeb/pxweb/fi/StatFin/StatFin__tym__tyti/statfin_tyti_pxt_004.px/?rxid=34171367-a303-4674-b19b-5c7ea280cb59
###########################################################

employers_employees <-
    get_pxweb_data( url  = "http://pxnet2.stat.fi/PXWeb/api/v1/fi/StatFin/tym/tyti/statfin_tyti_pxt_004.px",
                    dims = list( Ajanjakso = c( '**' ),
                                 Sukupuoli = c( 'S' ),
                                 Tiedot    = c( 'Tyolliset',
                                                'Palkansaaja',
                                                'Yrittaja_pl_mmt',
                                                'Tyonantajayrittaja_pl_mmt',
                                                'Yksinyrittaja_pl_mmt' ),
                                 Vuosi     = c( '*' ) ),
                    clean = TRUE ) # "Clean and melt the data to R format."

# Drop unnecessary columns
ee <- employers_employees[-(1:2)]

# Remove ", 1000 henkeä" from 'Tiedot'
ee$Tiedot <- gsub( ', 1000 henkeä', '', ee$Tiedot )
# Replace info within "(...)" with "*"
# https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r
ee$Tiedot <- gsub( '\\([^\\)]+\\)', '*', ee$Tiedot )

# Spread years to columns
ee <- spread( ee, Vuosi, values )

# Store for later use and write to CSV
ee_table <- ee
write.table( ee_table,
             file = paste0( png_dir, 'employers_employees.csv' ),
             row.names = FALSE,
             sep  = ';' )

# Remove possibly empty (all 'NA') columns
# (especially the current year might not have data)
ee <- Filter( function( x ) !all( is.na( x ) ), ee )

# Year 2000 = 100 (omit first column with the info texts)
ee[-(1)] <- round( ( ee[-(1)] / ee$`2000`[row( ee[-( 1 )] )] ) * 100, digits = 2 )

# Store for later use and write to CSV
ee_table_2000is100 <- ee
write.table( ee_table_2000is100,
             file = paste0( png_dir, 'employers_employees_2000-is-100.csv' ),
             row.names = FALSE,
             sep  = ';' )

# Gather back to long format
ee <- gather( ee, Tiedot, 'Indeksi' )
colnames( ee )[1] <- 'Ryhmat'
colnames( ee )[2] <- 'Vuosi'

# Use only data from 2000 and later
ee <- ee[ee$Vuosi >= 2000, ]

# Change the ordering of the "Tiedot" factor
# https://stackoverflow.com/questions/1296646/how-to-sort-a-dataframe-by-columns
# https://stackoverflow.com/questions/10074483/r-how-to-reorder-legend-key-in-ggplot2-line-plot-to-match-the-final-values-in-e
lastvalues = ee[ee$Vuosi == max( ee$Vuosi ), ]
lastvalues = lastvalues[with( lastvalues, order( -Indeksi, Ryhmat ) ), ]
ee$Ryhmät <- factor( ee$Ryhmat,
                     levels = lastvalues$Ryhmat,
                     ordered = TRUE )
