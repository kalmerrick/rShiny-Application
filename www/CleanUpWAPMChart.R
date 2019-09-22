library(data.table)

setwd('X:/Strategic Policy and Evaluation/DASR/Justice Databases/Project Justice Chart Pack/R Shiny/JPRCDashboard')
data.WAPMData <- fread('www/Data/WAPMData.csv') # read in data
data.WAPMData <- melt(data.WAPMData, 'Category', variable.name = 'Month', value.name = 'Value') # melt data
data.WAPMData[, Month := as.Date(Month, "%d/%m/%Y")] # convert dates

# Extract Info
data.WAPMData[, Measure := substr(Category, 1, 3)]
data.WAPMData[, PrisonerType := substr(Category, 5, 6)]
data.WAPMData[, Gender := substr(Category, 8, 8)]
data.WAPMData[, IndigenousStatus := substr(Category, 10, 10)]
data.WAPMData[, ANZSOC := substr(Category, 12, 13)]
setcolorder(data.WAPMData, c('Month', 'Category', 'Measure', 'PrisonerType', 'Gender', 'IndigenousStatus', 'ANZSOC', 'Value'))

# Create All Gender Category
data.WAPMData.AllGender <- data.WAPMData[,.(Value = sum(Value)), by = .(Month, Measure, PrisonerType, IndigenousStatus, ANZSOC)]
data.WAPMData.AllGender[, Gender := 'All']
data.WAPMData.AllGender[, Category := '']
setcolorder(data.WAPMData.AllGender, c('Month', 'Category', 'Measure', 'PrisonerType', 'Gender', 'IndigenousStatus', 'ANZSOC', 'Value'))
data.WAPMData <- rbind(data.WAPMData, data.WAPMData.AllGender)

# Create All ATSI Category
data.WAPMData.AllATSI <- data.WAPMData[,.(Value = sum(Value)), by = .(Month, Measure, PrisonerType, Gender, ANZSOC)]
data.WAPMData.AllATSI[, IndigenousStatus := 'All']
data.WAPMData.AllATSI[, Category := '']
setcolorder(data.WAPMData.AllATSI, c('Month', 'Category', 'Measure', 'PrisonerType', 'Gender', 'IndigenousStatus', 'ANZSOC', 'Value'))
data.WAPMData <- rbind(data.WAPMData, data.WAPMData.AllATSI)

rm(data.WAPMData.AllGender, data.WAPMData.AllATSI)

MeasureMap <- data.table(
  Code = c('ARR', 'DEP', 'DAP', 'CPP', 'LOS'),
  Name = c('ARR - Arrivals', 'DEP - Departures', 'DAP - Daily Average Population', 'CPP - Census Population', 'LOS - Length of Stay')
)
PrisonerTypeMap <- data.table(
  Code = c('AL', 'SN', 'UN', 'FD'),
  Name = c('AL - All', 'SN - Sentenced', 'UN - Unsentenced', 'FD - Fine Default')
)
GenderMap <- data.table(
  Code = c('All', 'M', 'F'),
  Name = c('All', 'M - Male', 'F - Female')
)
IndigenousStatusMap <- data.table(
  Code = c('All', 'A', 'N'),
  Name = c('All', 'A - ATSI', 'N - Non-ATSI')
)
ANZSOCMap <- data.table(
  Code = c('00', paste0('0', 1:9), as.character(10:17), '99'),
  Name = c('00 - All', '01 - Homicide', '02 - Assault', '03 - Sexual Assault', '04 - Endangering Persons', '05 - Abduction, Harassment', '06 - Robbery', '07 - Burglary', '08 - Theft', '09 - Fraud', '10 - Drug', '11 - Weapons', '12 - Property', '13 - Public order', '14 - Traffic', '15 - Government', '16 - Miscellaneous', '17 - Remand', '99 - Unknown')
)