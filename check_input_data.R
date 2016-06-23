#tmp set wd on my local machine
setwd("C:/Users/ichernyak/Documents/GitHub/India_GtG/Process_for_PLEXOS_import/")

# Module to check transmission system data before exporting to PLEXOS
# This can run directly after input_params
library(data.table)
library(ggplot2)
library(openxlsx)

# create workbook with data check results
data.check.workbook <- createWorkbook()

#------------------------------------------------------------------------------|
# 1. Check InputFiles ----
#------------------------------------------------------------------------------|


# regions and zones
regions <- fread(file.path(inputfiles.dir, map.newregion.file))
zones <- fread(file.path(inputfiles.dir, map.newzone.file))

zones.and.regions <- merge(regions, zones, by = "BusNumber")
zones.and.regions <- unique(zones.and.regions[,.(ZoneName, RegionName)])
setkey(zones.and.regions,ZoneName)

## insert as a sheet in workbook
addWorksheet(data.check.workbook,"zones and regions")
writeData(data.check.workbook, sheet = "zones and regions",zones.and.regions)

# generators
## number of generators by fuel
generator.fuels <- fread(file.path(inputfiles.dir, map.gen.to.fuel.file))

fuel.table <- as.data.table(xtabs(~ Fuel, data = generator.fuels))

## insert as a sheet in workbook
addWorksheet(data.check.workbook,"number of generators")
writeData(data.check.workbook, sheet = "number of generators",fuel.table)

# check new RE generator properties
RE.scenarios <- data.table(file = sapply(RE.gen.file.list,function(x) x[1]),  ### this requires inputparams to load RE.gen.file.list
                           scenario = sapply(RE.gen.file.list,function(x) x[2]))

choose.scenario <- function(){
  options(width = 10)
  message("Choose an RE scenario to check.")
  print(unique(RE.scenarios$scenario),quote = FALSE)
  
  x <- readline("Enter a number to choose scenario: ") # stop code if none chosen?
  
  if(is.na(as.numeric(x))){
    stop("Enter a valid number to choose a scenario")
  }
  
  scenario.to.check <<- unique(RE.scenarios$scenario)[as.numeric(x)]
}

choose.scenario()

RE.file.paths <- RE.scenarios[scenario == scenario.to.check, file]

## stop and generate error if column names in RE input files do not match
RE.input <- data.table()
tryCatch(
  for(i in RE.file.paths){
    RE.input <- rbind(RE.input, fread(file.path(inputfiles.dir,i)))
  },
  error = function(err) {
    stop("Column names in RE InputFiles do not match: ",  
         paste(RE.file.paths, collapse="; "))
  }
)

## table - number of RE generators by fuel and region
REgens.table <- as.data.table(xtabs(~ Fuel + Node.Region, data = RE.input))

### insert as a sheet in workbook
addWorksheet(data.check.workbook,"Number of RE gens")
writeData(data.check.workbook, sheet = "Number of RE gens",REgens.table)

## boxplot of num units X max.capacity by fuel and region
RE.input[, units.max.capacity := Num.Units * Max.Capacity]
REgens.boxplots <- ggplot(data = RE.input,
                          aes(x = Node.Zone, y = units.max.capacity)) + 
  geom_boxplot(aes(fill = Fuel)) + 
 # theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  facet_wrap(~ Fuel, scales= "free_y")

### insert as a sheet in workbook
addWorksheet(data.check.workbook,"RE gens boxplots")
print(REgens.boxplots) #plot needs to be showing
insertPlot(data.check.workbook, "RE gens boxplots", width = 12, height = 6, 
           fileType = "png")

# build in check for standard column names in REgens inputfiles

# node properties 

# line properties 

# interface properties 

#------------------------------------------------------------------------------|
# 2. Check for isolated nodes ----
#------------------------------------------------------------------------------|




#------------------------------------------------------------------------------|
# 3. Post-compilation data check ----
#------------------------------------------------------------------------------|

# numer of generators, nodes, lines, interfaces, transformers 
# by region and total

# table - number of generators by fuel type and state



# box plots of generator capacity by fuel
gen.capacity <- generator.data.table[,.(ZoneName,RegionName,
                                        MaxOutput.MW,Units,Fuel)]
gen.capacity[,units.max.capacity := MaxOutput.MW * Units]

ggplot(data = gen.capacity, aes(x = factor(0), y = units.max.capacity)) + 
  geom_boxplot() +
  facet_wrap(~ Fuel, scale = "free_y") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## save in worksheet


# total capacity by fuel and region
ggplot(data = gen.capacity, aes(x = Fuel, y = units.max.capacity)) + 
  geom_bar(aes(fill = Fuel), stat = "identity") +
  facet_wrap(~ ZoneName, scale = "free_y") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) 
  

## save in worksheet


# box plots of transmission line reactances and resistances 
line.ratings <- line.data.table[,.(Reactance.pu,Resistance.pu,ACorDC,FromKV)]

## resistance
ggplot(data = line.ratings) + 
  geom_boxplot(aes(x = factor(0), y = Resistance.pu)) +
  facet_wrap(~ ACorDC + FromKV, scale = "free_y") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

## reactance
ggplot(data = line.ratings) +
  geom_boxplot(aes(x = factor(0), y = Reactance.pu)) +
  facet_wrap(~ ACorDC + FromKV, scale = "free_y") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


# Save workbook with all data check tables and plots
saveWorkbook(data.check.workbook, "PSSE2PLEXOS_data_check.xlsx", 
             overwrite = TRUE)
