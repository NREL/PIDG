

# 
# #------------------------------------------------------------------------------|
# # Error checking: input files ----
# #------------------------------------------------------------------------------|
# 
# #----make sure there are no duplicates in some of the mapping files:
# 
# # nodes -> regions: PLEXOS allows only one node per region
# if (rename.regions) { 
#   nodes.regions <- fread(file.path(inputfiles.dir, map.newregion.file))
#   if (any(nodes.regions[,length(RegionName) > 1, by = "node.number"][[2]])) {
#     print("WARNING: at least one node is assigned to more than one region.")
#   }
# }
# 
# # make sure each generator has only one fuel
# gens.fuels <- fread(file.path(inputfiles.dir, map.gen.to.fuel.file))
# if (any(gens.fuels[,length(Fuel) > 1, by = "Generator.Name"][[2]])) {
#   print("WARNING: at least one generator is assigned more than one fuel.")
# }
# 
# # make sure there are no fuel blanks
# if (any(gens.fuels[,Fuel == "" | is.na(Fuel)])) {
#   print("WARNING: at least one generator is assigned a blank fuel.")
# }
# 
# # # make sure each fuel has only one min gen
# # fuels.mingens <- fread(file.path(inputfiles.dir, min.gen.file))
# # if (any(fuels.mingens[,length(MinStableLevel) > 1, by = "Fuel"][[2]])) {
# #   print("WARNING: at least one fuel is assigned more than one min gen level.")
# # }
# # 
# # # make sure each fuel has only one ramp
# # fuels.rmps <- fread(file.path(inputfiles.dir, map.ramps.to.fuel.file))
# # if (any(fuels.rmps[,length(maxRamp) > 1, by = "Fuel"][[2]])) {
# #   print("WARNING: at least one fuel is assigned more than one max ramp.")
# # }
# 
# # # make sure each fuel has only one price
# # f.prce <-data.table(read.csv(file.path(inputfiles.dir, map.fuel.price.to.fuel.file), 
# #   stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE))
# # if (any(f.prce[,length(Price) > 1, by = "Fuel"][[2]])) {
# #   print("WARNING: at least one fuel is assigned more than one price.")
# # }
# 
# # # make sure each region has only one load file
# # f.prce <- fread(file.path(inputfiles.dir, map.region.to.load.file))
# # if (any(f.prce[,length(LoadFile) > 1, by = "RegionName"][[2]])) {
# #   print("WARNING: at least one region is assigned more than one load file.")
# # }
