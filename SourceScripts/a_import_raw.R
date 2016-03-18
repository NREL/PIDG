## This script is intended to be able to read in a .raw data file from PSSE 
## (currently v31, but this should be able to be updated) and convert it to 
## PLEXOS-readable format that includes additional characteristics not contained 
## in the original PSSE file. 

#This file reads in .raw file, breaks it up into subtables (generators, buses, 
#lines, etc) according to the naming conventions in the .raw file. As written, 
#it ignores any tables that are empty in the .raw file. Then, the column names 
#are reset to match PSSE .raw file format of version 31. These should be redone 
#if updating to a different version of PSSE .raw files.

#uses (from master):
# raw.file.path

#------------------------------------------------------------------------------|
# read in .raw file ----
#------------------------------------------------------------------------------|

num_col <- max(count.fields(paste0("../InputFiles/", raw.file.path), sep = ','), 
  na.rm = TRUE)
raw.table <- data.table(suppressWarnings(read.csv(paste0("../InputFiles/", 
  raw.file.path), stringsAsFactors = FALSE, fill = TRUE, header=FALSE, 
  col.names = paste0("V",seq_len(num_col)), 
  strip.white = TRUE, blank.lines.skip = FALSE)))
# need to suppress warnings b/c read.csv doesn't like blanks in the file

# find indices of breaks between subtables
end.of.table <- "0 /End of"
end.of.tables.indices <- grep(end.of.table, raw.table[,V1])
end.of.header <- 3 #last non-data line
table.delimiters <- c(end.of.header, end.of.tables.indices)

# pull out subtables from big .raw table and list of tables with no data
names.vec <- c()
no.data.vec <- c()
names.strings <- c()
for (i in 2:length(table.delimiters)) {
  table.start.index <- table.delimiters[i-1]
  table.end.index <- table.delimiters[i]
    #pull name of sub-table in question
    #gets cell that says "0 /End of xxx data"
  data.in.row <- raw.table[[1]][table.end.index] 
 #remove "0 /End of " from beginning and " data" from end
  cur.table.name.data <- substr(data.in.row, 11, nchar(data.in.row) - 5)
  cur.table.name <- paste0(gsub(" |-", ".", cur.table.name.data), ".table")
  #skip subsetting if there is no data between beginning and end of the subtable
  if (table.start.index == (table.end.index - 1)) {
    no.data.vec <- c(no.data.vec, cur.table.name); next} 
    #if there is data, pull that data and assign it to correct variable
  assign(cur.table.name, raw.table[
    (table.start.index + 1):(table.end.index - 1),])
  names.vec <- c(names.vec, cur.table.name)
}
rm(table.start.index, table.end.index, data.in.row, cur.table.name, i)

# clean subtables - remove NA-only or blank-only columns
for (table.name in names.vec) {
  cur.table <- get(table.name)
  clean.table <- cur.table[, which(sapply(cur.table, function(x) {
    !(all(is.na(x)) | all(x==""))} )), with=F]
  assign(table.name, clean.table)
}
rm(cur.table, clean.table)

# clean subtables - change number columns to numeric and remove extra single 
# quotes or spaces from character cols
for (table.name in names.vec) {
  cur.table <- get(table.name)
  cleaner.table <- cur.table[, lapply(.SD, function(x) {
    if (!is.na(suppressWarnings(as.numeric(x[1])))) {
      suppressWarnings(as.numeric(x))} else {
        gsub(" |'|\"", "", x)}})] #will throw warning if it reads in an NA
  assign(table.name, cleaner.table)
}
rm(cur.table, cleaner.table)

# print names.vec to see names of all tables that have just been created

#------------------------------------------------------------------------------|
# Misc. ----
#------------------------------------------------------------------------------|
# read in sbase to plexos mva base to put in transmission object attributes later 
# (PSSE version-specific)
mva.base <- as.numeric(raw.table[1,V2])

#------------------------------------------------------------------------------|
# modify columns (PSSE version-specific) ----
#------------------------------------------------------------------------------|
# Change column names in each table to what they correspond to, according to 
# PSSE documentation

#Bus.table
#1=load bus/passive node
#2=generator or plant bus
#3=swing bus
#4=disconnected bus
#Note: PSSE uses the term "Area" in this table, but this code changes that to 
#"Region" for consistency with Plexos. 
setnames(Bus.table, colnames(Bus.table), c("BusNumber", "BusName", "Voltage.kV", 
  "BusTypeCode", "Region", "Zone", "Owner", "BusVoltageMagnitude.pu", 
      "BusVoltageAngle.degrees"))

#Load.table, p.5-9
#LoadTypeCode if there are multiple loads are bus
#LoadStatus == 1 if in-service and == 0 for out-of-service
#Note: PSSE uses the term "Area" in this table, but this code changes that to 
#"Region" for consistency with Plexos. 
setnames(Load.table, colnames(Load.table), c("BusNumber", "LoadTypeCode", 
  "Status", "Region", "Zone", "ActivePower.MW", "ReactivePower.MVAR", 
  "ActivePowerOfConstCurrent.MW", "ReactivePowerOfConstCurrent.MVAR", 
  "ActivePowerOfConstAdmittance.MW", "ReactivePowerOfConstAdmittance.MVAR", 
  "Owner"))

#Fixed.shunt.table
#see manual
setnames(Fixed.shunt.table, colnames(Fixed.shunt.table), c("BusNumber", "ID", 
  "Status", "ActiveCompShuntAdmToGrnd.MW", "ReactiveCompShuntAdmToGrnd.MVAR"))

#Generator.table, p.5-13
#OtherBusReg = number of bus who voltage is regulated by this gen (otherwise 
#will regulate its own)
#WindControlMode = 0 - not wind; 1 - reactive power limits are specified here; 
#2, 3 - reactive power limits calculated
setnames(Generator.table, colnames(Generator.table), c("BusNumber", "ID", 
  "ActivePower.MW", "ReactivePower.MVAR", "MaxReactivePowerOutput.MVAR", 
  "MinReactivePowerOutput.MVAR", "VoltageSetpoint.pu", "OtherBusReg", "MVA", 
  "IMpedance1", "Impedance2", "XfrmrImpedance1", "XfrmrImpedance2", 
  "XfrmrTurnsRatio", "Status", "PctMVARToHoldVoltage", "MaxOutput.MW", 
  "MinOutput.MW", "Owner", "FractionOfOwnership", "WindControlMode", 
  "WindPowerFactor"))

#Branch.table
# RatingA is technical limit (not important here), RatingB is thermal limit, 
# RatingC is overload limit
setnames(Branch.table, colnames(Branch.table), c("BranchFromBus", "BranchToBus", 
  "ID", "Resistance.pu", "Reactance.pu","ChargingSusceptance.pu", "RatingA",
  "RatingB","RatingC", "FromBusAdmittanceReal.pu","FromBusAdmittanceImag.pu", 
  "ToBusAdmittanceReal.pu","ToBusAdmittanceImag.pu", "Status", "MeteredEnd", 
       "Length", "Owner", "FractionOfOwnership", "Owner2", 
  "FractionOfOwnership2"))

#Area.interchange.table
#Note: PSSE uses the term "Area" in this table, but this code changes that to 
#"Region" for consistency with Plexos. 
setnames(Area.interchange.table, colnames(Area.interchange.table), c("Region", 
  "SlackBusNumber", "DesiredNetInterchange.MW", "InterchangeTolerance.MW", 
  "RegionName"))

#Owner.table
setnames(Owner.table, colnames(Owner.table), c("Owner", "OwnerName"))

#Zone.table
setnames(Zone.table, colnames(Zone.table), c("Zone", "ZoneName"))

#Transformer.table, p.5-22
  #Not sure how to do this elegantly. Transformers can be two- or three-winding 
  #transformers, and each transformer entry will have a different number of 
  #lines accordingly (4 or 5, respectively. Go through and separate these out 
  #because it's otherwise very difficult to pull out data.) 
  #all transformers here are 2-winding, so I'm commenting this out. Leaving the 
  #code here for now in case we need it later.
#two.winding.txfmrs <- data.table(matrix(NA, ncol = 16, nrow = 0)); 
#three.winding.txfmrs <- data.table(matrix(NA, ncol = 16, nrow = 0))
#two.count <- 0; three.count <- 0
#i <- 1
#while (i <= nrow(Transformer.table)) {
#  if (is.na(Transformer.table[i,V3])) {i <- i + 1} else if (
#  Transformer.table[i,V3] == 0) { #2 windings (4 lines)
#    #two.winding.txfmrs <- rbind.fill(two.winding.txfmrs, 
#    Transformer.table[i:(i+3),]) 
#    #uncomment this to separate two-winding transformers into another table
#    i <- i + 4; two.count <- two.count + 1
#  } else {#3 windings (5 lines)
#    #rbind.fill(three.winding.txfmrs, Transformer.table[i:(i+4),]) 
#    #uncomment this to separate three-winding transformers into another table
#    i <- i + 5; three.count <- three.count + 1
#    }
#}
#if (three.count > 0) {print("WARNING: three-winding transformers exist in this 
#data set and are not properly dealt with. Please re-code.")}

  #there is a lot of data in the transformer tables. This currently takes only 
  #the data that Plexos seems to be reading in. There are three ratings
  #but Plexos seems to only read the first and third. Should check this, 
  #especially because some of the third ratings are zero.
Transformer.table.edit <- Transformer.table[, .(V1)][
  1:nrow(Transformer.table) %% 4 == 1]
setnames(Transformer.table.edit, "V1", "FromBusNumber")

Transformer.table.edit[,ToBusNumber := Transformer.table[, .(V2)][
  1:nrow(Transformer.table) %% 4 == 1]] 
Transformer.table.edit[,ID := Transformer.table[, .(V4)][
  1:nrow(Transformer.table) %% 4 == 1]] 
Transformer.table.edit[,Status := Transformer.table[, .(V12)][
  1:nrow(Transformer.table) %% 4 == 1]] 

Transformer.table.edit[,Resistance.pu := Transformer.table[, .(V1)][
  1:nrow(Transformer.table) %% 4 == 2]] 
Transformer.table.edit[,Reactance.pu := Transformer.table[, .(V2)][
  1:nrow(Transformer.table) %% 4 == 2]] 

Transformer.table.edit[,Rating.MW := Transformer.table[, .(as.numeric(V4))][
  1:nrow(Transformer.table) %% 4 == 3]] 
Transformer.table.edit[,OverloadRating.MW := Transformer.table[, .(V6)][
  1:nrow(Transformer.table) %% 4 == 3]] 
  #This results in some of the overload ratings being 0. Change them to the 
  #Rating. This applies to ~ half of the entries
#Transformer.table.edit[OverloadRating.MW == 0, OverloadRating.MW := Rating.MW]

# Two.terminal.dc.line.table
  #this table has three lines of data per DC line
DC.line.table <- Two.terminal.dc.line.table[, .(V1)][
  1:nrow(Two.terminal.dc.line.table) %% 3 == 2]
setnames(DC.line.table, "V1", "FromBusNumber")

DC.line.table[,ToBusNumber := Two.terminal.dc.line.table[, .(V1)][
  1:nrow(Two.terminal.dc.line.table) %% 3 == 0]]
DC.line.table[,ID := Two.terminal.dc.line.table[, .(V16)][
  1:nrow(Two.terminal.dc.line.table) %% 3 == 2]]
DC.line.table[,Resistance.pu := (Two.terminal.dc.line.table[, .(V3)][
  1:nrow(Two.terminal.dc.line.table) %% 3 == 1])/mva.base] 
#^^converting from Ohms to pu based on MVA base of 100. ??
DC.line.table[,MaxFlow.MW := Two.terminal.dc.line.table[, .(V4)][
  1:nrow(Two.terminal.dc.line.table) %% 3 == 1]]
DC.line.table[,LineName := Two.terminal.dc.line.table[, .(V1)][
  1:nrow(Two.terminal.dc.line.table) %% 3 == 1]]


#currently ignoring: 
# Switched.shunt.table


#------------------------------------------------------------------------------|
# Error checking: input files ----
#------------------------------------------------------------------------------|

#----make sure there are no duplicates in some of the mapping files:

# nodes -> regions: PLEXOS allows only one node per region
if (rename.regions) { 
  nodes.regions <- fread(file.path("../InputFiles", map.newregion.file))
  if (any(nodes.regions[,length(RegionName) > 1, by = "BusNumber"][[2]])) {
    print("WARNING: at least one node is assigned to more than one region.")
  }
}

# make sure each generator has only one fuel
gens.fuels <- fread(file.path("../InputFiles", map.gen.to.fuel.file))
if (any(gens.fuels[,length(Fuel) > 1, by = "Generator.Name"][[2]])) {
  print("WARNING: at least one generator is assigned more than one fuel.")
}

# make sure there are no fuel blanks
if (any(gens.fuels[,Fuel == "" | is.na(Fuel)])) {
  print("WARNING: at least one generator is assigned a blank fuel.")
}

# # make sure each fuel has only one min gen
# fuels.mingens <- fread(file.path("../InputFiles", min.gen.file))
# if (any(fuels.mingens[,length(MinStableLevel) > 1, by = "Fuel"][[2]])) {
#   print("WARNING: at least one fuel is assigned more than one min gen level.")
# }
# 
# # make sure each fuel has only one ramp
# fuels.rmps <- fread(file.path("../InputFiles", map.ramps.to.fuel.file))
# if (any(fuels.rmps[,length(maxRamp) > 1, by = "Fuel"][[2]])) {
#   print("WARNING: at least one fuel is assigned more than one max ramp.")
# }

# # make sure each fuel has only one price
# f.prce <-data.table(read.csv(paste0("../InputFiles/", map.fuel.price.to.fuel.file), 
#   stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE))
# if (any(f.prce[,length(Price) > 1, by = "Fuel"][[2]])) {
#   print("WARNING: at least one fuel is assigned more than one price.")
# }

# make sure each region has only one load file
f.prce <- fread(file.path("../InputFiles", map.region.to.load.RE.file))
if (any(f.prce[,length(LoadFile) > 1, by = "RegionName"][[2]])) {
  print("WARNING: at least one region is assigned more than one load file.")
}

