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

# read in files
num.cols <- max(count.fields(file.path(raw.file), sep = ','), na.rm = TRUE)

# can't use fread b/c not a regular file (diff rows have diff num cols)
# need to suppress warnings b/c read.csv doesn't like blanks in the file
raw.table <- data.table(
             suppressWarnings(
             read.csv(raw.file, 
                      stringsAsFactors = FALSE, 
                      fill = TRUE, 
                      header=FALSE, 
                      col.names = paste0("V", seq_len(num.cols)),
                      strip.white = TRUE, 
                      blank.lines.skip = FALSE)))

# clean up
rm(num.cols)

#------------------------------------------------------------------------------|
# start parsing ----
#------------------------------------------------------------------------------|

## find indices of table beginnings (row before each subtable begins)
header.end <- 3
table.ends <- grep("0 /End of", raw.table[,V1])

table.delims <- c(header.end, table.ends)

# clean up
rm(header.end, table.ends)


## pull out sub-tables from big .raw table and list of tables with no data
no.data.vec <- c()

for (i in 2:length(table.delims)) {
    
    start.index <- table.delims[i - 1]
    end.index <- table.delims[i]
    
    # get and clean sub-table name
    data.name <- raw.table[end.index, V1] 
    data.name <- gsub("0 /End of | data", "", data.name)
    data.name <- gsub(" |-", ".", data.name)
    data.name <- paste0(data.name, ".table")
    
    # skip if sub-table has no data
    if (start.index == (end.index - 1)) {
        no.data.vec <- c(no.data.vec, data.name)
        next} 
    
    # otherwise, pull data
    sub.table <- raw.table[(start.index + 1):(end.index - 1),]
    
    # clean sub-table: remove blank columns
    all.blank  <- sub.table[, sapply(.SD, function(x) all(x == "" | is.na(x)))]
    all.blank <- names(all.blank[all.blank])
    
    sub.table[, (all.blank) := NULL] 

    # clean sub-table: change "numeric" cols to numeric (suppress NA-related 
    # warnings) and remove extra quotes and spaces from character cols
    sub.table <- sub.table[, lapply(.SD, function(x) {
                if (!is.na(suppressWarnings(as.numeric(x[1])))) {
                     suppressWarnings(as.numeric(x)) } else {
                         gsub(" |'|\"", "", x)}})] 
    
    # save table with correct name
    assign(data.name, sub.table)
    
}

# clean up 
rm(start.index, end.index, table.delims, data.name, i, sub.table, all.blank)


#------------------------------------------------------------------------------|
# Misc. ----
#------------------------------------------------------------------------------|
# read in sbase to plexos mva base to put in transmission object attributes later 
# (PSSE version-specific)
mva.base <- as.numeric(raw.table[1,V2])

#------------------------------------------------------------------------------|
# modify columns (PSSE version-specific) ----
#------------------------------------------------------------------------------|
# change column names in each table to what they correspond to, according to 
# PSSE documentation. also change terminology from psse to plexos (i.e. 'bus' 
# to 'node'. etc)

# Bus.table
if (exists('Bus.table')) {
    
  setnames(Bus.table, 
           colnames(Bus.table), 
           c("node.number", "node.name", "kV", "bus.type", 
             "region.number", "zone.number", "owner.number", 
             "voltage.mag.pu", "voltage.angle.deg"))
} else {
    
  message("No Bus Table exists ... skipping")
}

# Load.table, p.5-9
if (exists('Load.table')) {
    
  setnames(Load.table, 
           colnames(Load.table), 
           c("node.number", "load.type", "status", "region", "zone", 
             "active.power.MW", "reactive.power.MVAR", 
             "active.power.const.current.MW", 
             "reactive.power.const.current.MVAR", 
             "active.power.const.admittance.MW", 
             "reactive.power.const.admittance.MVAR", "owner.number"))
} else {
    
  message("No Load Table exists ... skipping")
}


# Fixed.shunt.table
if (exists('Fixed.shunt.table')) {
    
  setnames(Fixed.shunt.table, 
           colnames(Fixed.shunt.table), 
           c("node.number", "id", "status", 
             "active.comp.shunt.adm.to.grnd.MW", 
             "reactive.comp.shunt.adm.to.grnd.MVAR"))
} else {
    
  message("No Fixed Shunt Table exists ... skipping")
}


# Generator.table, p.5-13
if (exists('Generator.table')) {
    
  generator.tablenames <- c("node.number", "id", 
           "active.power.MW", "reactive.power.MVAR", 
           "max.reactive.power.MVAR","min.reactive.power.MVAR", 
           "voltage.setpoint.pu", "other.bus.reg", "MVA", 
           "impedance1", "impedance2", "xfrmr.impedance1", "xfrmr.impedance2", 
           "xfrmr.turns.ratio", "status", "pct.MVAR.to.hold.voltage", 
           "max.capacity.MW", "min.output.MW", "wind.control.mode", 
           "wind.power.factor")
  
  # add as many owner.number/owner.fraction colnames as needed
  length.diff <- length(names(Generator.table)) - length(generator.tablenames)
  
  if (length.diff > 0) {
    pairs.to.add <- length.diff/2
    
    before <- generator.tablenames[1:(length(generator.tablenames) - 2)]
    after <- generator.tablenames[!(generator.tablenames %in% before)]
    
    add <- c()
    for (i in seq(pairs.to.add)) add <- c(add, 
      paste0('owner.number', i), paste0('owner.fraction', i))
    
    generator.tablenames <- c(before, add, after)
  }
  
  setnames(Generator.table, colnames(Generator.table), generator.tablenames)
  
} else {
  message("No Gen Table exists ... skipping")
}

# clean up
rm(i, length.diff, pairs.to.add, before, after, add, generator.tablenames)

# Branch.table
# RatingA is technical limit (not important here), RatingB is thermal limit, 
# RatingC is overload limit
if (exists('Branch.table')) {
    
  branch.tablenames = c("node.from.number", "node.to.number", 
                      "id", "resistance.pu", "reactance.pu",
                      "charging.susceptance.pu", 
                      "ratingA","ratingB","ratingC",
                      "node.from.admittance.real.pu",
                      "node.from.admittance.imag.pu",
                      "node.to.admittance.real.pu",
                      "node.to.admittance.imag.pu",
                      "status", "metered.end", "length")
  
    length.diff <- length(names(Branch.table)) - length(branch.tablenames)
    
    if (length.diff > 0) {
    
        pairs.to.add <- length.diff/2
        
        before <- branch.tablenames[1:(length(branch.tablenames) - 2)]
        after <- branch.tablenames[!(branch.tablenames %in% before)]
    
        add <- c()
        for (i in seq(pairs.to.add)) add <- c(add, 
            paste0('owner.number', i), paste0('owner.fraction', i))
    
            branch.tablenames <- c(before, add, after)
        }
  
    setnames(Branch.table, colnames(Branch.table), branch.tablenames)

}  else {
    
  message("No Branch Table exists ... skipping")
}

# clean up
rm(i, length.diff, pairs.to.add, before, after, add, branch.tablenames)



# Area.interchange.table
# Note: PSSE uses the term "Area" in this table, but this code changes that to 
# "Region" for consistency with Plexos.
if (exists('Area.interchange.table')){
    
  setnames(Area.interchange.table, 
           colnames(Area.interchange.table), 
           c("region.number", "slacknode.number", 
             "desired.net.interchange.MW", 
             "interchange.tolerance.MW", "region.name"))
} else {
    
  message("No Area Interchange Table exists ... skipping")
}


# Owner.table
if (exists('Owner.table')) {
    
  setnames(Owner.table, colnames(Owner.table), c("owner.number", "owner.name"))
} else {
    
  message("No Owner Table exists ... skipping")
}

# Zone.table
if (exists('Zone.table')) {
    
  setnames(Zone.table, colnames(Zone.table), c("zone.number", "zone.name"))
} else {
    
  message("No Zone Table exists ... skipping")
}


# Transformer.table, p.5-22
# This code separates 2- and 3- winding transformers (4 and 5 lines of data, 
# respectively) and is very slow. only run if needed. 
# two.winding.txfmrs <- data.table(matrix(NA, ncol = 16, nrow = 0)); 
# three.winding.txfmrs <- data.table(matrix(NA, ncol = 16, nrow = 0))
# two.count <- 0; three.count <- 0
# i <- 1
# while (i <= nrow(Transformer.table)) {
#   if (is.na(Transformer.table[i,V3])) {i <- i + 1} else if (
#   Transformer.table[i,V3] == 0) { #2 windings (4 lines)
#     #two.winding.txfmrs <- rbind.fill(two.winding.txfmrs, 
#                                       Transformer.table[i:(i+3),]) 
#     #uncomment this to separate two-winding transformers into another table
#     i <- i + 4; two.count <- two.count + 1
#   } else {#3 windings (5 lines)
#     #rbind.fill(three.winding.txfmrs, Transformer.table[i:(i+4),]) 
#     #uncomment this to separate three-winding transformers into another table
#     i <- i + 5; three.count <- three.count + 1
#     }
# }
# if (three.count > 0) {print("WARNING: three-winding transformers exist in this 
# data set and are not properly dealt with. Please re-code.")}
 

# Transformer.table - only handles 2-winding transformers. also only pulls data
# used by plexos.
Transformer.table[,i := 1:.N]

# create empty table to populate 
Transformer.table.v2 <- data.table(node.from.number = 
                                   numeric(length = nrow(Transformer.table)/4))

Transformer.table.v2$node.from.number <- Transformer.table[i %% 4 == 1, .(V1)]
Transformer.table.v2$node.to.number   <- Transformer.table[i %% 4 == 1, .(V2)]
Transformer.table.v2$id               <- Transformer.table[i %% 4 == 1, .(V4)]
Transformer.table.v2$status           <- Transformer.table[i %% 4 == 1, .(V12)]
Transformer.table.v2$resistance.pu    <- Transformer.table[i %% 4 == 2, .(V1)]
Transformer.table.v2$reactance.pu     <- Transformer.table[i %% 4 == 2, .(V2)]
Transformer.table.v2$rating.MW        <- Transformer.table[i %% 4 == 3, .(V4)]
Transformer.table.v2$overload.rating.MW <- Transformer.table[i %% 4 == 3, .(V6)] 

Transformer.table <- Transformer.table.v2

# clean up
rm(Transformer.table.v2)

# Two.terminal.dc.line.table
# this table has three lines of data per DC line
Two.terminal.dc.line.table[,i := 1:.N]

# create empty table to populate
DC.line.table <- data.table(node.from.number = 
                            numeric(length = nrow(Two.terminal.dc.line.table)/3))

DC.line.table$node.from.number <- Two.terminal.dc.line.table[i %% 3 == 2, .(V1)]
DC.line.table$node.to.number   <- Two.terminal.dc.line.table[i %% 3 == 0, .(V1)]
DC.line.table$id               <- Two.terminal.dc.line.table[i %% 3 == 2, .(V16)]
DC.line.table$resistance.pu    <- Two.terminal.dc.line.table[i %% 3 == 1, .(V3/mva.base)]
DC.line.table$max.flow.MW      <- Two.terminal.dc.line.table[i %% 3 == 1, .(V4)]
DC.line.table$id.num           <- Two.terminal.dc.line.table[i %% 3 == 1, .(V1)]


#------------------------------------------------------------------------------|
# Error checking: input files ----
#------------------------------------------------------------------------------|

#----make sure there are no duplicates in some of the mapping files:

# nodes -> regions: PLEXOS allows only one node per region
if (rename.regions) { 
  nodes.regions <- fread(file.path(inputfiles.dir, map.newregion.file))
  if (any(nodes.regions[,length(RegionName) > 1, by = "BusNumber"][[2]])) {
    print("WARNING: at least one node is assigned to more than one region.")
  }
}

# make sure each generator has only one fuel
gens.fuels <- fread(file.path(inputfiles.dir, map.gen.to.fuel.file))
if (any(gens.fuels[,length(Fuel) > 1, by = "Generator.Name"][[2]])) {
  print("WARNING: at least one generator is assigned more than one fuel.")
}

# make sure there are no fuel blanks
if (any(gens.fuels[,Fuel == "" | is.na(Fuel)])) {
  print("WARNING: at least one generator is assigned a blank fuel.")
}

# # make sure each fuel has only one min gen
# fuels.mingens <- fread(file.path(inputfiles.dir, min.gen.file))
# if (any(fuels.mingens[,length(MinStableLevel) > 1, by = "Fuel"][[2]])) {
#   print("WARNING: at least one fuel is assigned more than one min gen level.")
# }
# 
# # make sure each fuel has only one ramp
# fuels.rmps <- fread(file.path(inputfiles.dir, map.ramps.to.fuel.file))
# if (any(fuels.rmps[,length(maxRamp) > 1, by = "Fuel"][[2]])) {
#   print("WARNING: at least one fuel is assigned more than one max ramp.")
# }

# # make sure each fuel has only one price
# f.prce <-data.table(read.csv(file.path(inputfiles.dir, map.fuel.price.to.fuel.file), 
#   stringsAsFactors = FALSE, check.names = FALSE, strip.white = TRUE))
# if (any(f.prce[,length(Price) > 1, by = "Fuel"][[2]])) {
#   print("WARNING: at least one fuel is assigned more than one price.")
# }

# # make sure each region has only one load file
# f.prce <- fread(file.path(inputfiles.dir, map.region.to.load.file))
# if (any(f.prce[,length(LoadFile) > 1, by = "RegionName"][[2]])) {
#   print("WARNING: at least one region is assigned more than one load file.")
# }

