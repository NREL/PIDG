# a-parse-psse.R 
# 
# ----
# 
# reads in a psse file, parses tables contained in the file, renames columns
# according to psse file version, and writes out tables
# 
# inputs: 
#   * one .raw data file containing psse system information
#   * location where outputs should be saved
# outputs: 
#   * all renamed tables in the file as separate csv files
#   * metadata (original file name, psse version, mva base, names of tables, 
#       empty tables, skipped tables, size of tables)
#   
# constraints and assumptions:
#   * assumes psse version 31. to change this, alter the column-renaming step
#   * assumes 3 non-data lines (see variable header.end)
#   * assumes that only 2-winding transformers exist. some inefficient code is 
#       provided for separating 2- and 3-winding transformers (see below), 
#       but cleaning 3-winding transformer data needs to be written 
#


# #------------------------------------------------------------------------------|
# # fake user inputs (for devel) ----
# #------------------------------------------------------------------------------|
# 
# root.dir <- "~/GitHub/India_GtG/Process_for_PLEXOS_import/PSSE2PLEXOS/Update"
# 
# raw.file <- file.path(root.dir, "inputs/Base Case_2021-22-Peak-Demand_edit.raw")
# output.dir <- file.path(root.dir, "outputs_a-1_raw_psse")
# 

# #------------------------------------------------------------------------------|
# # setup ----
# #------------------------------------------------------------------------------|
# 
# # load packages
# pacman::p_load(data.table)
# 
# # make sure output.dir exists
# if (!dir.exists(output.dir)) {
#     dir.create(output.dir, recursive = TRUE)
# }



# read in files
num.cols <- max(count.fields(file.path(inputfiles.dir, raw.file.path), 
                             sep = ','), na.rm = TRUE)

# can't use fread b/c not a regular file (diff rows have diff num cols)
# need to suppress warnings b/c read.csv doesn't like blanks in the file
raw.table <- data.table(
             suppressWarnings(
             read.csv(file.path(inputfiles.dir, raw.file.path), 
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
    
    if (length(all.blank) > 0) {
        sub.table[, (all.blank) := NULL]     
    }
    
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
# Gather some metadata ----
#------------------------------------------------------------------------------|

mva.base <- as.numeric(raw.table[1, V2])

psse.version <- as.numeric(raw.table[1, V3])

# no.data.vec


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


# # Fixed.shunt.table
# if (exists('Fixed.shunt.table')) {
#     
#   setnames(Fixed.shunt.table, 
#            colnames(Fixed.shunt.table), 
#            c("node.number", "id", "status", 
#              "active.comp.shunt.adm.to.grnd.MW", 
#              "reactive.comp.shunt.adm.to.grnd.MVAR"))
# } else {
#     
#   message("No Fixed Shunt Table exists ... skipping")
# }


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

if (exists('Transformer.table')) {

  # first seperate two- and three-winding transformers
  Transformer.table[V1 %in% Bus.table[,node.number] & V3 %in% Bus.table[,node.number],
                    winding:=3]
  Transformer.table[V1 %in% Bus.table[,node.number] & V3 ==0,
                    winding:=2]
  Transformer.table[,winding:=na.locf(winding)]
  
  Transformer.table.2wind = Transformer.table[winding == 2]
  Transformer.table.2wind[,i := 1:.N]
  Transformer.table.3wind = Transformer.table[winding == 3]
  Transformer.table.3wind[,i := 1:.N]
  
  if(nrow(Transformer.table.2wind[i%%4 == 1 & V6!=1])>0){
    message(paste0('two-winding transformer impedance data sometimes specified in ',
                   'irregular units. refer to PSSE users manual'))
  }
  
  if(nrow(Transformer.table.3wind[i%%5 == 1 & V6!=1])>0){
    message(paste0('three-winding transformer impedance data sometimes specified in ',
                   'irregular units. refer to PSSE users manual'))
  }
  
  if(nrow(Transformer.table.3wind[i%%5 == 1 & !(V12 %in% c(0,1))])){
    message(paste0('three-winding transformers sometimes have irregular statuses. refer ',
                   'to PSSE users manual'))
  }
  
  # two-winding transformers from node 1 to node 2
  Transformer.table.2wind_1_2 <- data.table(node.from.number = 
                                              numeric(length = nrow(Transformer.table.2wind)/4))
  
  Transformer.table.2wind_1_2$node.from.number <- Transformer.table.2wind[i %% 4 == 1, .(V1)]
  Transformer.table.2wind_1_2$node.to.number   <- Transformer.table.2wind[i %% 4 == 1, .(V2)]
  Transformer.table.2wind_1_2$id               <- Transformer.table.2wind[i %% 4 == 1, .(V4)]
  Transformer.table.2wind_1_2$status           <- Transformer.table.2wind[i %% 4 == 1, .(V12)]
  Transformer.table.2wind_1_2$resistance.pu    <- Transformer.table.2wind[i %% 4 == 2, .(V1)]
  Transformer.table.2wind_1_2$reactance.pu     <- Transformer.table.2wind[i %% 4 == 2, .(V2)]
  Transformer.table.2wind_1_2$rating.MW        <- as.numeric(Transformer.table.2wind[i %% 4 == 3, V4])
  Transformer.table.2wind_1_2$overload.rating.MW <- as.numeric(Transformer.table.2wind[i %% 4 == 3, V6])
  
  # three-winding transformers from node 1 to node 2
  Transformer.table.3wind_1_2 <- data.table(node.from.number = 
                                              numeric(length = nrow(Transformer.table.3wind)/5))
  
  Transformer.table.3wind_1_2$node.from.number <- Transformer.table.3wind[i %% 5 == 1, .(V1)]
  Transformer.table.3wind_1_2$node.to.number   <- Transformer.table.3wind[i %% 5 == 1, .(V2)]
  Transformer.table.3wind_1_2$id               <- Transformer.table.3wind[i %% 5 == 1, .(V4)]
  Transformer.table.3wind_1_2$status           <- Transformer.table.3wind[i %% 5 == 1, .(V12)]
  Transformer.table.3wind_1_2$resistance.pu    <- Transformer.table.3wind[i %% 5 == 2, .(V1)]
  Transformer.table.3wind_1_2$reactance.pu     <- Transformer.table.3wind[i %% 5 == 2, .(V2)]
  Transformer.table.3wind_1_2$rating.MW        <- pmin(as.numeric(Transformer.table.3wind[i %% 5 == 3, V4]),
                                                       as.numeric(Transformer.table.3wind[i %% 5 == 4, V4]))
  Transformer.table.3wind_1_2$overload.rating.MW <- pmin(as.numeric(Transformer.table.3wind[i %% 5 == 3, V6]),
                                                         as.numeric(Transformer.table.3wind[i %% 5 == 4, V6]))
  # three-winding transformers from node 2 to node 3
  Transformer.table.3wind_2_3 <- data.table(node.from.number = 
                                              numeric(length = nrow(Transformer.table.3wind)/5))
  
  Transformer.table.3wind_2_3$node.from.number <- Transformer.table.3wind[i %% 5 == 1, .(V2)]
  Transformer.table.3wind_2_3$node.to.number   <- Transformer.table.3wind[i %% 5 == 1, .(V3)]
  Transformer.table.3wind_2_3$id               <- Transformer.table.3wind[i %% 5 == 1, .(V4)]
  Transformer.table.3wind_2_3$status           <- Transformer.table.3wind[i %% 5 == 1, .(V12)]
  Transformer.table.3wind_2_3$resistance.pu    <- Transformer.table.3wind[i %% 5 == 2, .(V4)]
  Transformer.table.3wind_2_3$reactance.pu     <- Transformer.table.3wind[i %% 5 == 2, .(V5)]
  Transformer.table.3wind_2_3$rating.MW        <- pmin(as.numeric(Transformer.table.3wind[i %% 5 == 4, V4]),
                                                       as.numeric(Transformer.table.3wind[i %% 5 == 0, V4]))
  Transformer.table.3wind_2_3$overload.rating.MW <- pmin(as.numeric(Transformer.table.3wind[i %% 5 == 4, V6]),
                                                         as.numeric(Transformer.table.3wind[i %% 5 == 0, V6])) 
  # three-winding transformers from node 3 to node 1
  Transformer.table.3wind_3_1 <- data.table(node.from.number = 
                                              numeric(length = nrow(Transformer.table.3wind)/5))
  
  Transformer.table.3wind_3_1$node.from.number <- Transformer.table.3wind[i %% 5 == 1, .(V3)]
  Transformer.table.3wind_3_1$node.to.number   <- Transformer.table.3wind[i %% 5 == 1, .(V1)]
  Transformer.table.3wind_3_1$id               <- Transformer.table.3wind[i %% 5 == 1, .(V4)]
  Transformer.table.3wind_3_1$status           <- Transformer.table.3wind[i %% 5 == 1, .(V12)]
  Transformer.table.3wind_3_1$resistance.pu    <- Transformer.table.3wind[i %% 5 == 2, .(V7)]
  Transformer.table.3wind_3_1$reactance.pu     <- Transformer.table.3wind[i %% 5 == 2, .(V8)]
  Transformer.table.3wind_3_1$rating.MW        <- pmin(as.numeric(Transformer.table.3wind[i %% 5 == 0, V4]),
                                                       as.numeric(Transformer.table.3wind[i %% 5 == 3, V4]))
  Transformer.table.3wind_3_1$overload.rating.MW <- pmin(as.numeric(Transformer.table.3wind[i %% 5 == 0, V6]),
                                                         as.numeric(Transformer.table.3wind[i %% 5 == 3, V6])) 
  
  Transformer.table = rbind(Transformer.table.2wind_1_2,
                            Transformer.table.3wind_1_2,
                            Transformer.table.3wind_2_3,
                            Transformer.table.3wind_3_1)
  
  # clean up
  rm(Transformer.table.2wind,Transformer.table.2wind_1_2,
     Transformer.table.3wind,Transformer.table.3wind_1_2,
     Transformer.table.3wind_2_3,Transformer.table.3wind_3_1)
  
  
} else {
  
  message("No Transformer Table exists ... skipping")
}

if (exists('Two.terminal.dc.line.table')) {

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

} else {
  
  message("No Two Terminal DC Line Table exists ... skipping")
}

#------------------------------------------------------------------------------|
# id skipped tables ----
#------------------------------------------------------------------------------|

## find tables that haven't been processed (i.e. colnames are stil V1, V2, ...)
all.tables <- ls(pattern = "^[A-Z].*(table)$")
done.tables <- character()
skip.tables <- character()

for (tab.name in all.tables) {
    
    if (colnames(get(tab.name))[1] == "V1") {
        
        skip.tables <- c(skip.tables, tab.name)
        all.tables <- all.tables
    } else {
        
        done.tables <- c(done.tables, tab.name) 
    }

    # transformers and DC lines have been handled
    skip.tables <- skip.tables[skip.tables != "Two.terminal.dc.line.table"]
    
}


# clean up
rm(all.tables, tab.name)

## get length of all done tables
tab.info <- c()

for (tab.name in done.tables) {
    info <- gsub("\\.table", "", tab.name)
    info <- gsub("\\.", " ", info)
    info <- paste0("Number of ", info,
                   ifelse((substr(info, nchar(info), nchar(info)) == "s" | 
                           substr(info, nchar(info) - 1, nchar(info)) == "ch"),
                       "es: ", "s: "),
                   nrow(get(tab.name)))
    
    tab.info <- c(tab.info, info)
}

# clean up
rm(tab.name, info)


# #------------------------------------------------------------------------------|
# # write out ----
# #------------------------------------------------------------------------------|
# 
# # write out csv files
# for (tab.name in done.tables) {
#     write.csv(get(tab.name), 
#               file.path(output.dir, paste0(tab.name, ".csv")),
#               row.names = FALSE, 
#               quote = FALSE)
# }
# 
# # write out report
# conn <- file(file.path(output.dir, "00-metadata.txt"))
# 
# writeLines(c(as.character(Sys.time()), "\n\n",
#              paste("psse file parsed:", basename(raw.file), "\n"),
#              paste("psse version:", psse.version, "\n"),
#              paste("mva base:", mva.base, "\n\n"),
#              paste("tables processed:\n\t-", paste0(done.tables, collapse = "\n\t- "), "\n\n"),
#              paste("tables skipped:\n\t-", paste0(skip.tables, collapse = "\n\t- "), "\n\n"),
#              paste("empty tables:\n\t-", paste0(no.data.vec, collapse = "\n\t- "), "\n\n"),
#              "----------\n\n",
#              paste("other information:\n\t-", paste0(tab.info, collapse = "\n\t- "))
#              ), 
#            conn, 
#            sep = "")
# 
# close(conn)
# 
# # clean up
# rm(tab.name, conn)
