# Run to check data and summarize the compiled database 
# list of missing items
missing.items.list <- c()

# create unique directory to save warnings and summary output
data.check.dir <- file.path(outputfiles.dir,"DataCheck",
                            format(Sys.time(), "%d-%m-%Y %H%M"))
dir.create(data.check.dir)

# create a warning file 
warnings <- file.path(data.check.dir,"warnings.txt")
file.create(warnings)
sink(warnings)
cat("***Warnings***")
sink()

# create a fatal warning file
fatal.warnings <- file.path(data.check.dir,"fatal.warnings.txt")
file.create(fatal.warnings)
sink(fatal.warnings)
cat("***Fatal Warnings***")
sink()
# 1. node has 2 regions or zones
# 2. node has no regions or zones
# 3. generator has no fuel


#------------------------------------------------------------------------------#
# Identify islands and isolated nodes ----
#------------------------------------------------------------------------------#

# check for nodes missing region and/or zone
nodes = Objects.sheet[class == "Node",.(Node = name)]

nodes = merge(nodes, 
              Memberships.sheet[parent_class == "Node" & 
                                  child_class == "Region",
                                .(Node = parent_object, Region = child_object)],
              by = "Node", all.x = T)

nodes = merge(nodes, 
              Memberships.sheet[parent_class == "Node" & 
                                  child_class == "Zone",
                                .(Node = parent_object, Zone = child_object)],
              by = "Node", all.x = T)

node.missing.region <- nodes[is.na(Region), .(Node, Region, Fatal = T)]
node.missing.zone <- nodes[is.na(Zone), .(Node, Zone, Fatal = T)]

missing.items.list <- c(missing.items.list,"node.missing.region", 
                        "node.missing.zone")

# TO DO: check for nodes with multiple region/zone memberships

# Identify islands - extract all edges (lines and transformers)
lines.from <- Memberships.sheet[parent_class == "Line" & 
                                  collection == "Node From",
                                .(line = parent_object, from = child_object)]

lines.to <- Memberships.sheet[parent_class == "Line" & 
                                collection == "Node To",
                              .(line = parent_object, to = child_object)]

lines <- merge(lines.from, lines.to, by = "line")

# add transformers

tfmr.from <- Memberships.sheet[parent_class == "Transformer" & 
                                 collection == "Node From",
                               .(line = parent_object, from = child_object)]

tfmr.to <- Memberships.sheet[parent_class == "Transformer" & 
                               collection == "Node To",
                             .(line = parent_object, to = child_object)]

tfmrs <- merge(tfmr.from, tfmr.to, by = "line")

# combine lines and transformers to create network edges
edges <- rbind(lines, tfmrs)[,.(from,to)]

# create graph object
network <- graph.data.frame(edges, directed = F, vertices = nodes)

# retrieve list of isolated nodes
components <- components(network)

names(components$csize) <- 1:length(components$csize)

components.table <- data.table(Node.Name = names(components$membership),
                               component.id = components$membership)

components.table[,csize := 
                   components$csize[which(names(components$csize) == component.id)],
                 by = "component.id"]

# grab scenarios on nodes
nodes <- merge(nodes,
               Properties.sheet[child_class == "Node" & property == "Units",
                                .(child_object, scenario)], 
               by.x = "Node", by.y = "child_object", all = T)

components.table[,node.in.scenario := 
                   ifelse(Node.Name %in% 
                            nodes[!is.na(scenario),Node], 1, 0)]

island.nodes <- components.table[csize != max(csize), .(Node.Name = Node.Name)]
island.nodes[,`In scenario?` := 
                ifelse(Node.Name %in% 
                         components.table[node.in.scenario != 0, Node.Name], "Yes", "No")]

write.csv(island.nodes,  
          file = file.path(data.check.dir,"isolated.nodes.csv"),
          quote = F, row.names = F)

# Export a report table

components.table <- components.table[,.(`Component size` = max(csize), 
                                    `Nodes with scenario` = sum(node.in.scenario)),
                                     by = "component.id"]

components.report.dir <- file.path(data.check.dir,"isolated.nodes.report.txt")

sink(components.report.dir)
cat(sprintf("REPORT: connected components in %s database.", choose.db))
cat("\n")
cat("This analysis is done on the base network - scenarios on Lines/Transformers are ignored.")
cat("\n")
cat(sprintf("List of nodes that belong to islands saved in %s/isolated.nodes.csv", data.check.dir))
cat("\n")
cat("Islands are any groups of nodes not connected to the largest connected component.")
cat("\n\n")
print(arrange(components.table, desc(`Component size`)), row.names = F)
sink()

# check that LPFs sum to 1 for each region 
node.lpf <- Properties.sheet[child_class == "Node" & 
                               property == "Load Participation Factor",
                             .(Node = child_object, LPF = as.numeric(value),
                               scenario = scenario)]

node.lpf <- merge(node.lpf, 
                  Memberships.sheet[parent_class == "Node" & 
                                      child_class == "Region",
                                    .(Node = parent_object, Region = child_object)],
                  by = "Node", all.x = T)

# sum LPF by region *** ignoring scenarios ***
region.lpf <- node.lpf[is.na(scenario), .(region.lpf = sum(LPF)), by = "Region"]

# generate warning if LPF does not sum to 1 in all regions
lpf.sum.to.one <- round(sum(region.lpf$region.lpf), 13) == nrow(region.lpf)

if(lpf.sum.to.one == F){
  sink(warnings, append = T)
  cat("\n\n")
  cat(paste0("WARNING: LPF does not sum to one (1) in at least one region."))
  cat("\n\n")
  print(region.lpf[region.lpf != 1, .(Region, 
                                      region.lpf = sprintf("%.10f", region.lpf))], 
        row.names = F,
        n = nrow(region.lpf))
  sink()
}

# clean up working evnironment
rm(network, edges, lines, lines.from, lines.to, tfmrs, tfmr.to, tfmr.from)

#------------------------------------------------------------------------------#
# Check generator properties ----
#------------------------------------------------------------------------------#

### pull generator capacity by fuel and state
generator.map <- Objects.sheet[class == "Generator", 
                               .(Generator = name, Fuel = category)]


generator.map <- merge(generator.map,
                       Properties.sheet[child_class == "Generator" &
                                          property == "Max Capacity",
                                        .(Generator = child_object,
                                          Capacity = value)], 
                       by = "Generator", all.x = T)

# pull generator nodes
generator.map <- merge(generator.map,
                       Memberships.sheet[parent_class == "Generator" &
                                           child_class == "Node",
                                         .(Generator = parent_object,
                                           Node = child_object)],
                       by = "Generator", all.x = T)

# pull regions
generator.map <- merge(generator.map,
                       Memberships.sheet[parent_class == "Node" &
                                           child_class == "Region",
                                         .(Node = parent_object,
                                           Region = child_object)],
                       by = "Node", all.x = T)

# pull RE units and scenarios
generator.map <- merge(generator.map,
                       Properties.sheet[child_class == "Generator" &
                                          property == "Units",
                                        .(Generator = child_object,
                                          Units = value, 
                                          scenario = scenario)], 
                       by = "Generator", all.x = T)

# clean up scenario name
generator.map[,scenario := gsub("{Object}","Scenario: ",scenario, fixed = T)]
generator.map[,scenario := ifelse(is.na(scenario),"No scenario",scenario)]

# flag generators with missing missing nodes, regions, fuels, capacity, units

gens.missing.units <- generator.map[is.na(Units), .(Generator, Units)]
gens.missing.capacity <- generator.map[is.na(Capacity), .(Generator, Capacity)]
gens.missing.fuel <- generator.map[is.na(Fuel), .(Generator, Fuel, Fatal = T)]
gens.missing.node <- generator.map[is.na(Node), .(Generator, Node)]

# add to missing items list
missing.items.list <- c(missing.items.list, "gens.missing.units", 
                        "gens.missing.capacity", "gens.missing.fuel",
                        "gens.missing.node")

# change colums that can be numeric to numeric
generator.map <- generator.map[, lapply(.SD, function(x) {
  if (!is.na(suppressWarnings(as.numeric(x[1])))) {
    suppressWarnings(as.numeric(x))} else x
})]

# summarize generator properties by fuel and save to OutputFiles
generator.fuels.summary <- generator.map[,.(avg.capacity = mean(Capacity),
                                            min.capacity = min(Capacity),
                                            max.capacity = max(Capacity),
                                            sd.capacity = sd(Capacity),
                                            avg.units = mean(Units),
                                            min.units = min(Units),
                                            max.units = max(Units),
                                            sd.units = sd(Units)), 
                                         by = "Fuel"]

write.csv(generator.fuels.summary,
          file = file.path(data.check.dir, "generator.summary.by.fuel.csv"),
          quote = F, row.names = F)

# plot generator capacity plus existing RE by state
conventional.fuels <- paste(c("HYDRO","NUCLEAR","OIL","COAL","LNG","GAS",
                              "LIGNITE","DIESEL","CCGT", "WHR","Existing RE"),
                            collapse = "|")

generator.map[,conventional := ifelse(grepl(conventional.fuels,Fuel) == TRUE,
                                      1,0)]

# alpabetize regions
region.names <- unique(generator.map$Region)[order(unique(generator.map$Region))]

message("...exporting regional capacity plots")

pb <- txtProgressBar(min = 0, max = length(unique(generator.map$Region)), style = 3)
pdf(file.path(data.check.dir,"region.capcaity.plots.pdf"),
    width = 12, height = 8)
stepi = 0

for(i in region.names){
  plot <- ggplot(data = arrange(generator.map[conventional == 1 & Region == i,], 
                                scenario)) +
    geom_bar(aes(x = Fuel, y = Capacity*Units, fill = scenario), stat = "identity") +
    ggtitle(paste0(i," Generation Capacity by Fuel")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    ylab("Capacity (MW)")
    
    suppressWarnings(
      plot(plot)
    )
    stepi = stepi + 1
    setTxtProgressBar(pb, stepi)
}
dev.off()

message("...exporting regional RE scenarios plots")
# plot state new RE build by scenario
pb <- txtProgressBar(min = 0, max = length(unique(generator.map$Region)), style = 3)
pdf(file.path(data.check.dir,"region.newRE.plots.pdf"),
    width = 12, height = 8)
stepi = 0

for(i in region.names){
  plot <- ggplot(data = generator.map[conventional == 0 & Region == i,]) +
    geom_bar(aes(x = Fuel, y = Units*Capacity), stat = "identity") +
    ggtitle(paste0(i," RE Capacity by Fuel")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    xlab("Capacity (MW)") +
    if(nrow(generator.map[conventional == 0 & Region == i & scenario != "No Scenario",]) != 0){
      facet_wrap(~ scenario, scales = "free_x")
    }
  
  suppressWarnings(
    plot(plot)
  )
  stepi = stepi + 1
  setTxtProgressBar(pb, stepi)
}
dev.off()
rm(stepi)

# plot new RE units by state and scenario

#gen.newRE.plot <- ggplot(data = generator.map[conventional == 0,]) +
  #geom_bar(aes(x = Fuel, y = Units, fill = Region), stat = "identity") +
  #facet_wrap(~ scenario, scales = "free") +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1))

#------------------------------------------------------------------------------#
# Check line and tfmr properties ----
#------------------------------------------------------------------------------#

line.map <- Objects.sheet[class %in% c("Line","Transformer"), 
                          .(Line = name, Region = category)]

# get line max flow
line.map <- merge(line.map,
                  Properties.sheet[child_class == "Line" &
                                     property == "Max Flow",
                                   .(Line = child_object, `Max Flow` = value)],
                  by = "Line", all.x = T)


# get line min flow
line.map <- merge(line.map,
                  Properties.sheet[child_class == "Line" &
                                     property == "Min Flow",
                                   .(Line = child_object, `Min Flow` = value)],
                  by = "Line", all.x = T)

# get node.to.kV and node.from.kV
# Node From
line.map <- merge(line.map,
                  Memberships.sheet[parent_class %in% c("Line","Transformer") & 
                              child_class == "Node" & collection == "Node From",
                             .(Line = parent_object, Node.From = child_object)],
                  by = "Line")

line.map <- merge(line.map,
                  Properties.sheet[child_class == "Node" & property == "Voltage",
                              .(Node.From = child_object, Node.From.kV = value)],
                  by = "Node.From")

# Node To
line.map <- merge(line.map,
                  Memberships.sheet[parent_class %in% c("Line","Transformer") & 
                                child_class == "Node" & collection == "Node To",
                               .(Line = parent_object, Node.To = child_object)],
                  by = "Line")

line.map <- merge(line.map,
                  Properties.sheet[child_class == "Node" & property == "Voltage",
                                  .(Node.To = child_object, Node.To.kV = value)],
                  by = "Node.To")

# pull reactance and resistance
line.map <- merge(line.map,
                  Properties.sheet[child_class %in% c("Line","Transformer")
                                   & property == "Reactance",
                                   .(Line = child_object, Reactance = value,
                                     reac.scenario = scenario)],
                  by = "Line")

line.map <- merge(line.map,
                  Properties.sheet[child_class %in% c("Line","Transformer")
                                   & property == "Resistance",
                                   .(Line = child_object, Resistance = value,
                                     resis.scenario = scenario)],
                  by = "Line")

# identify transformers
line.map[, tfmr := ifelse(grepl("tfmr",Line),"Transformer","Line")]

# clean-up scenario name
line.map[,resis.scenario:=gsub("{Object}","Scenario: ",resis.scenario,fixed = T)]
line.map[is.na(resis.scenario),resis.scenario:="No scenario"]

line.map[,reac.scenario:=gsub("{Object}","Scenario: ",reac.scenario,fixed = T)]
line.map[is.na(reac.scenario),reac.scenario:="No scenario"]

# change columns that can be numeric to numeric
line.map <- line.map[, lapply(.SD, function(x) {
  if (!is.na(suppressWarnings(as.numeric(x[1])))) {
    suppressWarnings(as.numeric(x))} else x
})]

# flag any lines with missing Node.From and/or Node.To
lines.missing.nodes <- line.map[is.na(Node.From) | is.na(Node.To),
                                .(Line, 
                                  Node = paste("From:",Node.From,"To:", Node.To),
                                  Fatal = T)]

missing.items.list <- c(missing.items.list,"lines.missing.nodes")

# plots of min and max flow by voltage (using Node.From.kV)
line.maxflow.plot <- ggplot(data = line.map) +
  geom_boxplot(aes(x = factor(Node.From.kV), y = `Max Flow`)) +
  xlab("Node From Voltage (kV)") +
  ylab("Max Flow (MW)")

line.minflow.plot <- ggplot(data = line.map) +
  geom_boxplot(aes(x = factor(Node.From.kV), y = `Min Flow`)) +
  xlab("Node From Voltage (kV)") +
  ylab("Min Flow (MW)")

# plots of reactance and resistance by voltage and scenario
line.reactance.plot <- ggplot(data = line.map) +
  geom_boxplot(aes(x = factor(Node.From.kV), y = Reactance)) +
  facet_wrap(tfmr ~ reac.scenario, scales = "free") +
  xlab("Node From Voltage (kV)")

line.resistance.plot <- ggplot(data = line.map) +
  geom_boxplot(aes(x = factor(Node.From.kV), y = Resistance)) +
  facet_wrap(tfmr ~ resis.scenario, scales = "free") +
  xlab("Node From Voltage (kV)")

# add to list of plots
line.plots <- c("line.maxflow.plot", "line.minflow.plot",
                      "line.reactance.plot","line.resistance.plot")

### export line plots to DataCheck folder
message("...exporting line property plots")
pb <- txtProgressBar(min = 0, max = length(line.plots), style = 3)
pdf(file.path(data.check.dir,"line.plots.pdf"),
    width = 12, height = 8)
for(i in 1:length(line.plots)){
  suppressWarnings(
    plot(get(line.plots[i]))
  )
  setTxtProgressBar(pb, i)
}
dev.off()

#------------------------------------------------------------------------------#
# check for fatal import/run errors ----
#------------------------------------------------------------------------------#

# make sure there are no required values missing in properties.sheet
problem.row.mask = Properties.sheet[, 
                                    !complete.cases(list(parent_object, child_object, parent_class, 
                                                         child_class, collection, property, value, band_id))]

if (any(problem.row.mask)) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat("WARNING: the following property sheet value(s) are missing. This will not import.","\n")
  print(Properties.sheet[problem.row.mask,
                         .(parent_object, child_object, property, value, scenario)],
        row.names = F)
  sink()
}

# make sure there are no blanks in Memberships.sheet
problem.row.mask = !complete.cases(Memberships.sheet)

if (any(problem.row.mask)) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat("WARNING: the following membership sheet value(s) are missing. ",
        "This will not import.", 
        "This may be caused by models being multiply defined in generic import ",
        "sheets, among other things.","\n")
  print(Memberships.sheet[problem.row.mask], row.names = F)
  sink()
}

# make sure no region has no nodes
all.regions <- Objects.sheet[class == "Region",name]
regions.w.nodes <- Memberships.sheet[parent_class == "Node" & collection == 
                                       "Region",child_object]
if (!all(all.regions %in% regions.w.nodes)) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat("WARNING: the following region(s) have no nodes. This will not import.", "\n")
  print(all.regions[!(all.regions %in% regions.w.nodes)], row.names = F)
  sink()
}

# make sure no object name has more than 50 characters 
if (any(Objects.sheet[,nchar(name) > 50])) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat("WARNING: the following object(s) have names with > 50 characters. This will not import.", "\n")
  print(Objects.sheet[nchar(name) > 50], row.names = F)
  sink()
}

# check for properties that periods that require non-NA period_type_ids
# have only tested a couple of these,
period_id_props = Properties.sheet[grepl("(Hour|Day|Week|Month|Year)$", property)]
period_id_table = data.table(period_id = c(6, 1, 2, 3, 4),
                             period = c("Hour$", "Day$", "Week$", "Month$", "Year$"))

period_id_props[, problem := NA]
period_id_props[grepl("Hour$", property) & period_type_id != "6", problem := TRUE]
period_id_props[grepl("Day$", property) & period_type_id != "1", problem := TRUE]
period_id_props[grepl("Week$", property) & period_type_id != "2", problem := TRUE]
period_id_props[grepl("Month$", property) & period_type_id != "3", problem := TRUE]
period_id_props[grepl("Year$", property) & period_type_id != "4", problem := TRUE]

period_id_props = period_id_props[problem == TRUE]

# we know that this doesn't work for max energy and target. 
known.issues = period_id_props[grepl("^(Max Energy|Target)", property)]

if (nrow(known.issues) > 0) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat(paste0("WARNING: the following property does not correspond to the ",
               "right period_type_id (Hour: 6, Day: 1, Week: 2, Month: 3, Year: 4). ",
               "This will not run properly.", "\n"))
  print(known.issues, row.names = F)
  sink()
}

# it problem doesn't work for these others, but we haven't checked
unknown.issues = period_id_props[!grepl("^(Max Energy|Target)", property)]

if (nrow(unknown.issues) > 0) {
  sink(warnings, append = T)
  cat("\n\n")
  cat(paste0("WARNING: the following property does not correspond to the ",
               "right period_type_id (Hour: 6, Day: 1, Week: 2, Month: 3, Year: 4). ",
               "This is untested but may not run properly.", "\n"))
  print(unknown.issues, row.names = F)
  sink()
}

rm(problem.row.mask, known.issues, unknown.issues, period_id_props)

# check to see if a property is defined twice for on object in one scenario
dupes = duplicated(Properties.sheet, 
                   by = c("parent_object", "child_object", "property", "scenario", 
                          "band_id"))

if (any(dupes)) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat(paste0("WARNING: the following properties are defined twice for ", 
               "the same object in the same scenario. This may import but ",
               "will not run.", "\n"))
  print(Properties.sheet[dupes], row.names = F)
  sink()
}

rm(dupes)

# check to make sure that all objects mentioned in properties sheet also exist
# as objects
object.list = Properties.sheet[,unique(child_object)]

object.list = object.list[!(object.list %in% Objects.sheet[,name])]

if (length(object.list) > 0) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat(paste0("WARNING: the following object(s) have defined properties but ",
               "are not defined in Objects.sheet. This may result in PLEXOS assigning ",
               "these properties to other object. This may not run.", "\n"))
  print(Objects.sheet[name %in% object.list,], row.names = F)
  sink()
}

rm(object.list)

# check to make sure all scenarios have {Object} in front of them
non.object.scens = Properties.sheet[,
                                    !(grepl("^\\{Object\\}", scenario) | is.na(scenario) | scenario == "")]

if (any(non.object.scens)) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat(paste0("WARNING: the following scenario entries need an object tag ",
               "(i.e. '{Object}Scenario A' instead of 'Scenario A' This will",
               " not be read correctly by PLEXOS.", "\n"))
  print(Properties.sheet[non.object.scens], row.names = F)
  sink()
}

rm(non.object.scens)

# check to make sure all data files have either slashes or {Object}
non.object.dfs = Properties.sheet[,
                                  !(grepl("^\\{Object\\}", filename) | is.na(filename) | grepl("[/\\\\]", filename))]

if (any(non.object.dfs)) {
  sink(fatal.warnings, append = T)
  cat("\n\n")
  cat(paste0("WARNING: the following datafile entries need an object tag ",
               "(i.e. '{Object}Scenario A' instead of 'Scenario A' This will",
               " not be read correctly by PLEXOS.", "\n"))
  print(Properties.sheet[non.object.dfs], row.names = F)
  sink()
}

rm(non.object.dfs)

# generate warnings and save .csv files for missing data
for(item in missing.items.list){
  if(nrow(get(item)) > 0){
    write.csv(get(item), 
              file = file.path(data.check.dir,paste0(item,".csv")),
              row.names = F, quote = F)
    
    object.name <- names(get(item))[1]
    missing.data <- names(get(item))[2]
    
    if("Fatal" %in% names(get(item))){
    sink(fatal.warnings, append = T)
    } else{sink(warnings, append = T)}
    
    cat("\n\n")
    cat(sprintf("WARNING: At least one %s ", object.name))
    cat(sprintf("is missing %s.", missing.data))
    cat("\n")
    cat(sprintf("See file DataCheck/%s.csv", item))
    sink()
  }
}

# show data check reports
file.show(components.report.dir)

if(length(readLines(warnings, warn = F)) > 1){
file.show(warnings)
}

if(length(readLines(fatal.warnings, warn = F)) > 1){
file.show(fatal.warnings)
}


