#clean data to allow the final export to import to Plexos and run with no errors

# uses 
#   units.to.delete.file

#------------------------------------------------------------------------------|
# Data cleaning ----
#------------------------------------------------------------------------------|

#------------------------------------------------------------------------------|
# Optional inputs ----
#------------------------------------------------------------------------------|

# retire plants from the "units_retired" file. This means: delete them 
# completely from the database, since they will not 
# requires that no two objects have the same name (mabye should put in a 
# check for this)
if (exists('units.to.delete.files')) {
  for (fname in units.to.delete.files) {
      if (file.exists(file.path(inputfiles.dir,fname))) {
        message(sprintf("... deleting units in  %s", fname))
        
        to.delete <- fread(file.path(inputfiles.dir, fname))
        Objects.sheet <- Objects.sheet[!(name %in% to.delete[,Object.Name])]
        Properties.sheet <- Properties.sheet[!(child_object %in% 
                                                 to.delete[,Object.Name])]
        Memberships.sheet <- 
          Memberships.sheet[!(child_object %in% to.delete[,Object.Name]) & 
                              !(parent_object %in% to.delete[,Object.Name])]
      } else {
        message(sprintf("... %s does not exist ... skipping", fname))
      } 
  }
} else {
   message("... units.to.delete.file does not exist ... skipping")
}

# add standard flow limits to lines with ratings of zero
# do this in a scenario (in script d)

# add standard flow limits to transformers with ratings of zero
# do this in a scenario (in script d)

#------------------------------------------------------------------------------|
# Alphabetize all categories ----
#------------------------------------------------------------------------------|

cat.by.class <- unique(Objects.sheet[!is.na(category),.(class, category)])

# order categories by class and category
setorder(cat.by.class, class, category)

# add rank of each category by class
cat.by.class[,rank := 1:.N, by = 'class']

# add this to categories .sheet so categories will be alphabetized
cat.to.categories <- initialize_table(Categories.prototype, nrow(cat.by.class), 
  list(class = cat.by.class$class, category = cat.by.class$category, 
  rank = cat.by.class$rank))
  
Categories.sheet <- merge_sheet_w_table(Categories.sheet, cat.to.categories)

# clean up
rm(cat.by.class, cat.to.categories)


#------------------------------------------------------------------------------|
# Error checking: final database ----
#------------------------------------------------------------------------------|
# there is probably a more efficient way to do this than scan through all these
# tables so many times


# Plexoscan't handle min stable levels that are less than zero. Change these
# to zero and notify user.
if (any(Properties.sheet[property=="Min Stable Level",
  as.numeric(value) < 0])) {
  message('Changing negative min stable levels to 0 MW... hope that is OK')
  Properties.sheet[property=="Min Stable Level" & as.numeric(value) < 0,
    value := "0"]
}

# make sure there are no required values missing in properties.sheet
problem.row.mask = Properties.sheet[, 
    !complete.cases(list(parent_object, child_object, parent_class, 
                         child_class, collection, property, value, band_id))]
    
if (any(problem.row.mask)) {
  print("WARNING: the following property sheet value(s) are missing. This will not import.")
  print(Properties.sheet[problem.row.mask,
    .(parent_object, child_object, property, value, scenario)])
}

# make sure there are no blanks in Memberships.sheet
problem.row.mask = !complete.cases(Memberships.sheet)

if (any(problem.row.mask)) {
  print("WARNING: the following membership sheet value(s) are missing. ",
        "This will not import.", 
        "This may be caused by models being multiply defined in generic import ",
        "sheets, among other things.")
  print(Memberships.sheet[problem.row.mask])
}

# make sure no region has no nodes
all.regions <- Objects.sheet[class == "Region",name]
regions.w.nodes <- Memberships.sheet[parent_class == "Node" & collection == 
    "Region",child_object]
if (!all(all.regions %in% regions.w.nodes)) {
  print("WARNING: the following region(s) have no nodes. This will not import.")
  print(all.regions[!(all.regions %in% regions.w.nodes)])
}

# make sure no object name has more than 50 characters 
if (any(Objects.sheet[,nchar(name) > 50])) {
  print("WARNING: the following object(s) have names with > 50 characters. This will not import.")
  print(Objects.sheet[nchar(name) > 50])
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
    print(paste0("WARNING: the following property does not correspond to the ",
        "right period_type_id (Hour: 6, Day: 1, Week: 2, Month: 3, Year: 4). ",
        "This will not run properly."))
    print(known.issues)
}

    # it problem doesn't work for these others, but we haven't checked
unknown.issues = period_id_props[!grepl("^(Max Energy|Target)", property)]

if (nrow(unknown.issues) > 0) {
    print(paste0("WARNING: the following property does not correspond to the ",
        "right period_type_id (Hour: 6, Day: 1, Week: 2, Month: 3, Year: 4). ",
        "This is untested but may not run properly."))
    print(unknown.issues)
}

rm(problem.row.mask, known.issues, unknown.issues, period_id_props)

# check to see if a property is defined twice for on object in one scenario
dupes = duplicated(Properties.sheet, 
                   by = c("parent_object", "child_object", "property", "scenario", 
                       "band_id"))

if (any(dupes)) {
    print(paste0("WARNING: the following properties are defined twice for ", 
                 "the same object in the same scenario. This may import but ",
                 "will not run."))
    print(Properties.sheet[dupes])
}

rm(dupes)

# check to make sure that all objects mentioned in properties sheet also exist
# as objects
object.list = Properties.sheet[,unique(child_object)]

object.list = object.list[!(object.list %in% Objects.sheet[,name])]

if (length(object.list) > 0) {
    print(paste0("WARNING: the following object(s) have defined properties but ",
          "are not defined in Objects.sheet. This may result in PLEXOS assigning ",
          "these properties to other object. This may not run."))
    print(object.list)
}

rm(object.list)

# check to make sure all scenarios have {Object} in front of them
non.object.scens = Properties.sheet[,
    !(grepl("^\\{Object\\}", scenario) | is.na(scenario) | scenario == "")]

if (any(non.object.scens)) {
    print(paste0("WARNING: the following scenario entries need an object tag ",
                 "(i.e. '{Object}Scenario A' instead of 'Scenario A' This will",
                 " not be read correctly by PLEXOS."))
    print(Properties.sheet[non.object.scens])
}

rm(non.object.scens)

# check to make sure all data files have either slashes or {Object}
non.object.dfs = Properties.sheet[,
    !(grepl("^\\{Object\\}", filename) | is.na(filename) | grepl("[/\\\\]", filename))]

if (any(non.object.dfs)) {
    print(paste0("WARNING: the following datafile entries need an object tag ",
                 "(i.e. '{Object}Scenario A' instead of 'Scenario A' This will",
                 " not be read correctly by PLEXOS."))
    print(Properties.sheet[non.object.dfs])
}

rm(non.object.dfs)
