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
    
# also need to retire RE plants in the PSSE file, since we ae replacing them 
# with our own. 
# This is kind of unstable--deletes gens that are WIND or SOLAR-PV and 
# don't have bus number (since added RE gens don't get a bus number)
# ---- if turned on, delete original RE gens ----
if (exists('delete.original.RE')) {
  if (delete.original.RE) {
    message("... deleting original WIND and SOLAR-PV generators")
    
    re.to.delete <- generator.data.table[Fuel %in% c("WIND", "SOLAR-PV") & 
        !is.na(BusNumber), Generator.Name] 
    
    Objects.sheet <- Objects.sheet[!(name %in% re.to.delete)]
    Properties.sheet <- Properties.sheet[!(child_object %in% re.to.delete)]
    Memberships.sheet <- Memberships.sheet[!(child_object %in% re.to.delete) & 
                                             !(parent_object %in% re.to.delete)]
  }
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
  print("WARNING: the following membership sheet value(s) are missing. This will not import.")
  print(Memberships.sheet[problem.row.mas])
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

