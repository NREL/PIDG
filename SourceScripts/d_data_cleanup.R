#clean data to allow the final export to import to Plexos and run with no errors

# uses 
#   units.to.delete.file

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
        Properties.sheet <- Properties.sheet[!(child_object %in% to.delete[,Object.Name]) &
                                             !(parent_object %in% to.delete[,Object.Name])]
        Memberships.sheet <- 
          Memberships.sheet[!(child_object %in% to.delete[,Object.Name]) & 
                              !(parent_object %in% to.delete[,Object.Name])]
      } else {
        message(sprintf(">>  %s does not exist ... skipping", fname))
      } 
  }
} else {
   message(">>  units.to.delete.file does not exist ... skipping")
}

# add standard flow limits to lines with ratings of zero
# do this in a scenario (in script d)

# add standard flow limits to transformers with ratings of zero
# do this in a scenario (in script d)


#------------------------------------------------------------------------------|
# Data cleaning ----
#------------------------------------------------------------------------------|


# Plexoscan't handle min stable levels that are less than zero. Change these
# to zero and notify user.
if (any(Properties.sheet[property=="Min Stable Level",
                         as.numeric(value) < 0])) {
  message('Changing negative min stable levels to 0 MW... hope that is OK')
  Properties.sheet[property=="Min Stable Level" & as.numeric(value) < 0,
                   value := "0"]
}


#------------------------------------------------------------------------------|
# Alphabetize all categories ----
#------------------------------------------------------------------------------|

cat.by.class <- unique(Objects.sheet[!is.na(category),.(class, category)])

# order categories by class and category
setorder(cat.by.class, class, category)

# add rank of each category by class
cat.by.class[,rank := 1:.N, by = 'class']

# add this to categories .sheet so categories will be alphabetized
cat.to.categories <- initialize_table(Categories.sheet, nrow(cat.by.class), 
  list(class = cat.by.class$class, category = cat.by.class$category, 
  rank = cat.by.class$rank))
  
Categories.sheet <- merge_sheet_w_table(Categories.sheet, cat.to.categories)

# clean up
rm(cat.by.class, cat.to.categories)




