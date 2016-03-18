#export populated tables to one excel workbook

#uses (from master):
# output.wb.name 
# copy.workbook.elsewhere (T/F)
# copy.destination (needed if copy.workbook.elsewhere is TRUE)

#---------------EXPORTING---------------
#Requires latest version of Java. Alternatively, can export to .csv files and 
#merge them into an excel sheet manually

sheets.list <- list(Objects.sheet, Categories.sheet, Memberships.sheet, 
  Attributes.sheet, Properties.sheet, Reports.sheet)
names(sheets.list) <- c("Objects", "Categories", "Memberships", "Attributes", 
  "Properties", "Reports")

workbook.to.export <- createWorkbook()
invisible(lapply(names(sheets.list), function(x) 
  addWorksheet(workbook.to.export, x)))
invisible(lapply(names(sheets.list), function(x) 
  writeData(workbook.to.export, sheet = x, as.data.frame(sheets.list[[x]]), 
    rowNames = FALSE)))

saveWorkbook(workbook.to.export, paste0("../OutputFiles/", output.wb.name), 
  overwrite = TRUE)
