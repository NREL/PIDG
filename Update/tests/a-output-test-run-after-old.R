tabs = ls(pattern = "^[A-Z].*(table)$")
tabs = c(tabs, "Transformer.table.edit")

for (t in tabs) {
    assign(paste0(t, "_old"), get(t))


}

Transformer.table.edit_old[, Rating.MW := as.character(Rating.MW)]

newtabs = ls(pattern = "^[A-Z].*(_old)$")

save(list = newtabs, file = "PSSE2PLEXOS/Update/output_tester/old.RData")



