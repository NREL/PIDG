load("~/GitHub/India_GtG/Process_for_PLEXOS_import/PSSE2PLEXOS/Update/output_tester/old.RData")

comp = ls(pattern = "^[A-Z].*(table_old)$")
comp = comp[!grepl("Transformer.table|Two.terminal", comp)]

for (n in comp) {
    cp = gsub("_old", "", n)
    
    print(cp)
    
    setnames(get(n), colnames(get(n)), colnames(get(cp)))

    print(all.equal(get(n), get(cp)))
}

Transformer.table.v2[,rating.MW := as.character(as.numeric(rating.MW))]

setnames(Transformer.table.edit_old, colnames(Transformer.table.edit_old),
         colnames(Transformer.table.v2))

print(all.equal(Transformer.table.v2, Transformer.table.edit_old))
