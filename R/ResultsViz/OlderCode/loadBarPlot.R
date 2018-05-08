

library(viridis)
ann = read.delim('~/../Downloads/annualLoads.txt',stringsAsFactors = F)

mat = ann %>% select(Export, Resp, Burial) %>%
  mutate_all(.,funs(abs(.)))
barplot(as.matrix(t(mat)),legend.text = c('Export','Resp','Burial'),
        names.arg = ann$Lake, col = viridis(3),
        ylab = 'Organic carbon load (g m-2 y-1)')
