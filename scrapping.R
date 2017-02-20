# 2nd try of scrapping script

library(rvest)
library(stringr)

# Start page of scrapping vegetables inforation
htmlpage <- read_html("http://efurshet.com/categories/ovosxi")

# Scrapping of all pages. Start from 2nd because for some reason it starts from zero page.
# Thus 1st page is scrapped two times.
furshetVegItems <- lapply(paste0('http://efurshet.com/categories/ovosxi/p-', 2:length(html_nodes(htmlpage, ".pgnt a"))-1),
                          function(url){
                              url %>% read_html() %>% 
                                  html_nodes("p.price, p.name") %>% 
                                  html_text()
                            })

furshetVegItems = unlist(furshetVegItems)
Encoding(furshetVegItems) <- "UTF-8"

furshetVegItems = str_replace_all(furshetVegItems, "\t", "")
furshetVegItems = str_replace_all(furshetVegItems, "\n", "")

#city = furshetVegItems[1]
#furshetVegItems = furshetVegItems[2:length(furshetVegItems)]

x <- c(1:length(furshetVegItems))[c(T,F)]
vegNames = furshetVegItems[x]
vegPrice = furshetVegItems[-x]

furshetVegItems

vegs = as.data.frame(cbind(vegNames, vegPrice))
#vegs$City = city

vegs$vegPrice = as.numeric(unlist(regmatches(vegs$vegPrice,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",vegs$vegPrice))))
vegs$priceMeasure = sub(".*/", "", vegPrice)
vegs$Size = regmatches(vegNames,gregexpr("[[:digit:]]+\\.*[[:digit:]]*[кг]+",vegNames))
# doesn't work as it has to, selects only 
vegs$Size2 = regmatches(vegNames,gregexpr("[[:digit:]]+\\.*[[:digit:]]*", vegNames))
vegs$UOM = regmatches(vegs$Size,gregexpr("[кг]+",vegs$Size))

vegs$Size2[vegs$priceMeasure == "кг"] = 1
vegs$UOM[vegs$UOM == "character(0)"] = ""
vegs$UOM[vegs$priceMeasure == "кг" & vegs$UOM == ""] = "кг"

vegs$Cat = regmatches(vegs$Names,gregexpr("[Помидор]",vegs$Names))
vegs


vegs$Size = as.character(vegs$Size)
vegs$Size2 = as.numeric(vegs$Size2)
vegs$UOM = as.character(vegs$UOM)

vegs$Size[vegs$Size == "character(0)"] = ""

vegs$modifiedPrice[vegs$UOM == "кг"] = vegs$vegPrice/vegs$Size2
vegs$modifiedPrice[vegs$UOM == "г"] = 1000*vegs$vegPrice/vegs$Size2

write.csv(vegs, "vegs.csv")
# Another way to scrap multiple pages

setwd("/home/sergiy/Documents/Work/Scrapping")



