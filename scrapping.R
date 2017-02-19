# 2nd try of scrapping script

library(rvest)
library(stringr)

htmlpage <- read_html("http://efurshet.com/categories/ovosxi")
htmlnodes = html_nodes(htmlpage, "p.price, p.name")

furshetVegItems = html_text(htmlnodes)
Encoding(furshetVegItems) <- "UTF-8"

furshetVegItems = str_replace_all(furshetVegItems, "\t", "")
furshetVegItems = str_replace_all(furshetVegItems, "\n", "")

city = furshetVegItems[1]
furshetVegItems = furshetVegItems[2:length(furshetVegItems)]

x <- c(1:length(furshetVegItems))[c(T,F)]
vegNames = furshetVegItems[x]
vegPrice = furshetVegItems[-x]

furshetVegItems

vegs = as.data.frame(cbind(vegNames, vegPrice))
vegs$City = city

vegs$vegPrice = as.numeric(unlist(regmatches(vegs$vegPrice,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",vegs$vegPrice))))
vegs$priceMeasure = sub(".*/", "", vegPrice)
vegs$Size = regmatches(vegNames,gregexpr("[[:digit:]]+\\.*[[:digit:]]*[кг]+",vegNames))

vegs$Size2 = regmatches(vegNames,gregexpr("[[:digit:]]+\\.*[[:digit:]]",vegNames))
vegs$Size2[vegs$priceMeasure == "кг"] = 1


vegs$UOM[vegs$priceMeasure == "кг"] = "кг"
vegs$UOM = regmatches(vegs$Size,gregexpr("[кг]+",vegs$Size))
vegs$Cat = regmatches(vegs$Names,gregexpr("[Морковь]",vegs$Names))
vegs

# Another way to scrap multiple pages



vegs2 <- lapply(paste0('http://efurshet.com/categories/ovosxi/p-', 1:length(html_nodes(htmlpage, ".pgnt a"))-1),
                function(url){
                    url %>% read_html() %>% 
                        html_nodes("p.price, p.name") %>% 
                        html_text()
                })
