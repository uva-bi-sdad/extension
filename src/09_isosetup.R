library(osrm)
library(sf)
library(cartography)

# OSRM backend instance for Virginia locations on the SDAD server
options(osrm.server = "http://104.248.112.16:5000/", osrm.profile = "driving")

# Test from Aaron's house
iso <- osrmIsochrone(loc = c(-77.116444, 38.896130), 
                     returnclass="sf",
                     breaks = seq(from = 0, to = 14, by = 2), 
                     res = 50)

osm3 <- getTiles(x = iso, crop = FALSE, type = "osm", zoom = 12)
tilesLayer(x = osm3)
bks <- sort(c(unique(iso$min), max(iso$max)))
cols <- paste0(carto.pal("turquoise.pal", n1 = length(bks)-1), 80)
choroLayer(x = iso, var = "center", breaks = bks,
           border = NA, col = cols,
           legend.pos = "topleft",legend.frame = TRUE,
           legend.title.txt = "Isochrones\n(min)",
           add = TRUE)

# https://rdrr.io/cran/osrm/man/osrmIsochrone.html
# https://github.com/rCarto/osrm
# http://project-osrm.org/docs/v5.23.0/api/#result-objects
# https://github.com/Project-OSRM/osrm-backend/blob/master/docs/http.md
# curl http://104.248.112.16:5000/route/v1/driving/-77.116444,38.896130;-77.070264,38.894856?steps=true
