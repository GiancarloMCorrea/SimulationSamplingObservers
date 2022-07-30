require(maps)
require(sp)
require(mapdata)

plot(NA, NA, ylim = c(-20, 20), xlim = c(-150, -70))
map(database = 'worldHires', add = TRUE)

locs = locator(40)

polydata = data.frame(x = locs$x, y = locs$y)
write.csv(polydata, 'polygonStock.csv', row.names = FALSE)

allGrids = read.csv('allGrids.csv')
head(allGrids)

plot(allGrids$x, allGrids$y, pch = '.')
map(database = 'worldHires', add = TRUE)

selPeru = locator(15)
lines(selPeru, col = 2)

peruGrids = allGrids[which(point.in.polygon(point.x = allGrids$x, point.y = allGrids$y, 
                                   pol.x = selPeru$x, pol.y = selPeru$y) == 1), ]
points(peruGrids$x, peruGrids$y, col = 'red', pch = 19)

write.csv(peruGrids, 'peruGrids.csv', row.names = FALSE)







selT = 1

# plot(saveDispAdv$lon, saveDispAdv$lat, pch = '.')
# points(saveDispAdv$lon[saveDispAdv$disp > 0.1], 
#        saveDispAdv$lat[saveDispAdv$disp > 0.1], 
#        pch = '.', col = 'red')

map.heatmap(lat = saveDispAdv$lat, lon = saveDispAdv$lon, 
            data = saveDispAdv$disp,
            xlim = c(-90,-70), ylim = c(-20,0)) +
  geom_polygon(data = peruMap, aes(long, lat, group = group), 
               fill = 8, color="black") +
  xlab('longitude') +
  ylab('latitude') +
  theme(legend.position = 'none') +
  theme(plot.margin = unit(c(0,0,0,0),"cm"))
