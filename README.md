# 3d-maps-Guwahati-Satellite-Imagery-Sentinel-2
A brief tutorial on generation of 3D map using SRTM and Landsat 8 Imagery
![Demo1](https://github.com/Hwoabam/3d-maps-Guwahati-Satellite-Imagery-Sentinel-2/blob/master/Media/Animation/sentinel2.gif)

The following packages are installed and loaded from the library:
```{r}
#load necessary packages
library(raster)
library(sp)
library(rayshader)
library(leaflet)
library(magick)
library(scales)
library(rgdal)
library(rgeos)
```

The images may be downloaded From Sentinel Hub: EO Browser, Using L2A datasets which are atmospherically corrected. Optimum results are seen when the cloud coverage of the area is 0% and so the data needs to be downloaded accordingly. The raster images are called using raster::raster. Brick is used while using multiband image(raster::brick). Later we can plot it using plotRGB function. 
```{r echo=TRUE}
guwahati_r = raster(File location"/2019-01-12_00-00_-_2019-01-12_23-59_Sentinel-2_S2L2A_B04_(Raw).TIFF")
guwahati_g = raster(File location"/2019-01-12_00-00_-_2019-01-12_23-59_Sentinel-2_S2L2A_B03_(Raw).TIFF")
guwahati_b = raster(File location"/2019-01-12_00-00_-_2019-01-12_23-59_Sentinel-2_S2L2A_B02_(Raw).TIFF")
 satellite_images = stack(guwahati_r, guwahati_g, guwahati_b)

plotRGB(satellite_images)
projectRaster(satellite_images, 
              crs= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot sentinel-2 image
plotRGB(satellite_images)
```
![Truecolor](https://github.com/Hwoabam/3d-maps-Guwahati-Satellite-Imagery-Sentinel-2/blob/master/Media/Plots/plotRgb.png)

Gamma correction of the Image. using square root of the stack.
```{r}
guwahati_rgb_corrected = sqrt(stack(guwahati_r, guwahati_g, guwahati_b))
plotRGB(guwahati_rgb_corrected)
```
![Gamma correction](https://github.com/Hwoabam/3d-maps-Guwahati-Satellite-Imagery-Sentinel-2/blob/master/Media/Plots/gammapro.png) 

The GIS data for elevation is downloaded from Derek Watkin's -"30 meter SRTM tile downloader". The downloaded SRTM hgt data is converted to a matrix. The result of the plot is an elevation intensity profile of the tile. In this purpose the default crs is used, which is WGS844 longitude latitude. We can plot the file using height_shade(raster_to_matrix) function
```{r echo=TRUE}
#Download DEM using elevatr or extract from SRTM tile downloader
Elevation_File <- raster(File location"/N26E091.hgt")
Elevation_File
#reproject the DEM
projectRaster(Elevation_File, 
              crs= "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#plot raster DEM
height_shade(raster_to_matrix(Elevation_File)) %>%
  plot_map()
```
![Elevation Heatmap](https://github.com/Hwoabam/3d-maps-Guwahati-Satellite-Imagery-Sentinel-2/blob/master/Media/Plots/Elevation_heatmap.png) 

Transforming data from Long/lat to UTM using 'bilinear' method of interpolation. since here elevation is a continuous variable
```{r}
crs(guwahati_r)
guwahati_elevation_utm = projectRaster(Elevation_File, crs = crs(guwahati_r), method = "bilinear")
crs(guwahati_elevation_utm)
```
Defining the area required to be cropped off from the area available in the data sets. The coordinates of the bottom left corner and the top right corner of the desired plot is entered. Defining an object 'scl' which gives a measure of the scalebar with 20km length, with reference to the map.
```{r echo=TRUE}
bot_lefty=91.529469
bot_leftx=26.065919
top_righty=91.870045
top_rightx=26.305610
distl2l= top_righty-bot_lefty  
scl=1-(20/(distl2l*111))
bottom_left = c(bot_lefty, bot_leftx)
top_right   = c(top_righty, top_rightx)
extent_latlong = SpatialPoints(rbind(bottom_left, top_right), proj4string=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
e = extent(extent_latlong)
e
scl
```
Defining a function to check the location of the area covered with reference to a map using theleaflet package. 
```{r}
#AoI Function
AOI <- function(xleft, ytop, xright, ybot) {
  x_coord <- c(xleft,xleft,xright,xright)
  y_coord <- c(ytop,ybot, ybot, ytop)
  xym <- cbind(x_coord, y_coord)
  p <- Polygon(xym)
  ps = Polygons(list(p),1)
  sps = SpatialPolygons(list(ps))
  proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  return(sps)
}
mask <- AOI(bot_lefty,bot_leftx,top_righty,top_rightx)
leaflet(mask) %>% 
  addTiles() %>% 
  addPolygons()
```
![Leaflet](https://github.com/Hwoabam/3d-maps-Guwahati-Satellite-Imagery-Sentinel-2/blob/master/Media/Plots/Capture23.PNG) 

The crop function is used for cropping the required area for the elevation and Satellite Imagery matrices.Now the Cropped area is to be transformed into a 3 layered RGB array and the elevations need to be transposed on the array. The aperm() function is used for this purpose.Then, the elevation data is converted into Base R matrix
```{r fig1, fig.height = 12, fig.width = 8, align= "center"}
guwahati_rgb_cropped = crop(guwahati_rgb_corrected, e)
elevation_cropped = crop(guwahati_elevation_utm, e)
names(guwahati_rgb_cropped) = c("r","g","b")
guwahati_r_cropped = raster_to_matrix(guwahati_rgb_cropped$r)
guwahati_g_cropped = raster_to_matrix(guwahati_rgb_cropped$g)
guwahati_b_cropped = raster_to_matrix(guwahati_rgb_cropped$b)
guwahatiel_matrix = raster_to_matrix(elevation_cropped)
guwahati_rgb_array = array(0,dim=c(nrow(guwahati_r_cropped),ncol(guwahati_r_cropped),3))
guwahati_rgb_array[,,1] = guwahati_r_cropped/255 #Red layer
guwahati_rgb_array[,,2] = guwahati_g_cropped/255 #Blue layer
guwahati_rgb_array[,,3] = guwahati_b_cropped/255 #Green layer
guwahati_rgb_array = aperm(guwahati_rgb_array, c(2,1,3))
plot_map(guwahati_rgb_array)
```
![Cropped Matrix](https://github.com/Hwoabam/3d-maps-Guwahati-Satellite-Imagery-Sentinel-2/blob/master/Media/Plots/Sent2_cropped.png) 

The contrast is then adjusted
```{r fig2, fig.height = 12, fig.width = 8, align= "center"}
guwahati_rgb_contrast = rescale(guwahati_rgb_array,to=c(0,1))
plot_map(guwahati_rgb_contrast)
```
![Cropped Matrix](https://github.com/Hwoabam/3d-maps-Guwahati-Satellite-Imagery-Sentinel-2/blob/master/Media/Plots/Sent2_contrast_corrected.png)

Then a 3D plot is generated in the Rgl window, with compass and scalebar renderings. 
```{r}
plot_3d(guwahati_rgb_contrast, guwahatiel_matrix, windowsize = c(1200,900), zscale = 7.5, shadowdepth = -150,
        zoom=0.5, phi=45,theta=-45,fov=70, background = "#F2E1D0", shadowcolor = "#523E2B")
render_compass(position = "W" )
render_scalebar(limits=c(0,10,20),label_unit = "km",position = "S", y=50,scale_length = c(scl,1))
render_snapshot(title_text = "Guwahati city | Imagery: Sentinel-2 | DEM: 30m SRTM", title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
```
![3D plot](https://github.com/Hwoabam/3d-maps-Guwahati-Satellite-Imagery-Sentinel-2/blob/master/Media/Snapshots/snap1.png)

A 24 second video is generated of the animation of the rotation of the plot using ffmpeg function at framerate of 60fps. 
```{r eval=FALSE, include=FALSE}
angles= seq(0,360,length.out = 1441)[-1]
for(i in 1:1440) {
  render_camera(theta=-45+angles[i])
  render_snapshot(filename =
  sprintf("guwahati_sentinel%i.png", i), 
                  title_text = " Guwahati Metropolitan Area | Topography",
                  title_bar_color = "#1f5214", title_color = "white", title_bar_alpha = 1)
}
rgl::rgl.close()
system("ffmpeg -framerate 60 -i guwahati_sentinel%d.png -vcodec libx264 -an Ghy_Sentinel_2.1.mp4 ")
```
For imprinting a text Watermark on the video, the following code is run in command prompt:  
```{r eval=FALSE, include=FALSE}
ffmpeg -i Ghy_Sentinel_2.mp4 -vf drawtext="text='Hydrosense Lab IIT Delhi': fontcolor=black:fontsize=24: x=20: y=(h-50)" -codec:a copy output.mp4
```


