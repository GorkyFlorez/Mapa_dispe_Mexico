library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

Mexico         <- getData('GADM', country='Mexico', level=1) %>% st_as_sf()

Area <- mapedit::drawFeatures()       # Creamos el objeto
Area  <-Area %>% st_as_sf()   

Area_Mex = st_intersection(Mexico,Area )
library(elevatr)
elev = get_elev_raster(Area , z=9)
Poligo_alt    <- crop(elev, Area_Mex  )                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Area_Mex  )
plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
aspecte    = terrain(Poligo_alt, opt = "aspect")
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)

hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colore = c( 
  "#588157",#celeste
  "#dda15e", # maroon 
  "#90e0ef")#amarillo pastel

colores = c("#582f0e", "#7f4f24", "#936639", "#a68a64","#b6ad90", "#c2c5aa", 
            "#a4ac86","#656d4a", "#414833", "#333d29")


Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")

library(rgbif)
Furcraea <- occ_search(scientificName="Furcraea parmentieri")
Furcraea_sa <- subset(Furcraea$data , scientificName == "Furcraea parmentieri (Roezl) García-Mend.")

library(ggnewscale) 
Genral =ggplot()+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6, 
              show.legend = T)+
  scale_fill_gradientn(colours = colores, 
                       name='Elevacion \n(msnm)',
                       breaks = c(0,500,1000,2000,3000,4000,5000),
                       labels = c("[0 - 499] ","[500 - 999]",
                                  "[1000 - 1999]", "[2000 - 2999]", 
                                  "[3000 - 3999]", "[4000 - 4999]",
                                  "[5000 - 5999]"))+
  geom_sf(data = Mexico, color="black", size=0.3, fill= NA)+
  geom_point(data =Furcraea_sa   , aes(x=decimalLongitude, y=decimalLatitude) ,
             size=1, show.legend = F, shape=21, fill="red")+
  coord_sf(xlim = c(-106.7048, -95.6), ylim = c(15.65768,21.5 )) +
  theme(legend.position = c(0.08, 0.6),
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "#f4f3ee"),
        legend.text=element_text(size=9),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#f4f3ee"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  labs(title = '', fill = '',  x = 'Longitud', y = 'Latitud') +
  guides(fill = guide_legend(title.position = "top",direction = "vertical"))+
  annotate(geom = "text", x = -102, y = 16, hjust = 0, vjust = 1, 
           label = "Furcraea parmentieri (Roezl) García-Mend.",size = 3, family="serif", color = 
             "red",  fontface="italic")+
  annotate(geom = "text", x = -97, y = 21, hjust = 0, vjust = 1, 
           label = "B) Mapa de Dispersion",size = 3, family="serif", color = 
             "black",  fontface="italic")

Micro= ggplot()+
geom_sf(data = Mexico, color="black", fill="white", size=0.3, fill= NA)+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#f4f3ee"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -96, y = 32, hjust = 0, vjust = 1, 
           label = "A) Macrolocalizacion",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -95, y = 20, hjust = 0, vjust = 1, 
           label = "Gulf of Mexio",size = 3, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -105, y = 15, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "black",  fontface="italic")

# Mapa final
library(cowplot)
Final=ggdraw() +
  coord_equal(xlim = c(0, 26), ylim = c(0, 15), expand = FALSE) +
  draw_plot(Genral , width = 26, height = 26,x = 0.001, y =-5.3)+
  draw_plot(Micro , width = 8, height = 8,x = 1.2, y =-0.5)+
  
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "white", fill = NA, size = 1))+
  annotate(geom = "text", x = 8, y = 3, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)

ggsave(plot=Final,"Mapa/Mapa de dispersion2.png",units = "cm",width = 26, #alto
       height = 15, #ancho
       dpi=1200)


