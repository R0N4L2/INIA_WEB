require(shiny)
require(leaflet)
library(leafgl)
require(sf)
require(shinydashboard)
require(leaflet.extras)
library(plotly)
library(xlsx)
library(shinyWidgets)
library(leafem)


#ingreso del archivo shape (shp)
shapefile <-st_read(paste0(getwd(),"/DATA/DivAdmCalc/Nuble_comunas.shp"))
shapefile1 <-st_read(paste0(getwd(),"/DATA/Tavg1985Poligono/Tavg1985Reclass.shp"))
shapefile2 <-st_read(paste0(getwd(),"/DATA/aptitud_wc85/aptitud_wc85.shp")) 
shapefile3 <-st_read(paste0(getwd(),"/DATA/AptitudEcocropFuturo/Combinado/Aptitud_cultivos_wcfuturo.shp"),options="ENCODING=WINDOWS-1252")

shapeEdge <-st_read(paste0(getwd(),"/DATA/Nuble_borde/NubleRegion.shp"))

shapefile2$Year<-2019
shapefile3$Year<-2021

e2<-shapefile2$Especie%>%unique()
e3<-shapefile3$Especie%>%unique()
especies<-intersect(e2,e3)
shapefile4<-rbind(shapefile2,shapefile3)

cultivardf<-data.frame(shapefile4)[,!grepl("geometry",names(shapefile4))]
cultivardf[cultivardf$Clase==0,c("Clase","Aptitud")]<-unique(cultivardf[cultivardf$Clase==1,c("Clase","Aptitud")])
cultivardf[,"Clase"]<-100*(cultivardf[,"Clase"]-min(cultivardf[,"Clase"]))/(max(cultivardf[,"Clase"])-min(cultivardf[,"Clase"]))
shapefile4[,c("Clase","Aptitud")]<-cultivardf[,c("Clase","Aptitud")]
shapefile4<-shapefile4%>%filter(Especie %in% especies)%>%group_by(Aptitud,Especie,Year)%>%summarise(Clase=mean(Clase))

paleta<-unique(data.frame(shapefile4)[,c("Aptitud","Clase")])
paleta<-paleta[order(paleta$Clase),]

peso<-function(pol1,shp,r){
  b<-st_crs(shp$geometry)
  pol2<-st_sfc(pol1, crs = b)
  if(grepl("POLYGON",st_as_text(pol1))){
    w<-NULL
    for(i in r){
      c<-st_intersection(pol2,shp$geometry[i])
      x1<-as_Spatial(c)
      x2<-slot(x1,"polygons")
      y1<-as_Spatial(shp$geometry[i])
      y2<-slot(y1,"polygons")
      w<-c(w,slot(x2[[1]],"area")/slot(y2[[1]],"area"))
    }
    w<-w/sum(w)
  }else{w<-1}
  return(w)
}

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

mobileGPSDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "getGeo.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

# HEADER ------------------------------------------------------------------
#HELP
#steps <- read.csv2("help.csv")

options(viewer = NULL) # view in browser

header <- dashboardHeader(
  title = span(img(src = "Logo_GORE_Nuble.png", height = 30),"Aptitud Térmica para Cultivos",img(src = "inia_telesig.png", height = 30)),
  titleWidth =400,
  dropdownMenu(
    type = "notifications", 
    headerText = strong("Información"), 
    icon = icon("info-circle"), 
    badgeStatus = NULL,
    notificationItem(
      text = "Ingreso de coordenadas(lat,lon)",
      icon = icon("search")
      ),
    notificationItem(
      text ="Selecciona vértices de polígono",
      icon =icon("square")
      ),
    notificationItem(
      text = "Obtiene data del punto",
      icon = icon("map-marker-alt")
      ),
    notificationItem(
      text = "GPS, busca tu posición",
      icon = icon("crosshairs")
      ),
    notificationItem(
      text = "Acerca el zoom",
      icon = icon("plus")
      ),
    notificationItem(
      text = "Aleja el zoom",
      icon = icon("minus")
      )
  )
)

sidebar <- dashboardSidebar(
  mobileGPSDetect('isGPS'),
  sidebarSearchForm(label = "Ingrese Latitud y longitud", "searchText", "searchButton"),
  sidebarMenu(
    # Setting id makes input$tabs give the tabName of currently-selected tab
    id = "tabs",
    sliderInput("historia", "Línea de tiempo:",
                min = 2010, max = 2045,
                value = 2020, step = 5,sep = ""),
    sliderInput("opacidad", "Opacidad:",
                min = 0, max = 1,
                value = 1, step = 0.01),
    pickerInput("cultivos", "Especie:",c("Seleccione especie",especies),selected="Seleccione especie"),
    pickerInput("cultivovar", "Variedad:",c("Seleccione especie","Común"),selected="Seleccione variedad"),
    uiOutput("text")
  )
  ,collapsed =TRUE)
tabMapa<-tabPanel("Mapa",# test whether mobile or not
                  box(width = "100%",height=870,leafglOutput("map",width="100%",height=850)),
                      fluidRow(a(strong(img(src="artificyan.svg",height=30),"Powered by ARTIFICYAN"),
                             height=30,href="https://artificyan.cl/"),align='right'))
body <- dashboardBody(mobileDetect('isMobile'),tabMapa)
ui<-dashboardPage(skin="black",title="Telesig",header,sidebar,body)
server <- function(input, output){
  output$map <- renderLeaflet({
      leaflet()%>%addTiles()%>%leafem::addMouseCoordinates()%>%
      setView(lng=-72.1320133,lat=-36.6229604,zoom=10-input$isMobile*2)%>%
      addProviderTiles(providers$CartoDB.Positron, group="Mapa")%>%
      addProviderTiles(providers$Esri.WorldImagery, group="Satélite") %>% 
      addProviderTiles(providers$OpenTopoMap, group="Topográfico")%>%
      addLayersControl(baseGroups=c('Mapa','Satélite','Topográfico'),
                     options=layersControlOptions(collapsed=T))%>%  
      addControlGPS(options=gpsOptions(position="topleft",activate=T, 
                                       autoCenter=T,maxZoom = 60,setView=T))%>%
      addDrawToolbar(targetGroup='marcador',polylineOptions=F,polygonOptions=T,
        rectangleOptions=F,circleOptions=F,markerOptions=T,circleMarkerOptions=F,
        singleFeature=T)
  })
  
  observeEvent(input$searchButton,{
    if(input$searchButton){
      geo<-strsplit(input$searchText,",")
      geo<-as.numeric(geo[[1]])
      leafletProxy("map")%>%setView(lng=geo[2],lat=geo[1],zoom=9)
    }
  })
  toListen <- reactive({
    list(input$historia,input$cultivos,input$opacidad)
  })
  observeEvent(toListen(),{
    cultivar<-shapefile4%>%filter((Especie==input$cultivos)&(Year==2019+2*(input$historia>2020)))
    mypalette <- colorFactor(palette="RdYlGn", domain=seq(0,100,20), na.color="transparent")
    mypalette(0:100)
    mytext <- paste("Especie:",cultivar$Especie,"<br/>Aptitud de cultivo: ",cultivar$Aptitud,"<br/>",
                    "Porcentaje de Aptitud: ",cultivar$Clase,"%",sep="")%>%lapply(htmltools::HTML)
    leafletProxy("map")%>%clearShapes()%>%clearControls()%>%
      leafem::addFeatures(shapeEdge$geometry,weight=1,color="black",opacity=1,fillOpacity=0)%>%
      addPolygons(data=cultivar,color="#444444",weight=1,smoothFactor=.5,
              opacity=1.,fillOpacity=input$opacidad,
              fillColor=~mypalette(cultivar$Clase),  #color de las formas
              highlightOptions=highlightOptions(color="white",weight=2,bringToFront=T),
              label=mytext,labelOptions=labelOptions( 
                style=list("font-weight"="normal",padding="3px 8px"), 
                textsize="13px",direction="auto"))%>%
      addLegend(pal=mypalette,values=paleta$Clase,
                labFormat=labelFormat(prefix=paste(paleta$Aptitud,"(",sep=""),suffix="%)"),
                opacity=0.9,title="Aptitud de cultivo",position="topright")
  })
  observeEvent(input$map_draw_new_feature,{
    feature <- input$map_draw_new_feature
    if(length(feature$geometry$coordinates)>1){
      pol<-c(feature$geometry$coordinates[[1]],feature$geometry$coordinates[[2]])
      pol1<-st_point(pol)
      r<-st_within(pol1,shapefile)[[1]]
      r1<-st_within(pol1,shapefile1)[[1]]
      r2<-st_within(pol1,shapefile4)[[1]]
    }else{
      pol<-NULL
      coord<-feature$geometry$coordinates[[1]]
      for(i in seq(coord)){
        pol<-rbind(pol,c(coord[[i]][[1]],coord[[i]][[2]]))
      }
      pol1<-st_polygon(list(pol))
      r<-st_intersects(pol1,shapefile)[[1]]
      r1<-st_intersects(pol1,shapefile1)[[1]]
      r2<-st_intersects(pol1,shapefile4)[[1]]
    }
    if(length(r)>0){
      w<-peso(pol1,shapefile,r)
      w1<-peso(pol1,shapefile1,r1)
      w2<-peso(pol1,shapefile4,r2)
      tmin<-colSums(w*data.frame(shapefile)[r,grepl("tmin",names(shapefile))])%>%t
      if(row(tmin)%>%max()==1){tmin<-tmin%>%t}
      rownames(tmin) <- NULL
      tavg<-colSums(w*data.frame(shapefile)[r,grepl("tavg",names(shapefile))])%>%t
      if(row(tavg)%>%max()==1){tavg<-tavg%>%t}
      rownames(tavg) <- NULL
      tmax<-colSums(w*data.frame(shapefile)[r,grepl("tmax",names(shapefile))])%>%t
      if(row(tmax)%>%max()==1){tmax<-tmax%>%t}
      rownames(tmax) <- NULL
      anno<-names(shapefile)[grepl("tmax",names(shapefile))]%>%gsub("tmax","",.)%>%gsub("me","",.)%>%as.numeric()%>%t%>%t
      data2plot<-cbind(anno,tmin,tavg,tmax)
      colnames(data2plot)<-c("Year","Temp. Min [°C]","Temp. Media [°C]","Temp. Max [°C]")
      data2plot<-as.data.frame(data2plot)
      data2bar<-data.frame(shapefile4)[,!(names(shapefile4) %in% "geometry")]%>%filter(Year==2019+2*(input$historia>2020))
      data2bar<-cbind(w2,data2bar[r2,])
      data2bar2<-aggregate(data2bar$w2,list(data2bar$Especie), sum)
      colnames(data2bar2)<-c("Especie","x")
      data2bar<-merge(data2bar,data2bar2,by="Especie")
      data2bar3<-aggregate(data2bar$w2*data2bar$Clase/data2bar$x,list(data2bar$Especie), sum)
      colnames(data2bar3)<-c("Especie","Porcentaje de Aptitud[%]")
      data2bar3$Clase<-data2bar3[,"Porcentaje de Aptitud[%]"]%/%20*20
      data2bar3<-merge(data2bar3,paleta,by="Clase")
      data2bar<-data2bar3[,c("Especie","Aptitud","Porcentaje de Aptitud[%]")]
      #data2bar<-merge(data2bar[,c("Especie","Aptitud")],data2bar3,by="Especie")
      colnames(data2bar)<-c("Especie","Aptitud de cultivo","Porcentaje de Aptitud[%]")
      data2bar<-data2bar[order(-data2bar$'Porcentaje de Aptitud[%]'),][seq(min(length(data2bar[,1]),10)),]
      chkpoli<-st_as_text(pol1)
      datageo<-data.frame(c("Geometría:",chkpoli))
      datageo<-cbind(datageo,data.frame(c("Área [Km]:",(st_area(pol1)*111.32^2)%>%round(2))))
      datageo<-cbind(datageo,c("Región:",unique(shapefile$REGION[r])))
      datageo<-cbind(datageo,c("Provincia:",paste(unique(shapefile$PROVINCIA[r]), collapse = ', ')))
      datageo<-cbind(datageo,c("Comuna:",paste(unique(shapefile$COMUNA[r]), collapse = ', ')))
      if("POLYGON" %in% chkpoli){
        datageo<-cbind(datageo,c("Proporción de la comuna [%]:",paste(w*100, collapse = ', ')))
      }
      datageo<-datageo%>%t
      output$downloadData <- downloadHandler(
        filename = function() {
          paste0('data_geografica_', Sys.Date(), '.xlsx')
          },
        content = function(con) {
          #template de GORE
          wb<-createWorkbook()
          #
          sheet0<-createSheet(wb,sheetName="Datos Proyectos")
          #logo region
          addPicture("www/Logo_GORE_Nuble.png",sheet0,scale=.1,
                     startRow=1,startColumn=1)
          addPicture("www/inia_telesig.png",sheet0,scale=2,startRow=1,
                     startColumn=6)
          
          addDataFrame(rbind("Gobierno Regional de Ñuble","Instituto de investigaciones agropecuaria (INIA)","Aptitud Térmica para Cultivos",
                             '','','','',
                             "** Es una zonificación térmica, por lo que no se incorpora aptitud de suelo y riego **",
                             "** Lo que se propone en la plataforma, es solo una sugerencia **",
                             "** La decision final es responsabilidad de cada usuario **",
                             "Más informacion enviar correo a mclaret@inia.cl"),
                       sheet0,row.names=F,col.names=F,startRow=1,startColumn=3)
          
          sheet1<-createSheet(wb,sheetName="Datos Geográficos")
          addDataFrame(datageo,
                       sheet1,row.names=F,col.names=F,startRow=1,startColumn=1)
          
          sheet2<-createSheet(wb,sheetName="Aptitud térmica")
          addDataFrame(data2bar,
                       sheet2,row.names=F,col.names=T,startRow=1,startColumn=1)
          
          #save file
          saveWorkbook(wb,file=con) 
          }
        )
    }
    output$text <- renderUI({
      if(length(r)>0){
        fluidRow(column(3,downloadButton('downloadData', 'Descarga',class="mybutton"),
                        tags$head(tags$style(".skin-black .sidebar .mybutton{color: black;}")))
                 )
      }else{h6("No pertenece a la Región")}
    })
  })
}
shinyApp(ui, server)
