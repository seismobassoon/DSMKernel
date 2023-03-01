#!/usr/bin/env python
# coding: utf-8

# pip install PyQtWebEngineWidgets



#-*- coding: utf-8 -*-
"""
Created on Fri Feb  3 16:35:46 2023
@author: Lorraine Delaroque
"""

import sys, time

import io
import pytz

from obspy.clients.fdsn import Client

from DataProcessor_Fonctions import get_depth_color # Load the function "plot_events" provided in tp_obsp

from PyQt5.QtWidgets import QMenu, QPushButton, QMessageBox, QDialog,QProgressBar
from PyQt5.QtWidgets import QDateTimeEdit, QWidget,QVBoxLayout,QToolBar, QGridLayout
from PyQt5.QtWidgets import QAction, QLineEdit, QDoubleSpinBox, QTabWidget
from PyQt5.QtWidgets import QApplication, QLabel, QMainWindow, QComboBox


from pyqtlet import L, MapWidget 

from PyQt5 import QtCore, QtGui
from PyQt5.QtCore import Qt, QSettings, QTimer

class Window(QMainWindow):
    
    # DEF MAIN WINDOW
    #-----------------------------------------
    def __init__(self, parent=None):
        
        # INITIALIZER
        #-----------------------------------------------------------------------------------------------------
        super().__init__(parent)
        
        Progress = 10
        splash_screen.progressUpdate(Progress)
        time.sleep(2)
        
        self.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.setWindowTitle("Geodpy Project - Python Menus & Toolbars")

        self.resize(1000, 800)
        self.mapWidget = MapWidget()
        
        Progress = 30
        splash_screen.progressUpdate(Progress)
        time.sleep(2)


        self.setCentralWidget(self.mapWidget)
        QtCore.QMetaObject.connectSlotsByName(self)

        # MAP GENERATOR
        #-----------------------------------------------------------------------------------------------------
        self.map = L.map(self.mapWidget)
        self.map.setView([0, 0], 2)
        
        Progress = 50
        splash_screen.progressUpdate(Progress)
        time.sleep(2)
        

        
        # ADD MAP LAYERS
        #-----------------------------------------------------------------------------------------------------
        osm_layer = L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
            'attribution':'Map data &copy; OpenStreetMap contributors'})
        stamen_terrain_layer = L.tileLayer('http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png')
        stamen_toner_layer = L.tileLayer('http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png')
        stamen_water_layer = L.tileLayer('http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png')

        layers = {'OpenStreetMap':osm_layer,'Stamen Terrain':stamen_terrain_layer,'Stamen Toner':stamen_toner_layer,'Stamen Water Color':stamen_water_layer}
        
        # ADD DRAWINGS
        #-----------------------------------------------------------------------------------------------------
        self.drawControl = L.control.draw()
        self.map.addControl(self.drawControl)
        
        self.layersControl=L.control.layers(layers).addTo(self.map)
        
        Progress = 75
        splash_screen.progressUpdate(Progress)
        time.sleep(2)
        
        
        # CONTENT
        #----------------------------------------------------------------------------------------------------
        self._createActions()
        self._createMenuBar()
        self._createToolBars()
        self._connectActions()
        
        Progress = 100
        splash_screen.progressUpdate(Progress)
        time.sleep(2)
        
        # SELECT COORDINATES MANUALLY
        #-------------------------------------------------
     
        def coords(self,x,coord_lat,coord_lng):
            lat,lng = x['latlng']['lat'],x['latlng']['lng']
            #print(lat,lng)
            coord_lat.append(lat)
            coord_lng.append(lng)
            print(coord_lat)
            print(coord_lng)
          
            global minlat 
            minlat= min(coord_lat)
            global maxlat
            maxlat=max(coord_lat)
            global minlng
            minlng=min(coord_lng)
            global maxlng
            maxlng=max(coord_lng)
            print(minlat,maxlat,minlng,maxlng)
            self.minLatChoice.setValue(minlat)
            self.maxLatChoice.setValue(maxlat)
            self.minLonChoice.setValue(minlng)
            self.maxLonChoice.setValue(maxlng)
            
       
        coord_lat=[]
        coord_lng=[]
        
        self.drawControl.featureGroup.toGeoJSON(lambda x: print(x))
        #map.on('draw:created', function (event)
        self.map.clicked.connect(lambda x: coords(self,x,coord_lat,coord_lng))



    # DEF MENU BAR
    #-----------------------------------------------
    def _createMenuBar(self):
        menuBar = self.menuBar()
        
        # Creating menus using a QMenu object
        fileMenu = QMenu("&About", self)      
        menuBar.addMenu(fileMenu)
        fileMenu.addAction(self.aboutAction)
        
        # Creating menus using a title
        helpMenu = menuBar.addMenu("&Help")
        helpMenu.addAction(self.helpContentAction)
        
        exitMenu = menuBar.addMenu("&Exit")
        exitMenu.addAction(self.exitAction)
                
        
        
    
    def _createToolBars(self):     
        self.settings = QSettings("MyQtApp", "App1")
        # Using a title
        self.mainToolBar = QToolBar("Toolbar",self)
        self.addToolBar(Qt.RightToolBarArea,self.mainToolBar)
        self.mainToolBar.setMovable(False)
        
        # Adding a toggle button in order to hide or not the toolbar
        """
        self.hideButton=QPushButton('Hide Toolbar',self)
        self.hideButton.clicked.connect(self.toggleToolbar)
        """
        #self.mainToolBar.addWidget(self.hideButton)
        
        # Using a QToolBar object and a toolbar area
        self.addToolBar(Qt.RightToolBarArea, self.mainToolBar)
        
        # ADDING THE TOOLS IN THE TOOL BAR
        #----------------------------------------------------
        # CLIENT ACTION
        self.mainToolBar.addWidget(self.clientLabel)
        self.clientChoice=QComboBox(self)
        self.mainToolBar.addWidget(self.clientChoice)
        self.clientChoice.addItems(["AUSPASS","BRG","EIDA","EMSC","ETH","GEOFON","GEONET","GFZ","ICGC","IESDMC","INGV","IPGP","IRIS","IRISPH5","ISC","KNMI","KOERI","LMU","NCEDC","NIEP","NOA","ODC","RASPISHAKE","RESIF","RESIFPH5","SCEDC","UIB-NORSAR","USGS","USP"])
        # ESSAYER DE CHOISIR TOUS LES CLIENTS
        
        
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        self.mainToolBar.addAction(separator)    
        
        # DATE ACTION
        self.mainToolBar.addWidget(self.dateLabel)
        layout1=QLabel("From: ")
        layout2=QLabel("To: ")
        
        self.dateStartChoice = QDateTimeEdit(self,calendarPopup=True)
        self.dateEndChoice = QDateTimeEdit(self,calendarPopup=True)
        
        self.mainToolBar.addWidget(layout1)
        self.mainToolBar.addWidget(self.dateStartChoice)
        self.mainToolBar.addWidget(layout2)
        self.mainToolBar.addWidget(self.dateEndChoice)

                
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        self.mainToolBar.addAction(separator)    
        
        # COORDINATES ACTION
        self.mainToolBar.addWidget(self.coorLabel)
        layoutCoor1=QLabel("Minimum latitude: ")
        layoutCoor2=QLabel("Maximum latitude: ")
        layoutCoor3=QLabel("Minimum longitude: ")
        layoutCoor4=QLabel("Maximum longitude: ")
       
        self.minLatChoice = QDoubleSpinBox()
        self.maxLatChoice = QDoubleSpinBox()
        self.minLonChoice = QDoubleSpinBox()
        self.maxLonChoice = QDoubleSpinBox()
        
        self.mainToolBar.addWidget(layoutCoor1)
        self.mainToolBar.addWidget(self.minLatChoice)
        self.mainToolBar.addWidget(layoutCoor2)
        self.mainToolBar.addWidget(self.maxLatChoice)
        self.mainToolBar.addWidget(layoutCoor3)
        self.mainToolBar.addWidget(self.minLonChoice)
        self.mainToolBar.addWidget(layoutCoor4)
        self.mainToolBar.addWidget(self.maxLonChoice)
        
        self.minLatChoice.setMinimum(-90)
        self.minLatChoice.setMaximum(90)
        self.maxLatChoice.setMinimum(-90)
        self.maxLatChoice.setMaximum(90)
        self.minLonChoice.setMinimum(-180)
        self.minLonChoice.setMaximum(180)
        self.maxLonChoice.setMinimum(-180)
        self.maxLonChoice.setMaximum(180)
                
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        self.mainToolBar.addAction(separator)    
        
        # LOCATION ACTION
        self.mainToolBar.addWidget(self.locLabel)
        self.locChoice = QLineEdit()
        self.mainToolBar.addWidget(self.locChoice)
        self.locChoice.setMaxLength(5)
        self.locChoice.setPlaceholderText("Enter the location")
                
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        self.mainToolBar.addAction(separator)    
        
        # MAGNITUDE ACTION
        self.mainToolBar.addWidget(self.magLabel)
        self.magChoice = QDoubleSpinBox()
        self.mainToolBar.addWidget(self.magChoice)
        self.magChoice.setMinimum(0)
        self.magChoice.setMaximum(10)
        
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        self.mainToolBar.addAction(separator)    
        
        # NETWORK ACTION
        
        
        self.mainToolBar.addWidget(self.networkLabel)
        self.networkChoice = QComboBox(self)
        self.mainToolBar.addWidget(self.networkChoice)
        self.clientChoice.currentIndexChanged.connect(self.updateNetworkChoice)
        
        
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        self.mainToolBar.addAction(separator)
        
        # SEARCH ACTION ------------------------------------------
        self.searchButton = QPushButton("Search")
        self.mainToolBar.addWidget(self.searchButton)
        self.searchButton.clicked.connect(self.startSearch)
        
    def toggleToolbar(self):
        if self.mainToolBar.isVisible():
            self.mainToolBar.setVisible(False)
            self.hideButton.setText('Show Toolbar')
        else:
            self.mainToolBar.setVisible(True)
            self.hideButton.setText('Hide Toolbar')

        
    
        
        # FINISHED - NOTHING TO CHANGE
    def updateNetworkChoice(self):
        self.networkChoice.clear()
        selectedValue = self.clientChoice.currentText()
        if selectedValue == "AUSPASS":
            self.networkChoice.addItems([""])
        elif selectedValue == "BRG":
            self.networkChoice.addItems(["BM"])
        elif selectedValue == "EIDA":
            self.networkChoice.addItems(["7J","ZN"])
        elif selectedValue == "EMSC":
            self.networkChoice.addItems([""])
        elif selectedValue == "ETH":  
            self.networkChoice.addItems(["1C","6H","9E","9L","EH","NA","NL","QZ","X5","XI","XJ","XM","Y1","YJ","YN","YQ","YR","YY","Z4","ZF"])
        elif selectedValue == "GEOPHON":         
            self.networkChoice.addItems(["GE"])   
        elif selectedValue == "GEONET":    
            self.networkChoice.addItems(["GC"])
        elif selectedValue == "GFZ":         
            self.networkChoice.addItems(["4Q","GX","ZE"])
        elif selectedValue == "ICGC":        
            self.networkChoice.addItems([""])
        elif selectedValue == "IESDMC":     
            self.networkChoice.addItems(["3D","3Q","4P","TV","X3","Y1","YD","ZH"])
        elif selectedValue == "IPGP":    
            self.networkChoice.addItems(["G"])
        elif selectedValue == "IRIS":    
            self.networkChoice.addItems(["1M","4R","EA","EI","II","IU","YW"])
        elif selectedValue == "IRISPH5":          
            self.networkChoice.addItems([""])
        elif selectedValue == "ISC":
            self.networkChoice.addItems(["1E","1H","5D","6F","8C","8J","HS",'IM',"JA","OF","TH","XA","XI","XJ","XL","XP","XS","XW","YD","YI","YJ","YK","YS","YV","Z5","ZA","ZD","ZM","ZS","ZT"])
        elif selectedValue == "KNMI":
            self.networkChoice.addItems(["NL","NA"])
        elif selectedValue == "KOERI":
            self.networkChoice.addItems(["KO"])
        elif selectedValue == "LMU":
            self.networkChoice.addItems(["Z6"])
        elif selectedValue == "NCEDC":
            self.networkChoice.addItems([""])
        elif selectedValue == "NIEP":
            self.networkChoice.addItems(["Y8"])
        elif selectedValue == "NOA":
            self.networkChoice.addItems([""])
        elif selectedValue == "ODC":
            self.networkChoice.addItems([""])
        elif selectedValue == "RASPISHAKE":
            self.networkChoice.addItems([""])
        elif selectedValue == "RESIF":
            self.networkChoice.addItems(["FR","ZO"])
        elif selectedValue == "RESIFPH5":
            self.networkChoice.addItems([""])
        elif selectedValue == "UIB-NORSAR":
            self.networkChoice.addItems([""])
        elif selectedValue == "USGS":
            self.networkChoice.addItems(["3F","GT","IU","MI","NC","NT","UL","XC","XG","XU","XZ","Y3","YV","Z7","ZZ"])
        elif selectedValue == "USC":
            self.networkChoice.addItems([""])
    


    def _createActions(self):
        # Creating action using the first constructor
        self.clientLabel = QLabel(self)
        self.clientLabel.setText("<b>Client</b>")
        self.clientLabel.setAlignment(Qt.AlignCenter)
        
        # Creating actions using the second constructor
        self.networkLabel = QLabel(self)
        self.networkLabel.setText("<b>Network</b>")
        self.networkLabel.setAlignment(Qt.AlignCenter)
        
        self.magLabel = QLabel(self)
        self.magLabel.setText("<b>Magnitude min</b>")
        self.magLabel.setAlignment(Qt.AlignCenter)
        
        self.dateLabel = QLabel(self)
        self.dateLabel.setText("<b>Date</b>")
        self.dateLabel.setAlignment(Qt.AlignCenter)
        
        self.locLabel = QLabel(self)
        self.locLabel.setText("<b>Location</b>")
        self.locLabel.setAlignment(Qt.AlignCenter)
        
        self.coorLabel = QLabel(self)
        self.coorLabel.setText("<b>Coordinates</b>")
        self.coorLabel.setAlignment(Qt.AlignCenter)
        
        #self.searchAction = QAction("&Search...",self)
        
        self.exitAction = QAction("&Exit", self)
        self.helpContentAction = QAction("&Help Content", self)
        self.aboutAction = QAction("&About", self)
        

    def about(self):
        # Logic for showing an about dialog content goes here...
        self.centralWidget.setText("<b>About</b> clicked")
        
    def helpContent(self):
        # Logic for launching help goes here...
        self.centralWidget.setText("<b>Help > Help Content</b> clicked")
    
    # CONNECTING ACTIONS
    #-------------------------------------------------------------------
    def _connectActions(self):
        # Connect File actions
        self.exitAction.triggered.connect(self.close)
        # Connect Help actions
        self.helpContentAction.triggered.connect(self.helpContent)
        self.aboutAction.triggered.connect(self.about)
   
    
    def startSearch(self):
        if not self.locChoice.text():
            error_message = QMessageBox(QMessageBox.Critical, "Error","All field must be filled!",QMessageBox.Ok)
            error_message.exec()
                
            style = "border: 1px solid red;"
            if not self.locChoice.text():
                self.locChoice.setStyleSheet(style)
                #if not self.networkChoice.text():
                    #self.locChoice.setStyleSheet(style)
        else:
            # RESET OF THE INITIAL STYLE PARAMETERS
            style = "border: 1px solid black;"
            self.locChoice.setStyleSheet(style)
            #self.locChoice.setStyleSheet(style)
            # DATE INITIALIZATION (convert QDateTime in UTCDateTime)

            localStartTime = self.dateStartChoice.dateTime()
            pyStartTime = localStartTime.toPyDateTime()
            UTCStartTime = pyStartTime.astimezone(pytz.UTC)
            
            localEndTime = self.dateEndChoice.dateTime()
            pyEndTime = localEndTime.toPyDateTime()
            UTCEndTime = pyEndTime.astimezone(pytz.UTC)
            
            start = UTCStartTime
            end = UTCEndTime

            #start = UTCDateTime(self.dateStartChoice)
            #end = UTCDateTime(self.dateEndChoice)
            
            # COORDINATES INITIALIZATION (convert QDoubleSpinBox to int)
            valueMinLat = self.minLatChoice.value()
            intMinLat = int(valueMinLat)
            valueMaxLat = self.maxLatChoice.value()
            intMaxLat = int(valueMaxLat)
            valueMinLon = self.minLonChoice.value()
            intMinLon = int(valueMinLon)
            valueMaxLon = self.maxLonChoice.value()
            intMaxLon = int(valueMaxLon)
            
            # MAGNITUDE INITIALIZATION (convert QDoubleSpinBox to int)
            valueMagMin = self.magChoice.value()
            intMag = int(valueMagMin)
            
            # CLIENT INITIALIZATION (convert QComboBox to str)
            client_select = str(self.clientChoice.currentText())
            
            # NETWORK INITIALIZATION (convert QLineEdit to str)
            networkValue = str(self.networkChoice.currentText())
            #networkValue = self.networkChoice.text()
            #strNetwork = str(networkValue)
            
            events_center = Client(client_select).get_events(    
                minlatitude = intMinLat,
                maxlatitude = intMaxLat,
                minlongitude = intMinLon,
                maxlongitude = intMaxLon,
                
                minmagnitude = intMag,
                starttime=start,
                endtime=end,
            )
            print("\nFound %s event(s) from %s Data Center:\n" % (len(events_center),client_select))
            print(events_center)

            
            # NETWORK INITIALIZATION
            network_select = networkValue
            client = Client(client_select)
            inventory = client.get_stations(network=network_select, level="channel")
            
            
            # DISPLAYING THE STATION 
            stations = []
            for net in inventory:  # in fact this loop is only necessary for multiple networks
                for sta in net:
                    stations.append(
                        [net.code, sta.code, sta.latitude, sta.longitude, sta.elevation]
                    )
            comments='ISC'
            origin=[0, 0]
            
            # plot_events_stations(self,events_center, stations, origin=[0, 0], zoom=2, color="blue",comments="ISC")
            for event in events_center:
                for origin, magnitude in zip(event.origins, event.magnitudes):
                    lat, lon, depth, mag = (
                        origin.latitude,
                        origin.longitude,
                        origin.depth,
                        magnitude.mag,
                    )
                    infos = "Lat/Long: (%s %s)<br/>Depth: %s m<br/>Magnitude: %s<br/>Comment: %s" % (lat, lon, depth, mag, comments)
                    self.events = L.circleMarker([lat, lon], {
                        'radius':50 * 2 ** (mag) / 2 ** 10,
                        #tooltip=infos,
                        'color':get_depth_color(depth),
                        #fill=True,
                        'fillColor':"#FF8C00"
                    })
                    self.map.addLayer(self.events)
                    popup_html = "<em> %s </em>" % infos
                    self.events.bindPopup(popup_html)
                    
            #self.update_map()
            def openWindow():
                dialog = QDialog()
                dialog.setWindowTitle("Station details")
                dialog.show
            

            #
            for net, sta, lat, lon, elev in stations:
                self.name = ".".join([net, sta])
                self.lat=lat
                self.lon=lon
                self.elev=elev
                self.infos = "Name: %s<br/>Lat/Long: (%s, %s)<br/>Elevation: %s m" % (self.name, lat, lon, elev)
                
                self.marker = L.marker([lat, lon], {
                    #tooltip=infos,
                    'color':"blue",
                    'fillColor':"#FF8C00",
                    #number_of_sides=3,
                    #radius=10,
                    'fillOpacity':0.3,
                    #rotation=30,

                })
                
                self.map.addLayer(self.marker)
              
                popup_html="""
                <!DOCTYPE html>
                <html>
                    <button id ='popup-button' onclick="openWindow()">Click me</button> 

                	<script type="text/javascript">
                        function openWindow() {
                            window.open("https://github.com/LorrN13")
                            }
                    </script>
                </html>
                    
                """
 
                
                self.marker.bindPopup(popup_html)

               
                
                #popupButton = self.marker.findElement A CONTINUER VOIR TELEPHONE
                

 
            #self.update_map()
              
        
    def update_map(self):

        data = io.BytesIO()
        self.map.save(data,close_file=False)
        self.qwebengine.setHtml(data.getvalue().decode()) 
        
    
        

class MyDialog(QDialog):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("Seismic station details")
        self.resize(500,600)
        
        # TAB DESCRIPTION/PERIODOGRAM/PROCESSING
        self.tabs = QTabWidget()
        self.Description = QWidget()
        self.Periodogram = QWidget()
        self.Processing = QWidget()
        
        #ADD WIDGET & CONTENT AT EVERY TAB
        self.tabs.addTab(self.Description,"Description")
        self.tabs.addTab(self.Periodogram,"Periodogram")
        self.tabs.addTab(self.Processing,"Processing")
        
        # Ajouter des layouts à chaque onglet
        self.Description.layout = QVBoxLayout()
        self.Periodogram.layout = QVBoxLayout()
        self.Processing.layout = QVBoxLayout()

        # Ajouter des widgets à chaque layout d'onglet
        self.Description.layout.addWidget(QWidget())
        self.Periodogram.layout.addWidget(QWidget())
        self.Processing.layout.addWidget(QWidget())

        # Définir les layouts pour chaque onglet
        self.Description.setLayout(self.Description.layout)
        self.Periodogram.setLayout(self.Periodogram.layout)
        self.Processing.setLayout(self.Processing.layout)

        # Ajouter le widget QTabWidget à la fenêtre MyDialog
        self.layout = QVBoxLayout()
        self.layout.addWidget(self.tabs)
        self.setLayout(self.layout)
        
        self._contentTab1()
        
        
    def _contentTab1(self):
        sectionName = QLabel("<b>Name:</b> [...]")
        self.Description.layout.addWidget(sectionName)
        
        code = QLabel("<b>Code:</b> [...]")
        self.Description.layout.addWidget(code)
        coordinates = QLabel("<b>Coordinates:</b> [...]")
        self.Description.layout.addWidget(coordinates)
        elevation = QLabel("<b>Elevation:</b> [...]")
        self.Description.layout.addWidget(elevation)
        
        separator = QAction(self)
        separator.setSeparator(True)  
        self.Description.addAction(separator) 
        
        Channel = QLabel("<b> Channel </b>")
        self.Description.layout.addWidget(Channel)
        channelChoice = QLineEdit()
        self.Description.layout.addWidget(channelChoice)
        channelChoice.setPlaceholderText("Enter a channel")
        
        
class SplashScreen(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("My Application")
        self.setFixedSize(1000, 800)
        self.setStyleSheet("QMainWindow {background: 'white';}")
        self.central_widget = QWidget(self)
        self.setCentralWidget(self.central_widget)
        
        self.logo_label=QLabel(self)
        pixmap=QtGui.QPixmap('logo ipgp.png')
        self.logo_label.setPixmap(pixmap)
        self.logo_label.setAlignment(Qt.AlignCenter)
        
        
        #•self.central_widget.setLayout(QVBoxLayout(self.central_widget))
        #self.central_widget.layout().addWidget(self.logo_label)
        
        chargement = QLabel("Loading...")
        self.pbar = QProgressBar(self)
        self.pbar.setGeometry(30, 40, 200, 25)

        # Créer le layout de la grille
        grid_layout = QGridLayout(self.central_widget)
        grid_layout.setColumnStretch(0, 1) # Première colonne étirée pour aligner à gauche
        grid_layout.setColumnStretch(1, 1) # Deuxième colonne étirée pour aligner à droite
        grid_layout.setRowStretch(0, 1) # Première ligne étirée pour aligner en haut
        grid_layout.setRowStretch(2, 1) # Troisième ligne étirée pour aligner en bas

        # Ajouter les widgets à la grille
        grid_layout.addWidget(self.logo_label, 1, 0, Qt.AlignCenter) # Aligner au centre de la première colonne
        grid_layout.addWidget(chargement, 2, 0, Qt.AlignCenter) # Aligner au centre de la deuxième colonne
        grid_layout.addWidget(self.pbar, 3, 0) # Étirer sur les deux colonnes dans la troisième ligne
        
        self.show()
        
        self.timer = QTimer()
        self.timer.setSingleShot(True)
        self.timer.timeout.connect(self.close)
        self.timer.start(5000)
        
    def progressUpdate(self, progress):
        self.pbar.setValue(progress)

        


if __name__ == "__main__":
    app = QApplication(sys.argv)
    splash_screen = SplashScreen()
    #app.aboutToQuit(saveSettings)
    win = Window()
    #♣dia = MyDialog()
    win.show()
    #dia.show()
    sys.exit(app.exec_())