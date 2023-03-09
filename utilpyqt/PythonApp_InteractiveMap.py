#!/usr/bin/env python
# coding: utf-8

# pip install PyQtWebEngineWidgets



#-*- coding: utf-8 -*-
"""
Created on Fri Feb  3 16:35:46 2023
@author: Lorraine Delaroque
"""

import sys, time, os
import pytz
import numpy as np

from obspy.clients.fdsn import Client
from obspy import read, UTCDateTime

from DataProcessor_Fonctions import get_depth_color, get_periodogram # Load the function "plot_events" provided in tp_obsp

from PyQt5.QtWidgets import QMenu, QPushButton, QMessageBox, QDialog,QProgressBar, QFrame, QCheckBox
from PyQt5.QtWidgets import QDateTimeEdit, QWidget,QVBoxLayout,QToolBar, QGridLayout,QListWidgetItem
from PyQt5.QtWidgets import QAction, QLineEdit, QDoubleSpinBox, QTabWidget,QSlider,QListWidget
from PyQt5.QtWidgets import QApplication, QLabel, QMainWindow, QComboBox,QHBoxLayout,QDesktopWidget

from matplotlib.figure import Figure
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas


from pyqtlet import L, MapWidget 

from PyQt5 import QtCore, QtGui
from PyQt5.QtCore import Qt, QSettings, QTimer, pyqtSlot

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
        self.setStyleSheet("QMainWindow {background: 'white';}")

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
    
        screen_width = QDesktopWidget().screenGeometry().width()
        toolbar_width = int(screen_width*0.1)
        self.mainToolBar.setFixedWidth(toolbar_width)
  
        
        self.mainToolBar.setStyleSheet("background-color: #f2f2f2; padding: 4px;")
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
        self.clientChoice.setFixedWidth(150)
    
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
        self.locChoice.setText("*")
        self.locChoice.setPlaceholderText("Enter the location")
                
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        self.mainToolBar.addAction(separator)    
        
        # MAGNITUDE ACTION
            
        self.mainToolBar.addWidget(self.magLabel)
        '''
        self.magChoice = QSlider()
        
        layout = QHBoxLayout()
        self.mag = QLabel()
        self.magChoice = QSlider()
        
        # Placement du QSlider à côté du QLabel
        layout.addWidget(self.magChoice)
        layout.addWidget(self.mag)
   
        def show(self):
            self.magLabel.setText(str(self.magChoice.value()))
        
        '''
        '''
        # Customization of the QSlider
        layoutMag = QHBoxLayout()
        
        self.magChoice.setRange(0,10)
        self.magChoice.setSingleStep(0.1)
        self.magChoice.setTickPosition(QSlider.TicksAbove)
        self.magChoice.setTickInterval(1)
        self.magChoice.setValue(4)
        self.magChoice.setOrientation(1)
        self.magChoice.setFixedWidth(125)
        
        label = QLabel("0")
        layoutMag.addWidget(self.magChoice)
        layoutMag.addWidget(label)
        
        widget = QWidget()
        widget.setLayout(layoutMag)
        self.mainToolBar.addWidget(widget)
        
        def update_mag_value(value):
            label.setText(str(value))
            
        self.magChoice.valueChanged.connect(update_mag_value)
      
        
        
        self.mainToolBar.addWidget(self.magChoice)
        '''
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
        self.searchButton.setStyleSheet("""
                background-color: #4CAF50;
                border: none;
                border-radius: 5px;
                color: white;
                padding: 10px;
                text-align: center;
                text-decoration: none;
                display: inline-block;
                font-size: 16px;
                margin-top: 10px;
                cursor: pointer;
                """)
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
        self.clientLabel.setStyleSheet("font-family: bold;")
        self.clientLabel.setAlignment(Qt.AlignLeft)
        
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
            
            global start,end
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
            
            # LOCATION
            global location
            location = self.locChoice.text()
            
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
            global network_select
            network_select = networkValue
            global client
            client = Client(client_select)
            inventory = client.get_stations(network=network_select, level="channel")
            
            
            # DISPLAYING THE STATION 
            global stations
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
            

            self.listStation = QListWidget()    
            self.listStation.itemDoubleClicked.connect(self.buildExamplePopup)

            for net, sta, lat, lon, elev in stations:
                self.name = ".".join([net, sta])
                self.lat=lat
                self.lon=lon
                self.elev=elev
                self.infos = "Name: %s<br/>Lat/Long: (%s, %s)<br/>Elevation: %s m" % (self.name, lat, lon, elev)
                QListWidgetItem(self.name,self.listStation)
                
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
              
                popup_html="<b>%s</b>" %self.infos
                
                self.marker.bindPopup(popup_html)
                #js = "{marker}.on('click',{function})".format(marker=self.marker, function=self.on_marker_clicked)
                
            self.showDialog()
            self.show()
                

            
    
    def showDialog(self):
        self.dialog = QDialog()
        label = QLabel('My stations list')
        label.setAlignment(Qt.AlignCenter)
        
        # Créer un QVBoxLayout
        layout = QVBoxLayout()
        layout.addWidget(label)
        layout.addWidget(self.listStation)
        
        # Appliquer le QVBoxLayout à la QDialog
        self.dialog.setLayout(layout)
        
        # Centrer la QDialog
        self.dialog.setGeometry(500, 500, 400, 400)
        self.dialog.setWindowTitle('Seismic stations list')
        self.dialog.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.listStation.setGeometry(100, 100, 200, 200)
        self.dialog.setModal(False)
        self.dialog.exec_()
    

    @pyqtSlot(QListWidgetItem)           
    def buildExamplePopup(self,item):
        exPopup = ExamplePopup(item.text(),self)
        exPopup.setWindowTitle("Seismic station {} details".format(item.text()))
        exPopup.show()


class ExamplePopup(QDialog):

    def __init__(self, name,parent=None):
        super().__init__(parent)
        self.setWindowIcon(QtGui.QIcon('logo.jpg'))
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
        
        self.name = name
        

        self._contentTab1()
        #self._contentTab2()
        self._contentTab3()
        
        
    def _contentTab1(self):
        sectionName = QLabel("<b>Name:</b> {}".format(self.name))
        self.Description.layout.addWidget(sectionName)
        
        # FAIRE UN TRUC ICI POUR ARRANGER L'AFFICHAGE

        # DESCRIPTION INFORMATION
        for i in stations:
            nameStation = i[0] + "." + i[1]
            if nameStation==self.name:
                latStation = i[2]
                lonStation = i[3]
                elevStation = i[4]
                break
       
        coordinates = QLabel("<b>Coordinates:</b> {}, {}".format(latStation, lonStation))
        self.Description.layout.addWidget(coordinates)
        elevation = QLabel("<b>Elevation:</b> {}".format(elevStation))
        self.Description.layout.addWidget(elevation)
        
        # SEPARATOR
        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setFrameShadow(QFrame.Sunken)
        self.Description.layout.addWidget(separator)
        
        # CHANNEL INITIALIZATION
        Channel = QLabel("<b>Channel: </b>")
        self.Description.layout.addWidget(Channel)
        self.channelChoice = QLineEdit()
        self.Description.layout.addWidget(self.channelChoice)
        self.channelChoice.setPlaceholderText("Enter a channel")
        self.channel = self.channelChoice.text()
        
        # SEPARATOR
        self.Description.layout.addWidget(separator)
        
        # DATE INITIALIZATION
        date = QLabel("Period of the seismic trace")
        self.Description.layout.addWidget(date)
        dateMin = start
        dateMax = end
        
        layoutTime = QHBoxLayout()
        self.startTrace = QDateTimeEdit()
        self.endTrace = QDateTimeEdit()
        layoutTime.addWidget(self.startTrace)
        layoutTime.addWidget(self.endTrace)
        self.Description.layout.addLayout(layoutTime)
        
        # Connecter les signaux pour détecter les changements dans les champs
        # Les seuls champs à remplir pour afficher la trace sont le channel, le starttime et le endtime
        # Les autres sont "globaux" donc déjà avec une valeur.
        self.channelChoice.editingFinished.connect(self.plot_seismic)
        self.startTrace.editingFinished.connect(self.plot_seismic)
        self.endTrace.editingFinished.connect(self.plot_seismic)
        
        # Création d'un widget pour afficher la graphique de la trace sismique
        self.figure = Figure(figsize=(6,4))
        self.canvas = FigureCanvas(self.figure)
        self.Description.layout.addWidget(self.canvas)
        
        # DOWNLOAD DATA
        self.download_button = QPushButton("Download",self)
        self.Description.layout.addWidget(self.download_button)
                
        
        
        # FAIRE EN SORTE QUE CA AFFICHE 
    def plot_seismic(self):
            
        if (self.channelChoice.text() and self.startTrace.dateTime() and self.endTrace.dateTime()):
            '''
            pyStartTime = self.startTrace.toPyDateTime()
            UTCStartTime = pyStartTime.astimezone(pytz.UTC)
            
            pyEndTime = self.endTrace.toPyDateTime()
            UTCEndTime = pyEndTime.astimezone(pytz.UTC)
            '''
            self.nameStation = self.name.split('.')[1]

            
            self.st = client.get_waveforms(
                network = network_select,
                station = self.nameStation,
                location = location,
                channel = self.channelChoice.text(),
                starttime = UTCDateTime("2018-02-12T03:08:02"),
                endtime = UTCDateTime("2018-02-12T03:08:02") +90,
                attach_response = True,
            )
            
            self.st.write('./trace.mseed')
            st2 = read("trace.mseed")
            self.figure.clear()
            st2.plot(fig=self.figure)

            self.canvas.draw()
            os.remove('trace.mseed')
            
            self.download_button.clicked.connect(self.download_data)
    
    def download_data(self):
        # ESSAYER DE TROUVER UNE ALTERNATIVE POUR LE CHANNEL BH* OU BH?
        self.st.write('./{}_{}.mseed'.format(network_select,self.nameStation))
            
            
        # si la valeur change, plotter la trace
        #self.plot_seismic(name,channel,startTrace,endTrace)
        
    def _contentTab2(self):
        
        x = self.st[0].data
        sampling_rate = self.st[0].stats.sampling_rate
        
        # Print the sampling rate on the window
        samplingLabel = QLabel("Sampling rate: {}".format(sampling_rate))
        self.Periodogram.layout.addWidget(samplingLabel)
        
        
        freqs,psd = get_periodogram(x, fs = sampling_rate, semilog = True, show = False)
        
        # Creation de la figure
        
        self.figure_spectrum = Figure(figsize=(6,4),dpi=100)
        ax = self.figure_spectrum.add_subplot(111)
        ax.semilogx(freqs, 10 * np.log10(psd))
        ax.set_xlabel('Frequency (Hz)')
        ax.set_ylabel('Power (dB)')
        
        self.canvas_spectrum = FigureCanvas(self.figure_spectrum)
        self.Periodogram.layout.addWidget(self.canvas_spectrum)    
        
        self.canvas_spectrum.draw()
        
        #self.figure_spectrum.clear()
        
        

    def _contentTab3(self):
        instrumental_response = QCheckBox("Remove instrumental response")
        self.Processing.layout.addWidget(instrumental_response)
        
        remove_mean = QCheckBox("Remove the mean")
        self.Processing.layout.addWidget(remove_mean)
        
        
        

        
            
  
class SplashScreen(QMainWindow):
    def __init__(self):
        super().__init__()
        
        
        self.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.setWindowTitle("Geodpy Project - Python Menus & Toolbars")
        self.setFixedSize(1000, 800)
        self.setStyleSheet("QMainWindow {background: 'white';}")
        self.central_widget = QWidget(self)
        self.setCentralWidget(self.central_widget)
        
        # IPGP Logo
        self.logo_ipgp=QLabel(self)
        pixmap_ipgp=QtGui.QPixmap('logo ipgp.png').scaled(600,375)
        self.logo_ipgp.setPixmap(pixmap_ipgp)
        self.logo_ipgp.setAlignment(Qt.AlignCenter)
        self.logo_ipgp.setStyleSheet("margin-top: 20px;")
        
        # PyQt5 logo
        self.logo_pyqt = QLabel(self)
        pixmap_pyqt = QtGui.QPixmap('pyqt_logo.png').scaled(100,100)
        self.logo_pyqt.setPixmap(pixmap_pyqt)
        self.logo_pyqt.setAlignment(Qt.AlignCenter)
        
        
        #•self.central_widget.setLayout(QVBoxLayout(self.central_widget))
        #self.central_widget.layout().addWidget(self.logo_label)
        
        chargement = QLabel("Loading...")
        self.pbar = QProgressBar(self)
        self.pbar.setStyleSheet("""
           QProgressBar {

               border-radius: 6px;
               padding: 1px;
               width: 12px;
               text-align:center;
               
           }
           QProgressBar::chunk {
               border-radius: 6px;
               background-color: crimson;
           }
        """)
        self.pbar.setGeometry(30, 40, 150, 25)

        # Créer le layout de la grille
        grid_layout = QGridLayout(self.central_widget)
        grid_layout.setAlignment(Qt.AlignCenter)
        grid_layout.setContentsMargins(50,50,50,50)
        grid_layout.setVerticalSpacing(40)
        

        # Ajouter les widgets à la grille
        grid_layout.addWidget(self.logo_ipgp, 0, 0, Qt.AlignCenter) # Aligner au centre de la première colonne
        grid_layout.addWidget(self.logo_pyqt, 1, 0, Qt.AlignCenter)
        grid_layout.addWidget(chargement, 2, 0, Qt.AlignCenter) # Aligner au centre de la deuxième colonne
        grid_layout.addWidget(self.pbar, 3, 0) # Étirer sur les deux colonnes dans la troisième ligne
        
        self._center_screen()
        self.show()
        
        self.timer = QTimer()
        self.timer.setSingleShot(True)
        self.timer.timeout.connect(self.close)
        self.timer.start(5000)
        
    def progressUpdate(self, progress):
        self.pbar.setValue(progress)
    
    def _center_screen(self):
        screen_rect = QApplication.desktop().availableGeometry(self)
        window_rect = self.geometry()
        dx = int((screen_rect.width() - window_rect.width()) / 2)
        dy = int((screen_rect.height() - window_rect.height()) / 2)
        self.move(dx,dy)

        


if __name__ == "__main__":
    app = QApplication(sys.argv)
    splash_screen = SplashScreen()
    app.processEvents()
    #app.aboutToQuit(saveSettings)
    win = Window()
    #♣dia = MyDialog()
    win.show()
    #dia.show()
    sys.exit(app.exec_())