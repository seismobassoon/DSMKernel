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
from obspy import UTCDateTime

from DataProcessor_Fonctions import get_depth_color, get_periodogram # Load the function "plot_events" provided in tp_obsp

from PyQt5.QtWidgets import QMenu, QPushButton, QMessageBox, QDialog,QProgressBar, QFrame, QCheckBox
from PyQt5.QtWidgets import QDateTimeEdit, QWidget,QVBoxLayout,QToolBar, QGridLayout,QListWidgetItem,QTreeView
from PyQt5.QtWidgets import QAction, QLineEdit, QDoubleSpinBox, QTabWidget,QSlider,QListWidget, QRadioButton
from PyQt5.QtWidgets import QApplication, QLabel, QMainWindow, QComboBox,QHBoxLayout,QDesktopWidget,QButtonGroup

from matplotlib.figure import Figure
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas


from pyqtlet import L, MapWidget 

from PyQt5 import QtCore, QtGui
from PyQt5.QtGui import QPixmap, QStandardItemModel
from PyQt5.QtCore import Qt, QSettings, QTimer, pyqtSlot, pyqtSignal

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
        self.setWindowTitle("Geodpy Project - Python application for scientific research")
        self.setStyleSheet("""
                           QMainWindow {
                               background: 'white';
                                }
                           
                           QToolBar {
                               transition: height 0.5s ease;
                               }
                           
                           """)

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
        satellite_layer = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')

        layers = {'OpenStreetMap':osm_layer,'Stamen Terrain':stamen_terrain_layer,'Stamen Toner':stamen_toner_layer,'Stamen Water Color':stamen_water_layer,'Satellite':satellite_layer}
        
        # ADD DRAWINGS
        #-----------------------------------------------------------------------------------------------------
        self.drawControl = L.control.draw()
        #self.map.addControl(self.drawControl)
        satellite_layer.addTo(self.map)
        self.layersControl=L.control.layers(layers).addTo(self.map)
        
        Progress = 75
        splash_screen.progressUpdate(Progress)
        time.sleep(2)
        
        
        # CONTENT
        #----------------------------------------------------------------------------------------------------
        self._createActions()
        self._createMenuBar()
        self._createToolBars()
        self._createToolBars2()
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
            # ADD LATITUDE/LONGITUDE VALUES TO BOTH TOOLBARS
            print(minlat,maxlat,minlng,maxlng)
            self.minLatChoice.setValue(minlat)
            self.maxLatChoice.setValue(maxlat)
            self.minLonChoice.setValue(minlng)
            self.maxLonChoice.setValue(maxlng)
            
            self.minLatEventChoice.setValue(minlat)
            self.maxLatEventChoice.setValue(maxlat)
            self.minLonEventChoice.setValue(minlng)
            self.maxLonEventChoice.setValue(maxlng)
            
       
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
        
        searchStation = menuBar.addMenu("&Search by station")
        searchStation.addAction(self.searchStationAction)
        
        searchEvent = menuBar.addMenu("&Search by event")
        searchEvent.addAction(self.searchEventAction)
        
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
        self.from_Date=QLabel("From: ")
        self.to_Date=QLabel("To: ")
        
        self.dateStartChoice = QDateTimeEdit(self,calendarPopup=True)
        self.dateEndChoice = QDateTimeEdit(self,calendarPopup=True)
        
        self.mainToolBar.addWidget(self.from_Date)
        self.mainToolBar.addWidget(self.dateStartChoice)
        self.mainToolBar.addWidget(self.to_Date)
        self.mainToolBar.addWidget(self.dateEndChoice)

                
        # SEPARATING----------------------------------------------

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

        self.mainToolBar.addAction(separator)    
        
        # LOCATION ACTION
        self.mainToolBar.addWidget(self.locLabel)
        self.locChoice = QLineEdit()
        self.mainToolBar.addWidget(self.locChoice)
        self.locChoice.setMaxLength(5)
        self.locChoice.setText("*")
        self.locChoice.setPlaceholderText("Enter the location")
                
        # SEPARATING----------------------------------------------
 
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

        self.mainToolBar.addAction(separator)    
        
        # NETWORK ACTION
        
        
        self.mainToolBar.addWidget(self.networkLabel)
        self.networkChoice = QComboBox(self)
        self.mainToolBar.addWidget(self.networkChoice)
        self.clientChoice.currentIndexChanged.connect(self.updateNetworkChoice)
        
        
        # SEPARATING----------------------------------------------

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
        self.mainToolBar.setVisible(False)
    
        
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
            
    # TOOLBAR OF THE EVENT SECTION -----------------------------------------------------------------------------------        
    def _createToolBars2(self):
        # TOOLBAR SET UP
        self.eventToolBar = QToolBar("Toolbar",self)
    
        screen_width = QDesktopWidget().screenGeometry().width()
        toolbar_width = int(screen_width*0.1)
        self.eventToolBar.setFixedWidth(toolbar_width)
  
        
        self.eventToolBar.setStyleSheet("background-color: #f2f2f2; padding: 4px;")
        self.addToolBar(Qt.RightToolBarArea,self.eventToolBar)
        self.eventToolBar.setMovable(False)
        self.eventToolBar.setVisible(False)
        
        # CLIENT SELECTION
        self.eventToolBar.addWidget(self.clientEventLabel)
        self.clientEventChoice = QComboBox(self)
        self.eventToolBar.addWidget(self.clientEventChoice)
        self.clientEventChoice.setFixedWidth(150)
    
        self.clientEventChoice.addItems(["AUSPASS","BRG","EIDA","EMSC","ETH","GEOFON","GEONET","GFZ","ICGC","IESDMC","INGV","IPGP","IRIS","IRISPH5","ISC","KNMI","KOERI","LMU","NCEDC","NIEP","NOA","ODC","RASPISHAKE","RESIF","RESIFPH5","SCEDC","UIB-NORSAR","USGS","USP"])
     
        # SEPARATING
        separator = QAction(self)
        separator.setSeparator(True)  
        self.eventToolBar.addAction(separator)    
        
        # DATE OF THE EVENT
        self.eventToolBar.addWidget(self.dateEventLabel)
        self.from_eventDate = QLabel("From: ")
        self.to_eventDate = QLabel("To: ")
        
        self.dateStartEventChoice = QDateTimeEdit(self,calendarPopup=True)
        self.dateEndEventChoice = QDateTimeEdit(self,calendarPopup=True)
        
        self.eventToolBar.addWidget(self.from_eventDate)
        self.eventToolBar.addWidget(self.dateStartEventChoice)
        self.eventToolBar.addWidget(self.to_eventDate)
        self.eventToolBar.addWidget(self.dateEndEventChoice)
        
        self.eventToolBar.addAction(separator)
        
        # COORDINATES OF THE EVENT
        self.eventToolBar.addWidget(self.coorEventLabel)
        layoutCoor1=QLabel("Minimum latitude: ")
        layoutCoor2=QLabel("Maximum latitude: ")
        layoutCoor3=QLabel("Minimum longitude: ")
        layoutCoor4=QLabel("Maximum longitude: ")
       
        self.minLatEventChoice = QDoubleSpinBox()
        self.maxLatEventChoice = QDoubleSpinBox()
        self.minLonEventChoice = QDoubleSpinBox()
        self.maxLonEventChoice = QDoubleSpinBox()
        
        self.eventToolBar.addWidget(layoutCoor1)
        self.eventToolBar.addWidget(self.minLatEventChoice)
        self.eventToolBar.addWidget(layoutCoor2)
        self.eventToolBar.addWidget(self.maxLatEventChoice)
        self.eventToolBar.addWidget(layoutCoor3)
        self.eventToolBar.addWidget(self.minLonEventChoice)
        self.eventToolBar.addWidget(layoutCoor4)
        self.eventToolBar.addWidget(self.maxLonEventChoice)
        
        self.minLatEventChoice.setMinimum(-90)
        self.minLatEventChoice.setMaximum(90)
        self.maxLatEventChoice.setMinimum(-90)
        self.maxLatEventChoice.setMaximum(90)
        self.minLonEventChoice.setMinimum(-180)
        self.minLonEventChoice.setMaximum(180)
        self.maxLonEventChoice.setMinimum(-180)
        self.maxLonEventChoice.setMaximum(180)
        
        self.eventToolBar.addAction(separator)
        
        # MAGITUDE MINIMUM
        self.eventToolBar.addWidget(self.magEventLabel)
        self.magEventChoice = QDoubleSpinBox()
        self.eventToolBar.addWidget(self.magEventChoice)
        self.magEventChoice.setMinimum(0)
        self.magEventChoice.setMaximum(10)
        
        
        self.searchEventButton = QPushButton("Search")
        self.searchEventButton.setStyleSheet("""
                background-color: #CD5C5C;
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
        self.eventToolBar.addWidget(self.searchEventButton)
        self.searchEventButton.clicked.connect(self.startEventSearch)
        
        # COORDINATES OF THE EVENT
       

    # CREATE ACTION FOR BOTH TOOLBARS
    def _createActions(self):
        # Creating action using the first constructor
        self.clientLabel = QLabel(self)
        self.clientLabel.setText("<b>Client</b>")
        self.clientLabel.setStyleSheet("font-family: bold;")
        self.clientLabel.setAlignment(Qt.AlignCenter)
        
        self.clientEventLabel = QLabel(self)
        self.clientEventLabel.setText("<b>Client</b>")
        self.clientEventLabel.setStyleSheet("font-family: bold;")
        self.clientEventLabel.setAlignment(Qt.AlignCenter)
        
        # Creating actions using the second constructor
        self.networkLabel = QLabel(self)
        self.networkLabel.setText("<b>Network</b>")
        self.networkLabel.setAlignment(Qt.AlignCenter)
        
        self.magLabel = QLabel(self)
        self.magLabel.setText("<b>Magnitude constraints</b>")
        self.magLabel.setAlignment(Qt.AlignCenter)
        
        self.magEventLabel = QLabel(self)
        self.magEventLabel.setText("<b>Magnitude constraints</b>")
        self.magEventLabel.setAlignment(Qt.AlignCenter)
        
        self.dateLabel = QLabel(self)
        self.dateLabel.setText("<b>Date</b>")
        self.dateLabel.setAlignment(Qt.AlignCenter)
        
        self.dateEventLabel = QLabel(self)
        self.dateEventLabel.setText("<b>Date</b>")
        self.dateEventLabel.setAlignment(Qt.AlignCenter)
        
        self.locLabel = QLabel(self)
        self.locLabel.setText("<b>Location</b>")
        self.locLabel.setAlignment(Qt.AlignCenter)
        
        self.coorLabel = QLabel(self)
        self.coorLabel.setText("<b>Coordinates</b>")
        self.coorLabel.setAlignment(Qt.AlignCenter)
        
        self.coorEventLabel = QLabel(self)
        self.coorEventLabel.setText("<b>Coordinates</b>")
        self.coorEventLabel.setAlignment(Qt.AlignCenter)
        
        #self.searchAction = QAction("&Search...",self)
        
        self.exitAction = QAction("&Exit", self)
        self.helpContentAction = QAction("&Help Content", self)
        self.aboutAction = QAction("&About", self)
        
        self.searchStationAction = QAction("Search by station",self)
        self.searchStationAction.triggered.connect(self.showMainToolBar)
        
        self.searchEventAction = QAction("Search by event",self)
        self.searchEventAction.triggered.connect(self.showSecondToolBar)
        

    def about(self):
        # Logic for showing an about dialog content goes here...
        msg = QMessageBox()
        msg.setWindowTitle("About the project")
        msg.setText("Welcome in this interface!")
        msg.setIcon(QMessageBox.Question)
        msg.setStandardButtons(QMessageBox.Ok)
        msg.setDefaultButton(QMessageBox.Ok)
        msg.exec_()
        
        
    def helpContent(self):
        # Logic for launching help goes here...
        QMessageBox.aboutQt(self)
        
    def showMainToolBar(self):

        self.map.addControl(self.drawControl)
        self.eventToolBar.setVisible(False)
        self.mainToolBar.setVisible(not self.mainToolBar.isVisible())
        
    def showSecondToolBar(self):

        self.map.addControl(self.drawControl)
        self.mainToolBar.setVisible(False)
        self.eventToolBar.setVisible(not self.eventToolBar.isVisible())
    
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
                print(events_center)
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
            self.listStation.itemDoubleClicked.connect(self.buildStationPopup)

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
                
            self.showStationDialog()
            self.show()
                

            
    
    def showStationDialog(self):
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
        self.dialog.show()
    

    @pyqtSlot(QListWidgetItem)           
    def buildStationPopup(self,item):
        exPopup = StationPopup(item.text(),self)
        exPopup.setWindowTitle("Seismic station {} details".format(item.text()))
        exPopup.show()
            
        
    # START SEARCH EVENT SECTION
    #--------------------------------------------------------------------------------------
    def startEventSearch(self):
        localStartTime = self.dateStartEventChoice.dateTime()
        pyStartTime = localStartTime.toPyDateTime()
        UTCStartTime = pyStartTime.astimezone(pytz.UTC)
        
        localEndTime = self.dateEndEventChoice.dateTime()
        pyEndTime = localEndTime.toPyDateTime()
        UTCEndTime = pyEndTime.astimezone(pytz.UTC)
        
        global startEvent,endEvent
        startEvent = UTCStartTime
        endEvent = UTCEndTime

        #start = UTCDateTime(self.dateStartChoice)
        #end = UTCDateTime(self.dateEndChoice)
        
        # COORDINATES INITIALIZATION (convert QDoubleSpinBox to int)
        valueMinLat = self.minLatEventChoice.value()
        intMinLat = int(valueMinLat)
        valueMaxLat = self.maxLatEventChoice.value()
        intMaxLat = int(valueMaxLat)
        valueMinLon = self.minLonEventChoice.value()
        intMinLon = int(valueMinLon)
        valueMaxLon = self.maxLonEventChoice.value()
        intMaxLon = int(valueMaxLon)
        
        # CLIENT INITIALIZATION (convert QComboBox to str)
        client_select = str(self.clientEventChoice.currentText())
        
        # MAGNITUDE INITIALIZATION (convert QDoubleSpinBox to int)
        valueMagMin = self.magEventChoice.value()
        intMag = int(valueMagMin)
        
        self.events_center = Client(client_select).get_events(    
            minlatitude = intMinLat,
            maxlatitude = intMaxLat,
            minlongitude = intMinLon,
            maxlongitude = intMaxLon,
            
            minmagnitude = intMag,
            starttime=startEvent,
            endtime=endEvent,
        )
        print("\nFound %s event(s) from %s Data Center:\n" % (len(self.events_center),client_select))
        print(self.events_center)
 
        
        global clientEvent
        clientEvent = Client(client_select)
        inventoryEvent = clientEvent.get_stations(network="*", level="channel")
        
        
        # DISPLAYING THE STATION 
        global stationsEvent
        stationsEvent = []
        for net in inventoryEvent:  # in fact this loop is only necessary for multiple networks
            for sta in net:
                stationsEvent.append(
                    [net.code, sta.code, sta.latitude, sta.longitude, sta.elevation]
                )
        comments='ISC'
        origin=[0, 0]
        
        
        
        self.listEvent = QListWidget(self)    
        self.listEvent.itemDoubleClicked.connect(self.buildEventPopup)
        
        for event in self.events_center:
            for origin, magnitude in zip(event.origins, event.magnitudes):
                
                lat, lon, depth, mag = (
                    origin.latitude,
                    origin.longitude,
                    origin.depth,
                    magnitude.mag,
                )
                infos = "Lat/Long: (%s %s)<br/>Depth: %s m<br/>Magnitude: %s<br/>Comment: %s" % (lat, lon, depth, mag, comments)
                
                '''
                item = QListWidgetItem()
                item.setText(str(event))
                self.listEvent.addItem(item)
            
                '''
                self.nameEvent = "Mw %.2f, (%.2f,%.2f), depth. %.2f m" % (mag,lat,lon,depth)
                QListWidgetItem(self.nameEvent,self.listEvent)
               

                events = L.circleMarker([lat, lon], {
                    'radius':50 * 2 ** (mag) / 2 ** 10,
                    'color':get_depth_color(depth),
                    'fillColor':"#FF8C00"
                })
                self.map.addLayer(events)
                popup_html = "<em> %s </em>" % infos
                events.bindPopup(popup_html)
        
        
        
        for net, sta, lat, lon, elev in stationsEvent:
            name = ".".join([net, sta])
            infos = "Name: %s<br/>Lat/Long: (%s, %s)<br/>Elevation: %s m" % (name, lat, lon, elev)
            '''
            # Generate the list of checkbox for the station selection 
            checkbox = QCheckBox(name,self)
            self.checkboxes[name] = checkbox
            layoutCheckBox.addWidget(checkbox)
            '''
            marker = L.marker([lat, lon], {
                'color':"blue",
                'fillColor':"#FF8C00",
                'fillOpacity':0.3,

            })
            
            self.map.addLayer(marker)
          
            popup_html="<b>%s</b>" %infos
            
            marker.bindPopup(popup_html)
            
        self.showEventDialog()
        self.show()
        

    def showEventDialog(self):
        self.dialogEvent = QDialog()
        label = QLabel('<b>My events list</b>')
        label.setAlignment(Qt.AlignCenter)
        
        # Search event by name
        searchEventEdit = QLineEdit()
        searchEventEdit.setPlaceholderText('Search...')
        searchEventEdit.setStyleSheet("QLineEdit { color: #888888; } ")
        searchEventEdit.textChanged.connect(self.updateListEventWidget)
        
        # Créer un QVBoxLayout
        layout = QVBoxLayout()
        layout.addWidget(searchEventEdit)
        layout.addWidget(label)
        layout.addWidget(self.listEvent)
        
        # Appliquer le QVBoxLayout à la QDialog
        self.dialogEvent.setLayout(layout)
        
        # Centrer la QDialog
        self.dialogEvent.setGeometry(500, 500, 400, 400)
        self.dialogEvent.setWindowTitle('Seismic events list')
        self.dialogEvent.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.listEvent.setGeometry(100, 100, 200, 200)
        self.dialogEvent.setModal(False)
        self.dialogEvent.show()
    
    # Pour filtrer la recherche
    def updateListEventWidget(self, text):
        for index in range(self.listEvent.count()):
            item = self.listEvent.item(index)
        if text.lower() in item.text().lower():
            item.setHidden(False)
        else:
            item.setHidden(True)
    

    @pyqtSlot(QListWidgetItem)           
    def buildEventPopup(self,item):
        # Recupération de la position de l'élément séléctionné dans la liste
        index = self.listEvent.row(item)
        
        global eqo, eqoMag
        eqo = self.events_center[index].origins[0]
        eqoMag = self.events_center[index].magnitudes[0].mag
        
        '''
        selected_event = item.text()
        print("Selected event :",selected_event)
        for event_num, event in self.event_dict.items():
            if event == selected_event:
                print("selected event number: ",event_num)
                break
        '''
        
        exPopup = EventPopup(item.text(),self)
        exPopup.setWindowTitle("Seismic event {} details".format(item.text()))
        exPopup.show()
        

    
class StationPopup(QDialog):

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
        
        layoutTime = QHBoxLayout()
        self.startTrace = QDateTimeEdit()
        self.endTrace = QDateTimeEdit()
        # Boundaries
        #self.startTrace.setMinimumDateTime(start)
        self.startTrace.setDateTime(start)
        ##self.endTrace.setMaximumDateTime(end)
        self.endTrace.setDateTime(end)
        # Add widgets to tab
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
            try:
                # Convert QDateTime value to UTCDateTime
                localStartTime = self.startTrace.dateTime()
                pyStartTime = localStartTime.toPyDateTime()
                UTCStartTime = pyStartTime.astimezone(pytz.UTC)
                
                localEndTime = self.endTrace.dateTime()
                pyEndTime = localEndTime.toPyDateTime()
                UTCEndTime = pyEndTime.astimezone(pytz.UTC)
                '''
                start = UTCDateTime(UTCStartTime)
                end = UTCDateTime(UTCEndTime)
                '''
                start = UTCDateTime("2018-02-12T03:08:02")
                self.nameStation = self.name.split('.')[1]
    
                
                self.st = client.get_waveforms(
                    network = network_select,
                    station = self.nameStation,
                    location = location,
                    channel = self.channelChoice.text(),
                    #starttime = UTCStartTime,
                    #endtime = UTCEndTime,
                    starttime = start,
                    endtime = start + 90,
                    attach_response = True,
                )
                
                self.figure.clear()
                self.st.plot(fig=self.figure)
    
                self.canvas.draw()
                
                # Faire une copie de la trace pour conserver l'original dans le "processing"
                self.original_st = self.st.copy()
    
                
                self.download_button.clicked.connect(self.download_data)
                
            except Exception:
                error_message = QMessageBox(QMessageBox.Critical, "No data found","Please, check the parameter filled!",QMessageBox.Ok)
                error_message.exec()
            
            #self._contentTab2()
            
  
    
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
        
        
        freqs,psd = get_periodogram(x, sampling_rate, show = False)
        fig = Figure(figsize=(6, 4), dpi=100)
        ax = fig.add_subplot(111)
        ax.semilogx(freqs, 10 * np.log10(psd))
        ax.set_xlabel('Frequency (Hz)')
        ax.set_ylabel('Power spectral density (dB/Hz)')

        canvas = FigureCanvas(fig)
        canvas.draw()
        
        # Creation de la figure
        '''
        st = obspy.read("vendee4.6.mseed")
        x = st[0].data
        sampling_rate = st[0].stats.sampling_rate

        # Calculate the spectrum
        freqs, psd = get_periodogram(x, sampling_rate,show=False)

        # Create Matplotlib figure and canvas
        fig = Figure(figsize=(5, 4), dpi=100)
        ax = fig.add_subplot(111)
        ax.semilogx(freqs, 10 * np.log10(psd))
        ax.set_xlabel('Frequency (Hz)')
        ax.set_ylabel('Power spectral density (dB/Hz)')

        canvas = FigureCanvas(fig)
        '''
        #self.figure_spectrum.clear()
        
        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setFrameShadow(QFrame.Sunken)
        self.Description.layout.addWidget(separator)
        
        # SPECTROGRAM - A METTRE DANS LE FILTRAGE ET METTRE ICI LE SPECTRE DE FREQUENCE ??
        spectrogramLabel = QLabel("<b>Spectrogram</b>")
        self.Periodogram.layout.addWidget(spectrogramLabel)
        
        '''
        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setFrameShadow(QFrame.Sunken)
        self.Periodogram.layout.addWidget(separator)
        '''
        
        

    def _contentTab3(self):
        instrumental_response = QCheckBox("Remove instrumental response")
        self.Processing.layout.addWidget(instrumental_response)
        instrumental_response.stateChanged.connect(self.instrument_response_checkbox)
        '''
        inventory[0].plot_response(min_freq=1e-4,outfile='response.png')
        figLabel = QLabel(self)
        pixmap = QPixmap('response.png')
        figLabel.setixmap(pixmap)
        self.Processing.layout.addWidget(figLabel)
        os.remove('response.png')
        '''
        

        remove_mean = QCheckBox("Remove the mean")
        self.Processing.layout.addWidget(remove_mean)
        remove_mean.stateChanged.connect(self.detrend_checkbox)
        
        self.figure_mean = Figure(figsize=(6,4))
        self.canvas_mean = FigureCanvas(self.figure_mean)
        self.Processing.layout.addWidget(self.canvas_mean)
        
        
        separator = QFrame()
        separator.setFrameShape(QFrame.HLine)
        separator.setFrameShadow(QFrame.Sunken)
        self.Processing.layout.addWidget(separator)
        
        grid = QGridLayout()
        
        filteringLabel = QLabel("<b>Filtering</b>")
        self.Processing.layout.addWidget(filteringLabel)
        
        buttonGroup = QButtonGroup()
        lowpassBox = QRadioButton("lowpass")
        bandpassBox = QRadioButton("bandpass")
        highpassBox= QRadioButton("highpass")
        
        # Creation du groupe de buttons
        buttonGroup.addButton(lowpassBox)
        buttonGroup.addButton(bandpassBox)
        buttonGroup.addButton(highpassBox)
        
        # Choose the filter
        grid.addWidget(lowpassBox,0,0)
        grid.addWidget(bandpassBox,1,0)
        grid.addWidget(highpassBox,2,0)
        
        # Choose the fequency range of the LOWPASS
        freqMin_lp = QLineEdit()
        freqMax_lp = QLineEdit()
        freqMin_lp.setPlaceholderText("Min freq")
        freqMax_lp.setPlaceholderText("Max freq")
        
        # Choose the fequency range of the BANDPASS
        freqMin_bp = QLineEdit()
        freqMax_bp = QLineEdit()
        freqMin_bp.setPlaceholderText("Min freq")
        freqMax_bp.setPlaceholderText("Max freq")
        
        # Choose the fequency range of the HIGHPASS
        freqMin_hp = QLineEdit()
        freqMax_hp = QLineEdit()
        freqMin_hp.setPlaceholderText("Min freq")
        freqMax_hp.setPlaceholderText("Max freq")
        
        # Add minimal frequency
        grid.addWidget(freqMin_lp,0,1)
        grid.addWidget(freqMin_bp,1,1)
        grid.addWidget(freqMin_hp,2,1)
        
        # Add maximal frequency
        grid.addWidget(freqMax_lp,0,2)
        grid.addWidget(freqMax_bp,1,2)
        grid.addWidget(freqMax_hp,2,2)
        
        
        self.Processing.layout.addLayout(grid)

        
        #Variable de classe pour stocker la valeur du filtre sélectionné
        self.filterSelected=None
        freqMin_bp.textChanged.connect(lambda: self.checkFieldsFilled_BP(bandpassBox,freqMin_bp,freqMax_bp,buttonGroup))
        freqMax_bp.textChanged.connect(lambda: self.checkFieldsFilled_BP(bandpassBox,freqMin_bp,freqMax_bp,buttonGroup))
        
        freqMin_lp.textChanged.connect(lambda: self.checkFieldsFilled_LP(lowpassBox,freqMin_lp,freqMax_lp,buttonGroup))
        freqMax_lp.textChanged.connect(lambda: self.checkFieldsFilled_LP(lowpassBox,freqMin_lp,freqMax_lp,buttonGroup))
        
        freqMin_hp.textChanged.connect(lambda: self.checkFieldsFilled_HP(highpassBox,freqMin_hp,freqMax_hp,buttonGroup))
        freqMax_hp.textChanged.connect(lambda: self.checkFieldsFilled_HP(highpassBox,freqMin_hp,freqMax_hp,buttonGroup))
        
        
        
        self.figure_filter = Figure(figsize=(6,4))
        self.canvas_filter = FigureCanvas(self.figure_filter)
        self.Processing.layout.addWidget(self.canvas_filter)
        
        
    # DETRENDING DATA    
    def detrend_checkbox(self,state):
        if state == 2 and hasattr(self,'st'):
            self.st.detrend("demean")
        else:
            pass
        
    def instrument_response_checkbox(self,state):
        if state == 2 and hasattr(self,'st'):
            self.st.remove_response(output="VEL")
            #self.figure_mean.clear()
            self.st.plot(fig=self.figure_mean)
            
        else:
            pass
            
    def checkFieldsFilled_BP(self,bandpassBox,freqMin_bp,freqMax_bp, buttonGroup):
        if bandpassBox.isChecked() and freqMin_bp.text() and freqMax_bp.text() and hasattr(self,'st'):
            freqmin = freqMin_bp.text()
            freqmax = freqMax_bp.text()
            
            def convert_freqmin(freqmin):
                try:
                    freqmin_bp = int(freqmin)
                except ValueError:
                    freqmin_bp = float(freqmin)
                return freqmin_bp
            
            def convert_freqmax(freqmax):
                try:
                    freqmax_bp = int(freqmax)
                except ValueError:
                    freqmax_bp = float(freqmax)
                return freqmax_bp
            
            freqmin_bp = convert_freqmin(freqmin)
            freqmax_bp = convert_freqmax(freqmax)
                    

            
            #if self.filterSelected is not None:
            self.st.filter("bandpass",freqmin=freqmin_bp,freqmax=freqmax_bp)
            self.figure.clear()
            self.st.plot(fig=self.figure_filter)
            
        else:
            pass
        
    def checkFieldsFilled_LP(self,lowpassBox,freqMin_lp,freqMax_lp, buttonGroup):
        if lowpassBox.isChecked() and freqMin_lp.text() and freqMax_lp.text():
            freqmin = freqMin_lp.text()
            freqmax = freqMax_lp.text()
            
            def convert_freqmin(freqmin):
                try:
                    freqmin_lp = int(freqmin)
                except ValueError:
                    freqmin_lp = float(freqmin)
                return freqmin_lp
            
            def convert_freqmax(freqmax):
                try:
                    freqmax_lp = int(freqmax)
                except ValueError:
                    freqmax_lp = float(freqmax)
                return freqmax_lp
            
            freqmin_lp = convert_freqmin(freqmin)
            freqmax_lp = convert_freqmax(freqmax)
                    

            
            #if self.filterSelected is not None:
            self.st.filter("lowpass",freqmin=freqmin_lp,freqmax=freqmax_lp)
            self.st.plot(fig=self.figure_filter)
            
        else:
            pass
        
    def checkFieldsFilled_HP(self,highpassBox,freqMin_hp,freqMax_hp, buttonGroup):
        if highpassBox.isChecked() and freqMin_hp.text() and freqMax_hp.text():
            freqmin = freqMin_hp.text()
            freqmax = freqMax_hp.text()
            
            def convert_freqmin(freqmin):
                try:
                    freqmin_hp = int(freqmin)
                except ValueError:
                    freqmin_hp = float(freqmin)
                return freqmin_hp
            
            def convert_freqmax(freqmax):
                try:
                    freqmax_hp = int(freqmax)
                except ValueError:
                    freqmax_hp = float(freqmax)
                return freqmax_hp
            
            freqmin_hp = convert_freqmin(freqmin)
            freqmax_hp = convert_freqmax(freqmax)
                    

            
            #if self.filterSelected is not None:
            self.st.filter("highpass",freqmin=freqmin_hp,freqmax=freqmax_hp)
            self.st.plot(fig=self.figure_filter)
            
        else:
            pass
    

class EventPopup(QDialog):

    def __init__(self, name,parent=None):
        super().__init__(parent)
        self.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.resize(900,700)
        
        # TAB DESCRIPTION/PERIODOGRAM/PROCESSING
        self.tabs = QTabWidget()
        self.Description = QWidget()
        self.Section = QWidget()
        
        #ADD WIDGET & CONTENT AT EVERY TAB
        self.tabs.addTab(self.Description,"Description")
        self.tabs.addTab(self.Section,"Record section")
        
        # Ajouter des layouts à chaque onglet
        self.Description.layout = QVBoxLayout()
        self.Section.layout = QVBoxLayout()
        
        # Ajouter des widgets à chaque layout d'onglet
        self.Description.layout.addWidget(QWidget())
        self.Section.layout.addWidget(QWidget())

        # Définir les layouts pour chaque onglet
        self.Description.setLayout(self.Description.layout)
        self.Section.setLayout(self.Section.layout)

        # Ajouter le widget QTabWidget à la fenêtre MyDialog
        self.layout = QVBoxLayout()
        self.layout.addWidget(self.tabs)
        self.setLayout(self.layout)
        

        self._contentTab1()
        
    def _contentTab1 (self):
        
        
        layout = QHBoxLayout(self)
        
        # LEFT PART
        leftLayout = QVBoxLayout()
        layout.addLayout(leftLayout)
        
        listLabel = QLabel("<b>List station</b>")
        leftLayout.addWidget(listLabel)
        
        model = QStandardItemModel(0,2,self)
        model.setHorizontalHeaderLabels(["","Stations"])
        
        tree_view = QTreeView()
        tree_view.setModel(model)
        leftLayout.addWidget(tree_view)
     
        
        # Separation of the window into two parts
        separator = QFrame(self)
        separator.setFrameShape(QFrame.VLine)
        separator.setFrameShadow(QFrame.Sunken)
        layout.addWidget(separator) 
        
        # RIGHT PART
        rightLayout = QVBoxLayout()
        layout.addLayout(rightLayout)
        
        eqLabel = QLabel("<b>Earthquake information</b>")
        rightLayout.addWidget(eqLabel)
        
        # EQ INFO
        eqoMagLabel = QLabel("eqo.mag: {}".format(eqoMag))
        eqoLatLabel = QLabel("eqo.latitude: {}".format(eqo.latitude))
        eqoLonLabel = QLabel("eqo.longitude: {}".format(eqo.longitude))
        eqoDepthLabel = QLabel("eqo.depth: {}".format(eqo.depth))
        eqoStartLabel = QLabel("eqo.start: {}".format(eqo.time))
        rightLayout.addWidget(eqoMagLabel)
        rightLayout.addWidget(eqoLatLabel)
        rightLayout.addWidget(eqoLonLabel)
        rightLayout.addWidget(eqoDepthLabel)
        rightLayout.addWidget(eqoStartLabel)
        
        # CHANNEL INITIALIZATION
        channelLayout = QHBoxLayout()
        rightLayout.addLayout(channelLayout)
        Channel = QLabel("<b>Channel: </b>")
        channelLayout.addWidget(Channel)
        self.channelChoice = QLineEdit()
        channelLayout.addWidget(self.channelChoice)
        self.channelChoice.setPlaceholderText("Enter a channel")
        self.channel = self.channelChoice.text()
        
        
        
        self.Description.layout.addLayout(layout)
        
        eqContent = QVBoxLayout
        
            
        
            
  
class SplashScreen(QMainWindow):
    def __init__(self):
        super().__init__()
        
        
        self.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.setWindowTitle("Geodpy Project - Python application for scientific research")
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
        
        
        #self.central_widget.setLayout(QVBoxLayout(self.central_widget))
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