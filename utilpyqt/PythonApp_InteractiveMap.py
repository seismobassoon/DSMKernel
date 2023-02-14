#!/usr/bin/env python
# coding: utf-8

# pip install PyQtWebEngineWidgets



#-*- coding: utf-8 -*-
"""
Created on Fri Feb  3 16:35:46 2023
@author: Lorraine Delaroque
"""

import sys
import numpy as np
import folium
from folium.plugins import Draw

import streamlit as st
from streamlit_folium import st_folium
import ipywidgets.embed as embed
import io,json
import pytz
from ipyleaflet import Map, basemaps, basemap_to_tiles, DrawControl
from obspy.clients.fdsn import Client
from bokeh.models import BoxSelectTool,Tap
from bokeh.plotting import figure,show
from DataProcessor_Fonctions import get_depth_color # Load the function "plot_events" provided in tp_obsp

from PyQt5.QtWidgets import QMenu, QPushButton, QMessageBox, QDialog
from PyQt5.QtWidgets import QDateTimeEdit
from PyQt5.QtWidgets import QAction, QLineEdit, QDoubleSpinBox
from PyQt5.QtWidgets import QApplication, QLabel, QMainWindow, QComboBox

#from PyQt5.QtGui import QIcon
#import qrc_resources

from PyQt5 import QtWidgets, QtCore, QtWebEngineWidgets
from PyQt5.QtCore import Qt, QSettings

class Window(QMainWindow):
    
    # DEF MAIN WINDOW
    #-----------------------------------------
    def __init__(self, parent=None):
        
        # INITIALIZER
        #---------------------------------------------
        super().__init__(parent)
        self.setWindowTitle("Geodpy Project - Python Menus & Toolbars")
        self.resize(800, 600)
        
        self.centralWidget = QtWidgets.QWidget(self)
        self.centralWidget.setObjectName("centralWidget")
        
        self.qwebengine = QtWebEngineWidgets.QWebEngineView(self.centralWidget)
        self.qwebengine.setGeometry(QtCore.QRect(50,50,800,600))
        self.qwebengine.setObjectName("qwebengine")
        


        self.map = folium.Map(zoom_start=2,location=[0,0])
        draw = Draw(export=True)
        draw.add_to(self.map) 
        self.map.add_child(folium.LatLngPopup())

        self.update_map()
        
        
        self.setCentralWidget(self.centralWidget)
        QtCore.QMetaObject.connectSlotsByName(self)

        
        self._createActions()
        self._createMenuBar()
        
        #self._selectCoordinates()
        #self._showCoordinates()
        self._createToolBars()
        self._connectActions()
        #self._saveValues()
        #self._closeEvent()
        #self._on_create()
        #self._select_rectangle()
        #self.get_events_stations()
        
    

    # SELECT COORDINATES MANUALLY
    #-------------------------------------------------
    
    '''
    def _select_rectangle(self):
        rect = folium.RectangleMarker(
            bounds=[(45.5,-122.7),(45.6,-122.6)],
            color='yellow',
            fill_color='yellow',
            fill=True
        .add_to(self.map))
        self.update_map()
        
        def update_doublespinbox(**kwargs):
            bounds = rect.bounds
            lat_min,lng_min=bounds[0]
            lat_max,lng_max=bounds[0]
            
            self.minLatChoice.setValue(lat_min)
            self.maxLatChoice.setValue(lat_max)
            self.minLonChoice.setValue(lng_min)
            self.maxLonChoice.setValue(lng_max)
            
        rect.on('draw',update_doublespinbox) 
    '''
    # DEF MENU BAR
    #-----------------------------------------------
    def _createMenuBar(self):
        menuBar = self.menuBar()
        
        # Creating menus using a QMenu object
        fileMenu = QMenu("&About", self)       #QMenu(title,parent)
        menuBar.addMenu(fileMenu)
        fileMenu.addAction(self.aboutAction)
        
        # Creating menus using a title
        helpMenu = menuBar.addMenu("&Help")
        helpMenu.addAction(self.helpContentAction)
        
        
        exitMenu = menuBar.addMenu("&Exit")
        exitMenu.addAction(self.exitAction)
        
        #helpMenu = menuBar.addMenu(QIcon(":help-content.svg"), "&Help")
    
    def _createToolBars(self):
        
                
        self.settings = QSettings("MyQtApp", "App1")
        # Using a title
        mainToolBar = self.addToolBar("Tools")
        mainToolBar.setMovable(True)
        
        # Using a QToolBar object and a toolbar area
        self.addToolBar(Qt.RightToolBarArea, mainToolBar)
        
        # ADDING THE TOOLS IN THE TOOL BAR
        #----------------------------------------------------
        # CLIENT ACTION
        mainToolBar.addWidget(self.clientLabel)
        self.clientChoice=QComboBox(self)
        mainToolBar.addWidget(self.clientChoice)
        self.clientChoice.addItems(["AUSPASS","BRG","EIDA","EMSC","ETH","GEOFON","GEONET","GFZ","ICGC","IESDMC","INGV","IPGP","IRIS","IRISPH5","ISC","KNMI","KOERI","LMU","NCEDC","NIEP","NOA","ODC","RASPISHAKE","RESIF","RESIFPH5","SCEDC","UIB-NORSAR","USGS","USP"])
        
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        mainToolBar.addAction(separator)    
        
        # DATE ACTION
        mainToolBar.addWidget(self.dateLabel)
        layout1=QLabel("From: ")
        layout2=QLabel("To: ")
        
        self.dateStartChoice = QDateTimeEdit(self,calendarPopup=True)
        self.dateEndChoice = QDateTimeEdit(self,calendarPopup=True)
        
        mainToolBar.addWidget(layout1)
        mainToolBar.addWidget(self.dateStartChoice)
        mainToolBar.addWidget(layout2)
        mainToolBar.addWidget(self.dateEndChoice)

                
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        mainToolBar.addAction(separator)    
        
        # COORDINATES ACTION
        mainToolBar.addWidget(self.coorLabel)
        layoutCoor1=QLabel("Minimum latitude: ")
        layoutCoor2=QLabel("Maximum latitude: ")
        layoutCoor3=QLabel("Minimum longitude: ")
        layoutCoor4=QLabel("Maximum longitude: ")
       
        self.minLatChoice = QDoubleSpinBox()
        self.maxLatChoice = QDoubleSpinBox()
        self.minLonChoice = QDoubleSpinBox()
        self.maxLonChoice = QDoubleSpinBox()
        mainToolBar.addWidget(layoutCoor1)
        mainToolBar.addWidget(self.minLatChoice)
        mainToolBar.addWidget(layoutCoor2)
        mainToolBar.addWidget(self.maxLatChoice)
        mainToolBar.addWidget(layoutCoor3)
        mainToolBar.addWidget(self.minLonChoice)
        mainToolBar.addWidget(layoutCoor4)
        mainToolBar.addWidget(self.maxLonChoice)
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
        mainToolBar.addAction(separator)    
        
        # LOCATION ACTION
        mainToolBar.addWidget(self.locLabel)
        self.locChoice = QLineEdit()
        mainToolBar.addWidget(self.locChoice)
        self.locChoice.setMaxLength(5)
        self.locChoice.setPlaceholderText("Enter the location")
                
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        mainToolBar.addAction(separator)    
        
        # MAGNITUDE ACTION
        mainToolBar.addWidget(self.magLabel)
        self.magChoice = QDoubleSpinBox()
        mainToolBar.addWidget(self.magChoice)
        self.magChoice.setMinimum(0)
        self.magChoice.setMaximum(10)
        
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        mainToolBar.addAction(separator)    
        
        # NETWORK ACTION
        
        
        mainToolBar.addWidget(self.networkLabel)
        #self.networkChoice = QLineEdit()
        self.networkChoice = QComboBox(self)
        mainToolBar.addWidget(self.networkChoice)
        #self.networkChoice.setMaxLength(5)
        #self.networkChoice.setPlaceholderText("Enter the network")
        self.clientChoice.currentIndexChanged.connect(self.updateNetworkChoice)
        
        
        
        
        
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        mainToolBar.addAction(separator)
        
        # SEARCH ACTION ------------------------------------------
        self.searchButton = QPushButton("Search")
        mainToolBar.addWidget(self.searchButton)
        self.searchButton.clicked.connect(self.startSearch)
    
        #mainToolBar.addAction(self.searchAction)
        
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
            self.networkChoice.addItems([""])
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
        
        
    '''  
    def _createAction(self):
        actions like make icons !!
        
    def _createStatusBar(self):
    
    '''
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
                    infos = "(%s %s) depth=%s m mag=%s (%s)" % (lat, lon, depth, mag, comments)
                    folium.CircleMarker(
                        location=[lat, lon],
                        radius=50 * 2 ** (mag) / 2 ** 10,
                        tooltip=infos,
                        color=get_depth_color(depth),
                        fill=True,
                        fill_color="#FF8C00",
                    ).add_to(self.map)
                    
            self.update_map()

            def open_window(**kwargs):
                window = QDialog()
                window.exec_()

            #
            for net, sta, lat, lon, elev in stations:
                name = ".".join([net, sta])
                infos = "%s (%s, %s) %s m" % (name, lat, lon, elev)
                marker = folium.features.RegularPolygonMarker(
                    location=[lat, lon],
                    tooltip=infos,
                    color="blue",
                    fill_color="#FF8C00",
                    number_of_sides=3,
                    radius=10,
                    fill_opacity=0.3,
                    rotation=30,
                    popup='<i>Hello</i>',
                    callback=open_window
                ).add_to(self.map)
                #FastMarkerCluster(marker).add_to(self.map)
                '''
                marker.add_child(folium.ClickForMarker(popup="Hello").add_to(self.map))
                marker.on_click(open_window)
                marker.add_to(self.map)
                '''
 
            self.update_map()
              
        
    def update_map(self):

        data = io.BytesIO()
        self.map.save(data,close_file=False)
        self.qwebengine.setHtml(data.getvalue().decode())   
    ''' 
    def _closeEvent(self):
        self.settings.setValue("Start_datetime_edit_value",self.dateStartChoice.dateTime().toString())
        self.settings.setValue("End_datetime_edit_value",self.dateEndChoice.dateTime().toString())
        self.settings.setValue("Min_Latitude_value",self.minLatChoice.value())
        self.settings.setValue("Max_Latitude_value",self.maxLatChoice.value())
        self.settings.setValue("Min_Longitude_value",self.minLonChoice.value())
        self.settings.setValue("Max_Longitude_value",self.maxLonChoice.value())
        self.settings.setValue("Location_value",self.locChoice.text())
        self.settings.setValue("Magnitude_value",self.magChoice.value())
        self.settings.setValue("Network_value",self.networkChoice.text())
        #super().closeEvent()


class WebEnginePage(QtWebEngineWidgets.QWebEnginePage):
       def javaScriptConsoleMessage(self, level, msg, line, sourceID):
          coords_dict = json.loads(msg)
          coords = coords_dict['geometry']['coordinates'][0]
          print(coords)
    
     '''   
if __name__ == "__main__":
    app = QApplication(sys.argv)
    win = Window()
    win.show()
    sys.exit(app.exec_())
    
    

