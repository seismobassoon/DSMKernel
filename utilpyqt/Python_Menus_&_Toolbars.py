#!/usr/bin/env python
# coding: utf-8

# pip install PyQtWebEngineWidgets

# In[1]:


# -*- coding: utf-8 -*-
"""
Created on Fri Feb  3 16:35:46 2023

@author: Lorraine Delaroque
"""

import sys
import folium
from folium.plugins import Draw
import io

from obspy import UTCDateTime
from obspy.clients.fdsn import Client
from tp_obspy_utils import plot_events # Load the function "plot_events" provided in tp_obsp

from PyQt5.QtWidgets import QMenu, QPushButton
from PyQt5.QtWidgets import QToolBar, QDateEdit
from PyQt5.QtWidgets import QAction, QSpinBox, QLineEdit, QDoubleSpinBox
from PyQt5.QtWidgets import QApplication, QLabel, QMainWindow, QComboBox

#from PyQt5.QtGui import QIcon
#import qrc_resources

from PyQt5 import QtWidgets, QtCore, QtWebEngineWidgets
from PyQt5.QtCore import Qt

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
        
        # MAP CREATION 
        self.map = folium.Map(zoom_start=2,location=[0,0])
        #self.map = Draw(draw_options={'rectangle': True},edit_options={'remove': True, 'edit': True, 'save': True, 'cancel': False})
        #self.map.draw.add_to(self.map)
        
        data = io.BytesIO()
        self.map.save(data,close_file=False)
        self.qwebengine.setHtml(data.getvalue().decode())

        self.setCentralWidget(self.centralWidget)
        QtCore.QMetaObject.connectSlotsByName(self)
        
        self._createActions()
        self._createMenuBar()
        self._createToolBars()
        self._connectActions()
        #self._obspyProcessing()
        
        
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
        # Using a title
        mainToolBar = self.addToolBar("Tools")
        mainToolBar.setMovable(True)
        
        # Using a QToolBar object and a toolbar area
        self.addToolBar(Qt.RightToolBarArea, mainToolBar)
        
        # ADDING THE TOOLS IN THE TOOL BAR
        #----------------------------------------------------
        # CLIENT ACTION
        mainToolBar.addWidget(self.clientLabel)
        self.clientChoice=QComboBox()
        mainToolBar.addWidget(self.clientChoice)
        self.clientChoice.addItems(["AusPass","BGR","EMSC","ETH","GEOFON","ICGC","IESDMC","INGV","IPGP","IRISDMC","ISC","KAGSR","KOERI","LMU","NCEDC","NIEP","NOA","ODC","RASPISHAKE","RESIF","SCEDC","UIB-NORSAR","USGS","USP"])
                
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        mainToolBar.addAction(separator)    
        
        # DATE ACTION
        mainToolBar.addWidget(self.dateLabel)
        self.dateStartChoice = QDateEdit()
        self.dateEndChoice = QDateEdit()
        mainToolBar.addWidget(self.dateStartChoice)
        mainToolBar.addWidget(self.dateEndChoice)
                
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        mainToolBar.addAction(separator)    
        
        # COORDINATES ACTION
        mainToolBar.addWidget(self.coorLabel)
        self.minLatChoice = QDoubleSpinBox()
        self.maxLatChoice = QDoubleSpinBox()
        self.minLonChoice = QDoubleSpinBox()
        self.maxLonChoice = QDoubleSpinBox()
        mainToolBar.addWidget(self.minLatChoice)
        mainToolBar.addWidget(self.maxLatChoice)
        mainToolBar.addWidget(self.minLonChoice)
        mainToolBar.addWidget(self.maxLonChoice)
                
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
        self.networkChoice = QLineEdit()
        mainToolBar.addWidget(self.networkChoice)
        self.networkChoice.setMaxLength(5)
        self.networkChoice.setPlaceholderText("Enter the network")
        
        # SEPARATING----------------------------------------------
        separator = QAction(self)
        separator.setSeparator(True)  
        mainToolBar.addAction(separator)
        
        # SEARCH ACTION ------------------------------------------
        self.searchButton = QPushButton("Search")
        #self.searchButton.clicked.connect(_obspyProcessing)
        mainToolBar.addWidget(self.searchButton)
    
        #mainToolBar.addAction(self.searchAction)
        
        # FINISHED - NOTHING TO CHANGE


    def _createActions(self):
        # Creating action using the first constructor
        self.clientLabel = QLabel(self)
        self.clientLabel.setText("Client")
        self.clientLabel.setAlignment(Qt.AlignCenter)
        
        # Creating actions using the second constructor
        self.networkLabel = QLabel(self)
        self.networkLabel.setText("Network")
        self.networkLabel.setAlignment(Qt.AlignCenter)
        
        self.magLabel = QLabel(self)
        self.magLabel.setText("Magnitude min")
        self.magLabel.setAlignment(Qt.AlignCenter)
        
        self.dateLabel = QLabel(self)
        self.dateLabel.setText("Date")
        self.dateLabel.setAlignment(Qt.AlignCenter)
        
        self.locLabel = QLabel(self)
        self.locLabel.setText("Location")
        self.locLabel.setAlignment(Qt.AlignCenter)
        
        self.coorLabel = QLabel(self)
        self.coorLabel.setText("Coordinates")
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
   
    '''
    def _searchRun(self):
        self.searchAction.activated.connect(self.searchAction)
    
    # OBSPY PROCESSING
    #----------------------------------------------------------------------
    def _obspyProcessing(self):
        
        # DATE INITIALIZATION
        start = UTCDateTime(self.dateStartChoice)
        end = UTCDateTime(self.dateEndChoice)
        
        # CLIENT INITIALIZATION
        client_select = str(self.clientChoice)
        
        events_center = Client(client_select).get_events(    
            minlatitude = int(self.minLatChoice),
            maxlatitude = int(self.maxLatChoice),
            minlongitude = int(self.minLonChoice),
            maxlongitude = int(self.maxLonChoice),
            
            minmagnitude = int(self.magChoice),
            starttime=start,
            endtime=end,
        )
        print("\nFound %s event(s) from %s Data Center:\n" % (len(events_center),self.clientChoice))
        print(events_center)
        
        # NETWORK INITIALIZATION
        network_select = self.networkChoice
        
        # DISPLAYING THE STATION 
        stations = []
        for net in inventory:  # in fact this loop is only necessary for multiple networks
            for sta in net:
                stations.append(
                    [net.code, sta.code, sta.latitude, sta.longitude, sta.elevation]
                )
        
        events_stations_map=plot_events_stations(events_center, stations, origin=[45.5, 8.0], zoom=3, color="blue",comments="ISC")
        events_stations_map
    '''
        
if __name__ == "__main__":
    app = QApplication(sys.argv)
    win = Window()
    win.show()
    sys.exit(app.exec_())
    
    

