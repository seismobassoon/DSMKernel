#!/usr/bin/env python
# coding: utf-8

# pip install PyQtWebEngineWidgets

# In[1]:


#-*- coding: utf-8 -*-
"""
Created on Fri Feb  3 16:35:46 2023

@author: Lorraine Delaroque
"""

import sys
import folium
from folium.plugins import Draw
from folium.map import Popup, Layer
from folium.plugins import MarkerCluster
from folium import ClickForMarker, features
import io
import pytz
import IPython

from obspy import read
from obspy.clients.fdsn import Client

from DataProcessor_Fonctions import get_depth_color, get_periodogram # Load the function "plot_events" provided in tp_obsp

from PyQt5.QtWidgets import QMenu, QPushButton, QMessageBox
from PyQt5.QtWidgets import QToolBar, QDateTimeEdit, QFormLayout
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
        


        self.map = folium.Map(zoom_start=2,location=[0,0])
        Draw(export=True).add_to(self.map) 
        self.update_map()
        '''
        self.map.get_root().add_child(
            folium.Element("""
                <script>
                    var rectangle;
                    self.map.on('draw:created',function (e) {
                        var type = e.layerType,
                            layer = e.layer;
                        if (type ==='rectangle') {
                            rectangle = layer.getBounds();
                            console.log(rectangle);
                        }
                    });
                </script>
            """)
        )
        '''
        # En générant une carte folium dans qwebengine, il faut l'actuatiser à chaque modification
        # Besoin de créer un fonction qui va s'en charger
        self.update_map()

        self.setCentralWidget(self.centralWidget)
        QtCore.QMetaObject.connectSlotsByName(self)
        
        self._createActions()
        self._createMenuBar()
        
        #self._selectCoordinates()
        #self._showCoordinates()
        self._createToolBars()
        self._connectActions()
        #self.get_events_stations()
    
    # SELECT COORDINATES MANUALLY
    #-------------------------------------------------
    '''
    def _selectCoordinates(self):
        folium.plugins.Draw(draw_options={'rectangle': True},
                            edit_options={'remove': True, 'edit': True, 'save': True, 'cancel': False}
                            ).add_to(self.map)
        self.update_map()


        
    
    def _showCoordinates(self,**kwargs):
        if kwargs.get('type') == 'rectangle':
            rectangle = kwargs.get('coordinates')
            IPython.display.clear_output(wait=True)
            IPython.display.display(IPython.display.Markdown(f'Minimum latitude: {rectangle[0][0][0]}'))
            IPython.display.display(IPython.display.Markdown(f'Maximum latitude: {rectangle[0][2][0]}'))
            IPython.display.display(IPython.display.Markdown(f'Minimum longitude: {rectangle[0][0][1]}'))
            IPython.display.display(IPython.display.Markdown(f'Maximum longitude: {rectangle[0][2][1]}'))
            
        self.map.onShapeChange()
        self.update_map()
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
        mainToolBar.addWidget(self.searchButton)
        self.searchButton.clicked.connect(self.startSearch)
    
        #mainToolBar.addAction(self.searchAction)
        
        # FINISHED - NOTHING TO CHANGE


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
        if not self.locChoice.text() or \
            not self.networkChoice.text():
                error_message = QMessageBox(QMessageBox.Critical, "Error","All field must be filled!",QMessageBox.Ok)
                error_message.exec()
                
                style = "border: 1px solid red;"
                if not self.locChoice.text():
                    self.locChoice.setStyleSheet(style)
                if not self.networkChoice.text():
                    self.locChoice.setStyleSheet(style)
        else:
            # RESET OF THE INITIAL STYLE PARAMETERS
            style = "border: 1px solid black;"
            self.locChoice.setStyleSheet(style)
            self.locChoice.setStyleSheet(style)
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
            networkValue = self.networkChoice.text()
            strNetwork = str(networkValue)
            
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
            network_select = strNetwork
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
            '''
            def get_description_station(event=None,feature=None,id=None,properties=None):
                 new_window = QWindow()
                 new_window.show()
            '''
            #markers=[]
            for net, sta, lat, lon, elev in stations:
                name = ".".join([net, sta])
                infos = "%s (%s, %s) %s m" % (name, lat, lon, elev)
                folium.features.RegularPolygonMarker(
                    location=[lat, lon],
                    tooltip=infos,
                    color="blue",
                    fill_color="#FF8C00",
                    number_of_sides=3,
                    radius=10,
                    fill_opacity=0.3,
                    rotation=30,
                    popup='<i>Mt. Hood Meadows</i>'
                ).add_child(folium.ClickForMarker(popup='Waypoint')).add_to(self.map)
                #self.map.add_child(folium.ClickForMarker(callback=get_description_station))
                #markers.append(marker)
                #popup.add_child(folium.Popup(popup_html,parse_html=True))

              
                
                    
            self.update_map()    
            #marker_info=folium.plugins.FasterMarkerCluster(marker).add_child(folium.Pupup("Seismic station 1 Info")).add_to(self.map)
            #self.markers.connect(self.get_description_station)
           
            
            # AFFICHER UNE NOUVELLE FENÊTRE APRES AVOIR CLIQUE SUR UN MARKER
            #self.map.add_child(folium.ClickForMarker(popup=folium.Popup(max_width=450).add_child(self.get_description_station)))
            #self.update_map()
            
    
    
    
    
    '''
    # OBSPY PROCESSING
    #----------------------------------------------------------------------
    def get_events_stations(self):
    '''    
        

    
        
    def update_map(self):

        data = io.BytesIO()
        self.map.save(data,close_file=False)
        self.qwebengine.setHtml(data.getvalue().decode())   

        
if __name__ == "__main__":
    app = QApplication(sys.argv)
    win = Window()
    win.show()
    sys.exit(app.exec_())
    
    

