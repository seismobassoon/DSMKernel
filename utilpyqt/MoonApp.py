# -*- coding: utf-8 -*-
"""
Created on Thu Apr 13 16:23:56 2023

@author: Lorraine Delaroque
"""

import sys
import folium
from folium.plugins import Draw

import io

from PyQt5.QtWidgets import QApplication, QMainWindow

from PyQt5 import QtWidgets, QtCore
from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtGui import QIcon

class Window(QMainWindow):
    
    # DEF MAIN WINDOW
    #-----------------------------------------
    def __init__(self, parent=None):
        
        # INITIALIZER
        #---------------------------------------------
        super().__init__(parent)
        self.setWindowTitle("Geodpy Project - Python application for scientific research")
        self.setWindowIcon(QIcon('logo.jpg'))
        self.resize(800, 600)
        
        self.centralWidget = QtWidgets.QWidget(self)
        self.centralWidget.setObjectName("centralWidget")
        
        
        self.qwebengine = QWebEngineView(self.centralWidget)
        self.qwebengine.setGeometry(QtCore.QRect(50,50,800,600))
        self.qwebengine.setObjectName("qwebengine")
        

        # Create a new Folium Map
        self.map = folium.Map([0,0],
                              attr='LOLA/USGS', 
                              max_zoom=5, 
                              tiles='https://s3.amazonaws.com/opmbuilder/301_moon/tiles/w/hillshaded-albedo/{z}/{x}/{y}.png'
                              )

        draw = Draw(export=True)
        draw.add_to(self.map) 
        self.map.add_child(folium.LatLngPopup())
        

        self.update_map()

        self.setCentralWidget(self.centralWidget)
        QtCore.QMetaObject.connectSlotsByName(self)
        
    def update_map(self):

        data = io.BytesIO()
        self.map.save(data,close_file=False)
        self.qwebengine.setHtml(data.getvalue().decode())   
        

if __name__ == "__main__":
    app = QApplication(sys.argv)
    win = Window()
    win.show()
    sys.exit(app.exec_())