import sys, time
import webview
import js2py
import os


from PyQt5.QtGui import QTextDocument, QPixmap
from PyQt5.QtWidgets import QDialog, QPushButton,  QWidget, QLabel,QProgressBar
from PyQt5.QtWidgets import QApplication, QMainWindow, QVBoxLayout, QProgressBar

from PyQt5.QtWebEngineWidgets import QWebEngineView
from PyQt5.QtWebChannel import QWebChannel
from pyqtlet import L, MapWidget

from PyQt5 import QtCore
from PyQt5.QtCore import pyqtSlot, QTimer,Qt, QObject,QThread, pyqtSignal,QSize
from flask import Flask,jsonify

from django.contrib import admin
from django.conf.urls import url
from django.shortcuts import render
from django.http import HttpResponse

from jinja2 import Template

class SplashScreen(QMainWindow):
    def __init__(self):
        super().__init__()
        self.setWindowTitle("My Application")
        self.setFixedSize(600, 400)
        self.setStyleSheet("QMainWindow {background: 'white';}")
        self.central_widget = QWidget(self)
        self.setCentralWidget(self.central_widget)
        self.logo_label=QLabel(self)
        pixmap=QPixmap('logo ipgp.png')
        self.logo_label.setPixmap(pixmap)
        self.logo_label.setAlignment(Qt.AlignCenter)
        self.central_widget.setLayout(QVBoxLayout(self.central_widget))
        self.central_widget.layout().addWidget(self.logo_label)
        
        '''
        # Process bar
        self.progress_bar = QProgressBar(self)
        self.progress_bar.setMaximum(100)
        self.progress_bar.setValue(0)
        self.progress_bar.setAlignment(Qt.AlignCenter)
        self.label_text = QLabel("Loading...")
        self.label_text.setAlignment(Qt.AlignCenter)

        # Displaying
        layout = QVBoxLayout()
        layout.addStretch(1)
        layout.addWidget(self.label_text)
        layout.addWidget(self.logo_label)
        layout.addWidget(self.progress_bar)
        
        #self.central_widget.setLayout(QVBoxLayout(self.central_widget))
        #self.central_widget.layout().addWidget(self.logo_label)
        '''
        #self.central_widget.setLayout(layout)
        self.show()
        
        self.timer = QTimer()
        self.timer.setSingleShot(True)
        self.timer.timeout.connect(self.close)
        self.timer.start(5000)
    '''    
    def set_progress(self, value):
        self.progress_bar.setValue(value)
        if value >= 100:
            self.close()
    '''
        

class Window(QMainWindow):
    def __init__(self, parent=None):

        super().__init__(parent)

        self.setWindowTitle("Main Window")
        
        self.mapWidget = MapWidget()
        self.setCentralWidget(self.mapWidget)

        self.webChannel=QWebChannel(self.mapWidget.page)

        QtCore.QMetaObject.connectSlotsByName(self)

        # MAPPING
        self.map = L.map(self.mapWidget)
        self.map.setView([0, 0], 2)
        L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png').addTo(self.map)
        

        self.marker = L.marker([48.8566, 2.3522])
        
  
        popup_html = "<button>ouvrir la boîte de dialogue</button>"
        
        
        """
        <!DOCTYPE html>
        <html>
        <head>
            <title>Boîte de dialogue</title>
            <meta charset='UTF-8'>
            <meta name='viewport' content='width=device-width,initial-scale=1.0'>
        </head>
        <body>
            <div class='popup'>
                <button onclick='open_dialog()'>ouvrir la boîte de dialogue</button>
            </div>
            
            <script type=text/javascript>
                function open_dialog() {
                    console.log("Hello!");
                }
            </script>
        
        </body>

        </html>
        """

        
        self.marker.bindPopup(popup_html)
        self.map.addLayer(self.marker)
        
        #self.loadProgress.connect(self.parent().splash_screen.set_progress)
        

        


    
if __name__ == "__main__":

    app = QApplication(sys.argv)
    
    splash_screen = SplashScreen()
    app.processEvents()
    
    win = Window()
    win.show()
    sys.exit(app.exec_())