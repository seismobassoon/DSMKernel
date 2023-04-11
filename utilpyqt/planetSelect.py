# -*- coding: utf-8 -*-
"""
Created on Mon Apr  3 12:19:50 2023

@author: Lenovo
"""
"""
Created on Mon Apr  3 11:14:39 2023

@author: Lorraine
"""

import sys
from PyQt5.QtCore import Qt, QRect
from PyQt5.QtGui import QIcon, QCursor
from PyQt5.QtWidgets import QMainWindow, QApplication, QHBoxLayout, QPushButton, QLabel, QVBoxLayout, QWidget

class planetSelect(QMainWindow):
    def __init__(self):
        super().__init__()
        self.resize(900,600)
        self.setStyleSheet("background-color: white;")
        self.setWindowIcon(QIcon('logo.jpg'))
        self.setWindowTitle("Geodpy Project - Python application for scientific research")
        # Créer le layout vertical pour le widget central
        centralWidget = QVBoxLayout()
        centralWidget.setContentsMargins(225,150,225,150)

        
        # Add the title of the layout
        # ----------------------------------------------------------------------------------------
        title = QLabel()
        title.setAlignment(Qt.AlignCenter)
        title.setText('Where would you like to go?')
        title.setStyleSheet("""
            QLabel {
                font-family: calibri;
                font-size: 45px;
                font-weight: bold;
                height: 50px;
            }
        """)
        centralWidget.addWidget(title)
        
        # CrEATING A WIDGET THAT WILL CONTAIN THE BUTTONS
        # -----------------------------------------------------------------------------------------
        buttonWidget = QWidget()
        
        # Créer le layout horizontal pour les boutons
        layout = QHBoxLayout()
        
        # Ajouter les boutons à ce layout
        # EARTH
        selectEarth = QPushButton("Earth")
        selectEarth.setStyleSheet("""
                                  QPushButton {
                                      border-radius: 15px;
                                      border: 4px solid black;
                                      background-color: white;
                                      color: black;
                                      font-family: calibri;
                                      font-weight: bold;
                                      height: 100px;
                                      font-size: 50px;
                                      
                                      }
                                  QPushButton:hover {
                                      background-color: white;
                                      border: 4px solid rgb(228,0,0);
                                      color: rgb(228,0,0);
                                      
                                      background-image: url('earth.jpg');
                                      background-position: center;

                                      }

                                  """)
        layout.addWidget(selectEarth)
        
        # MOON
        selectMoon = QPushButton("Moon")
        layout.addWidget(selectMoon)
        selectMoon.setStyleSheet("""
                                  QPushButton {
                                      border-radius: 15px;
                                      border: 4px solid black;
                                      background-color: white;
                                      color: black;
                                      font-family: calibri;
                                      font-weight: bold;
                                      height: 100px;
                                      font-size: 50px;
                                      
                                      }
                                  QPushButton:hover {
                                      background-color: white;
                                      border: 4px solid rgb(228,0,0);
                                      color: rgb(228,0,0);
                                      
                                      background-image: url('moon.jpg');
                                      background-position: center;
                                      
                                      }
                                  """)
        
        selectMars = QPushButton("Mars")
        layout.addWidget(selectMars)
        selectMars.setStyleSheet("""
                                  QPushButton {
                                      border-radius: 15px;
                                      border: 4px solid black;
                                      background-color: white;
                                      color: black;
                                      font-family: calibri;
                                      font-weight: bold;
                                      height: 100px;
                                      font-size: 50px;
                                      }
                                  QPushButton:hover {
                                      background-color: white;
                                      border: 4px solid rgb(228,0,0);
                                      color: rgb(228,0,0);
                                      
                                      background-image: url('mars.jpg');
                                      background-position: center;
                                      }
                                  """)
        
        # Définir le layout horizontal comme étant le layout du widget des boutons
        buttonWidget.setLayout(layout)
        centralWidget.addWidget(buttonWidget)
        
        
        # Add cursors
        #-------------------------------------------------------------------------
        cursor = QCursor(Qt.PointingHandCursor)
        selectEarth.setCursor(cursor)
        selectMoon.setCursor(cursor)
        selectMars.setCursor(cursor)
        
        widget = QWidget()
        widget.setLayout(centralWidget)
        self.setCentralWidget(widget)

        


if __name__ == "__main__":
    app = QApplication(sys.argv)
    win = planetSelect()
    win.show()
    sys.exit(app.exec_())

