#!/usr/bin/env python
# coding: utf-8

# pip install PyQtWebEngineWidgets

# Inspiration (sidebar) from the github of ingwant/PyQt5-Video-Book/#010_sidebar/


#-*- coding: utf-8 -*-
"""
Created on Fri Feb  3 16:35:46 2023
@author: Lorraine Delaroque
"""

from pyqtlet import L, MapWidget 
from PyQt5 import QtCore, QtGui, QtWidgets
from superqt import QRangeSlider

import obspy
from obspy import read_inventory, UTCDateTime
from obspy.geodetics import gps2dist_azimuth
from obspy.clients.fdsn import Client
from DataProcessor_Fonctions import get_depth_color, plot_record_section_degree

#from firebase_admin import credentials, storage, auth

from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas

import os, pytz, random
import numpy as np

#from config import uid

#import lxml.etree

#import firebase_admin
#from firebase_admin import credentials
#from firebase_admin import auth, db


class Ui_MainWindow(object):
    def setupUi(self, MainWindow):
        MainWindow.resize(1000, 600)
        MainWindow.setWindowTitle("DSM Kernel GUI Project - Python application for scientific research")
        
        self.centralwidget = QtWidgets.QWidget(MainWindow)
        self.centralwidget.setObjectName("centralwidget")
        self.gridLayout = QtWidgets.QGridLayout(self.centralwidget)
        self.gridLayout.setContentsMargins(0, 0, 0, 0)
        self.gridLayout.setSpacing(0)
        self.gridLayout.setObjectName("gridLayout")
        self.icon_only_widget = QtWidgets.QWidget(self.centralwidget)
        self.icon_only_widget.setObjectName("icon_only_widget")
        self.verticalLayout_3 = QtWidgets.QVBoxLayout(self.icon_only_widget)
        self.verticalLayout_3.setContentsMargins(0, 0, 0, 0)
        self.verticalLayout_3.setSpacing(0)
        self.verticalLayout_3.setObjectName("verticalLayout_3")
        self.horizontalLayout_3 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_3.setObjectName("horizontalLayout_3")
        self.logo_label_1 = QtWidgets.QLabel(self.icon_only_widget)
        self.logo_label_1.setMinimumSize(QtCore.QSize(40, 40))
        self.logo_label_1.setMaximumSize(QtCore.QSize(40, 40))
        self.logo_label_1.setText("")
        self.logo_label_1.setPixmap(QtGui.QPixmap("./icon/services-32.ico"))
        self.logo_label_1.setScaledContents(True)
        self.logo_label_1.setObjectName("logo_label_1")
        self.horizontalLayout_3.addWidget(self.logo_label_1)
        self.verticalLayout_3.addLayout(self.horizontalLayout_3)
        self.verticalLayout = QtWidgets.QVBoxLayout()
        self.verticalLayout.setSpacing(0)
        self.verticalLayout.setObjectName("verticalLayout")
        
        self.home_btn_1 = QtWidgets.QPushButton(self.icon_only_widget)
        self.home_btn_1.setText("")
        icon = QtGui.QIcon()
        icon.addPixmap(QtGui.QPixmap("./icon/explosion-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        icon.addPixmap(QtGui.QPixmap("./icon/explosion-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)
        self.home_btn_1.setIcon(icon)
        self.home_btn_1.setIconSize(QtCore.QSize(20, 20))
        self.home_btn_1.setCheckable(True)
        self.home_btn_1.setAutoExclusive(True)
        self.home_btn_1.setObjectName("home_btn_1")
        self.verticalLayout.addWidget(self.home_btn_1)
        self.dashborad_btn_1 = QtWidgets.QPushButton(self.icon_only_widget)
        self.dashborad_btn_1.setText("")
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap("./icon/time-5-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        icon1.addPixmap(QtGui.QPixmap("./icon/time-5-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)
        self.dashborad_btn_1.setIcon(icon1)
        self.dashborad_btn_1.setIconSize(QtCore.QSize(20, 20))
        self.dashborad_btn_1.setCheckable(True)
        self.dashborad_btn_1.setAutoExclusive(True)
        self.dashborad_btn_1.setObjectName("dashborad_btn_1")
        self.verticalLayout.addWidget(self.dashborad_btn_1)
        
        self.depth_btn_1 = QtWidgets.QPushButton(self.icon_only_widget)
        self.depth_btn_1.setText("")
        icondepth = QtGui.QIcon()
        icondepth.addPixmap(QtGui.QPixmap("./icon/arrow-down-6-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        icondepth.addPixmap(QtGui.QPixmap("./icon/arrow-down-6-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)
        self.depth_btn_1.setIcon(icondepth)
        self.depth_btn_1.setIconSize(QtCore.QSize(20, 20))
        self.depth_btn_1.setCheckable(True)
        self.depth_btn_1.setAutoExclusive(True)
        self.depth_btn_1.setObjectName("depth_btn_1")
        self.verticalLayout.addWidget(self.depth_btn_1)
        
        
        self.orders_btn_1 = QtWidgets.QPushButton(self.icon_only_widget)
        self.orders_btn_1.setText("")
        icon2 = QtGui.QIcon()
        icon2.addPixmap(QtGui.QPixmap("./icon/compass-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        icon2.addPixmap(QtGui.QPixmap("./icon/compass-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)
        self.orders_btn_1.setIcon(icon2)
        self.orders_btn_1.setIconSize(QtCore.QSize(20, 20))
        self.orders_btn_1.setCheckable(True)
        self.orders_btn_1.setAutoExclusive(True)
        self.orders_btn_1.setObjectName("orders_btn_1")
        self.verticalLayout.addWidget(self.orders_btn_1)
        self.products_btn_1 = QtWidgets.QPushButton(self.icon_only_widget)
        self.products_btn_1.setText("")
        icon3 = QtGui.QIcon()
        icon3.addPixmap(QtGui.QPixmap("./icon/battery-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        icon3.addPixmap(QtGui.QPixmap("./icon/battery-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)
        self.products_btn_1.setIcon(icon3)
        self.products_btn_1.setIconSize(QtCore.QSize(20, 20))
        self.products_btn_1.setCheckable(True)
        self.products_btn_1.setAutoExclusive(True)
        self.products_btn_1.setObjectName("products_btn_1")
        self.verticalLayout.addWidget(self.products_btn_1)
        '''
        self.customers_btn_1 = QtWidgets.QPushButton(self.icon_only_widget)
        self.customers_btn_1.setText("")
        icon4 = QtGui.QIcon()
        icon4.addPixmap(QtGui.QPixmap("./icon/group-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        icon4.addPixmap(QtGui.QPixmap("./icon/group-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)
        self.customers_btn_1.setIcon(icon4)
        self.customers_btn_1.setIconSize(QtCore.QSize(20, 20))
        self.customers_btn_1.setCheckable(True)
        self.customers_btn_1.setAutoExclusive(True)
        self.customers_btn_1.setObjectName("customers_btn_1")
        self.verticalLayout.addWidget(self.customers_btn_1)
        '''
        self.verticalLayout_3.addLayout(self.verticalLayout)
        spacerItem = QtWidgets.QSpacerItem(20, 375, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout_3.addItem(spacerItem)
        self.exit_btn_1 = QtWidgets.QPushButton(self.icon_only_widget)
        self.exit_btn_1.setText("")
        icon5 = QtGui.QIcon()
        icon5.addPixmap(QtGui.QPixmap("./icon/close-window-64.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.exit_btn_1.setIcon(icon5)
        self.exit_btn_1.setIconSize(QtCore.QSize(20, 20))
        self.exit_btn_1.setObjectName("exit_btn_1")
        self.verticalLayout_3.addWidget(self.exit_btn_1)
        self.gridLayout.addWidget(self.icon_only_widget, 0, 0, 1, 1)
        
        self.full_menu_widget = QtWidgets.QWidget(self.centralwidget)
        self.full_menu_widget.setObjectName("full_menu_widget")
        self.full_menu_widget.setFixedWidth(300)
        self.full_menu_widget.setHidden(True)
        self.verticalLayout_4 = QtWidgets.QVBoxLayout(self.full_menu_widget)
        self.verticalLayout_4.setObjectName("verticalLayout_4")
        self.horizontalLayout_2 = QtWidgets.QHBoxLayout()
        self.horizontalLayout_2.setSpacing(0)
        self.horizontalLayout_2.setObjectName("horizontalLayout_2")
        self.logo_label_2 = QtWidgets.QLabel(self.full_menu_widget)
        self.logo_label_2.setMinimumSize(QtCore.QSize(50, 50))
        self.logo_label_2.setMaximumSize(QtCore.QSize(50, 50))
        self.logo_label_2.setText("")
        self.logo_label_2.setPixmap(QtGui.QPixmap("./icon/services-32.ico"))
        self.logo_label_2.setScaledContents(True)
        self.logo_label_2.setObjectName("logo_label_2")
        self.horizontalLayout_2.addWidget(self.logo_label_2)
        self.logo_label_3 = QtWidgets.QLabel(self.full_menu_widget)
        font = QtGui.QFont()
        font.setPointSize(15)
        self.logo_label_3.setFont(font)
        self.logo_label_3.setObjectName("logo_label_3")
        self.horizontalLayout_2.addWidget(self.logo_label_3)
        self.verticalLayout_4.addLayout(self.horizontalLayout_2)
        self.verticalLayout_2 = QtWidgets.QVBoxLayout()
        self.verticalLayout_2.setSpacing(2)
        self.verticalLayout_2.setObjectName("verticalLayout_2")

        #%% MAGNTIUDE
        self.magnitude = QtWidgets.QLabel(self.full_menu_widget)
        self.magnitude.setText('Magnitude contraints')
        self.magnitude.setStyleSheet("""
                                           QLabel {
                                               color: white;
                                               padding: 15px;
                                               font-size: 15px;
                                               font-weight: bold;
                                               }
                                           """)
        icon1 = QtGui.QIcon()
        icon1.addPixmap(QtGui.QPixmap("./icon/compass-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        icon1.addPixmap(QtGui.QPixmap("./icon/compass-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)

        #self.mag_slider = QtWidgets.QSlider()
        
        self.mag_slider = QRangeSlider(QtCore.Qt.Orientation.Horizontal)
        self.mag_slider.setValue((40,70))
        self.mag_slider.setFixedWidth(150)
        self.mag_slider.setRange(0,100)
        self.mag_slider.setStyleSheet("""
                                      QSlider {
                                          
                                        min-height: 20px;
                                    }
                                    
                                    QSlider::groove:horizontal {
                                        border: 0px;
                                        background: qlineargradient(x1:0, y1:0, x2:1, y2:1, stop:0 #888, stop:1 #ddd);
                                        height: 20px;
                                        border-radius: 10px;
                                    }
                                    
                                    QSlider::handle {
                                        background: qradialgradient(cx:0, cy:0, radius: 1.2, fx:0.35,
                                                                    fy:0.3, stop:0 #eef, stop:1 #002);
                                        height: 20px;
                                        width: 20px;
                                        border-radius: 10px;
                                    }
                                    
                                    QSlider::sub-page:horizontal {
                                        background: qlineargradient(x1:0, y1:0, x2:1, y2:1, stop:0 #227, stop:1 #77a);
                                        border-top-left-radius: 10px;
                                        border-bottom-left-radius: 10px;
                                    }
                                    
                                    QRangeSlider {
                                        qproperty-barColor: qlineargradient(x1:0, y1:0, x2:1, y2:1, stop:0 #444, stop:1 #777);
                                    }
                                      """)
        self.mag_min = QtWidgets.QDoubleSpinBox()
        self.mag_min.setMinimum(0)
        self.mag_min.setButtonSymbols(QtWidgets.QAbstractSpinBox.NoButtons)
        self.mag_min.setValue(4)
        self.mag_min.setStyleSheet("""
                                     QDoubleSpinBox {
                                         color: white;
                                         width: 20px;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         }
                                     QAbstractDoubleSpinBox::up-button {
                                         background-color: transparent;
                                         color: transparent;
                                         }
                                     QAbstractDoubleSpinBox::down-button {
                                         background-color: transparent;
                                         color: transparent;
                                         }
                                     """)
        self.mag_min.setAlignment(QtCore.Qt.AlignLeft)
        self.mag_max = QtWidgets.QDoubleSpinBox()
        self.mag_max.setAlignment(QtCore.Qt.AlignRight)
        self.mag_max.setButtonSymbols(QtWidgets.QAbstractSpinBox.NoButtons)
        self.mag_max.setMaximum(100)
        self.mag_max.setValue(7)
        self.mag_max.setStyleSheet("""
                                     QDoubleSpinBox {
                                         color: white;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         width: 20px;
                                         }
                                     QAbstractDoubleSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractDoubleSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)

        mag_layout = QtWidgets.QHBoxLayout(self.full_menu_widget)
        mag_layout.addWidget(self.mag_min)
        mag_layout.addWidget(self.mag_slider)
        mag_layout.addWidget(self.mag_max)
        
        self.verticalLayout_2.addWidget(self.magnitude)
        self.verticalLayout_2.addLayout(mag_layout)
        #self.verticalLayout_2.addWidget(self.mag_slider)
        #self.verticalLayout_2.addWidget(self.mag_value)
        #self.verticalLayout_2.addStretch(1)
        
        self.mag_slider.valueChanged.connect(self.update_mag_value)
        self.mag_min.valueChanged.connect(self.update_mag_slider)
        self.mag_max.valueChanged.connect(self.update_mag_slider)
        
        # %% TIME PERIODE

        self.datetime = QtWidgets.QLabel(self.full_menu_widget)
        self.datetime.setText("Time period")
        self.datetime.setStyleSheet("color: white;font-size: 15px;font-weight: bold; padding:15px;")
        
        time_icon = QtGui.QIcon()
        time_icon.addPixmap(QtGui.QPixmap("./icon/compass-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        time_icon.addPixmap(QtGui.QPixmap("./icon/compass-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)

        
        self.start_layout = QtWidgets.QHBoxLayout(self.full_menu_widget)
        self.start_label = QtWidgets.QLabel()
        self.start_label.setFixedWidth(50)
        self.start_label.setText("From:")
        self.start_label.setStyleSheet("color:white;")
        
        self.start_edit = QtWidgets.QDateTimeEdit(calendarPopup=True)
        self.start_edit.setFixedWidth(120)
        self.start_edit.setDisplayFormat("dd/MM/yyyy hh:mm")
        self.start_edit.setStyleSheet("background-color: transparent; color: white; border: 2px solid white; border-radius: 5px;")
        self.start_layout.addWidget(self.start_label)
        self.start_layout.addWidget(self.start_edit)
        
        self.end_layout = QtWidgets.QHBoxLayout(self.full_menu_widget)
        self.end_label = QtWidgets.QLabel()
        self.end_label.setFixedWidth(50)
        self.end_label.setText("To:")
        self.end_label.setStyleSheet("color:white;")
        
        self.end_edit = QtWidgets.QDateTimeEdit(calendarPopup=True)
        self.end_edit.setFixedWidth(120)
        self.end_edit.setDisplayFormat("dd/MM/yyyy hh:mm")
        self.end_edit.setStyleSheet("background-color: transparent; color: white; border: 2px solid white; border-radius: 5px;")
        self.end_layout.addWidget(self.end_label)
        self.end_layout.addWidget(self.end_edit)
        
        self.verticalLayout_2.addWidget(self.datetime)
        self.verticalLayout_2.addLayout(self.start_layout)
        self.verticalLayout_2.addLayout(self.end_layout)
        
        #%% DEPTH
        self.depth = QtWidgets.QLabel(self.full_menu_widget)
        self.depth.setText('Depth contraints')
        self.depth.setStyleSheet("""
                                           QLabel {
                                               color: white;
                                               padding: 15px;
                                               font-size: 15px;
                                               font-weight: bold;
                                               }
                                           """)
        depth_icon = QtGui.QIcon()
        depth_icon.addPixmap(QtGui.QPixmap("./icon/arrow-down-6-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        depth_icon.addPixmap(QtGui.QPixmap("./icon/arrow-down-6-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.On)

        #self.mag_slider = QtWidgets.QSlider()
        
        self.depth_slider = QRangeSlider(QtCore.Qt.Orientation.Horizontal)
        self.depth_slider.setValue((50,300))
        self.depth_slider.setFixedWidth(150)
        self.depth_slider.setRange(0,600)
        self.depth_slider.setStyleSheet("""
                                      QSlider {
                                          
                                        min-height: 20px;
                                    }
                                    
                                    QSlider::groove:horizontal {
                                        border: 0px;
                                        background: qlineargradient(x1:0, y1:0, x2:1, y2:1, stop:0 #888, stop:1 #ddd);
                                        height: 20px;
                                        border-radius: 10px;
                                    }
                                    
                                    QSlider::handle {
                                        background: qradialgradient(cx:0, cy:0, radius: 1.2, fx:0.35,
                                                                    fy:0.3, stop:0 #eef, stop:1 #002);
                                        height: 20px;
                                        width: 20px;
                                        border-radius: 10px;
                                    }
                                    
                                    QSlider::sub-page:horizontal {
                                        background: qlineargradient(x1:0, y1:0, x2:1, y2:1, stop:0 #227, stop:1 #77a);
                                        border-top-left-radius: 10px;
                                        border-bottom-left-radius: 10px;
                                    }
                                    
                                    QRangeSlider {
                                        qproperty-barColor: qlineargradient(x1:0, y1:0, x2:1, y2:1, stop:0 #444, stop:1 #777);
                                    }
                                      """)
        self.depth_min = QtWidgets.QSpinBox()
        self.depth_min.setMinimum(0)
        self.depth_min.setMaximum(600)
        self.depth_min.setValue(50)
        self.depth_min.setStyleSheet("""
                                     QSpinBox {
                                         color: white;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         width: 20px;
                                         }
                                     QAbstractSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)
        self.depth_min.setAlignment(QtCore.Qt.AlignLeft)
        self.depth_max = QtWidgets.QSpinBox()
        self.depth_max.setAlignment(QtCore.Qt.AlignRight)
        self.depth_max.setMinimum(0)
        self.depth_max.setMaximum(600)
        self.depth_max.setValue(300)
        self.depth_max.setStyleSheet("""
                                     QSpinBox {
                                         color: white;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         width: 20px;
                                         }
                                     QAbstractSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)

        depth_layout = QtWidgets.QHBoxLayout(self.full_menu_widget)
        depth_layout.addWidget(self.depth_min)
        depth_layout.addWidget(self.depth_slider)
        depth_layout.addWidget(self.depth_max)
        
        self.verticalLayout_2.addWidget(self.depth)
        self.verticalLayout_2.addLayout(depth_layout)
        
        self.depth_slider.valueChanged.connect(self.update_depth_value)
        self.depth_min.valueChanged.connect(self.update_depth_slider)
        self.depth_max.valueChanged.connect(self.update_depth_slider)
        

        # %% COORDINATES
        self.coordinates = QtWidgets.QLabel(self.full_menu_widget)
        self.coordinates.setText("Coordinates")
        self.coordinates.setStyleSheet("color: white;font-size: 15px;font-weight: bold;padding:15px;")
        self.coordinates_layout = QtWidgets.QGridLayout(self.full_menu_widget)
        
        self.lat_min = QtWidgets.QDoubleSpinBox()
        self.lat_max = QtWidgets.QDoubleSpinBox()
        self.lng_min = QtWidgets.QDoubleSpinBox()
        self.lng_max = QtWidgets.QDoubleSpinBox()
        
        self.coordinates_layout.addWidget(self.lat_min,2,1)
        self.coordinates_layout.addWidget(self.lat_max,0,1)
        self.coordinates_layout.addWidget(self.lng_min,1,0)
        self.coordinates_layout.addWidget(self.lng_max,1,2)
        
        self.verticalLayout_2.addWidget(self.coordinates)
        self.verticalLayout_2.addLayout(self.coordinates_layout)
        
        self.lat_min.setAlignment(QtCore.Qt.AlignCenter)
        self.lat_max.setAlignment(QtCore.Qt.AlignCenter)
        self.lng_min.setAlignment(QtCore.Qt.AlignCenter)
        self.lng_max.setAlignment(QtCore.Qt.AlignCenter)   
        
        self.lat_min.setMinimum(-90)
        self.lat_min.setMaximum(90)
        self.lat_max.setMinimum(-90)
        self.lat_max.setMaximum(90)
        self.lng_min.setMinimum(-180)
        self.lng_min.setMaximum(180)
        self.lng_max.setMinimum(-180)
        self.lng_max.setMaximum(180)
        
        self.lat_min.setStyleSheet("background-color: transparent; color: white; border: 2px solid white; border-radius: 5px;")
        self.lat_max.setStyleSheet("background-color: transparent; color: white; border: 2px solid white; border-radius: 5px;")
        self.lng_min.setStyleSheet("background-color: transparent; color: white; border: 2px solid white; border-radius: 5px;")
        self.lng_max.setStyleSheet("background-color: transparent; color: white; border: 2px solid white; border-radius: 5px;")
        
        
        # %% CHANNEL
        self.channel_label = QtWidgets.QLabel(self.full_menu_widget)
        self.channel_label.setText("Channel")
        self.channel_label.setStyleSheet("""
                                           QLabel {
                                               color: white;
                                               font-size: 15px;
                                               font-weight: bold;
                                               padding:15px;
                                               }
                                          """)
        self.channel_choice = QtWidgets.QComboBox(self.full_menu_widget)
        self.channel_choice.setFixedWidth(150)
        self.channel_choice.addItems(["BH?","LH?"])
        self.channel_choice.setStyleSheet("""
                                          QComboBox {
                                              background-color: transparent;
                                              color: white;
                                              border: 2px solid white;
                                              border-radius: 5px;
                                              }
                                          
                                          QComboBox::item:selected {
                                              background-color: #0072c6;
                                              color: #ffffff;
                                              }

                                          QComboBox::item:!selected {
                                              background-color: #f0f0f0;
                                              color: #333333;
                                              }
                                
                                          QComboBox::item:disabled {
                                              background-color: #dddddd;
                                              color: #999999;
                                              }
                                          """)
                                          
        self.verticalLayout_2.addWidget(self.channel_label)
        self.verticalLayout_2.addWidget(self.channel_choice)
        
        # put self.verticalLayout_2 in a qwidget
        self.verticalLayout_2_widget = QtWidgets.QWidget()
        self.verticalLayout_2_widget.setLayout(self.verticalLayout_2)
        self.verticalLayout_2_widget.setFixedWidth(250)
        
        #self.verticalLayout_4.addLayout(self.verticalLayout_2)
        self.verticalLayout_4.addWidget(self.verticalLayout_2_widget)
        spacerItem1 = QtWidgets.QSpacerItem(20, 373, QtWidgets.QSizePolicy.Minimum, QtWidgets.QSizePolicy.Expanding)
        self.verticalLayout_4.addItem(spacerItem1)
        
        # %% BOTTOM TOOLBAR
        icon7 = QtGui.QIcon()
        icon7.addPixmap(QtGui.QPixmap("./icon/search-13-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.search_btn = QtWidgets.QPushButton(self.full_menu_widget)
        self.search_btn.setCursor(QtCore.Qt.PointingHandCursor)
        self.search_btn.clicked.connect(self.get_station)
        self.search_btn.setIcon(icon7)
        self.search_btn.setIconSize(QtCore.QSize(14, 14))
        self.search_btn.setObjectName("search_btn")
        self.search_btn.setText("Start your project!")
        self.search_btn.setCheckable(True)
        self.search_btn.setAutoExclusive(True)
        self.search_btn.setStyleSheet("""
                                      QPushButton {
                                          
                                          background-color: #959595;
                                          border: none;
                                          border-radius: 7px;
                                          color: white;
                                          padding: 10px;
                                          text-align: center;
                                          text-decoration: none;
                                          font-size: 16px;
                                          margin-top: 10px;
                                      }
                                      
                                      QPushButton:hover {
                                          background-color: #858585;
                                      }
                """)
        self.verticalLayout_4.addWidget(self.search_btn)
        
        
        self.exit_btn_2 = QtWidgets.QPushButton(self.full_menu_widget)
        self.exit_btn_2.setIcon(icon5)
        self.exit_btn_2.setIconSize(QtCore.QSize(14, 14))
        self.exit_btn_2.setObjectName("exit_btn_2")
        self.verticalLayout_4.addWidget(self.exit_btn_2)
        self.gridLayout.addWidget(self.full_menu_widget, 0, 1, 1, 1)
        
        # %% SET UP - RIGHT WIDGET
        self.widget_3 = QtWidgets.QWidget(self.centralwidget)
        self.widget_3.setObjectName("widget_3")
        self.verticalLayout_5 = QtWidgets.QVBoxLayout(self.widget_3)
        self.verticalLayout_5.setContentsMargins(0, 0, 0, 0)
        self.verticalLayout_5.setSpacing(0)
        self.verticalLayout_5.setObjectName("verticalLayout_5")
        self.widget = QtWidgets.QWidget(self.widget_3)
        self.widget.setMinimumSize(QtCore.QSize(0, 40))
        self.widget.setObjectName("widget")
        self.horizontalLayout_4 = QtWidgets.QHBoxLayout(self.widget)
        self.horizontalLayout_4.setContentsMargins(0, 0, 9, 0)
        self.horizontalLayout_4.setSpacing(0)
        self.horizontalLayout_4.setObjectName("horizontalLayout_4")
    
        self.change_btn = QtWidgets.QPushButton(self.widget)
        self.change_btn.setText("")
        self.change_btn.setCursor(QtCore.Qt.PointingHandCursor)
        icon6 = QtGui.QIcon()
        icon6.addPixmap(QtGui.QPixmap("./icon/menu-4-32.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.change_btn.setIcon(icon6)
        self.change_btn.setIconSize(QtCore.QSize(14, 14))
        self.change_btn.setCheckable(True)
        
        self.change_btn.setObjectName("change_btn")
        self.horizontalLayout_4.addWidget(self.change_btn)
        spacerItem2 = QtWidgets.QSpacerItem(236, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem2)
        self.horizontalLayout = QtWidgets.QHBoxLayout()
        self.horizontalLayout.setSpacing(10)
        self.horizontalLayout.setObjectName("horizontalLayout")
       
        # PROJECT NAME
        self.project_edit_name = QtWidgets.QLineEdit(self.widget)
        #self.project_edit_name.editingFinished()
        self.project_edit_name.setObjectName("search_input")
        self.project_edit_name.setPlaceholderText("Set project name here")
        self.horizontalLayout.addWidget(self.project_edit_name)
        
        # USER
        
        self.horizontalLayout_4.addLayout(self.horizontalLayout)
        spacerItem3 = QtWidgets.QSpacerItem(236, 20, QtWidgets.QSizePolicy.Expanding, QtWidgets.QSizePolicy.Minimum)
        self.horizontalLayout_4.addItem(spacerItem3)
        self.user_btn = QtWidgets.QPushButton(self.widget)
        self.user_btn.setText("")
        icon8 = QtGui.QIcon()
        icon8.addPixmap(QtGui.QPixmap("./icon/user-48.ico"), QtGui.QIcon.Normal, QtGui.QIcon.Off)
        self.user_btn.setIcon(icon8)
        self.user_btn.setObjectName("user_btn")
        self.horizontalLayout_4.addWidget(self.user_btn)
        self.verticalLayout_5.addWidget(self.widget)
        
        # %% MAP GENERATOR - RIGHT WIDGET
        
        self.page = QtWidgets.QWidget()
        self.page.setFixedHeight(600)
        self.page.setObjectName("page")
        self.gridLayout_2 = QtWidgets.QGridLayout(self.page)
        self.gridLayout_2.setObjectName("gridLayout_2")
        
        self.mapwidget = MapWidget()
        self.map = L.map(self.mapwidget)
        self.map.setView([0, 0], 2)
        SW = (-90,-180)
        NE = (90,180)
        self.map.setMaxBounds((SW,NE))

        # ADD MAP LAYERS
        osm_layer = L.tileLayer('http://{s}.tile.osm.org/{z}/{x}/{y}.png', {
            'attribution':'Map data &copy; OpenStreetMap contributors'})
        stamen_terrain_layer = L.tileLayer('http://{s}.tile.stamen.com/terrain/{z}/{x}/{y}.png')
        stamen_toner_layer = L.tileLayer('http://{s}.tile.stamen.com/toner/{z}/{x}/{y}.png')
        stamen_water_layer = L.tileLayer('http://{s}.tile.stamen.com/watercolor/{z}/{x}/{y}.png')
        satellite_layer = L.tileLayer('https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}')

        layers = {'OpenStreetMap':osm_layer,'Stamen Terrain':stamen_terrain_layer,'Stamen Toner':stamen_toner_layer,'Stamen Water Color':stamen_water_layer,'Satellite':satellite_layer}
        
        #DRAW STYLE
        style = {
            'draw': {
                'marker': False,
                'circlemarker': False,
                'polygon': False,
                'polyline': False,
                'rectangle': {
                    'shapeOptions': {
                        'color': 'blue',
                        'fillOpacity': 0.3
                        }
                    }
                }}
        
        # ADD DRAWINGS
        self.drawEventControl = L.control.draw(options=style)
        self.map.addControl(self.drawEventControl)
        
        satellite_layer.addTo(self.map)
        self.layersControl=L.control.layers(layers).addTo(self.map)
        
        #%% GET COORDINATES
        coord_lat=[]
        coord_lng=[]
        
        def coords(self, x, coord_lat, coord_lng):
            latlngs = x["layer"]["_latlngs"]
            
            latitudes = []
            longitudes = []
            
            for key1, value1 in latlngs.items():
                for key2, value2 in value1.items():
                    latitude = value2.get('lat')
                    if latitude is not None:
                        latitudes.append(latitude)
                
            for key1, value1 in latlngs.items():
                for key2, value2 in value1.items():
                    longitude = value2.get('lng')
                    if longitude is not None:
                        longitudes.append(longitude)
                        

            min_lat = min(latitudes)
            max_lat = max(latitudes)
            min_lng = min(longitudes)
            max_lng = max(longitudes)
            
            # ADD LATITUDE/LONGITUDE VALUES TO BOTH TOOLBARS
            self.lat_min.setValue(min_lat)
            self.lat_max.setValue(max_lat)
            self.lng_min.setValue(min_lng)
            self.lng_max.setValue(max_lng)
        
        self.drawEventControl.featureGroup.toGeoJSON(lambda x: print(x))
        self.map.drawCreated.connect(lambda x: coords(self,x,coord_lat,coord_lng))
        # %% BOTTOM WINDOW
        
        
        self.label_4 = QtWidgets.QGridLayout(self.page)
        self.label_4.addWidget(self.mapwidget)

        self.gridLayout_2.addLayout(self.label_4, 0, 0, 1, 1)
        self.verticalLayout_5.addWidget(self.page)

        self.gridLayout.addWidget(self.widget_3, 0, 2, 1, 1)
        MainWindow.setCentralWidget(self.centralwidget)
        
        #¢ BOTTOM TOOLBAR
        self.bottom_toolbar = QtWidgets.QWidget()
        self.bottom_toolbar.setMinimumSize(QtCore.QSize(0, 40))
        self.bottom_toolbar.setObjectName("bottom toolbar")
        
        hbox = QtWidgets.QHBoxLayout(self.bottom_toolbar)
        elevation = QtWidgets.QLabel("Elevation:")
        elevation.setFixedWidth(50)
        self.elevation_slider = QRangeSlider(QtCore.Qt.Orientation.Horizontal)
        self.elevation_slider.setValue((200,800))
        self.elevation_slider.setFixedWidth(150)
        self.elevation_slider.setRange(0,1000)
        self.elevation_slider.valueChanged.connect(self.update_elev_value)
        self.elevation_slider.setStyleSheet("""
                                            QRangeSlider {
                                                height:20px;
                                                }
                                            QRangeSlider::groove:horizontal {
                                                border-width: 0.5px;
                                                height: 4px;
                                                background-color: #d9d9d9;
                                                border-color: #000000;
                                                border-radius: 4px;
                                            }
                                            QRangeSlider::add-page:horizontal {
                                                background-color: #d9d9d9;
                                            }
                                            QRangeSlider::handle:horizontal {
                                                border-color: #000000;
                                                border-width: 3px;
                                                background-color: #4d4d4d;
                                                width: 10px;
                                                height: 22px;
                                                border-radius: 2px;
                                            }
                                            QRangeSlider::handle:hover:horizontal {
                                                border-color: #4d4d4d;
                                                background-color: #999999;
                                            }
                                            QRangeSlider::handle:horizontal:left {
                                                background-color: red; /* Couleur à gauche de l'étiquette de gauche */
                                            }
                                            QRangeSlider::handle:horizontal:right {
                                                background-color: #cccccc; /* Couleur à droite de l'étiquette de droite */
                                            }
                                            """)
        
        self.elev_min = QtWidgets.QSpinBox()
        self.elev_min.setMinimum(0)
        self.elev_min.setMaximum(1000)
        self.elev_min.setValue(200)
        self.elev_min.valueChanged.connect(self.update_elev_slider)
        self.elev_min.setStyleSheet("""
                                     QSpinBox {
                                         color: black;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         }
                                     QAbstractSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)
        self.elev_min.setAlignment(QtCore.Qt.AlignRight)
        self.elev_max = QtWidgets.QSpinBox()
        self.elev_max.setMaximum(1000)
        self.elev_max.setValue(800)
        self.elev_max.valueChanged.connect(self.update_elev_slider)
        self.elev_max.setStyleSheet("""
                                     QSpinBox {
                                         color: black;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         }
                                     QAbstractSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)
        # COORDINATES
        min_lat_label = QtWidgets.QLabel("Lat min:")
        min_lat_label.setFixedWidth(50)
        max_lat_label = QtWidgets.QLabel("Lat max:")
        max_lat_label.setFixedWidth(50)
        min_lng_label = QtWidgets.QLabel("Lng min:")
        min_lng_label.setFixedWidth(50)
        max_lng_label = QtWidgets.QLabel("Lng max:")
        max_lng_label.setFixedWidth(50)
        
        # COORDINATES FOR STATIONS
        self.min_lat = QtWidgets.QDoubleSpinBox()
        self.min_lat.setValue(-90)
        self.min_lat.setMinimum(-90)
        self.min_lat.setMaximum(90)
        self.max_lat = QtWidgets.QDoubleSpinBox()
        self.max_lat.setValue(90)
        self.max_lat.setMinimum(-90)
        self.max_lat.setMaximum(90)
        self.min_lng = QtWidgets.QDoubleSpinBox()
        self.min_lng.setValue(-180)
        self.min_lng.setMinimum(-180)
        self.min_lng.setMaximum(180)
        self.max_lng = QtWidgets.QDoubleSpinBox()
        self.max_lng.setValue(180)
        self.max_lng.setMinimum(-180)
        self.max_lng.setMaximum(180)
        self.min_lat.setStyleSheet("""
                                     QDoubleSpinBox {
                                         color: black;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         }
                                     QAbstractSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)
        self.max_lat.setStyleSheet("""
                                     QDoubleSpinBox {
                                         color: black;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         }
                                     QAbstractSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)
        self.min_lng.setStyleSheet("""
                                     QDoubleSpinBox {
                                         color: black;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         }
                                     QAbstractSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)
        self.max_lng.setStyleSheet("""
                                     QDoubleSpinBox {
                                         color: black;
                                         background-color: transparent;
                                         border: 1px solid transparent;
                                         }
                                     QAbstractSpinBox::up-button {
                                         background-color: transparent;
                                         }
                                     QAbstractSpinBox::down-button {
                                         background-color: transparent;
                                         }
                                     """)

        hbox.addWidget(elevation)
        hbox.addWidget(self.elev_min)
        hbox.addWidget(self.elevation_slider)
        hbox.addWidget(self.elev_max)
        hbox.addWidget(min_lat_label)
        hbox.addWidget(self.min_lat)
        hbox.addWidget(max_lat_label)
        hbox.addWidget(self.max_lat)
        hbox.addWidget(min_lng_label)
        hbox.addWidget(self.min_lng)
        hbox.addWidget(max_lng_label)
        hbox.addWidget(self.max_lng)
        self.bottom_toolbar.setLayout(hbox)
        self.verticalLayout_5.addWidget(self.bottom_toolbar)

        self.retranslateUi(MainWindow)
        self.change_btn.toggled['bool'].connect(self.icon_only_widget.setVisible) # type: ignore
        self.change_btn.toggled['bool'].connect(self.full_menu_widget.setHidden) # type: ignore

        #○self.products_btn_1.toggled['bool'].connect(self.products_btn_2.setChecked) # type: ignore
        #self.customers_btn_1.toggled['bool'].connect(self.customers_btn_2.setChecked) # type: ignore
        #self.products_btn_2.toggled['bool'].connect(self.products_btn_1.setChecked) # type: ignore
        #self.customers_btn_2.toggled['bool'].connect(self.customers_btn_1.setChecked) # type: ignore
        self.exit_btn_2.clicked.connect(MainWindow.close) # type: ignore
        self.exit_btn_1.clicked.connect(MainWindow.close) # type: ignore
        QtCore.QMetaObject.connectSlotsByName(MainWindow)
        # %%
    def update_mag_value(self,value):
        min_val, max_val = value
        self.mag_min.setValue(min_val / 10)
        self.mag_max.setValue(max_val / 10)

        
    def update_mag_slider(self,value):

        min_val = self.mag_min.value() * 10
        max_val = self.mag_max.value() * 10
        self.mag_slider.setValue((min_val, max_val))
        
    def update_depth_value(self,value):
        min_val, max_val = value
        self.depth_min.setValue(min_val)
        self.depth_max.setValue(max_val)

        
    def update_depth_slider(self,value):

        min_val = self.depth_min.value()
        max_val = self.depth_max.value()
        self.depth_slider.setValue((min_val, max_val))
        

    def retranslateUi(self, MainWindow):
        _translate = QtCore.QCoreApplication.translate
        self.logo_label_3.setText(_translate("MainWindow", "Events\nparameters"))
        self.exit_btn_2.setText(_translate("MainWindow", "Exit"))

    def update_elev_value(self, value):
        min_val, max_val = value
        self.elev_min.setValue(min_val)
        self.elev_max.setValue(max_val)

    def update_elev_slider(self, value):
        #self.elev_min.setValue(min_val)
        #self.elev_max.setValue(max_val)
        #print(self.elevation_slider.value())
        
        min_val = self.elev_min.value()
        max_val = self.elev_max.value()
        self.elevation_slider.setValue((min_val,max_val))
        
        
    
    # %% GET STATION   
    def get_station(self):
        # CLIENT
        try :
            client = Client("IRIS")
        except obspy.clients.fdsn.header.FDSNNoServiceException as e:
            QtWidgets.QMessageBox.warning("Error", "Internet connection is required")
        # DATE TIME CONVERSION

        localStartTime = self.start_edit.dateTime()
        pyStartTime = localStartTime.toPyDateTime()
        self.starttime = pyStartTime.astimezone(pytz.UTC)
        
        localEndTime = self.end_edit.dateTime()
        pyEndTime = localEndTime.toPyDateTime()
        self.endtime = pyEndTime.astimezone(pytz.UTC)
        
        #NETWORK 
        #network = "*"
        print("Inventory in process...")
        # INVENTORY
        self.inventory = client.get_stations(network="IU", level='channel', channel = self.channel_choice.currentText())
        #bucket = storage_client.bucket()
     
        self.stations = []
        
        for net in self.inventory:
            for sta in net:
                if self.elev_min.value() <= sta.elevation <= self.elev_max.value():
                    if self.min_lat.value() <= sta.latitude <= self.max_lat.value():
                        if self.min_lng.value() <= sta.longitude <= self.max_lng.value():
                            self.stations.append([net.code, sta.code, sta.latitude, sta.longitude, sta.elevation])
        
               
                '''
                filename = "{}_{}.xml".format(net.code,sta.code)
                station_inventory = read_inventory(network=net.code, station=sta.code,starttime=self.starttime,endtime=self.endtime)
                
                temporary_file = "{}.temp".format(filename)
                station_inventory.write(temporary_file, format='stationxml')
                
                blob = bucket.blob(f"users/{uid}/stations/{filename}")
                blob.upload_from_filename(temporary_file)
                
                os.remove(temporary_file)
                '''
        print("Now, plotting the stations!")
        print(self.stations) 
        self.plot_station()
        
    # %% PLOT STATIONS
    def plot_station(self):
        
        self.station_list = QtWidgets.QListWidget()  
        markersGroup= L.featureGroup()
        
        for net, sta, lat, lon, elev in self.stations:
            self.name = ".".join([net, sta])
            infos = "Name: %s<br/>Lat/Long: (%s, %s)<br/>Elevation: %s m" % (self.name, lat, lon, elev)
            QtWidgets.QListWidgetItem(self.name,self.station_list)
            marker = L.marker([lat, lon], {
                'color':"blue",
                'fillColor':"#FF8C00",
                'fillOpacity':0.3,

            })
            
            popup_html="<b>%s</b>" % infos
            
            marker.bindPopup(popup_html)
            markersGroup.addLayer(marker)
            
        self.map.addLayer(markersGroup)  
        self.showStationDialog()
        
    # %% STATION TRIES   
    def list_stations_tries(self):
        self.stations_tries = {}
        for station in self.stations:
            if station[0] not in self.stations_tries:
                self.stations_tries[station[0]] = []
            self.stations_tries[station[0]].append(station[1])
        return self.stations_tries
    
    # %% STATION DIALOG    
    def showStationDialog(self):
        self.sta_dialog = QtWidgets.QDialog()
        label = QtWidgets.QLabel('<b>Would you like to keep all?</b>')
        label.setAlignment(QtCore.Qt.AlignCenter)
        
        self.ui_search = QtWidgets.QLineEdit()
        self.ui_search.setStyleSheet("QLineEdit { color: #888888; border: 1px solid #959595;border-radius: 5px;} ")
        self.ui_search.setPlaceholderText("Search a station here...")
        self.ui_search.textChanged.connect(self.updateListStationWidget)
        
        # SEPARATOR
        separator = QtWidgets.QFrame()
        separator.setFrameShape(QtWidgets.QFrame.HLine)
        separator.setFrameShadow(QtWidgets.QFrame.Sunken)
        
        # QLISTWIDGET BODY
        self.tags_model = SearchProxyModel()
        self.tags_model.setSourceModel(QtGui.QStandardItemModel())
        self.tags_model.setDynamicSortFilter(True)
        self.tags_model.setFilterCaseSensitivity(QtCore.Qt.CaseInsensitive)

        # Création de la liste exaustive des stations sismiques à choisir pour réaliser la "record section"
        self.ui_tags = QtWidgets.QTreeView()
        self.ui_tags.setSortingEnabled(True)
        self.ui_tags.sortByColumn(0, QtCore.Qt.AscendingOrder)
        self.ui_tags.setEditTriggers(QtWidgets.QAbstractItemView.NoEditTriggers)
        self.ui_tags.setHeaderHidden(True)
        self.ui_tags.setRootIsDecorated(True)
        self.ui_tags.setUniformRowHeights(True)
        self.ui_tags.setModel(self.tags_model)
        
        or_label = QtWidgets.QLabel("Or")
        or_label.setAlignment(QtCore.Qt.AlignCenter)
        or_label.setStyleSheet("font-weight: bold;")
        
        # ADD VLAYOUT
        layout = QtWidgets.QVBoxLayout()
        layout.addWidget(label)
        layout.addWidget(self.ui_search)
        #layout.addWidget(self.station_list)
        layout.addWidget(self.ui_tags)
        layout.addWidget(separator)
        layout.addWidget(or_label)
        self.list_stations_tries()
        self.create_model()

        
        hbox = QtWidgets.QHBoxLayout()
        keep_station = QtWidgets.QLabel("Select the percent of station to keep")
        self.percent_station = QtWidgets.QSpinBox()
        self.percent_station.setValue(100)
        self.percent_station.setMinimum(0)
        self.percent_station.setMaximum(101)
        self.percent_station.setButtonSymbols(QtWidgets.QAbstractSpinBox.NoButtons)
        self.percent_station.setStyleSheet("""
                                           QSpinBox {
                                               color: black;
                                               background-color: transparent;
                                               border: 1px solid transparent;
                                               width: 10px;
                                               }
                                           QAbstractSpinBox::up-button {
                                               background-color: transparent;
                                               }
                                           QAbstractSpinBox::down-button {
                                               background-color: transparent;
                                               }
                                           """)
        self.percent_btn = QtWidgets.QPushButton("Ok")
        self.percent_btn.setStyleSheet("""
                                       QPushButton {
                                           background-color: #959595;
                                           border: none;
                                           border-radius: 5px;
                                           color: white;
                                           padding: 10px;
                                           text-align: center;
                                           text-decoration: none;
                                       }
                                       
                                       QPushButton:hover {
                                           background-color: #858585;
                                       }
                                       """)
        self.percent_btn.clicked.connect(self.randomlyUncheckItems)
        
        percent = QtWidgets.QLabel("%")
        hbox.addWidget(keep_station)
        hbox.addWidget(self.percent_station)
        hbox.addWidget(percent)
        hbox.addWidget(self.percent_btn)
        layout.addLayout(hbox)
        
        button = QtWidgets.QPushButton("Next step")
        button.clicked.connect(self.get_events)
        button.setCursor(QtCore.Qt.PointingHandCursor)
        button.setStyleSheet("""
                             QPushButton {
                                 background-color: #959595;
                                 border: none;
                                 border-radius: 5px;
                                 color: white;
                                 padding: 10px;
                                 text-align: center;
                                 text-decoration: none;
                                 font-size: 16px;
                                 margin-top: 10px;
                             }
                             
                             QPushButton:hover {
                                 background-color: #858585;
                             }
                                 """)
        stationXML_btn = QtWidgets.QPushButton("Download XML files\n of the current selection")
        #stationXML_btn.clicked.connect(self.get_stationXML)
        stationXML_btn.clicked.connect(self.downlaod_checked_items)
        stationXML_btn.setCursor(QtCore.Qt.PointingHandCursor)
        stationXML_btn.setStyleSheet("""
                                             QPushButton {
                                                 color: #959595;
                                                 border: 2px white;
                                                 background-color: transparent;
                                                 font-family: calibri;
                                                 font-style: italic;
                                                 }
                                             QPushButton:hover {
                                                 text-decoration: underline;
                                                 }

                                             """)
        layout.addWidget(button)
        layout.addWidget(stationXML_btn)
        # Appliquer le QVBoxLayout à la QDialog
        self.sta_dialog.setLayout(layout)
        
        # Centrer la QDialog
        self.sta_dialog.setGeometry(500, 645, 400, 400)
        self.sta_dialog.setWindowTitle('Seismic stations list')
        self.sta_dialog.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.station_list.setGeometry(100, 100, 200, 200)
        self.sta_dialog.setModal(False)
        self.sta_dialog.exec_()
        
        
 
    def create_model(self):
        model = self.ui_tags.model().sourceModel()
        self.populate_tree(self.stations_tries, model.invisibleRootItem())
        self.ui_tags.sortByColumn(0, QtCore.Qt.AscendingOrder)
        
    def populate_tree(self, children, parent):
        for child in sorted(children):
            node = QtGui.QStandardItem(child)
            node.setCheckable(True)
            node.setCheckState(QtCore.Qt.Checked)
            parent.appendRow(node)

            if isinstance(children, dict):
                self.populate_tree(children[child], node)
                
    def downlaod_checked_items(self):
        self.checked_items = {}
    
        source_model = self.tags_model.sourceModel()
    
        # Parcourir tous les éléments de la liste
        for row in range(source_model.rowCount()):
            network_item = source_model.item(row)
    
            # Vérifier si le parent est coché
            if network_item.checkState() == QtCore.Qt.Checked:
                network_name = network_item.text()
                self.checked_items[network_name] = []
    
                # Parcourir tous les enfants de l'élément parent
                for station_row in range(network_item.rowCount()):
                    station_item = network_item.child(station_row)
    
                    # Vérifier si l'enfant est coché
                    if station_item.checkState() == QtCore.Qt.Checked:
                        self.checked_items[network_name].append(station_item.text())
                        
        #print(checked_items)
        # Créer les répertoires et télécharger les fichiers XML correspondants
        '''
        folder = "stations"
        if not os.path.exists(directory):
            os.makedirs(directory)
        '''
        directory = QtWidgets.QFileDialog.getExistingDirectory(None, "Sélectionner un dossier de destination")
        if directory:
            stations_directory = os.path.join(directory, "stations")  # Dossier parent "stations"
            if not os.path.exists(stations_directory):
                os.makedirs(stations_directory)
        
            for parent_name, station_names in self.checked_items.items():
                parent_directory = os.path.join(stations_directory, parent_name)
                if not os.path.exists(parent_directory):
                    os.makedirs(parent_directory)
        
                for station_name in station_names:
                    net = self.inventory.select(network=network_name)[0]
                    sta = net.select(station=station_name)[0]
                    filename = '{}_{}.xml'.format(net.code, sta.code)
                    file_path = os.path.join(parent_directory, filename)
        
                    station_inventory = read_inventory(network=net.code, station=sta.code, starttime=self.starttime, endtime=self.endtime, client="IRIS")
                    station_inventory.write(file_path, format="stationxml")
                    
            QtWidgets.QMessageBox.information(None, "Download completed", "Stations data successfully downloaded.")
                        
    '''
    def remove_markers(self):
    # Get the percentage value from the QSpinBox
        percentage = self.percent_station.value()
    
        # Calculate the number of markers to remove
        num_markers = len(self.stations)
        num_to_remove = int(num_markers * 0.5)
        
        # Randomly select which markers to remove
        indices_to_remove = random.sample(range(num_markers), num_to_remove)
        
        # Remove the selected markers from the map
        for i, (net, sta, lat, lon, elev) in enumerate(self.stations):
            # Check if the marker's index is in indices_to_remove
            if i in indices_to_remove:
                # Remove the marker from the map if it exists
                marker_name = ".".join([net, sta])
                print("remove %s", marker_name)
                if self.map.hasLayer(marker_name):
                    self.map.removeLayer(marker_name)
    '''           
    '''
    def remove_random_children(self,parent):
        num_children = parent.model().rowCount(parent.index())
        num_children_to_remove = int(num_children * int(self.percent_station)/10)
        children_to_remove = random.sample(range(num_children), num_children_to_remove)
        for idx in sorted(children_to_remove, reverse=True):
            parent.removeRow(idx)
    '''
    '''
    def removeRandomItems(self):
        percentage = self.percent_station.value()/100.0
        total_items = self.tags_model.rowCount()
        items_to_remove = int(total_items * percentage)
        indices = random.sample(range(total_items),items_to_remove)
        for index in indices:
            self.tags_model.removeRow(index)
        self.tags_model.invalidate()
        self.ui_tags.reset()
            
        print("stations removed!")
        print(dir(self.tags_model))
    '''
    def randomlyUncheckItems(self):
        percentage = self.percent_station.value()
        num_items = self.tags_model.rowCount()
        num_items_to_uncheck = int(num_items * percentage / 100)
        indices_to_uncheck = random.sample(range(num_items), num_items_to_uncheck)
    
        # Tri des indices en ordre décroissant pour supprimer les doublons dans le cas où sample() renvoie des doublons
        indices_to_uncheck = sorted(indices_to_uncheck, reverse=True)
    
        source_model = self.tags_model.sourceModel()
    
        for index in indices_to_uncheck:
            source_index = self.tags_model.mapToSource(self.tags_model.index(index, 0))
            item = source_model.itemFromIndex(source_index)
            item.setCheckState(QtCore.Qt.Unchecked)
    
        self.ui_tags.viewport().update()
    
    def updateListStationWidget(self, text):
        regExp = QtCore.QRegExp(self.ui_search.text(), QtCore.Qt.CaseInsensitive, QtCore.QRegExp.FixedString)

        self.tags_model.text = self.ui_search.text().lower()
        self.tags_model.setFilterRegExp(regExp)

        if len(self.ui_search.text()) >= 1 and self.tags_model.rowCount() > 0:
            self.ui_tags.expandAll()
        else:
            self.ui_tags.collapseAll()
            
            
    # %% GET EVENTS & PLOTTING       
    def get_events(self):
        # lIST STOCKANT TOUTES LES STATIONS GARDEES
        self.checked_stations = {}
    
        source_model = self.tags_model.sourceModel()
    
        # Parcourir tous les éléments de la liste
        for row in range(source_model.rowCount()):
            network_item = source_model.item(row)
    
            # Vérifier si le parent est coché
            if network_item.checkState() == QtCore.Qt.Checked:
                network_name = network_item.text()
                self.checked_stations[network_name] = []
    
                # Parcourir tous les enfants de l'élément parent
                for station_row in range(network_item.rowCount()):
                    station_item = network_item.child(station_row)
    
                    # Vérifier si l'enfant est coché
                    if station_item.checkState() == QtCore.Qt.Checked:
                        self.checked_stations[network_name].append(station_item.text())
        print("Checked stations: ",self.checked_stations)
        
        # DEBUT DE LA FENETRE SUR LES STATONS
        self.sta_dialog.close()
        client = Client("IRIS")
        # CONVERT MAGNITUDE
        valueMagMin = self.mag_min.value()
        valueMagMax = self.mag_max.value()
        
        min_mag = int(valueMagMin)
        max_mag = int(valueMagMax)
        
        # CONVERT DEPTH
        valueDepthMin = self.depth_min.value()
        valueDepthMax = self.depth_max.value()
        
        min_depth = int(valueDepthMin)
        max_depth = int(valueDepthMax)
        
        print(min_depth)
        print(max_depth)
        
        # CONVERT COORDINATES
        valueMinLat = self.lat_min.value()
        latitude_min = int(valueMinLat)
        valueMaxLat = self.lat_max.value()
        latitude_max = int(valueMaxLat)
        valueMinLon = self.lng_min.value()
        longitude_min = int(valueMinLon)
        valueMaxLon = self.lng_max.value()
        longitude_max = int(valueMaxLon)
        
        self.events_center = client.get_events(
            minlatitude = latitude_min,
            maxlatitude = latitude_max,
            minlongitude = longitude_min,
            maxlongitude = longitude_max,
            
            mindepth = min_depth,
            maxdepth = max_depth,
            
            minmagnitude = min_mag,
            maxmagnitude = max_mag,
            starttime = self.starttime,
            endtime = self.endtime
            
            )
        
        comments = 'ISC'
        origin = [0,0]
        
        eventsGroup = L.featureGroup()
        self.event_list = QtWidgets.QListWidget()    

        for event in self.events_center:
            for origin, magnitude in zip(event.origins, event.magnitudes):
                lat, lon, depth, mag = (
                    origin.latitude,
                    origin.longitude,
                    origin.depth,
                    magnitude.mag,
                )
                depth_km = depth / 1000
                infos = "Lat/Long: (%s %s)<br/>Depth: %s m<br/>Magnitude: %s<br/>Comment: %s" % (lat, lon, depth_km, mag, comments)
                
                self.nameEvent = "Mw %.2f, (%.2f,%.2f), depth. %.2f km" % (mag,lat,lon,depth_km)
                QtWidgets.QListWidgetItem(self.nameEvent,self.event_list)
                
                events = L.circleMarker([lat, lon], {
                    'radius':50 * 2 ** (mag) / 2 ** 10,
                    'color':get_depth_color(depth),
                    'fillColor':"#FF8C00"
                })
                eventsGroup.addLayer(events)
                popup_html = "<em> %s </em>" % infos
                events.bindPopup(popup_html)
                
                
        #self.update_map()
        self.map.addLayer(eventsGroup)
        self.showEventDialog()
        
        
    def showEventDialog(self):
        self.event_dialog = QtWidgets.QDialog()
        nbr_event = QtWidgets.QLabel("\nFound %s event(s) from IRIS Data Center:\n"% (len(self.events_center)))
        nbr_event.setAlignment(QtCore.Qt.AlignCenter)
        
                                     
        label = QtWidgets.QLabel('<b>Which event would you like to use?</b>')
        label.setAlignment(QtCore.Qt.AlignCenter)
        
        # Search event by name
        searchEventEdit = QtWidgets.QLineEdit()
        searchEventEdit.setPlaceholderText('Search an event here...')
        searchEventEdit.setStyleSheet("QLineEdit { color: #888888; border: 1px solid #959595;border-radius: 5px;} ")
        searchEventEdit.textChanged.connect(self.updateListEventWidget)
        
        self.error = QtWidgets.QLabel("Sélectionnez un élément de la liste")
        
        # Next step : plot record section
        button = QtWidgets.QPushButton("Next step")
        #button.clicked.connect(self.record_section_dialog)
        button.setCursor(QtCore.Qt.PointingHandCursor)
        button.setStyleSheet("""
                             QPushButton {
                                background-color: #959595;
                                border: none;
                                border-radius: 5px;
                                color: white;
                                padding: 10px;
                                text-align: center;
                                text-decoration: none;
                                font-size: 16px;
                                margin-top: 10px;
                             }
                             
                            QPushButton:hover {
                                background-color: #858585;
                            }
                """)
        
        # Créer un QVBoxLayout
        layout = QtWidgets.QVBoxLayout()
        layout.addWidget(searchEventEdit)
        layout.addWidget(label)
        layout.addWidget(self.error)
        layout.addWidget(self.event_list)
        layout.addWidget(button)
        
        # Appliquer le QVBoxLayout à la QDialog
        self.event_dialog.setLayout(layout)
        
        # ESSAI
        self.event_list.itemSelectionChanged.connect(self.on_list_item_selection_changed)
        button.clicked.connect(self.on_button_clicked)
        
        # Centrer la QDialog
        self.event_dialog.setGeometry(500, 635, 400, 400)
        self.event_dialog.setWindowTitle('Seismic events list')
        self.event_dialog.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.event_list.setGeometry(100, 100, 200, 200)
        self.event_dialog.setModal(False)
        self.event_dialog.show()
        
    def on_list_item_selection_changed(self):
        if self.event_list.currentItem() is not None:
            self.error.setText("")
        else:
            self.error.setText("Sélectionnez d'abord un élément de la liste !")

    #‼@QtCore.pyqtSlot(QtWidgets.QListWidgetItem)  
    def on_button_clicked(self):
        if self.event_list.currentItem() is not None:
            # L'utilisateur a sélectionné un élément, faites quelque chose ici
            #print("L'utilisateur a sélectionné :", self.event_list.currentItem().text())
            item = self.event_list.currentItem()
            index = self.event_list.row(item)
            self.eqo = self.events_center[index].origins[0]
            self.eqoMag = self.events_center[index].magnitudes[0].mag
            self.eqoStart = self.eqo.time
            self.eqoLatitude = self.eqo.latitude
            self.eqoLongitude = self.eqo.longitude
            print("eqo.latitude =", self.eqoLatitude)
            print("eqo.longitude =", self.eqoLongitude)
            
            self.record_section_dialog(item)
        else:
            # Afficher le message d'erreur
            self.error.setText("Sélectionnez d'abord un élément de la liste !")
            self.error.setStyleSheet("color: red")
  

    
    def updateListEventWidget(self, text):
        for index in range(self.event_list.count()):
            item = self.event_list.item(index)
            if text.lower() in item.text().lower():
                item.setHidden(False)
            else:
                item.setHidden(True)
                
    '''           
    @QtCore.pyqtSlot(QtWidgets.QListWidgetItem)           
    def buildEventPopup(self,item):
        # Recupération de la position de l'élément séléctionné dans la liste
        index = self.event_list.row(item)
        
        self.eqo = self.events_center[index].origins[0]
        print("eqo : ",self.eqo)
        self.eqoMag = self.events_center[index].magnitudes[0].mag
        
        exPopup = self.record_section_dialog(item)
        exPopup.setWindowTitle("Seismic event {} details".format(item.text()))
        exPopup.show()
    '''
    # %%  RECORD SECTION  
    def record_section_dialog(self,item):
        self.event_dialog.close()
        self.section_dialog = QtWidgets.QDialog()
        self.section_dialog.setWindowIcon(QtGui.QIcon('logo.jpg'))
        self.section_dialog.setWindowTitle("{}".format(item.text()))
        self.section_dialog.resize(900,700)
        
        vbox = QtWidgets.QVBoxLayout()
        title = QtWidgets.QLabel()
        title.setText("<b>Record section</b>")
        title.setAlignment(QtCore.Qt.AlignCenter)
        
        eq_lat = self.eqo.latitude
        eq_lon = self.eqo.longitude
        eq_start = self.eqo.time
        
        # Liste de stations selectionnées
        self.stations_communes = []
        for station in self.stations:
            if station[0] in self.checked_stations and station[1] in self.checked_stations[station[0]]:
                self.stations_communes.append(station)
        print("Stations communes: ",self.stations_communes)
        
        #
        network_set = {s[0] for s in self.stations_communes}
        print("Network set : ",network_set)
        stations_set = {s[1] for s in self.stations_communes}
        print("Stations set : ", stations_set)
        
        # GET SEISMIC TRACE
        client = Client("IRIS")
        print("Getting seismic traces...")
        self.st = client.get_waveforms(
            network = ",".join(network_set),
            station = ",".join(stations_set),
            location = "00",
            channel = str(self.channel_choice.currentText()),
            starttime = eq_start,
            endtime = eq_start + 14400,
            attach_response = True,
            )
        
        # %% ROTATION PROCESSING
        '''
        print("Rotation now!")
        back_azimuths = []
        for station_info in self.stations_communes:
            network, station, station_latitude, station_longitude, _ = station_info
        
            # Calcul du back-azimuth à partir des coordonnées
            _, back_azimuth, _ = gps2dist_azimuth(station_latitude, station_longitude, self.eqoLatitude, self.eqoLongitude)
            back_azimuths.append(back_azimuth)
    
        # Calcul de l'azimuth de référence (moyenne des back-azimuths)
        reference_azimuth = sum(back_azimuths) / len(back_azimuths)
        
        # Appliquer la rotation horizontale pour chaque station
        for trace in self.st:
            trace_station = trace.stats.station
        
            # Trouver l'index correspondant à la station dans la liste station_list
            station_index = next((i for i, station_info in enumerate(self.stations_communes) if station_info[1] == trace_station), None)
            if station_index is not None:
                back_azimuth = back_azimuths[station_index]
        
                # Appliquer la rotation horizontale avec l'azimut de référence
                trace.rotate(method="NE->RT", back_azimuth=back_azimuth, inventory=None)
        self.st.plot()
        '''
        
        # %% Signal processing
        stz = self.st.select(component="Z")
        stz.remove_response(output="VEL")
        stz.filter("bandpass",freqmin=0.05,freqmax=0.2)
        
        print("now plotting the seismic trace!")
        #name = 'record_section_ev_%s.png' % str(self.starttime)
        name = "essai.png"
        #stz.plot();

        self.figure_record_section = plot_record_section_degree(stz, self.stations_communes, eq_lat, eq_lon, outfile=name)
        self.canvas_record_section = FigureCanvas(self.figure_record_section)
        
        
        download_btn = QtWidgets.QPushButton("Download this section")
        download_btn.clicked.connect(self.download_seismic_data)
        download_btn.setCursor(QtCore.Qt.PointingHandCursor)
        download_btn.setStyleSheet("""
                                   QPushButton {
                                       background-color: #959595;
                                       border: none;
                                       border-radius: 5px;
                                       color: white;
                                       padding: 10px;
                                       text-align: center;
                                       text-decoration: none;
                                       font-size: 16px;
                                       margin-top: 10px;
                                  }
                                   QPushButton:hover {
                                       background-color: rgb(86, 101, 115);
                                   }
                                       """)
                                       
        vbox.addWidget(title)
        vbox.addWidget(self.canvas_record_section)    
        vbox.addWidget(download_btn)
        self.section_dialog.setLayout(vbox)
        self.section_dialog.exec_()
        
    def download_seismic_data(self):
        directory = QtWidgets.QFileDialog.getExistingDirectory(None, "Select a destination folder")
        if directory:
            
            filename = self.eqoStart.strftime('%Y%m%dT%H%M%S')           
            seismic_data_directory = os.path.join(directory, filename)  # Dossier parent "seismic_data"
            
            if not os.path.exists(seismic_data_directory):
                os.makedirs(seismic_data_directory)

            format_choices = ["mseed", "sac"]
            selected_format, _ = QtWidgets.QInputDialog.getItem(None, "Select the data format", "Format:", format_choices, 0, False)

            temp_file =  os.path.join(directory, "temp_file")
            if not os.path.exists(temp_file):
                os.makedirs(temp_file)

            '''
            for trace in self.st:
                    
                filename = f"{trace.stats.station}_{trace.stats.starttime.strftime('%Y%m%dT%H%M%S')}.{selected_format}"
                filepath = os.path.join(temp_file, filename)
                
                start_time = UTCDateTime(trace.stats.starttime)  # Conversion en UTCDateTime
                sac_header = {
                    'network': trace.stats.network,
                    'station': trace.stats.station,
                    'location': trace.stats.location,
                    'channel': trace.stats.channel,
                    'starttime': start_time,
                    'sampling_rate': trace.stats.sampling_rate,
                }
                
                if selected_format == "mseed":
                    trace.write(filepath, format='MSEED')
                elif selected_format == "sac":
                    trace.write(filepath, format='SAC',sac_header=sac_header, append=True)
            '''
            for trace in self.st:
                network = trace.stats.network
                station = trace.stats.station
                start_time = trace.stats.starttime.strftime('%Y%m%dT%H%M%S')
                end_time = trace.stats.endtime.strftime('%Y%m%dT%H%M%S')
                for component in trace.stats.channel.split(','):
                    component = component.strip()  # Remove leading/trailing whitespaces if any
                    filename = f"{network}_{station}_{component}_{start_time}_{end_time}.{selected_format}"
                    filepath = os.path.join(seismic_data_directory, filename)
    
                    if selected_format == "mseed":
                        trace.write(filepath, format='MSEED')
                    elif selected_format == "sac":
                        trace.write(filepath, format='SAC')
        QtWidgets.QMessageBox.information(None, "Download completed", "Seismic data successfully downloaded.")
        
        
        
#import resource_rc

class SearchProxyModel(QtCore.QSortFilterProxyModel):
    def __init__(self, parent=None):
        super(SearchProxyModel, self).__init__(parent)
        self.text = ''

    # Recursive search
    
    def _accept_index(self, idx):
        if idx.isValid():
            text = idx.data(role=QtCore.Qt.DisplayRole).lower()
            condition = text.find(self.text) >= 0

            if condition:
                return True
            for childnum in range(idx.model().rowCount(parent=idx)):
                if self._accept_index(idx.model().index(childnum, 0, parent=idx)):
                    return True
        return False

    def filterAcceptsRow(self, sourceRow, sourceParent):
        # Only first column in model for search
        idx = self.sourceModel().index(sourceRow, 0, sourceParent)
        return self._accept_index(idx)

    def lessThan(self, left, right):
        leftData = self.sourceModel().data(left)
        rightData = self.sourceModel().data(right)
        return leftData < rightData
    
    
    
        