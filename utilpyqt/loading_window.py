# -*- coding: utf-8 -*-
"""
Created on Wed Feb 22 11:04:53 2023

@author: Lenovo
"""

import sys, time
from PyQt5 import QtCore, QtWidgets
from PyQt5.QtWidgets import QMainWindow, QDialog, QLabel, QGridLayout, QWidget, QProgressBar
from PyQt5.QtCore import QThread, pyqtSignal
from PyQt5.QtCore import QSize

class Main_Window(QMainWindow):
    def __init__(self):
        QMainWindow.__init__(self)
        Progress = 10
        LoadWin.progressUpdate(Progress)
        time.sleep(2)
        self.setMinimumSize(QSize(640, 480))
        self.setWindowTitle("Main Window")

        centralWidget = QWidget(self)
        self.setCentralWidget(centralWidget)
        Progress = 30
        LoadWin.progressUpdate(Progress)
        time.sleep(2)

        gridLayout = QGridLayout(self)
        centralWidget.setLayout(gridLayout)
        Progress = 50
        LoadWin.progressUpdate(Progress)
        time.sleep(2)

        title = QLabel("I am the main window", self)
        title.setAlignment(QtCore.Qt.AlignCenter)
        gridLayout.addWidget(title, 0, 0)

        #   Running the loading screen in own thread, but the event handler is the same :(
        # self.Thread = Thread_Load()
        # self.Thread.start()


        #   Call event handler to process the queue
        #app.processEvents()
        Progress = 100
        LoadWin.progressUpdate(Progress)
        self.show()
        LoadWin.close()

class Load_Window(QDialog):
    
    def __init__(self):
        QDialog.__init__(self)
        self.setGeometry(0,0,500,500)
        self.setWindowTitle("Load Window")

        title = QLabel("I am the Load window", self)
        self.pbar = QProgressBar(self)
        
        self.pbar.setGeometry(30, 40, 200, 25)
        
        title.setAlignment(QtCore.Qt.AlignCenter)
        title.move(200,200)
        self.show()

    def progressUpdate(self, progress):
        self.pbar.setValue(progress)

if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    LoadWin = Load_Window()
    app.processEvents()

    MainWin=Main_Window()
    sys.exit(app.exec_())