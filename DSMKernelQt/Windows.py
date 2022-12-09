# This Python file uses the following encoding: utf-8

from PyQt6 import QtWidgets,uic
from PyQt6 import QtCore
from PyQt6.QtMultimedia import QMediaPlayer
from PyQt6.QtCore import QUrl,QFile

QtCore.QDir.addSearchPath('images','images')
'''
class WelcomeWindow2(QtWidgets.QMainWindow,Ui_MainWindow):
    def __init__(self):
        super(MainWindow,self).__init__()
        self.setupUi(self)
        #uic.loadUi("mainwindow.ui",self)
'''

class WelcomeWindow(QtWidgets.QMainWindow):
    def __init__(self):
        super().__init__()
        uic.loadUi("mainwindow.ui",self)

        self.effect = QMediaPlayer()
        self.effect.setSource(QUrl("qrc:/music/music/quatuor.mp3"))
        #self.effect.setLoopCount(1)
        #self.effect.setVolume(1)

        self.effect.play()
