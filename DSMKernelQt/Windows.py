# This Python file uses the following encoding: utf-8

from PyQt6 import QtWidgets,uic
#from mainwindow import Ui_MainWindow
from PyQt6 import QtCore
from PyQt6.QtMultimedia import QSoundEffect
from PyQt6.QtCore import QUrl

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
        file=":/music/quatuor.m4a"
        effect = QSoundEffect()
        effect.setSource(QUrl.fromLocalFile(file))
        effect.setLoopCount(-2)
        effect.play()
