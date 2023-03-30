# -*- coding: utf-8 -*-
"""
Created on Wed Mar 29 15:06:54 2023

@author: Lenovo
"""

import sys
import time
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QProgressBar, QLabel, QFrame, QHBoxLayout, QVBoxLayout, QMainWindow
from PyQt5.QtWidgets import QLineEdit, QDialog, QGraphicsDropShadowEffect, QGraphicsBlurEffect
from PyQt5.QtCore import Qt, QTimer
from PyQt5.QtGui import QCursor, QIcon, QColor

class MainWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        self.resize(900,600)
        self.setWindowIcon(QIcon('logo.jpg'))
        self.setWindowTitle("Geodpy Project - Python application for scientific research")
        self.setStyleSheet("""
                           QMainWindow {
                               background-color: white;
                               
                               }
                           """)
        widget_left = QWidget()
        widget_left.setFixedSize(450,600)
        layout_left = QVBoxLayout(widget_left)
        self.title = Title()
        layout_left.addWidget(self.title)
        
        #Shadow pattern
        
        
        widget_right = QWidget()
        widget_right.setObjectName('rightWidget')
        widget_right.setFixedSize(300,350)
        
        layout_right = QVBoxLayout(widget_right) 
        layout_right.setContentsMargins(0, 0, 0, 0) # supprime les marges pour éviter que l'effet ne soit tronqué
  
        shadow = QGraphicsDropShadowEffect()
        shadow.setBlurRadius(15)
        shadow.setOffset(0, 0)
        shadow.setColor(QColor(255, 0, 0))
        '''
        widget_right.setGraphicsEffect(shadow) # applique l'effet au widget_right
        '''
        blur_effect = QGraphicsBlurEffect()
        blur_effect.setBlurRadius(10)
        layout_right.addWidget(LogInScreen())


        self.layout = QHBoxLayout()
        self.layout.addWidget(widget_left)
        self.layout.addWidget(widget_right)
        
        centralWidget = QWidget()
        centralWidget.setLayout(self.layout)
        self.setCentralWidget(centralWidget)


class Title(QWidget):
    def __init__(self):
        super().__init__()
        layout = QVBoxLayout()
        self.labelTitle = QLabel()
        self.labelTitle.setObjectName('LabelTitle')
        self.labelTitle.setStyleSheet("""
                                      QLabel {
                                          font-family: verdana, arial, Helvetica,sans-serif;
                                          font-size: 45px;
                                          font-weight: bold;
                                          color: rgb(228,0,0);
                                          
                                      }
                                      """)
        
        self.labelTitle.resize(self.width() - 10, 10)
        self.labelTitle.move(0, 40) # x, y
        self.labelTitle.setText('Python App\nProject')
        self.labelTitle.setAlignment(Qt.AlignLeft)
        layout.addWidget(self.labelTitle)
        layout.addSpacing(-10)
        
        self.text = QLabel()
        self.text.setText("Welcome to this application!\nPlease, log in to continue")
        self.text.setStyleSheet("font-size: 25px;color:black;")
        self.text.setAlignment(Qt.AlignLeft)
        layout.addWidget(self.text)
        self.setLayout(layout)

class LogInScreen(QWidget):
    def __init__(self):
        super().__init__()
        self.setStyleSheet("""
                           background-color: white;
                           """)
        self.resize(290,290)
        
        layout = QVBoxLayout()
        layout.setAlignment(Qt.AlignCenter)
        
        self.labelLogin = QLabel("Log In")
        self.labelLogin.setAlignment(Qt.AlignCenter)
        self.labelLogin.setFixedSize(181, 61)
        self.labelLogin.setStyleSheet("""
                                      QLabel {
                                          color: rgb(228,0,0);
                                          font-family: verdana, arial, sans-serif;
                                          font-size: 36px;
                                          font-weight: bold;
                                          }
                                      
                                      """)
        
        layout.addWidget(self.labelLogin)
        layout.addSpacing(20)
        self.username = QLineEdit()
        self.username.setFixedSize(181,31)
        self.username.setPlaceholderText("Username")
        self.username.setStyleSheet("""
                                    QLineEdit {
                                        border: 2px solid rgb(38,38,48);
                                        border-radius: 15px;
                                        color: rgb(36,36,36);
                                        background-color: #FFF;
                                        padding-left: 15px;
                                        }
                                    
                                    QLineEdit:hover {
                                        border: 2px solid rgb(48, 50, 62);
                                        }
                                    QLineEdit:focus {
                                        border: 2px solid rgb(228,0,0);
                                        background-color: #FFF;
                                        }
                                    
                                    """)
                                    
        self.password = QLineEdit()
        self.password.setFixedSize(181,31)
        self.password.setPlaceholderText("Password")
        self.password.setEchoMode(QLineEdit.Password)
        self.password.setStyleSheet("""
                                    QLineEdit {
                                        border: 2px solid rgb(38,38,48);
                                        border-radius: 15px;
                                        color: rgb(36,36,36);
                                        background-color: #FFF;
                                        padding-left: 15px;
                                        }
                                    
                                    QLineEdit:hover {
                                        border: 2px solid rgb(48, 50, 62);
                                        }
                                    QLineEdit:focus {
                                        border: 2px solid rgb(228,0,0);
                                        background-color: #FFF;
                                        }
                                    
                                    """)
                                    
        
        self.login = QPushButton("Login")
        self.login.setFixedSize(181, 31)
        self.login.setStyleSheet("""
                                      QPushButton {
                                          border-radius: 15px;
                                          background-color: rgb(228,0,0);
                                          color: white;
                                          font-family: calibri;
                                          font-weight: bold;
                                          }
                                      QPushButton:hover {
                                          background-color: rgb(193, 0,0);
                                          }
                                      """)
                          
        cursor = QCursor(Qt.PointingHandCursor)
        self.login.setCursor(cursor)
        
        self.register = QPushButton("Register")
        self.register.setFixedSize(181,31)
        self.register.setCursor(cursor)
        self.register.setStyleSheet("""
                                    QPushButton {
                                        color: red;
                                        border: 2px white;
                                        background-color: white;
                                        font-family: calibri;
                                        }
                                    QPushButton:hover {
                                        text-decoration: underline;
                                        }
                                    """)
        self.passwordForgotten = QPushButton("Password forgotten ?")
        self.passwordForgotten.setCursor(cursor)
        self.passwordForgotten.setFixedSize(181,31)
        self.passwordForgotten.setStyleSheet("""
                                             QPushButton {
                                                 color: gray;
                                                 border: 2px white;
                                                 background-color: white;
                                                 font-family: calibri;
                                                 font-style: italic;
                                                 }
                                             QPushButton:hover {
                                                 text-decoration: underline;
                                                 }

                                             """)
    
                    
        layout.addWidget(self.username)
        layout.addSpacing(15)
        layout.addWidget(self.password)
        layout.addSpacing(15)
        layout.addWidget(self.login)
        layout.addWidget(self.register)
        layout.addWidget(self.passwordForgotten)
        self.setLayout(layout)
        
        
    def on_button_clicked(self):
        if self.isChecked():
            self.setIcon(QIcon('eye_slash_icon.png'))
            self.parent().password_edit.setEchoMode(QLineEdit.Normal)
        else:
            self.setIcon(QIcon('eye_icon.png'))
            self.parent().password_edit.setEchoMode(QLineEdit.Password)
        self.setIconSize(self.size())
        
        
class PasswordButton(QPushButton):
    def __init__(self, parent=None):
        super().__init__(parent)
        self.setCheckable(True)
        self.setIcon(QIcon('eye_icon.png'))
        self.setIconSize(self.size())
        self.setStyleSheet('QPushButton:checked { background-color: #7FDBFF; }')
        self.clicked.connect(self.on_button_clicked)

    def on_button_clicked(self):
        if self.isChecked():
            self.setIcon(QIcon('eye_slash_icon.png'))
            self.parent().password_edit.setEchoMode(QLineEdit.Normal)
        else:
            self.setIcon(QIcon('eye_icon.png'))
            self.parent().password_edit.setEchoMode(QLineEdit.Password)
        self.setIconSize(self.size())       
        
if __name__ == "__main__":
    app = QApplication(sys.argv)
    app.setStyleSheet("""
                      #rightWidget {
                      border: 2px solid red;
                      qproperty-graphicsEffect: shadow;
                          }
                      """)
    win = MainWindow()
    win.show()
    sys.exit(app.exec_())