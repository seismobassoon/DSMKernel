# -*- coding: utf-8 -*-
"""
Created on Wed Mar 29 15:06:54 2023

@author: Lenovo
"""

import sys
import time
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QProgressBar, QLabel, QFrame, QHBoxLayout, QVBoxLayout, QMainWindow
from PyQt5.QtWidgets import QLineEdit, QDialog, QGraphicsDropShadowEffect, QGraphicsBlurEffect, QMessageBox, QDialog
from PyQt5.QtCore import Qt, QTimer, QRect
from PyQt5.QtGui import QCursor, QIcon, QColor

import pyrebase

config = {
  "apiKey": "apiKey",
  "authDomain": "projectId.firebaseapp.com",
  "databaseURL": "https://databaseName.firebaseio.com",
  "storageBucket": "projectId.appspot.com",
  "serviceAccount": "path/to/serviceAccountCredentials.json"
}

firebase = pyrebase.initialize_app(config)

class LogInScreen(QMainWindow):
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
        widget_left.setFixedSize(350,450)
        layout_left = QVBoxLayout(widget_left)
        self.title = Title()
        layout_left.addWidget(self.title)
        
        #Shadow pattern
        
        widget_right = QWidget()
        widget_right.setObjectName('rightWidget')
        widget_right.setFixedSize(250,350)
        
        layout_right = QVBoxLayout(widget_right) 
        layout_right.setContentsMargins(0, 0, 0, 0) # supprime les marges pour éviter que l'effet ne soit tronqué
        '''
        shadow = QGraphicsDropShadowEffect()
        shadow.setBlurRadius(15)
        shadow.setOffset(0, 0)
        shadow.setColor(QColor(0, 0, 0))
        
        widget_right.setGraphicsEffect(shadow) # applique l'effet au widget_right
        '''
        login = LogInWidget()
        layout_right.addWidget(login)


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
        
        self.text = QLabel()
        self.text.setText("Welcome to this application!\nPlease, log in to continue.")
        self.text.setStyleSheet("font-size: 25px;color:black;")
        
        self.text.setAlignment(Qt.AlignLeft)
        layout.addWidget(self.text)
        layout.addSpacing(-10)
        
        self.setLayout(layout)

class LogInWidget(QWidget):
    def __init__(self):
        super().__init__()
        self.setStyleSheet("""
                           background-color: white;
                           """)
        self.resize(290,290)
        
        self.layout = QVBoxLayout()
        self.layout.setAlignment(Qt.AlignCenter)
        
        self.labelLogin = QLabel("Log In")
        self.labelLogin.setAlignment(Qt.AlignCenter)
        self.labelLogin.setFixedSize(181, 61)
        self.labelLogin.setStyleSheet("""
                                      QLabel {
                                          color: black;
                                          font-family: verdana, arial, sans-serif;
                                          font-size: 36px;

                                          }
                                      
                                      """)
        
        self.layout.addWidget(self.labelLogin)
        self.layout.addSpacing(20)
        self.username = QLineEdit()
        self.username.setFixedSize(181,31)
        self.username.setPlaceholderText("Username")
        self.username.setStyleSheet("""
                                    QLineEdit {
                                        border: 2px solid rgb(38,38,48);
                                        border-radius: 15px;
                                        color: rgb(36,36,36);
                                        background-color: #F4F4F4;
                                        padding-left: 15px;
                                        }
                                    
                                    QLineEdit:hover {
                                        border: 2px solid rgb(48, 50, 62);
                                        background-color: #F4F4F4;
                                        }
                                    QLineEdit:focus {
                                        border: 2px solid rgb(228,0,0);
                                        background-color: #F4F4F4;
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
                                        background-color: #F4F4F4;
                                        padding-left: 15px;
                                        }
                                    
                                    QLineEdit:hover {
                                        border: 2px solid rgb(48, 50, 62);
                                        background-color: #F4F4F4
                                        }
                                    QLineEdit:focus {
                                        border: 2px solid rgb(228,0,0);
                                        background-color: #F4F4F4;
                                        }
                                    
                                    """)
                                    
        
        self.login = QPushButton("Login")
        self.login.setFixedSize(181, 31)
        self.login.clicked.connect(self.handleLogin)
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
        self.register.clicked.connect(self.showRegistrationForm)
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
    
                    
        self.layout.addWidget(self.username)
        self.layout.addSpacing(15)
        self.layout.addWidget(self.password)
        self.layout.addSpacing(15)
        self.layout.addWidget(self.login)
        self.layout.addWidget(self.register)
        self.layout.addWidget(self.passwordForgotten)
        self.setLayout(self.layout)
    
        
    def handleLogin(self):
        if (self.username.text() == 'ipgp' and self.password.text() == 'sismo'):
            self.openNewWindow()
        else:
            msg_box = QMessageBox()
            msg_box.setWindowTitle("Error")
            msg_box.setText("Bad user or password.")
            msg_box.setIcon(QMessageBox.Critical)
            msg_box.setStandardButtons(QMessageBox.Ok)
            msg_box.setDefaultButton(QMessageBox.Ok)
            msg_box.setStyleSheet("""
                                  QMessageBox{
                                      background-color: white;
                                      font-family: calibri;
                                      }
                                  QMessageBox QPushButton {
                                      color: white;
                                      background-color: rgb(228,0,0);
                                      border-radius: 5px;
                                      padding: 5px;
                                      width: 80px
                                      }
                                  QMessageBox QPushButton:hover {
                                      background-color:rgb(193, 0,0);
                                      }
                                
                                  """)
            msg_box.exec_()
    
    def showRegistrationForm(self):
        # Clear the current layout
        for i in reversed(range(self.layout().count())):
            self.layout().itemAt(i).widget().setParent(None)

        # Create the new widgets for the registration form
        self.labelRegister = QLabel("Register")
        self.labelRegister.setAlignment(Qt.AlignCenter)
        self.labelRegister.setFixedSize(181, 61)

        self.newUsername = QLineEdit()
        self.newUsername.setFixedSize(181, 31)
        self.newUsername.setPlaceholderText("Create username")

        self.newPassword = QLineEdit()
        self.newPassword.setFixedSize(181, 31)
        self.newPassword.setPlaceholderText("Create password")
        self.newPassword.setEchoMode(QLineEdit.Password)

        self.confirmPassword = QLineEdit()
        self.confirmPassword.setFixedSize(181, 31)
        self.confirmPassword.setPlaceholderText("Confirm password")
        self.confirmPassword.setEchoMode(QLineEdit.Password)

        self.registerButton = QPushButton("Register")
        self.registerButton.setFixedSize(181, 31)
        self.registerButton.clicked.connect(self.handleRegistration)

        backButton = QPushButton("Back")
        backButton.setFixedSize(181, 31)
        #backButton.clicked.connect(self.showLogin)

        # Add the new widgets to the layout
        layout = QVBoxLayout()
        layout.setAlignment(Qt.AlignCenter)
        layout.addWidget(self.labelRegister)
        layout.addWidget(self.newUsername)
        layout.addSpacing(15)
        layout.addWidget(self.newPassword)
        layout.addSpacing(15)
        layout.addWidget(self.confirmPassword)
        layout.addSpacing(15)
        layout.addWidget(self.registerButton)
        layout.addWidget(backButton)
        self.setLayout(layout)
    
    def handleRegister(self):
        username = self.username.text()
        password = self.password.text()
        confirmPassword = self.conformPassword.text()
        
        if not username or not password or not confirmPassword:
            QMessageBox.warning(self, 'Error', 'All fields are required')
            return
        
        if password != confirmPassword:
            QMessageBox.warning(self, 'Error', 'Passwords do not match')
            return
    
    def openNewWindow(self):
        win = QMainWindow()
        win.show()
        
        
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
        
class MainWindow(QMainWindow):
    def __init__(self, parent=None):
        super(MainWindow, self).__init__(parent)
        self.resize(500,500)
'''        
class MainWindow(QWidget):
    def __init__(self, session):
        super().__init__()
        self.session = session
        self.initUI()
        
    def initUI(self):
        self.setGeometry(100,100,300,200)
        self.setWindowTitle('Main Window')
        if self.session.get('loggedIn', False):
            self.label = QLabel(f'Welcome, {self.session.get("username")}!', self)
        else:
            self.label = QLabel('Please log in to continue.', self)
        layout = QVBoxLayout()
        layout.addWidget(self.label)
        self.setLayout(layout)
 '''       
        
if __name__ == "__main__":
    app = QApplication(sys.argv)
    app.setStyleSheet("""
                      #rightWidget {
                          border: 2px solid #F0EFEF;
                          }
                      """)
    log = LogInScreen()
    log.show()
    sys.exit(app.exec_())
    '''
    if log.exec_() == QDialog.Accepted:
        win = MainWindow()
        win.show()
        sys.exit(app.exec_())'''