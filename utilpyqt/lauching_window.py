import sys
import time
from PyQt5 import QtCore, QtWidgets, QtGui
from PyQt5.QtWidgets import QApplication, QWidget, QPushButton, QProgressBar, QLabel, QFrame, QHBoxLayout, QVBoxLayout, QDialog
from PyQt5.QtCore import Qt, QTimer
from PyQt5.QtGui import QImage, QPixmap

class SplashScreen(QWidget):
    def __init__(self):
        super().__init__()
        self.setWindowTitle('Spash Screen Example')
        self.setFixedSize(1100, 700)
        self.setWindowFlag(Qt.FramelessWindowHint)
        self.setAttribute(Qt.WA_TranslucentBackground)
        self.setStyleSheet("background-image: url('wallpaper_splash_screen.jpg');")

        self.counter = 0
        self.n = 300 # total instance

        self.initUI()

        self.timer = QTimer()
        self.timer.timeout.connect(self.loading)
        self.timer.start(30)

    def initUI(self):
        layout = QVBoxLayout()
        self.setLayout(layout)

        #self.frame = QFrame()
        '''
        self.frame.setStyleSheet("""
                                 QFrame {
                                     background-image: url('wallpaper_splash_screen.jpg');
                                     color: rgb(220, 220, 220);
                                 }
                                 """)'''
        #layout.addWidget(self.frame)

        self.labelTitle = QLabel()
        self.labelTitle.setObjectName('LabelTitle')
        self.labelTitle.setStyleSheet("""
                                      QLabel {
                                          font-size: 60px;
                                          color: #93deed;
                                          
                                      }
                                      """)
        
        # center labels
        self.labelTitle.resize(self.width() - 10, 150)
        self.labelTitle.move(0, 40) # x, y
        self.labelTitle.setText('Python App - Project')
        self.labelTitle.setAlignment(Qt.AlignCenter)
        layout.addWidget(self.labelTitle)
       
        # add logo
        logo_label = QLabel()
        logo_image = QPixmap('wallpaper_splash_screen.jpg')
        logo_label.setPixmap(logo_image)
        logo_label.move(200, 230)
        logo_label.setObjectName('LabelLogo')


        logo_label.setAlignment(Qt.AlignCenter)
        layout.addWidget(logo_label)
        #layout.addWidget(logo_label)
        

        self.labelDescription = QLabel()
        self.labelDescription.resize(self.width() - 10, 50)
        self.labelDescription.move(0, self.labelTitle.height())
        self.labelDescription.setObjectName('LabelDesc')
        self.labelDescription.setStyleSheet("""
                                            QLabel {
                                                font-size: 30px;
                                                color: #c2ced1;
                                            }
                                            """)
        self.labelDescription.setText('<strong>Working on Task #1</strong>')
        self.labelDescription.setAlignment(Qt.AlignCenter)
        layout.addWidget(self.labelDescription)

        self.progressBar = QProgressBar()
        self.progressBar.setStyleSheet("""
                                       QProgressBar {
                                           background-color: #DA7B93;
                                           color: rgb(200, 200, 200);
                                           border-style: none;
                                           border-radius: 10px;
                                           text-align: center;
                                           font-size: 30px;
                                       }
                                       QProgressBar::chunk {
                                           border-radius: 10px;
                                           background-color: qlineargradient(spread:pad x1:0, x2:1, y1:0.511364, y2:0.523, stop:0 #1C3334, stop:1 #376E6F);
                                       }
                                       """)
        self.progressBar.resize(self.width() - 200 - 10, 50)
        self.progressBar.move(100, self.labelDescription.y() + 330)
        self.progressBar.setAlignment(Qt.AlignCenter)
        self.progressBar.setFormat('%p%')
        self.progressBar.setTextVisible(True)
        self.progressBar.setRange(0, self.n)
        self.progressBar.setValue(20)
        layout.addWidget(self.progressBar)

        self.labelLoading = QLabel()
        self.labelLoading.resize(self.width() - 10, 50)
        self.labelLoading.move(0, self.progressBar.y() + 70)
        self.labelLoading.setObjectName('LabelLoading')
        self.labelLoading.setStyleSheet("""
                                        QLabel {
                                            font-size: 30px;
                                            color: #e8e8eb;
                                        }
                                        """)
        self.labelLoading.setAlignment(Qt.AlignCenter)
        self.labelLoading.setText('loading...')
        self.labelLoading.setScaledContents(True)
        layout.addWidget(self.labelLoading)

    def loading(self):
        self.progressBar.setValue(self.counter)

        if self.counter == int(self.n * 0.3):
            self.labelDescription.setText('<strong>Working on Task #2</strong>')
        elif self.counter == int(self.n * 0.6):
            self.labelDescription.setText('<strong>Working on Task #3</strong>')
        elif self.counter >= self.n:
            self.timer.stop()
            self.close()

            time.sleep(1)

            self.myApp = LogInWindow()
            self.myApp.show()

        self.counter += 1
        


class LogInWindow(QWidget):
    def __init__(self):
        super().__init__()
        '''
        self.setObjectName("Form")
        self.resize(850, 549)
        self.setAttribute(QtCore.Qt.WA_TranslucentBackground)
        '''
        self.widget = QWidget(self)
        self.widget.setGeometry(QtCore.QRect(20, 20, 590, 420))
        self.widget.setStyleSheet("""
                                  QPushButton#loginButton {
                                      background-color:rgba(85, 98, 112, 255);\n"
                                      color:rgba(255, 255, 255, 200);\n"
                                      border-radius:5px;\n"
                                  }
                                  
                                  QPushButton#loginButton:pressed {
                                      padding-left:5px;
                                      padding-top:5px;
                                      background-color:rgba(255, 107, 107, 255);
                                      background-position:calc(100% - 10px)center;
                                  }
                                  QPushButton#loginButton:hover {
                                      background-color:rgba(255, 107, 107, 255);
                                  }
                                  """)      
                                  
        self.widget.setObjectName("widget")
        # rectangle de droite
        self.label = QtWidgets.QLabel(self.widget)
        self.label.setGeometry(QtCore.QRect(290, 40, 260, 330))
        self.label.setStyleSheet("background-color: qlineargradient(x1:0, y1:0, x2:1, y2:0, stop:0 #fcfafa, stop:1 #cfcfcf); border-radius:10px;")
        self.label.setText("")
        self.label.setObjectName("label")
        
        # rectangle de gauche
        self.label_2 = QtWidgets.QLabel(self.widget)
        self.label_2.setGeometry(QtCore.QRect(40, 25, 270, 360))
        self.label_2.setStyleSheet("background-color: qlineargradient(x1:0, y1:0, x2:1, y2:0, stop:0 #0f0303, stop:1 #ad0909); border-radius:10px;")
        self.label_2.setText("")
        self.label_2.setObjectName("label_2")
        
        self.labelLogin = QtWidgets.QLabel(self.widget)
        self.labelLogin.setGeometry(QtCore.QRect(330, 80, 101, 31))
        font = QtGui.QFont()
        font.setPointSize(15)
        font.setBold(True)
        font.setWeight(75)
        self.labelLogin.setFont(font)
        self.labelLogin.setStyleSheet("color:rgba(0, 0, 0, 200);")
        self.labelLogin.setObjectName("labelLogin")
        
        self.lineEdit = QtWidgets.QLineEdit(self.widget)
        self.lineEdit.setGeometry(QtCore.QRect(330, 140, 190, 40))
        font = QtGui.QFont()
        font.setPointSize(9)
        self.lineEdit.setFont(font)
        self.lineEdit.setStyleSheet("""
                                    QLineEdit {
                                        background-color:rgba(0, 0, 0, 0);
                                        border:2px solid rgba(0, 0, 0, 0);
                                        border-bottom-color:rgba(46, 82, 101, 200);
                                        color:rgb(0, 0, 0);
                                        padding-bottom:7px;
                                        }
                                    """)
        self.lineEdit.setObjectName("lineEdit")
        
        
        self.password = QtWidgets.QLineEdit(self.widget)
        self.password.setGeometry(QtCore.QRect(330, 200, 190, 40))
        font = QtGui.QFont()
        font.setPointSize(9)
        self.password.setFont(font)
        self.password.setStyleSheet("""
                                      QLineEdit {
                                          background-color:rgba(0, 0, 0, 0);
                                          border:2px solid rgba(0, 0, 0, 0);
                                          border-bottom-color:rgba(46, 82, 101, 200);
                                          color:rgb(0, 0, 0);
                                          padding-bottom:7px;
                                          }
                                          
                                          """)
        self.password.setEchoMode(QtWidgets.QLineEdit.Password)
        self.password.setObjectName("password")
        
        self.loginButton = QtWidgets.QPushButton(self.widget)
        self.loginButton.setGeometry(QtCore.QRect(330, 280, 191, 40))
        font = QtGui.QFont()
        font.setPointSize(11)
        font.setBold(True)
        font.setWeight(75)
        self.loginButton.setFont(font)
        self.loginButton.setObjectName("loginButton")
        
        self.label_4 = QtWidgets.QLabel(self.widget)
        self.label_4.setGeometry(QtCore.QRect(337, 326, 191, 21))
        self.label_4.setStyleSheet("color:rgba(0, 0, 0, 200);")
        self.label_4.setObjectName("label_4")
        self.label_5 = QtWidgets.QLabel(self.widget)
        self.label_5.setGeometry(QtCore.QRect(60, 50, 181, 41))
        font = QtGui.QFont()
        font.setPointSize(22)
        font.setBold(True)
        font.setWeight(75)
        self.label_5.setFont(font)
        self.label_5.setStyleSheet("color:rgba(255, 255, 255, 220);")
        self.label_5.setObjectName("label_5")
        self.label_6 = QtWidgets.QLabel(self.widget)
        self.label_6.setGeometry(QtCore.QRect(60, 106, 231, 41))
        font = QtGui.QFont()
        font.setPointSize(10)
        self.label_6.setFont(font)
        self.label_6.setStyleSheet("color:rgba(255, 255, 255, 220);")
        self.label_6.setObjectName("label_6")
        self.label_7 = QtWidgets.QLabel(self.widget)
        self.label_7.setGeometry(QtCore.QRect(50, 221, 251, 201))
        font = QtGui.QFont()
        font.setFamily("Mountain")
        font.setPointSize(150)
        self.label_7.setFont(font)
        self.label_7.setStyleSheet("color:rgba(255, 107, 107, 255);")
        self.label_7.setObjectName("label_7")

        self.label.setGraphicsEffect(QtWidgets.QGraphicsDropShadowEffect(blurRadius=25, xOffset=0, yOffset=0))
        self.label_2.setGraphicsEffect(QtWidgets.QGraphicsDropShadowEffect(blurRadius=25, xOffset=0, yOffset=0))
        self.loginButton.setGraphicsEffect(QtWidgets.QGraphicsDropShadowEffect(blurRadius=25, xOffset=3, yOffset=3))

        self.retranslateUi()
        QtCore.QMetaObject.connectSlotsByName(self)

    def retranslateUi(self):
        _translate = QtCore.QCoreApplication.translate
        self.setWindowTitle(_translate("Form", "Form"))
        self.labelLogin.setText(_translate("Form", "Log In"))
        
        self.lineEdit.setPlaceholderText(_translate("Form", " Username"))
        self.password.setPlaceholderText(_translate("Form", "  Password"))
        
        self.loginButton.setText(_translate("Form", "L o g  I n"))
        self.label_4.setText(_translate("Form", "Forgot your User Name or Password?"))
        self.label_5.setText(_translate("Form", "Project"))
        self.label_6.setText(_translate("Form", "Hi,\n"
"Welcome to this application\n"
"Please, log in if you already\n"
"have an account"))
        self.label_7.setText(_translate("Form", "-"))

if __name__ == '__main__':
    # don't auto scale when drag app to a different monitor.
    # QApplication.setAttribute(Qt.HighDpiScaleFactorRoundingPolicy.PassThrough)
    
    app = QApplication(sys.argv)

    splash = SplashScreen()
    splash.show()

    try:
        sys.exit(app.exec_())
    except SystemExit:
        print('Closing Window...')