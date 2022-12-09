# This Python file uses the following encoding: utf-8
import sys
from PyQt6 import QtWidgets, uic
import Windows
import resources


if __name__ == "__main__":
    app = QtWidgets.QApplication([])
    window = Windows.WelcomeWindow()
    window.show()
    sys.exit(app.exec())
