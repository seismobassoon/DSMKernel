from PyQt6.QtWidgets import QApplication, QWidget

import sys

app = QApplication(sys.argv) # on peut utiliser sys.argv comme une variable

window = QWidget()

window.show()
sys.exit(app.exec())
