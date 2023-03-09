from PyQt5.QtWidgets import QDialog, QVBoxLayout, QApplication
from matplotlib.backends.backend_qt5agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure
import obspy,sys
import numpy as np

class MyDialog(QDialog):
    def __init__(self, file_path, parent=None):
        super().__init__(parent)
        self.setWindowTitle("MSEED Viewer")
        self.setGeometry(100, 100, 800, 600)
        
        # Lecture du fichier MiniSEED avec ObsPy
        st = obspy.read(file_path)
        
        # Extrait les données de la première trace
        trace = st[0]
        data = trace.data
        sr = trace.stats.sampling_rate
        
        # Créer un graphique Matplotlib
        fig = Figure(figsize=(5, 4), dpi=100)
        ax = fig.add_subplot(1, 1, 1)
        
        # Tracer les données dans le graphique
        t = np.arange(len(data)) / sr
        ax.plot(t, data)
        ax.set_xlabel('Time (s)')
        ax.set_ylabel('Amplitude')
        ax.set_title('MSEED Viewer')
        
        # Ajouter le graphique à la fenêtre QDialog
        canvas = FigureCanvas(fig)
        layout = QVBoxLayout(self)
        layout.addWidget(canvas)

if __name__ == "__main__":
    app = QApplication(sys.argv)
    file_path = './vendee4.6.mseed'
    dialog = MyDialog(file_path)
    dialog.exec_()