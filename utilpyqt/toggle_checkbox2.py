import os, sys
from PyQt5 import QtCore, QtGui, QtWidgets
from PyQt5.QtWidgets import QWidget
from PyQt5.QtCore import Qt, QModelIndex, QRegExp, pyqtSignal, QObject

tags = {
    "Animals": [
        "Birds",
        "Various"
    ],
    "Brick": [
        "Blocks",
        "Special"
    ],
    "Manmade": [
        "Air Conditioners",
        "Audio Equipment"
    ],
    "Food": [
        "Fruit",
        "Grains and Seeds"
    ]
}

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


class TagsBrowserWidget(QtWidgets.QWidget):

    clickedTag = QtCore.pyqtSignal(list)

    def __init__(self, parent=None):
        super(TagsBrowserWidget, self).__init__(parent)
        self.resize(300,500)

        # controls
        self.ui_search = QtWidgets.QLineEdit()
        self.ui_search.setPlaceholderText('Search...')

        self.tags_model = SearchProxyModel()
        self.tags_model.setSourceModel(QtGui.QStandardItemModel())
        self.tags_model.setDynamicSortFilter(True)
        self.tags_model.setFilterCaseSensitivity(QtCore.Qt.CaseInsensitive)

        self.ui_tags = QtWidgets.QTreeView()
        self.ui_tags.setSortingEnabled(True)
        self.ui_tags.sortByColumn(0, QtCore.Qt.AscendingOrder)
        self.ui_tags.setEditTriggers(QtWidgets.QAbstractItemView.NoEditTriggers)
        self.ui_tags.setHeaderHidden(True)
        self.ui_tags.setRootIsDecorated(True)
        self.ui_tags.setUniformRowHeights(True)
        self.ui_tags.setModel(self.tags_model)

        # layout
        main_layout = QtWidgets.QVBoxLayout()
        main_layout.addWidget(self.ui_search)
        main_layout.addWidget(self.ui_tags)
        self.setLayout(main_layout)

        # signals
        self.ui_tags.doubleClicked.connect(self.tag_double_clicked)
        self.ui_search.textChanged.connect(self.search_text_changed)

        # init
        self.create_model()

    def create_model(self):
        model = self.ui_tags.model().sourceModel()
        self.populate_tree(tags, model.invisibleRootItem())
        self.ui_tags.sortByColumn(0, QtCore.Qt.AscendingOrder)


    def populate_tree(self, children, parent):
        for child in sorted(children):
            node = QtGui.QStandardItem(child)
            parent.appendRow(node)

            if isinstance(children, dict):
                self.populate_tree(children[child], node)


    def tag_double_clicked(self, index):
        text = index.data(role=QtCore.Qt.DisplayRole)
        print([text])
        self.clickedTag.emit([text])


    def search_text_changed(self, text):
        regExp = QtCore.QRegExp(self.ui_search.text(), QtCore.Qt.CaseInsensitive, QtCore.QRegExp.FixedString)

        self.tags_model.text = self.ui_search.text().lower()
        self.tags_model.setFilterRegExp(regExp)

        if len(self.ui_search.text()) >= 1 and self.tags_model.rowCount() > 0:
            self.ui_tags.expandAll()
        else:
            self.ui_tags.collapseAll()


if __name__ == "__main__":
    app = QtWidgets.QApplication(sys.argv)
    ex = TagsBrowserWidget()
    ex.show()
    sys.exit(app.exec_())
 
