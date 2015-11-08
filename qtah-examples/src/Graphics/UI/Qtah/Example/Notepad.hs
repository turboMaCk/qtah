-- This file is part of Qtah.
--
-- Copyright 2015 Bryan Gardiner <bog@khumba.net>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License version 3
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- | The Qt notepad example.
module Graphics.UI.Qtah.Example.Notepad (run) where

import Control.Monad (unless)
import Foreign.Hoppy.Runtime.Support (withScopedPtr)
import Graphics.UI.Qtah.Signal (on)
import Graphics.UI.Qtah.Widgets.QAbstractButton (clickedSignal)
import Graphics.UI.Qtah.Widgets.QAction (triggeredSignal)
import qualified Graphics.UI.Qtah.Widgets.QApplication as QApplication
import qualified Graphics.UI.Qtah.Widgets.QFileDialog as QFileDialog
import qualified Graphics.UI.Qtah.Widgets.QLayout as QLayout
import qualified Graphics.UI.Qtah.Widgets.QMainWindow as QMainWindow
import Graphics.UI.Qtah.Widgets.QMainWindow (QMainWindow)
import qualified Graphics.UI.Qtah.Widgets.QMenu as QMenu
import qualified Graphics.UI.Qtah.Widgets.QMenuBar as QMenuBar
import qualified Graphics.UI.Qtah.Widgets.QPushButton as QPushButton
import qualified Graphics.UI.Qtah.Widgets.QTextEdit as QTextEdit
import Graphics.UI.Qtah.Widgets.QTextEdit (QTextEdit)
import qualified Graphics.UI.Qtah.Widgets.QVBoxLayout as QVBoxLayout
import qualified Graphics.UI.Qtah.Widgets.QWidget as QWidget

data Notepad = Notepad
  { myWindow :: QMainWindow
  , myText :: QTextEdit
  }

run :: IO ()
run = withScopedPtr QApplication.new $ \app -> do
  mainWindow <- makeMainWindow
  QWidget.show mainWindow
  QApplication.exec app

makeMainWindow :: IO QMainWindow
makeMainWindow = do
  window <- QMainWindow.new

  menu <- QMenuBar.new
  menuFile <- QMenuBar.addNewMenu menu "File"
  menuFileNew <- QMenu.addNewAction menuFile "New"
  menuFileOpen <- QMenu.addNewAction menuFile "Open"
  menuFileSave <- QMenu.addNewAction menuFile "Save"
  QMainWindow.setMenuBar window menu

  contents <- QWidget.new
  layout <- QVBoxLayout.new
  QWidget.setLayout contents layout

  text <- QTextEdit.new
  quitButton <- QPushButton.newWithText "Quit"
  QLayout.addWidget layout text
  QLayout.addWidget layout quitButton
  QMainWindow.setCentralWidget window contents

  let me = Notepad
           { myWindow = window
           , myText = text
           }

  _ <- on menuFileNew triggeredSignal $ \_ -> fileNew me
  _ <- on menuFileOpen triggeredSignal $ \_ -> fileOpen me
  _ <- on menuFileSave triggeredSignal $ \_ -> fileSave me
  _ <- on quitButton clickedSignal $ \_ -> QWidget.close window

  return window

fileNew :: Notepad -> IO ()
fileNew me = QTextEdit.setText (myText me) ""

fileOpen :: Notepad -> IO ()
fileOpen me = do
  fileName <- QFileDialog.getOpenFileName
              (myWindow me)
              "Open File"
              ""
              fileDialogFilter

  unless (null fileName) $ do
    contents <- readFile fileName
    QTextEdit.setText (myText me) contents

fileSave :: Notepad -> IO ()
fileSave me = do
  fileName <- QFileDialog.getSaveFileName
              (myWindow me)
              "Save File"
              ""
              fileDialogFilter

  unless (null fileName) $ do
    contents <- QTextEdit.toPlainText $ myText me
    writeFile fileName contents

fileDialogFilter :: String
fileDialogFilter = "Text Files (*.txt);;C++ Files (*.cpp *.h);;All Files (*)"