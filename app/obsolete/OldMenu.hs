module OldMenu (
    menu
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

menu :: IO ()
menu = do
    void initGUI          
    window <- windowNew   
    set window [ windowTitle         := "Sat 2 solver "
               , windowDefaultWidth  := 230
               , windowDefaultHeight := 250 ]
    display <- entryNew
    set display [ entryEditable := True
                , entryXalign   := 0 -- makes contents right-aligned
                , entryText     := "..." ]                      
    --containerAdd window display
    filesButton <- fileChooserButtonNew "Archivos" FileChooserActionOpen
    containerAdd window filesButton

    window `on` deleteEvent $ do -- handler to run on window destruction
        liftIO mainQuit
        return False
    widgetShowAll window  
    mainGUI               