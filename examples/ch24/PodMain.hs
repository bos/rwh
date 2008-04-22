{-- snippet all --}
-- ch23/PodMain.hs

module Main where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

import Control.Concurrent

data GUI = GUI {
      mainWin :: Window,
      mwAddBt :: Button,
      mwUpdateBt :: Button,
      mwDownloadBt :: Button,
      mwFetchBt :: Button,
      mwExitBt :: Button,
      statusWin :: Dialog,
      swOKBt :: Button,
      swCancelBt :: Button,
      swLabel :: Label,
      addWin :: Dialog,
      awOKBt :: Button,
      awCancelBt :: Button,
      awEntry :: Entry}

-- import Paths_Pod(getDataFileName)

main = withSocketsDo $ handleSqlError $
    do initGUI                  -- Initialize GTK engine
       gui <- loadGlade
       dbh <- connect "pod.db"

       connectGui gui dbh
       mainGUI
       
       disconnect dbh

loadGlade =
    do Just xml <- xmlNew "podresources.glade"

       -- Load main window
       mw <- xmlGetWidget xml castToWindow "mainWindow"

       -- Load main window buttons

       [mwAdd, mwUpdate, mwDownload, mwFetch, mwExit, swOK, swCancel,
        auOK, auCancel] <-
           mapM (xmlGetWidget xml castToButton)
           ["addButton", "updateButton", "downloadButton",
            "fetchButton", "exitButton", "okButton", "cancelButton",
            "auOK", "auCancel"]
       
       sw <- xmlGetWidget xml castToDialog "statusDialog"
       swl <- xmlGetWidget xml castToLabel "statusLabel"

       au <- xmlGetWidget xml castToDialog "addDialog"
       aue <- xmlGetWidget xml castToEntry "auEntry"

       return $ GUI mw mwAdd mwUpdate mwDownload mwFetch mwExit
              sw swOK swCancel swl au auOK auCancel aue

connectGui gui dbh =
    do -- When the close button is clicked, terminate GUI loop
       -- by calling GTK mainQuit function
       onDestroy (mainWin gui) mainQuit
       
       -- Main window buttons
       onClicked (mwAddBt gui) (guiAdd gui dbh)
       onClicked (mwUpdateBt gui) (guiUpdate gui dbh)
       onClicked (mwDownloadBt gui) (guiDownload gui dbh)
       onClicked (mwFetchBt gui) (guiFetch gui dbh)
       onClicked (mwExitBt gui) mainQuit

       -- We leave the status window buttons for later

guiAdd gui dbh = 
    do -- Initialize the add URL window
       entrySetText (awEntry gui) ""
       onClicked (awCancelBt gui) (widgetHide (addWin gui))
       onClicked (awOKBt gui) procOK
       
       -- Show the add URL window
       windowPresent (addWin gui)
    where procOK =
              do url <- entryGetText (awEntry gui)
                 widgetHide (addWin gui) -- Remove the dialog
                 add dbh url             -- Add to the DB

add dbh url = 
    do addPodcast dbh pc
       commit dbh
    where pc = Podcast {castId = 0, castURL = url}

guiUpdate gui dbh = 
    statusWindow gui dbh "Pod: Update" (update dbh)

statusWindow gui dbh title func =
    do -- Clear the status text
       labelSetText (swLabel gui) ""
       
       -- Disable the OK button, enable Cancel button
       widgetSetSensitivity (swOKBt gui) False
       widgetSetSensitivity (swCancelBt gui) True

       putStrLn "before forkIO"
       childThread <- forkIO childTasks
       putStrLn "after forkIO"
       onClicked (swCancelBt gui) (cancelChild childThread)
       
       -- Show the window
       windowPresent (statusWin gui)
    where childTasks =
              do updateLabel "Starting thread..."
                 putStrLn "Starting thread..."
                 func updateLabel
                 -- After the child task finishes, enable OK
                 -- and disable Cancel
                 enableOK
                 
          enableOK = 
              do widgetSetSensitivity (swCancelBt gui) False
                 widgetSetSensitivity (swOKBt gui) True
                 onClicked (swOKBt gui) (widgetHide (statusWin gui))
                 return ()

          updateLabel text =
              labelSetText (swLabel gui) text
          cancelChild childThread =
              do killThread childThread
                 yield
                 updateLabel "Action has been cancelled."
                 enableOK
          
                 

update dbh logf = 
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
    where procPodcast pc =
              do logf $ "Updating from " ++ (castURL pc)
                 updatePodcastFromFeed dbh pc

guiDownload gui dbh = fail "Not implemented"

guiFetch gui dbh = fail "Not implemented"

download dbh =
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
    where procPodcast pc =
              do putStrLn $ "Considering " ++ (castURL pc)
                 episodelist <- getPodcastEpisodes dbh pc
                 let dleps = filter (\ep -> epDone ep == False)
                             episodelist
                 mapM_ procEpisode dleps
          procEpisode ep =
              do putStrLn $ "Downloading " ++ (epURL ep)
                 getEpisode dbh ep

syntaxError = putStrLn 
  "Usage: pod command [args]\n\
  \\n\
  \pod add url      Adds a new podcast with the given URL\n\
  \pod download     Downloads all pending episodes\n\
  \pod fetch        Updates, then downloads\n\
  \pod update       Downloads podcast feeds, looks for new episodes\n"
{-- /snippet all --}