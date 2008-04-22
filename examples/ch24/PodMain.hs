{-- snippet imports --}
-- ch24/PodMain.hs

module PodMain where

import PodDownload
import PodDB
import PodTypes
import System.Environment
import Database.HDBC
import Network.Socket(withSocketsDo)

-- GUI libraries

import Graphics.UI.Gtk hiding (disconnect)
import Graphics.UI.Gtk.Glade

-- Threading

import Control.Concurrent

{-- /snippet imports --}

{-- snippet type --}
-- | Our main GUI type
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
{-- /snippet type --}

main gladepath = withSocketsDo $ handleSqlError $
    do initGUI                  -- Initialize GTK engine
       -- Every so often, we try to run other threads.
       timeoutAddFull (yield >> return True)
                      priorityDefaultIdle 100
       gui <- loadGlade gladepath
       dbh <- connect "pod.db"

       connectGui gui dbh
       mainGUI                  -- Main GTK loop; exits when GUI done
       
       disconnect dbh

loadGlade gladepath =
    do Just xml <- xmlNew gladepath

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

guiDownload gui dbh =
    statusWindow gui dbh "Pod: Download" (download dbh)

guiFetch gui dbh =
    statusWindow gui dbh "Pod: Fetch" 
                     (\logf -> update dbh logf >> download dbh logf)

statusWindow gui dbh title func =
    do -- Clear the status text
       labelSetText (swLabel gui) ""
       
       -- Disable the OK button, enable Cancel button
       widgetSetSensitivity (swOKBt gui) False
       widgetSetSensitivity (swCancelBt gui) True

       childThread <- forkIO childTasks
       onClicked (swCancelBt gui) (cancelChild childThread)
       
       -- Show the window
       windowPresent (statusWin gui)
    where childTasks =
              do updateLabel "Starting thread..."
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
       logf "Update complete."
    where procPodcast pc =
              do logf $ "Updating from " ++ (castURL pc)
                 updatePodcastFromFeed dbh pc

download dbh logf =
    do pclist <- getPodcasts dbh
       mapM_ procPodcast pclist
       logf "Download complete."
    where procPodcast pc =
              do logf $ "Considering " ++ (castURL pc)
                 episodelist <- getPodcastEpisodes dbh pc
                 let dleps = filter (\ep -> epDone ep == False)
                             episodelist
                 mapM_ procEpisode dleps
          procEpisode ep =
              do logf $ "Downloading " ++ (epURL ep)
                 getEpisode dbh ep
