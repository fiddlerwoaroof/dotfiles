import Control.Monad

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ratio ((%))
import Data.Word

import System.IO

import XMonad hiding ( (|||) )
import XMonad.Actions.CopyWindow(copy,copyWindow,kill1,killAllOtherCopies)
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.SpawnOn
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.BoringWindows hiding (Replace)
import XMonad.Layout.Circle
import XMonad.Layout.Combo
import XMonad.Layout.Decoration
import XMonad.Layout.DragPane
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.StackSet as W
import XMonad.Util.Dzen
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.Run(spawnPipe)
nmaster :: Int
nmaster = 1

ratio = 13/21

delta = 3/100

tiled :: Tall a
tiled = Tall nmaster delta ratio

myTabbed = renamed [Replace "Tabbed"] $ tabbedBottom shrinkText defaultTheme

threeLayout :: ThreeCol a
threeLayout = ThreeColMid nmaster delta ratio

writingLayout :: ModifiedLayout Rename (ModifiedLayout Gaps Full) a
writingLayout = renamed [Replace "Writing"] $ gaps [(L,500), (R, 500)] Full

imLayout :: ModifiedLayout Rename (ModifiedLayout AddRoster Accordion) Window
imLayout = renamed [Replace "im"] $ withIM myRatio empathyRoster Accordion where
    myRatio           = 1%6
    empathyRoster   = And (ClassName "Empathy") (Role "contact_list")


base :: NewSelect
  (ModifiedLayout Rename (Mirror ThreeCol))
  (NewSelect (ModifiedLayout Rename (Mirror Tall))
    (NewSelect Full
      (NewSelect Accordion
        (NewSelect Circle
          (NewSelect SpiralWithDir
            (NewSelect
              (ModifiedLayout Rename
                (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))
                (NewSelect
                  (ModifiedLayout Rename (Mirror Accordion))
                  (NewSelect
                    (ModifiedLayout Rename (ModifiedLayout Gaps Full))
                    (NewSelect
                      (ModifiedLayout Rename DragPane)
                      (NewSelect (ModifiedLayout Rename Grid)
                      (NewSelect TwoPane OneBig))))))))))) Word64
base =     mthree ||| wide ||| Full ||| Accordion ||| Circle ||| spiral (6/7)
       ||| myTabbed ||| wideAccordion ||| writingLayout ||| rows ||| grid
       ||| (TwoPane (3/100) (1/2)) ||| (OneBig (3/4) (3/4))
   where
      mthree = renamed [Replace "ThreeWide"] $ Mirror threeLayout
      wide = renamed [Replace "Wide"] $ Mirror tiled
      wideAccordion = renamed [Replace "WideAccordion"] $ Mirror Accordion
      grid = renamed [Replace "Gridding"] $ GridRatio (1/2)
      rows = renamed [Replace "WritingNew"] $ dragPane Vertical 1 0.5



myLayout = avoidStruts $ smartBorders $
   onWorkspace "web" (myTabbed ||| Full ||| (TwoPane (3/100) (1/2))) $
   onWorkspace "terminal" (tiled ||| threeLayout ||| Full) $
   onWorkspace "IM" imLayout $
   onWorkspace "images" (gimpLayout ||| tiled ||| threeLayout ||| base) $
   tiled ||| threeLayout ||| base
 where
   gimpLayout = renamed [Replace "gimp"] $ withIM (11/64) (Role "gimp-toolbox") $ ResizableTall 2 (1/118) (11/20) [1]

myDzenConfig :: DzenConfig
myDzenConfig = (timeout 1 >=> (onCurr (vCenter 100)) >=> (onCurr (hCenter 300)) >=> XMonad.Util.Dzen.font "xft:Source Code Pro:size=20:antialias=true")

makeLayoutList :: [t] -> [(t,t)] 
makeLayoutList [] = []
makeLayoutList (l:ls) = (l,l):(makeLayoutList ls)

changeLayout :: Maybe String -> X ()
changeLayout a =
  case a of
     Just r -> sendMessage $ JumpToLayout r
     _ -> error "this shouldn't happen"

-- This is the currently used layout change
-- activated with Meta+;
-- It displays the relevant list of layouts for the current display
-- TODO: show new layout as a popup message
nchooseLayout :: GSConfig String -> X ()
nchooseLayout conf = do
   loName <- wrapped_loName
   wsName <- wrapped_wsName 

   a <- gridselect conf $ makeLayoutList $
      cycleFront loName $ case wsName of
         "web" -> webList
         "terminal" -> terminalList
         "im" -> imList
         "images" -> imagesList
         _ -> defaultList

   case a of
      Just r -> do
         sendMessage $ JumpToLayout r
         dzenConfig myDzenConfig r
   return ()
 where
   wrapped_loName :: X [Char]
   wrapped_loName =  liftM (fromMaybe "") logLayout

   wrapped_wsName :: X [Char]
   wrapped_wsName =  liftM (fromMaybe "") logCurrent

   cycleFront :: String -> [String] -> [String]
   cycleFront n l = n:(Data.List.delete n l)

   basicList = ["Accordion", "Full", "Tabbed", "Spiral", "Wide", "ThreeWide", "WideAccordion", "Writing", "WritingNew", "Gridding", "TwoPane", "OneBig"]
   defaultList = ["Tall", "ThreeCol"] ++ basicList
   webList = ["Tabbed", "Full", "TwoPane"]
   terminalList = ["Tall", "ThreeCol", "Full"]
   imList = ["im"]
   imagesList = ["gimp", "Tall", "ThreeCol"] ++ basicList



myPP :: PP
myPP = sjanssenPP {
   ppCurrent = xmobarColor "grey" "white",
   ppHidden = xmobarColor "red" "black",
   ppHiddenNoWindows = id,
   ppTitle = xmobarColor "green" "" . shorten 126
}

myManageHook  :: Query (Endo (StackSet WorkspaceId (Layout Window) Window ScreenId ScreenDetail))
myManageHook = composeAll
   [
   (role =? "gimp-toolbox" <||> role =? "gimp-image-winow") --> (ask >>= doF . W.sink)
   , className =? "X-terminal-emulator"      --> doCopy "terminal"
   , className =? "Xmessage"  --> doFloat
   , className =? "Qasmixer" --> doFloat
   , className =? "Sonata" --> doFloat
   , className =? "feh" --> viewShift "images"
   , className =? "Display.im6" --> viewShift "images"
   , manageDocks
   ]
 where role = stringProperty "WM_WINDOW_ROLE"


doCopy :: WorkspaceId -> ManageHook
doCopy i = doF . copyWin i =<< ask
copyWin i a = copyWindow a i

viewShift :: WorkspaceId -> Query (Endo (StackSet WorkspaceId l Window ScreenId sd))
viewShift = doF . liftM2 (.) W.greedyView W.shift

dShow :: String -> X ()
dShow = dzenConfig myDzenConfig

copyNSwitch windows target = do
  windows $ copy target
  windows $ W.greedyView target
  dShow target

shiftNSwitch windows target = do
  windows $ shift target
  windows $ W.greedyView target
  dShow target

switchWorkspace :: WorkspaceId -> X ()
switchWorkspace target = do
  windows $ W.greedyView target
  dShow target

maximizeSwitch :: X ()
maximizeSwitch = do
  withFocused $ sendMessage . maximizeRestore
  windows W.focusUp

maximizeFlop :: X ()
maximizeFlop = do
  windows W.focusUp
  withFocused $ sendMessage . maximizeRestore

main :: IO ()
main = do
   xmproc <- spawnPipe "/home/edwlan/.cabal/bin/xmobar /home/edwlan/.xmobarrc"
   --xmproc1 <- spawnPipe "/home/edwlan/.cabal/bin/xmobar /home/edwlan/.xmobarrc1"
   xmonad $ ewmh defaultConfig {
         manageHook = myManageHook <+> manageSpawn <+> manageHook defaultConfig,
         --handleEventHook = handleTimerEvent,
         layoutHook = maximize myLayout,
         logHook = takeTopFocus >> (dynamicLogWithPP myPP {
            ppOutput = hPutStrLn xmproc
         }),
         modMask = mod4Mask,
         focusFollowsMouse = False,
         XMonad.workspaces = ["web", "terminal", "1", "2", "3", "4", "5", "6", "images", "IM"]
      } `additionalKeys` (
      [
         (((mod4Mask .|. controlMask, xK_q     ),
               spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")),

         ((mod4Mask .|. shiftMask, xK_backslash), maximizeSwitch),
         ((mod4Mask .|. controlMask, xK_backslash), maximizeFlop),
         ((mod4Mask, xK_backslash), withFocused (sendMessage . maximizeRestore)),
         ((mod4Mask .|. controlMask .|. shiftMask, xK_h ), sendMessage $ Move L),
         ((mod4Mask .|. controlMask .|. shiftMask, xK_j ), sendMessage $ Move D),
         ((mod4Mask .|. controlMask .|. shiftMask, xK_k   ), sendMessage $ Move U),
         ((mod4Mask .|. controlMask .|. shiftMask, xK_l), sendMessage $ Move R),
         ((mod4Mask .|. controlMask, xK_k), killAllOtherCopies),
         ((mod4Mask .|. controlMask, xK_m), withWorkspace defaultXPConfig (windows . shift)),

         ((mod4Mask, numPadKeys !! 1), switchWorkspace "web" ),
         ((mod4Mask, numPadKeys !! 2), switchWorkspace "terminal" ),
         ((mod4Mask, numPadKeys !! 3), switchWorkspace "1" ),
         ((mod4Mask, numPadKeys !! 4), switchWorkspace "2" ),
         ((mod4Mask, numPadKeys !! 5), switchWorkspace "3" ),
         ((mod4Mask, numPadKeys !! 6), switchWorkspace "4" ),
         ((mod4Mask, numPadKeys !! 7), switchWorkspace "5" ),
         ((mod4Mask, numPadKeys !! 8), switchWorkspace "6" ),
         ((mod4Mask, numPadKeys !! 9), switchWorkspace "images" ),

         ((mod4Mask .|. shiftMask, numPadKeys !! 1), copyNSwitch windows "web" ),
         ((mod4Mask .|. shiftMask, numPadKeys !! 2), copyNSwitch windows "terminal" ),
         ((mod4Mask .|. shiftMask, numPadKeys !! 3), copyNSwitch windows "1" ),
         ((mod4Mask .|. shiftMask, numPadKeys !! 4), copyNSwitch windows "2" ),
         ((mod4Mask .|. shiftMask, numPadKeys !! 5), copyNSwitch windows "3" ),
         ((mod4Mask .|. shiftMask, numPadKeys !! 6), copyNSwitch windows "4" ),
         ((mod4Mask .|. shiftMask, numPadKeys !! 7), copyNSwitch windows "5" ),
         ((mod4Mask .|. shiftMask, numPadKeys !! 8), copyNSwitch windows "6" ),
         ((mod4Mask .|. shiftMask, numPadKeys !! 9), copyNSwitch windows "images" ),

         ((mod4Mask .|. controlMask, numPadKeys !! 1), shiftNSwitch windows "web" ),
         ((mod4Mask .|. controlMask, numPadKeys !! 2), shiftNSwitch windows "terminal" ),
         ((mod4Mask .|. controlMask, numPadKeys !! 3), shiftNSwitch windows "1" ),
         ((mod4Mask .|. controlMask, numPadKeys !! 4), shiftNSwitch windows "2" ),
         ((mod4Mask .|. controlMask, numPadKeys !! 5), shiftNSwitch windows "3" ),
         ((mod4Mask .|. controlMask, numPadKeys !! 6), shiftNSwitch windows "4" ),
         ((mod4Mask .|. controlMask, numPadKeys !! 7), shiftNSwitch windows "5" ),
         ((mod4Mask .|. controlMask, numPadKeys !! 8), shiftNSwitch windows "6" ),
         ((mod4Mask .|. controlMask, numPadKeys !! 9), shiftNSwitch windows "images" ),

         ((mod4Mask, xK_1), switchWorkspace "web" ),
         ((mod4Mask, xK_2), switchWorkspace "terminal" ),
         ((mod4Mask, xK_3), switchWorkspace "1" ),
         ((mod4Mask, xK_4), switchWorkspace "2" ),
         ((mod4Mask, xK_5), switchWorkspace "3" ),
         ((mod4Mask, xK_6), switchWorkspace "4" ),
         ((mod4Mask, xK_7), switchWorkspace "5" ),
         ((mod4Mask, xK_8), switchWorkspace "6" ),
         ((mod4Mask, xK_9), switchWorkspace "images" ),
         ((mod4Mask, xK_grave), switchWorkspace "IM" ),

         ((mod4Mask .|. shiftMask, xK_1), copyNSwitch windows "web" ),
         ((mod4Mask .|. shiftMask, xK_2), copyNSwitch windows "terminal" ),
         ((mod4Mask .|. shiftMask, xK_3), copyNSwitch windows "1" ),
         ((mod4Mask .|. shiftMask, xK_4), copyNSwitch windows "2" ),
         ((mod4Mask .|. shiftMask, xK_5), copyNSwitch windows "3" ),
         ((mod4Mask .|. shiftMask, xK_6), copyNSwitch windows "4" ),
         ((mod4Mask .|. shiftMask, xK_7), copyNSwitch windows "5" ),
         ((mod4Mask .|. shiftMask, xK_8), copyNSwitch windows "6" ),
         ((mod4Mask .|. shiftMask, xK_9), copyNSwitch windows "images" ),
         ((mod4Mask .|. shiftMask, xK_grave), shiftNSwitch windows "IM" ),

         ((mod4Mask .|. controlMask, xK_1), shiftNSwitch windows "web" ),
         ((mod4Mask .|. controlMask, xK_2), shiftNSwitch windows "terminal" ),
         ((mod4Mask .|. controlMask, xK_3), shiftNSwitch windows "1" ),
         ((mod4Mask .|. controlMask, xK_4), shiftNSwitch windows "2" ),
         ((mod4Mask .|. controlMask, xK_5), shiftNSwitch windows "3" ),
         ((mod4Mask .|. controlMask, xK_6), shiftNSwitch windows "4" ),
         ((mod4Mask .|. controlMask, xK_7), shiftNSwitch windows "5" ),
         ((mod4Mask .|. controlMask, xK_8), shiftNSwitch windows "6" ),
         ((mod4Mask .|. controlMask, xK_9), shiftNSwitch windows "images" ),
         ((mod4Mask .|. controlMask, xK_grave), shiftNSwitch windows "IM" ),

         ((mod4Mask .|. shiftMask, xK_BackSpace), removeWorkspace),
         ((mod4Mask .|. shiftMask, xK_k), kill1),
         ((mod4Mask .|. shiftMask, xK_m), withWorkspace defaultXPConfig (windows . copy)),
         ((mod4Mask .|. shiftMask, xK_n), addWorkspacePrompt defaultXPConfig),
         ((mod4Mask .|. shiftMask, xK_Return), spawnHere "/usr/bin/x-terminal-emulator"),
         ((mod4Mask .|. shiftMask, xK_r), renameWorkspace defaultXPConfig),
         ((mod4Mask .|. shiftMask, xK_w), gridselectWorkspace defaultGSConfig (\ws -> greedyView ws . shift ws)),
         ((mod4Mask, xK_b), sendMessage ToggleStruts),
         ((mod4Mask, xK_g), goToSelected defaultGSConfig),
         ((mod4Mask, xK_KP_Subtract), spawn "/usr/bin/zsh /home/edwlan/bin/dmenu_queueplay_mpd"),
         ((mod4Mask, xK_KP_Multiply), spawn "/usr/bin/zsh /home/edwlan/bin/dmenu_queue_mpd"),
         ((mod4Mask, xK_KP_Divide), spawn "/usr/bin/zsh /home/edwlan/bin/dmenu_play_mpd"),
         ((mod4Mask, xK_KP_Add), spawn "/usr/bin/zsh /home/edwlan/bin/dzen_mpc_status"),
         ((mod4Mask, xK_p), spawnHere "/usr/bin/dmenu_run -f"),

         ((mod4Mask, xK_q), ((withSelectedWindow $ windows . W.focusWindow) defaultGSConfig) >> (windows $ W.shiftMaster)),
         ((mod4Mask, xK_semicolon), nchooseLayout defaultGSConfig),
         ((mod4Mask, xK_w), gridselectWorkspace defaultGSConfig (\ws -> greedyView ws))
      ] ++ zip (zip (repeat (mod4Mask)) ([xK_0])) (map (withNthWorkspace greedyView) [0..]) ++
         zip (zip (repeat (mod4Mask .|. shiftMask)) ([xK_0])) (map (withNthWorkspace copy) [0..]) 
         ++ zip (zip (repeat (mod4Mask)) (map (numPadKeys !!) ([0]))) (map (withNthWorkspace greedyView) [0..])
      )

numPadKeys = [xK_KP_Insert -- 0
             , xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             ]
