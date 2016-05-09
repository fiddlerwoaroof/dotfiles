{-# LANGUAGE RankNTypes #-}

import Control.Monad

import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ratio ((%))
import Data.Word

import System.IO

import XMonad hiding ( (|||) )
import XMonad.Actions.CopyWindow(copy,copyWindow,kill1,killAllOtherCopies)
import XMonad.Actions.CycleWindows()
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.GridSelect
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SpawnOn
import XMonad.Core()
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Combo()
import XMonad.Layout.Decoration
import XMonad.Layout.DragPane
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier()
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.OneBig
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect()
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Simplest
import XMonad.Layout.Spiral
import XMonad.Layout.SubLayouts()
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.StackSet as W
import XMonad.Util.Dzen
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad

nmaster :: Int
nmaster = 1

ratio = 13/21

delta = 3/100

tiled :: Tall a
tiled = Tall nmaster delta ratio

myTabbed :: ModifiedLayout Rename (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest) Word64
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
       (NewSelect (ModifiedLayout Rename (ModifiedLayout (Decoration TabbedDecoration DefaultShrinker) Simplest))
        (NewSelect (ModifiedLayout Rename (Mirror Accordion))
         (NewSelect (ModifiedLayout Rename (ModifiedLayout Gaps Full))
          (NewSelect (ModifiedLayout Rename DragPane)
           (NewSelect (ModifiedLayout Rename Grid)
            (NewSelect TwoPane OneBig))))))))))) Word64
base = mthree ||| wide ||| Full ||| Accordion ||| Circle ||| spiral (6/7)
   ||| myTabbed ||| wideAccordion ||| writingLayout ||| rows ||| grid
   ||| TwoPane (3/100) (1/2) ||| OneBig (3/4) (3/4)
  where
    mthree = renamed [Replace "ThreeWide"] $ Mirror threeLayout
    wide = renamed [Replace "Wide"] $ Mirror tiled
    wideAccordion = renamed [Replace "WideAccordion"] $ Mirror Accordion
    grid = renamed [Replace "Gridding"] $ GridRatio (1/2)
    rows = renamed [Replace "WritingNew"] $ dragPane Vertical 1 0.5


{-
 -
 -}

tmp = onWorkspace "web" (myTabbed ||| Full ||| TwoPane (3/100) (1/2)) $
      onWorkspace "terminal" (tiled ||| threeLayout ||| Full) $
      onWorkspace "IM" imLayout $
      --onWorkspace "images" (gimpLayout ||| tiled ||| threeLayout ||| base) $
       tiled ||| threeLayout ||| base
 --where
 --  gimpLayout = renamed [Replace "gimp"] $ withIM (11/64) (Role "gimp-toolbox") $ ResizableTall 2 (1/118) (11/20) [1]

myLayout = avoidStruts $ smartBorders tmp

myDzenConfig :: DzenConfig
myDzenConfig = timeout 1 >=> onCurr (vCenter 100)
                         >=> onCurr (hCenter 300)
                         >=> XMonad.Util.Dzen.font "xft:Source Code Pro:size=20:antialias=true"

makeLayoutList :: [t] -> [(t,t)]
makeLayoutList = map (\ l -> (l, l))

--changeLayout :: Maybe String -> X ()
--changeLayout a =
--  case a of
--     Just r -> sendMessage $ JumpToLayout r
--     _ -> error "this shouldn't happen"

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
   wrapped_loName :: X String
   wrapped_loName =  liftM (fromMaybe "") logLayout

   wrapped_wsName :: X String 
   wrapped_wsName =  liftM (fromMaybe "") logCurrent

   cycleFront :: String -> [String] -> [String]
   cycleFront n l = n: Data.List.delete n l

   basicList = ["Accordion", "Full", "Tabbed", "Spiral", "Wide", "ThreeWide", "WideAccordion", "Writing", "WritingNew", "Gridding", "TwoPane", "OneBig"]
   defaultList = ["Tall", "ThreeCol"] ++ basicList
   webList = ["Tabbed", "Full", "TwoPane"]
   terminalList = ["Tall", "ThreeCol", "Full"]
   imList = ["im"]
   imagesList = ["gimp", "Tall", "ThreeCol"] ++ basicList



myPP :: PP
myPP = namedScratchpadFilterOutWorkspacePP $ sjanssenPP {
   ppCurrent = xmobarColor "grey" "white",
   ppHidden = xmobarColor "red" "black", -- . noScratchPad,
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
   ] <+> manageScratchPad
 where role = stringProperty "WM_WINDOW_ROLE"

manageScratchPad :: ManageHook
manageScratchPad = namedScratchpadManageHook scratchpads

scratchpads :: [NamedScratchpad]
scratchpads = [
  -- run htop in xterm, find it by title, use default floating window placement
  NS "glances" "xterm -e glances" (title =? "glances") floatStyle ,
  NS "mutt" "xterm -e mutt" (title =? "mutt") floatStyle ,
  NS "mcabber" "xterm -e mcabber" (title =? "mcabber") floatStyle ,
  NS "mpc" "stterm -c mpc -e ncmpcpp" (className =? "ncmpcpp") floatStyle ,

  -- run gvim, find by role
  NS "notes" "bash -c 'cd $HOME/mywiki; source /usr/local/share/chruby/chruby.sh; chruby 2.2.2; SOYWIKI_VIM=\"gvim --role notes\" /home/edwlan/.gem/ruby/2.2.2/bin/soywiki'" (role =? "notes") floatStyle
  ] where cls = stringProperty "WM_WINDOW_CLASS"
          role = stringProperty "WM_WINDOW_ROLE"
          floatStyle = customFloating $ W.RationalRect l t w h
          w = 1     -- terminal height, 10%
          h = 0.3333       -- terminal width, 100%
          t = 0.015  -- distance from top edge, 90%
          l = 0  -- distance from left edge, 0%

doCopy :: WorkspaceId -> ManageHook
doCopy i = doF . copyWin i =<< ask

copyWin :: forall a i l s sd . (Eq a, Eq i, Eq s) => i -> a -> StackSet i l a s sd -> StackSet i l a s sd
copyWin i a = copyWindow a i

viewShift :: WorkspaceId -> Query (Endo (StackSet WorkspaceId l Window ScreenId sd))
viewShift = doF . liftM2 (.) W.greedyView W.shift

dShow :: String -> X ()
dShow = dzenConfig myDzenConfig

copyNSwitch :: forall l a s sd a1. (Eq s, Eq a) => ((StackSet String l a s sd -> StackSet String l a s sd) -> X a1)
                 -> String -> X ()
copyNSwitch ws target = do
  ws $ copy target
  ws $ W.view target
  dShow target

shiftNSwitch :: forall l a s sd a1. (Eq s, Ord a) => ((StackSet String l a s sd -> StackSet String l a s sd) -> X a1)
                  -> String -> X ()
shiftNSwitch ws target = do
  ws $ shift target
  ws $ W.view target
  dShow target

switchWorkspace :: forall l a s sd a1. (Eq s, Ord a) => ((StackSet String l a s sd -> StackSet String l a s sd) -> X a1)
                  -> String -> X ()
switchWorkspace ws target = do
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

makeSwitchComb mod keyList num action = ((getMod mod, keyList !! num), action)
  where
    getMod Nothing = mod4Mask
    getMod (Just mod) = mod4Mask .|. mod

workspaceNames = ["web", "2", "3", "4", "5", "6", "7", "terminal", "images"]

numKeys :: [KeySym]
numKeys = [xK_0, xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7, xK_8, xK_9]

numPadKeys :: [KeySym]
numPadKeys = [xK_KP_Insert -- 0
             , xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             ]

combForNum keyList action mod n = makeSwitchComb mod keyList n $ action windows $ workspaceNames !! (n-1)

multiMap funs seq = concatMap (`map` seq) funs

workspaceKeys =
    bindWorkspaces [
      bindNum copyNSwitch shiftMask,
      bindNum shiftNSwitch controlMask,

      bindNumPad copyNSwitch shiftMask,
      bindNumPad shiftNSwitch controlMask,

      combForNum numKeys switchWorkspace Nothing,
      combForNum numPadKeys switchWorkspace Nothing
      ] ++ [
      ((mod4Mask, xK_0), withNthWorkspace greedyView 0),
      ((mod4Mask .|. shiftMask, xK_0), withNthWorkspace copy 0),
      ((mod4Mask, head numPadKeys), withNthWorkspace greedyView 0) 
      ]
    where
      bindNum action = combForNum numKeys action . Just
      bindNumPad action = combForNum numPadKeys action . Just
      bindWorkspaces = flip multiMap [1..9]

main :: IO ()
main = do
   xmproc <- spawnPipe "/home/edwlan/.local/bin/xmobar /home/edwlan/.xmobarrc"
   --xmproc1 <- spawnPipe "/home/edwlan/.cabal/bin/xmobar /home/edwlan/.xmobarrc1"
   xmonad $ ewmh defaultConfig {
         manageHook = myManageHook <+> manageSpawn <+> manageHook defaultConfig,
         --handleEventHook = handleTimerEvent,
         layoutHook = maximize myLayout,
         logHook = takeTopFocus >> dynamicLogWithPP myPP {
           ppOutput = hPutStrLn xmproc
         },
         modMask = mod4Mask,
         focusFollowsMouse = False,
         clickJustFocuses = False,
         XMonad.workspaces = ["web", "2", "3", "4", "5", "6", "7", "terminal", "images", "IM"]
      } `additionalKeys` (workspaceKeys ++
      [
         ((mod4Mask .|. controlMask .|. shiftMask, xK_h ), sendMessage $ Move L),
         ((mod4Mask .|. controlMask .|. shiftMask, xK_j ), sendMessage $ Move D),
         ((mod4Mask .|. controlMask .|. shiftMask, xK_k   ), sendMessage $ Move U),
         ((mod4Mask .|. controlMask .|. shiftMask, xK_l), sendMessage $ Move R),
         ((mod4Mask .|. controlMask, xK_backslash), maximizeFlop),
         ((mod4Mask .|. controlMask, xK_grave), shiftNSwitch windows "IM" ),
         ((mod4Mask .|. controlMask, xK_m), withWorkspace defaultXPConfig (windows . shift)),
         ((mod4Mask .|. controlMask, xK_q     ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"),
         ((mod4Mask .|. controlMask, xK_t), killAllOtherCopies),
         ((mod4Mask .|. shiftMask, xK_backslash), maximizeSwitch),
         ((mod4Mask .|. shiftMask, xK_BackSpace), removeWorkspace),
         ((mod4Mask .|. shiftMask, xK_grave), shiftNSwitch windows "IM" ),
         ((mod4Mask .|. shiftMask, xK_m), withWorkspace defaultXPConfig (windows . copy)),
         ((mod4Mask .|. shiftMask, xK_n), addWorkspacePrompt defaultXPConfig),
         ((mod4Mask .|. shiftMask, xK_Return), spawnHere "/usr/bin/x-terminal-emulator"),
         ((mod4Mask .|. shiftMask, xK_r), renameWorkspace defaultXPConfig),
         ((mod4Mask .|. shiftMask, xK_t), kill1),
         ((mod4Mask .|. shiftMask, xK_w), gridselectWorkspace defaultGSConfig (\ws -> greedyView ws . shift ws)),
         --((mod4Mask, xK_apostrophe), scratchpadSpawnActionTerminal "gvim"),
         ((mod4Mask, xK_backslash), withFocused (sendMessage . maximizeRestore)),
         ((mod4Mask, xK_b), sendMessage ToggleStruts),
         ((mod4Mask, xK_g), goToSelected defaultGSConfig),
         --((mod4Mask, xK_grave), scratchpadSpawnActionTerminal "urxvt"),
         {-((mod4Mask, xK_grave), switchWorkspace "IM" ),-}
         ((mod4Mask, xK_KP_Add), spawn "/usr/bin/zsh /home/edwlan/bin/dzen_mpc_status"),
         ((mod4Mask, xK_KP_Divide), spawn "/usr/bin/zsh /home/edwlan/bin/dmenu_play_mpd"),
         ((mod4Mask, xK_KP_Multiply), spawn "/usr/bin/zsh /home/edwlan/bin/dmenu_queue_mpd"),
         ((mod4Mask, xK_KP_Subtract), spawn "/usr/bin/zsh /home/edwlan/bin/dmenu_queueplay_mpd"),
         ((mod4Mask, xK_p), spawnHere "/home/edwlan/bin/yeganesh_run -f"),
         ((mod4Mask, xK_q), (withSelectedWindow $ windows . W.focusWindow) defaultGSConfig >> windows W.shiftMaster),
         ((mod4Mask, xK_semicolon), nchooseLayout defaultGSConfig),
         ((mod4Mask, xK_w), gridselectWorkspace defaultGSConfig greedyView),

         ((mod4Mask, xK_Left), onPrevNeighbour W.view), 
         ((mod4Mask, xK_Right), onNextNeighbour W.view)

      ]
      ++ [
         ((mod4Mask .|. mod1Mask, xK_1), namedScratchpadAction scratchpads "notes"),
         ((mod4Mask .|. mod1Mask, numPadKeys !! 1), namedScratchpadAction scratchpads "notes"),
         ((mod4Mask .|. mod1Mask, xK_2), namedScratchpadAction scratchpads "mutt"),
         ((mod4Mask .|. mod1Mask, numPadKeys !! 2), namedScratchpadAction scratchpads "mutt"),
         --((mod4Mask .|. mod1Mask, xK_3), namedScratchpadAction scratchpads "irc"),
         --((mod4Mask .|. mod1Mask, numPadKeys !! 3), namedScratchpadAction scratchpads "irc"),
         ((mod4Mask .|. mod1Mask, xK_4), namedScratchpadAction scratchpads "htop"),
         ((mod4Mask .|. mod1Mask, numPadKeys !! 4), namedScratchpadAction scratchpads "htop"),
         ((mod4Mask .|. mod1Mask, xK_5), namedScratchpadAction scratchpads "mpc"),
         ((mod4Mask .|. mod1Mask, numPadKeys !! 5), namedScratchpadAction scratchpads "mpc"),
         ((mod4Mask .|. mod1Mask, xK_6), namedScratchpadAction scratchpads "mcabber"),
         ((mod4Mask .|. mod1Mask, numPadKeys !! 6), namedScratchpadAction scratchpads "mcabber")
      ])

