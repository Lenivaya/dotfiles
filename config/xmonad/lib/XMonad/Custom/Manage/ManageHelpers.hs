module XMonad.Custom.Manage.ManageHelpers where

import XMonad hiding (manageHook)
import XMonad.Hooks.ManageHelpers
import XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

isRole :: Query String
isRole = stringProperty "WM_WINDOW_ROLE"

matchAny :: String -> Query Bool
matchAny s =
  appName
    =? s
    <||> className
    =? s
    <||> resource
    =? s
    <||> title
    =? s
    <||> stringProperty "WM_WINDOW_ROLE"
    =? s
    <||> stringProperty "_NET_WM_WINDOW_ROLE"
    =? s
    <||> stringProperty "WM_WINDOW_TYPE"
    =? s
    <||> stringProperty "_NET_WM_WINDOW_TYPE"
    =? s
    <||> stringProperty "_OL_DECOR_DEL"
    =? s
    <||> stringProperty "WM_ICON_NAME"
    =? s
    <||> stringProperty "_NET_WM_ICON_NAME"
    =? s

centerFloat :: Rational -> Rational -> ManageHook
centerFloat w h = doRectFloat $ W.RationalRect x y w h
  where
    x = (1 - w) / 2
    y = (1 - h) / 2
