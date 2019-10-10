module XMonad.Actions.FloatSnapSpaced
    ( snapSpacedMagicMove
    ) where

import           Data.List
import           Data.Maybe
import           Data.Set                 (fromList)
import           XMonad
import           XMonad.Hooks.ManageDocks
import qualified XMonad.StackSet          as S

snapSpacedMagicMove :: Int -> Maybe Int -> Maybe Int -> Window -> X ()
snapSpacedMagicMove spacing collidedist snapdist w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w

    nx <- handleAxis True  d wa
    ny <- handleAxis False d wa

    io $ moveWindow d w (fromIntegral nx) (fromIntegral ny)
    float w
    where
        handleAxis horiz d wa = do
            ((mbl, mbr, bs), (mfl, mfr, fs)) <- getSnap horiz collidedist d w
            return $ if bs || fs
                     then wpos wa
                     else let b = case (mbl, mbr) of
                                       (Just bl, Just br) -> if wpos wa - bl           < br - wpos wa
                                                                then bl + spacing
                                                                else br + spacing
                                       (Just bl, Nothing) -> bl         + spacing
                                       (Nothing, Just br) -> br         + spacing
                                       (Nothing, Nothing) -> wpos wa    + spacing

                              f = case (mfl, mfr) of
                                       (Just fl, Just fr) -> if wpos wa + wdim wa - fl < fr - wpos wa - wdim wa
                                                                then fl - spacing
                                                                else fr - spacing
                                       (Just fl, Nothing) -> fl         - spacing
                                       (Nothing, Just fr) -> fr         - spacing
                                       (Nothing, Nothing) -> wpos wa    - spacing

                              newpos = if abs (b - wpos wa) <= abs (f - wpos wa - wdim wa)
                                          then b
                                          else f - wdim wa

                              in if isNothing snapdist || abs (newpos - wpos wa) <= fromJust snapdist
                                    then newpos
                                    else wpos wa
            where
                 (wpos, wdim, _, _) = constructors horiz

getSnap :: Bool -> Maybe Int -> Display -> Window -> X ((Maybe Int, Maybe Int, Bool), (Maybe Int, Maybe Int, Bool))
getSnap horiz collidedist d w = do
    wa <- io $ getWindowAttributes d w
    screen <- S.current <$> gets windowset
    let sr = screenRect $ S.screenDetail screen
        wl = S.integrate' . S.stack $ S.workspace screen
    gr <- fmap ($ sr) $ calcGap $ fromList [minBound .. maxBound]
    wla <- filter (collides wa) <$> io (mapM (getWindowAttributes d) $ filter (/= w) wl)

    return ( neighbours (back  wa sr gr wla) (wpos wa)
           , neighbours (front wa sr gr wla) (wpos wa + wdim wa)
           )
    where
        wborder = fromIntegral.wa_border_width

        (wpos, wdim, rpos, rdim) = constructors horiz
        (refwpos, refwdim, _, _) = constructors $ not horiz

        back  wa sr gr wla = dropWhile (< rpos sr)
                           $ takeWhile (< rpos sr + rdim sr)
                           $ sort
                           $ rpos sr :
                             rpos gr :
                             (rpos gr + rdim gr) :
                             foldr (\a as -> wpos a : (wpos a + wdim a + wborder a + wborder wa) : as)   [] wla

        front wa sr gr wla = dropWhile (<= rpos sr)
                           $ takeWhile (<= rpos sr + rdim sr)
                           $ sort
                           $ (rpos gr - 2 * wborder wa) :
                             (rpos gr + rdim gr - 2 * wborder wa) :
                             (rpos sr + rdim sr - 2 * wborder wa) :
                             foldr (\a as -> (wpos a - wborder a - wborder wa) : (wpos a + wdim a) : as) [] wla

        neighbours l v = ( listToMaybe $ reverse $ takeWhile (< v) l
                         , listToMaybe $ dropWhile (<= v) l
                         , v `elem` l
                         )

        collides wa oa = case collidedist of
                              Nothing   -> True
                              Just dist -> refwpos oa - wborder oa        < refwpos wa + refwdim wa + wborder wa + dist
                                        && refwpos wa - wborder wa - dist < refwpos oa + refwdim oa + wborder oa


constructors :: Bool -> (WindowAttributes -> Int, WindowAttributes -> Int, Rectangle -> Int, Rectangle -> Int)
constructors True  = ( fromIntegral.wa_x
                     , fromIntegral.wa_width
                     , fromIntegral.rect_x
                     , fromIntegral.rect_width
                     )
constructors False = ( fromIntegral.wa_y
                     , fromIntegral.wa_height
                     , fromIntegral.rect_y
                     , fromIntegral.rect_height
                     )
