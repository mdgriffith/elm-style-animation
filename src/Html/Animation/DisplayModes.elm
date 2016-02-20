module Html.Animation.DisplayModes (DisplayMode (..), DisplayType (..)) where



{-| A type that holds the previous display value and the target display value.
Don't use this directly, use the functions 'none', 'inline', etc, instead.

-}
type DisplayMode a =
      DisplayMode a DisplayType DisplayType

{-| Units representing display modes.

-}
type DisplayType
  = None
  | Inline
  | InlineBlock
  | Block


