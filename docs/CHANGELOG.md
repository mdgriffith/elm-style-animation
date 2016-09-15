


# Version 3.0.0
 * Broken up into two modules `Html.Animation` and `Html.Animation.Properties`.
 * True physics core added.
 * Transitions between using springs and easing will engage this physics core and make things look natural.
 * renamed original `forwardTo` to `forwardToIndex`
 * `forwardTo` is now used to create a helper function that prepares effects and style updates for a single record.  It is preferred over using `on`.
 * `set` is used to set a style immediately.  Useful for setting `display:none` at the end of a fadeOut.
 * Improved interruption logic.


# Version 2.0.1
 * Support for staggering animations.
 * Support for springs.
 * `forwardToAll` to animate every element in a list.
 * Support for stacking transforms.
 * Support for color transitions


# Version 1.0
 * Initial Release.  Support for easing, delay, duration, keyframes.