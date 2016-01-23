
# elm-html-animation

A library to simplify creating html animations in elm.  

Try it out!

First have [Elm installed](http://elm-lang.org/install), then

```bash
$ git clone https://github.com/mdgriffith/elm-html-animation.git
$ cd elm-html-animation/examples
$ elm-reactor
```

My focus was to create something I could use as a UI designer to prototype animations quickly, accurately, and without sneaky errors.


ElmHtmlAnimations supports __Declarative animations__, which tend to be very intuitive. State what the style should be, and it will animate to that style. 

 * __Animations can be smoothly interrupted.  Or chained together.__  

 * __Relative animation__ such as using `+=` and `-=` to animate a property based on its previous value is also available.  Though, be aware that mixing this and animation interruption can lead to unanticipated results.

 * __CSS properties and units are type checked.__  No more elusive property typos, it's all caught at compile-time!  Didn't know that the skew transform needs to have angle units?  No problemo, the compiler will let you know.  Didn't know that rgb colors _must_ be integers and not floats?  No worries, you don't need to know that.

 * __Use custom easing__ such as all the lovely functions in [this library](http://package.elm-lang.org/packages/Dandandan/Easing/2.0.1/Easing#easing-functions).

 * __Manage multiple animations__ by using the `forwardTo` function.  See the _Showcase_ example to see how it works.

 * __Infrastructure for more complicated animations__ is provided.  Want to do something like animate a position along a path?  You have the tools to do that.  (If you're curious, you'd start by writing a custom `to` function.)


## The Basics

To start off, you should be familiar with [the Elm Architecture](https://github.com/evancz/elm-architecture-tutorial/)


# Example 1: Showing a Menu on Hover
[demo](https://mdgriffith.github.io/elm-html-animation/examples/SideMenu.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/SideMenu.elm)

# Example 2: Chaining Animations
[demo](https://mdgriffith.github.io/elm-html-animation/examples/Chaining.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Chaining.elm)

# Example 3: Managing Multiple Animations
[demo](https://mdgriffith.github.io/elm-html-animation/examples/Showcase.html) / [view code](https://github.com/mdgriffith/elm-html-animation/blob/master/examples/Showcase.elm)




