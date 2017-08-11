
Probably want a "Why is this in Haskell?" section

Probably want an introduction to Haskell syntax and idioms
  particularly
    typeclasses
    functor and applicative
  although these could be quite handwavy

Probably want more work on the FRP introduction

The frames / transactions point can be pushed a bit harder
The correspondence with ES for events and CQRS for behaviors might play well

Can probably compress some of the bits around behaviors and dynamics and recursive do
  really need to slow down through the idea of events that fire with function values
  important to really stress that dyanmic used well can give you virtual dom equivalent behavior without having to diff or patch

Need to really trim down the switching
  maybe flick between a text field and a timer, sample them from the outside

Spend a bit of time on API / component design
  - dynamics in and events out as a default
  - some exceptions where you want to close the loop to contain information

Doing more with the collection management would be good, probably with less details
  - manage the todo item list with the whole model exposed
  - pare it back to just what needs to be exposed
  - show that there are tools in place to make that even simpler

Strip out all of the typeclass constraints

Code - DONE:
  for events:
    a timer ticking and firing red while a button press fires blue

  for recursive do:
    have the limit = 1 + number of presses of clear behind the scenes
    or do the limit on one page as a counter example, and the limited count on the next page

  for switching:
    switching between a text and a button
    - via hiding
    - via rebuilding
    switching between a text and a timer
    - via hiding
    - via rebuilding

Code - TODO:

  for collections:
    skip the filter, go straight to the bidirectional mark complete and to clear complete 

    complete : dComplete -> eComplete
    edit : dText -> eText
    button: () -> eRemove
    
    add :: () -> eText

    markAllComplete : dAllComplete -> eMarkAllComplete 
    clearComplete : dAnyComplete -> eClearComplete
 
    complete : (iComplete, eMarkAllComplete eClearComplete -> (dComplete eRemove)
    edit : dText -> (eText, eRemove)
    button: () -> eRemove

    show the item, play with the item in a test bed
    show how it gets used in a collection, that we have the whole model if we want it off to one side
    argue that if we don't need the model, we're doing more state management than we need / are leaking information

    have the todo item trigger a remove on empty text
    remove the text handling from the todo list

    further change: use EventWriterT to make managing removes easier

    talk about clear complete and mark complete
    show the components

    show how that works with the modelling of completeness

    show a version of the todo item takes eClearComplete and eMarkComplete as inputs and return dComplete as an output
    show the changes to the todo list

