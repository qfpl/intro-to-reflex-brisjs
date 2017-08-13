Strip out all of the typeclass constraints

events.md
- the frames / transactions point can be pushed a bit harder

behaviors.md
- mention the CQRS link

dynamic.md
  important to really stress that dyanmic used well can give you virtual dom equivalent behavior without having to diff or patch

dom.md

switching.md

  Possibly put some of this before the dom and some of it after

  Need to really trim down the switching
    basic example of switching, like a railway switchyard
    show widgetHold for switching out pieces of the DOM
    - with text and a button
    - with text and a timer

components.md

  Spend a bit of time on API / component design
    - dynamics in and events out as a default
    - some exceptions where you want to close the loop to contain information
  cover component design for things we'll use in the next bit

collections.md

  Doing more with the collection management would be good, probably with less details
    - manage the todo item list with the whole model exposed
    - pare it back to just what needs to be exposed
    - show that there are tools in place to make that even simpler


