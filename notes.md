Strip out all of the typeclass constraints

events.md
- the frames / transactions point can be pushed a bit harder
behaviors.md
- mention the CQRS link

dom.md

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
    
  we want
    wrap up the create / alter / delete stuff in an abstraction 
  
    TodoItemModel Bool Text
    components return (Event (TodoItemModel -> TodoItemModel), Event ()))
    that gets used to build up a model of that is going on
    we have a dynamic map of our state
    
    we can remove almost all of that
    text edit minds it's own business
    return the dynamic for completed



