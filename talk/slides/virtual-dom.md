
# Virtual DOM

##

The DOM is a mutable tree with mutable state at the nodes and at the leaves

##

Working with the DOM directly can be slow

##

The virtual DOM is a data type that models the DOM, allow with efficient diff and patch operations

##

That helps with the mutable state


##

Several frameworks assume the DOM is stateless

##

This is not true, and causes problems

##

You have to capture the state and loop it back through your model

##

React.js can avoid this, with local state captured in components

##

Either way, you end up with a mutable tree of with mutable state at the nodes and at the leaves

##

It's just a different tree
