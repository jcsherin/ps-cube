## Getting Started

    $ npm i -g purescript pulp bower
    $ npm i
    $ bower i

    $ npm run start
    $ open http://0.0.0.0:8080

## Design

### Coordinate System

The 3D vertices are mapped to 2D using a simple orthographic projection.
Their origin is pegged to the center of the cube, and the co-ordinate
system is left-handed. +x faces right, +y faces up and +z faces forward.

We map this co-ordinate system to the one used by HTML's `canvas` (with a `y * -1`) 
everytime we render the cube, and also when interpreting mouse events that originate
from canvas.

### Dragging

The intution for translating the mouse drag vectors between lock
and release, is to separate out the change in x from the change in
y. The change in x is the rotation around the y-axis and the change
in y is the rotation around the x-axis.

### State

Mutable refs are used for state management. Mutation is localized
into the the `updateState` combinator, keeping model functions
pure of the form `State -> State`.
