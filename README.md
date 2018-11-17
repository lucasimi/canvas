# Canvas

Canvas is a graphic server able to perform simple drawing operations. It can draw only three primitives: a point, a line and a triangle. The purpose of this project is to have a ready drawing tool to couple other software for drawing operations without much boilerplate. It runs a socket server on `localhost:8080` which accepts the following instructions

1. draw a point: `P x y r g b`
2. draw a line: `L x0 y0 r0 g0 b0 x1 y1 r1 g1 b1`
3. draw a triangle: `T x0 y0 r0 g0 b0 x1 y1 r1 g1 b1 x2 y2 r2 g2 b2`
4. clear the window: `clear`

All the parameters are floats. Positions (`x*`, `y*`) range from `-1.0` to `1.0` and colors (`r`, `g`, `b`) range from `0.0` to `1.0`. All drawing operations are displayed on 512x512 window, and the colors are linearly interpolated.


# Example
Open a telnet session with `telnet 0.0.0.0 8080` and send the following commands
```
T 0.0 0.0 1.0 0.0 0.0 0.0 1.0 0.0 1.0 0.0 1.0 0.0 0.0 0.0 1.0
L 0.0 0.0 1.0 0.0 0.0 -1.0 -1.0 0.0 0.0 1.0
```
The result is displayed as 
![Canvas](https://github.com/lucasimi/canvas/blob/master/canvas.png)


## Todo

1. Add options (width, height, port, ...)
2. Add the possibility to change backend (X, file, html5 canvas, ...)
