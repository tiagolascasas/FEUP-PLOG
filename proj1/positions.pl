:- dynamic pos/3.
:- dynamic waiterPos/2.

pos(nw, nw, o). pos(nw, n, o). pos(nw, ne, o).   	pos( n, nw, o). pos( n, n, o). pos( n, ne, o).   	pos(ne, nw, o). pos(ne, n, o). pos(ne, ne, o).
pos(nw,  w, o). pos(nw, c, o). pos(nw, e,  o).   	pos( n,  w, o). pos( n, c, o). pos( n,  e, o).   	pos(ne,  w, o). pos(ne, c, o). pos(ne,  e, o).
pos(nw, sw, o). pos(nw, s, o). pos(nw, se, o).   	pos( n, sw, o). pos( n, s, o). pos( n, se, o).   	pos(ne, sw, o). pos(ne, s, o). pos(ne, se, o).

pos( w, nw, o). pos( w, n, o). pos( w, ne, o).   	pos( c, nw, o). pos( c, n, o). pos( c, ne, o).   	pos( e, nw, o). pos( e, n, o). pos( e, ne, o).
pos( w,  w, o). pos( w, c, o). pos( w, e,  o).   	pos( c,  w, o). pos( c, c, o). pos( c,  e, o).   	pos( e, w, o).  pos( e, c, o). pos( e,  e, o).
pos( w, sw, o). pos( w, s, o). pos( w, se, o).   	pos( c, sw, o). pos( c, s, o). pos( c, se, o).   	pos( e, sw, o). pos( e, s, o). pos( e, se, o).

pos(sw, nw, o). pos(sw, n, o). pos(sw, ne, o).  	pos( s, nw, o). pos( s, n, o). pos( s, ne, o).   	pos(se, nw, o). pos(se, n, o). pos(se, ne, o).
pos(sw,  w, o). pos(sw, c, o). pos(sw, e,  o).   	pos( s,  w, o). pos( s, c, o). pos( s,  e, o).   	pos(se, w, o).  pos(se, c, o). pos(se,  e, o).
pos(sw, sw, o). pos(sw, s, o). pos(sw, se, o).   	pos( s, sw, o). pos( s, s, o). pos( s, se, o).   	pos(se, sw, o). pos(se, s, o). pos(se, se, o).

waiterPos(c, c).
pos(null, null, b).
pos(null, null, g).
