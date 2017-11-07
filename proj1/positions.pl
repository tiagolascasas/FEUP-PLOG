% Initial positions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic pos/3.
:- dynamic waiterPos/2.

%pos(nw, nw, o), pos(nw, n, o), pos(nw, ne, o),   	pos( n, nw, o), pos( n, n, o), pos( n, ne, o),   	pos(ne, nw, o), pos(ne, n, o), pos(ne, ne, o),
%pos(nw,  w, o), pos(nw, c, o), pos(nw, e,  o),   	pos( n,  w, o), pos( n, c, o), pos( n,  e, o),   	pos(ne,  w, o), pos(ne, c, o), pos(ne,  e, o),
%pos(nw, sw, o), pos(nw, s, o), pos(nw, se, o),   	pos( n, sw, o), pos( n, s, o), pos( n, se, o),   	pos(ne, sw, o), pos(ne, s, o), pos(ne, se, o),

%pos( w, nw, o), pos( w, n, o), pos( w, ne, o),   	pos( c, nw, o), pos( c, n, o), pos( c, ne, o),   	pos( e, nw, o), pos( e, n, o), pos( e, ne, o),
%pos( w,  w, o), pos( w, c, o), pos( w, e,  o),   	pos( c,  w, o), pos( c, c, o), pos( c,  e, o),   	pos( e, w, o),  pos( e, c, o), pos( e,  e, o),
%pos( w, sw, o), pos( w, s, o), pos( w, se, o),   	pos( c, sw, o), pos( c, s, o), pos( c, se, o),   	pos( e, sw, o), pos( e, s, o), pos( e, se, o),

%pos(sw, nw, o), pos(sw, n, o), pos(sw, ne, o),  	pos( s, nw, o), pos( s, n, o), pos( s, ne, o),   	pos(se, nw, o), pos(se, n, o), pos(se, ne, o),
%pos(sw,  w, o), pos(sw, c, o), pos(sw, e,  o),   	pos( s,  w, o), pos( s, c, o), pos( s,  e, o),   	pos(se, w, o),  pos(se, c, o), pos(se,  e, o),
%pos(sw, sw, o), pos(sw, s, o), pos(sw, se, o),   	pos( s, sw, o), pos( s, s, o), pos( s, se, o),   	pos(se, sw, o), pos(se, s, o), pos(se, se, o),

%waiterPos(c, c),
%pos(null, null, b),
%pos(null, null, g),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

initPositions :-
assert(pos(nw, nw, o)), assert(pos(nw, n, o)), assert(pos(nw, ne, o)),   	assert(pos( n, nw, o)), assert(pos( n, n, o)), assert(pos( n, ne, o)),   	assert(pos(ne, nw, o)), assert(pos(ne, n, o)), assert(pos(ne, ne, o)),
assert(pos(nw,  w, o)), assert(pos(nw, c, o)), assert(pos(nw, e,  o)),   	assert(pos( n,  w, o)), assert(pos( n, c, o)), assert(pos( n,  e, o)),   	assert(pos(ne,  w, o)), assert(pos(ne, c, o)), assert(pos(ne,  e, o)),
assert(pos(nw, sw, o)), assert(pos(nw, s, o)), assert(pos(nw, se, o)),   	assert(pos( n, sw, o)), assert(pos( n, s, o)), assert(pos( n, se, o)),   	assert(pos(ne, sw, o)), assert(pos(ne, s, o)), assert(pos(ne, se, o)),

assert(pos( w, nw, o)), assert(pos( w, n, o)), assert(pos( w, ne, o)),   	assert(pos( c, nw, o)), assert(pos( c, n, o)), assert(pos( c, ne, o)),   	assert(pos( e, nw, o)), assert(pos( e, n, o)), assert(pos( e, ne, o)),
assert(pos( w,  w, o)), assert(pos( w, c, o)), assert(pos( w, e,  o)),   	assert(pos( c,  w, o)), assert(pos( c, c, o)), assert(pos( c,  e, o)),   	assert(pos( e, w, o)),  assert(pos( e, c, o)), assert(pos( e,  e, o)),
assert(pos( w, sw, o)), assert(pos( w, s, o)), assert(pos( w, se, o)),   	assert(pos( c, sw, o)), assert(pos( c, s, o)), assert(pos( c, se, o)),   	assert(pos( e, sw, o)), assert(pos( e, s, o)), assert(pos( e, se, o)),

assert(pos(sw, nw, o)), assert(pos(sw, n, o)), assert(pos(sw, ne, o)),  	assert(pos( s, nw, o)), assert(pos( s, n, o)), assert(pos( s, ne, o)),   	assert(pos(se, nw, o)), assert(pos(se, n, o)), assert(pos(se, ne, o)),
assert(pos(sw,  w, o)), assert(pos(sw, c, o)), assert(pos(sw, e,  o)),   	assert(pos( s,  w, o)), assert(pos( s, c, o)), assert(pos( s,  e, o)),   	assert(pos(se, w, o)),  assert(pos(se, c, o)), assert(pos(se,  e, o)),
assert(pos(sw, sw, o)), assert(pos(sw, s, o)), assert(pos(sw, se, o)),   	assert(pos( s, sw, o)), assert(pos( s, s, o)), assert(pos( s, se, o)),   	assert(pos(se, sw, o)), assert(pos(se, s, o)), assert(pos(se, se, o)),

assert(waiterPos(c, c)),
assert(pos(null, null, b)),
assert(pos(null, null, g)).
