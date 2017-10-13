pos(nw, nw, g). pos(nw, n, b). pos(nw, ne, 0).   	pos( n, nw, b). pos( n, n, g). pos( n, ne, g).   	pos(ne, nw, g). pos(ne, n, g). pos(ne, ne, b).
pos(nw,  w, b). pos(nw, c, 0). pos(nw, e,  0).   	pos( n,  w, g). pos( n, c, g). pos( n,  e, b).   	pos(ne,  w, b). pos(ne, c, g). pos(ne,  e, b).
pos(nw, sw, b). pos(nw, s, 0). pos(nw, se, 0).   	pos( n, sw, b). pos( n, s, g). pos( n, se, g).   	pos(ne, sw, g). pos(ne, s, 0). pos(ne, se, g).

pos( w, nw, g). pos( w, n, b). pos( w, ne, g).   	pos( c, nw, b). pos( c, n, 0). pos( c, ne, 0).   	pos( e, nw, g). pos( e, n, 0). pos( e, ne, g).
pos( w,  w, g). pos( w, c, b). pos( w, e,  g).   	pos( c,  w, b). pos( c, c, 0). pos( c,  e, g).   	pos( e, w, b).  pos( e, c, g). pos( e,  e, b).
pos( w, sw, g). pos( w, s, b). pos( w, se, b).   	pos( c, sw, 0). pos( c, s, 0). pos( c, se, 0).   	pos( e, sw, g). pos( e, s, 0). pos( e, se, g).

pos(sw, nw, b). pos(sw, n, b). pos(sw, ne, 0).  	pos( s, nw, g). pos( s, n, 0). pos( s, ne, g).   	pos(se, nw, b). pos(se, n, b). pos(se, ne, b).
pos(sw,  w, b). pos(sw, c, 0). pos(sw, e,  b).   	pos( s,  w, b). pos( s, c, 0). pos( s,  e, g).   	pos(se, w, 0).  pos(se, c, b). pos(se,  e, 0).
pos(sw, sw, 0). pos(sw, s, b). pos(sw, se, 0).   	pos( s, sw, g). pos( s, s, g). pos( s, se, 0).   	pos(se, sw, 0). pos(se, s, b). pos(se, se, 0).
