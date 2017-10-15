pos(nw, nw, 0). pos(nw, n, g). pos(nw, ne, 0).   	pos( n, nw, 0). pos( n, n, 0). pos( n, ne, b).   	pos(ne, nw, 0). pos(ne, n, 0). pos(ne, ne, 0).
pos(nw,  w, 0). pos(nw, c, 0). pos(nw, e,  0).   	pos( n,  w, 0). pos( n, c, 0). pos( n,  e, 0).   	pos(ne,  w, 0). pos(ne, c, 0). pos(ne,  e, g).
pos(nw, sw, 0). pos(nw, s, 0). pos(nw, se, 0).   	pos( n, sw, 0). pos( n, s, 0). pos( n, se, 0).   	pos(ne, sw, 0). pos(ne, s, 0). pos(ne, se, 0).

pos( w, nw, 0). pos( w, n, 0). pos( w, ne, 0).   	pos( c, nw, b). pos( c, n, 0). pos( c, ne, 0).   	pos( e, nw, 0). pos( e, n, 0). pos( e, ne, 0).
pos( w,  w, 0). pos( w, c, 0). pos( w, e,  0).   	pos( c,  w, 0). pos( c, c, 0). pos( c,  e, 0).   	pos( e, w, 0).  pos( e, c, 0). pos( e,  e, 0).
pos( w, sw, 0). pos( w, s, 0). pos( w, se, 0).   	pos( c, sw, 0). pos( c, s, 0). pos( c, se, 0).   	pos( e, sw, 0). pos( e, s, 0). pos( e, se, b).

pos(sw, nw, 0). pos(sw, n, 0). pos(sw, ne, 0).  	pos( s, nw, 0). pos( s, n, 0). pos( s, ne, 0).   	pos(se, nw, 0). pos(se, n, 0). pos(se, ne, 0).
pos(sw,  w, 0). pos(sw, c, 0). pos(sw, e,  0).   	pos( s,  w, 0). pos( s, c, 0). pos( s,  e, 0).   	pos(se, w, 0).  pos(se, c, 0). pos(se,  e, w).
pos(sw, sw, 0). pos(sw, s, 0). pos(sw, se, 0).   	pos( s, sw, 0). pos( s, s, 0). pos( s, se, 0).   	pos(se, sw, 0). pos(se, s, g). pos(se, se, 0).
