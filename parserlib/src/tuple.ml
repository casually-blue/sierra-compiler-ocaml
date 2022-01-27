let flatten2 (a,b) = (a,b)
let flatten3 ((a,b),c) = (a,b,c)
let flatten4 (((a,b),c),d) = (a,b,c,d)
let flatten5 ((((a,b),c),d),e) = (a,b,c,d,e)
let flatten6 (((((a,b),c),d),e),f) = (a,b,c,d,e,f)
let flatten7 ((((((a,b),c),d),e),f),g)= (a,b,c,d,e,f,g)
let flatten8 (((((((a,b),c),d),e),f),g),h) = (a,b,c,d,e,f,g,h)

let flatmap ff constr value rest = Ok((constr (ff value)), rest)
