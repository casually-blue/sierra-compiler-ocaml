let flatten2  (a,b)                                     = (a,b)
let flatten3  ((a,b),c)                                 = (a,b,c)
let flatten4  (((a,b),c),d)                             = (a,b,c,d)
let flatten5  ((((a,b),c),d),e)                         = (a,b,c,d,e)
let flatten6  (((((a,b),c),d),e),f)                     = (a,b,c,d,e,f)
let flatten7  ((((((a,b),c),d),e),f),g)                 = (a,b,c,d,e,f,g)
let flatten8  (((((((a,b),c),d),e),f),g),h)             = (a,b,c,d,e,f,g,h)
let flatten9  ((((((((a,b),c),d),e),f),g),h),i)         = (a,b,c,d,e,f,g,h,i)
let flatten10 (((((((((a,b),c),d),e),f),g),h),i),j)     = (a,b,c,d,e,f,g,h,i,j)
let flatten11 ((((((((((a,b),c),d),e),f),g),h),i),j),k) = (a,b,c,d,e,f,g,h,i,j,k)
