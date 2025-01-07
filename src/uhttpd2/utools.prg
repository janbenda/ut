FUNCTION UIsMobile()

   LOCAL c := UServer()[ 'HTTP_USER_AGENT' ]
   LOCAL s := '/Mobile|iP(hone|od|ad)|Android|BlackBerry|IEMobile|Kindle|NetFront|Silk-Accelerated|(hpw|web)OS|Fennec|Minimo|Opera M(obi|ini)|Blazer|Dolfin|Dolphin|Skyfire|Zune/'

   RETU Len( hb_regex( s, c ) ) > 0
