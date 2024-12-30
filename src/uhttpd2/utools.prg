function UIsMobile()
    
    local c := UServer()['HTTP_USER_AGENT']
    local s := '/Mobile|iP(hone|od|ad)|Android|BlackBerry|IEMobile|Kindle|NetFront|Silk-Accelerated|(hpw|web)OS|Fennec|Minimo|Opera M(obi|ini)|Blazer|Dolfin|Dolphin|Skyfire|Zune/'
        
retu len( HB_RegEx( s, c ) ) > 0
