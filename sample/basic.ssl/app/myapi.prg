function myapi( oDom )
	
	do case
		case oDom:GetProc() == 'hello' 	; DoHello( oDom )
		case oDom:GetProc() == 'clickme' 	; DoButton( oDom )
		
		otherwise
			oDom:SetError( "Proc doesn't exist: " + oDom:GetProc() )
	endcase

retu oDom:Send() 

// --------------------------------------------------------- //

function DoHello( oDom )

	oDom:Console( 'Hello !' )

return nil 

// --------------------------------------------------------- //

function DoButton( oDom )

	local cHtml 	:= '<h5>SSL Information !</h5><hr>'
	local hInfo	:= UGetInfoSSL() 	 
	local nLen 	:= len( hInfo )
	local n, aPair

	cHtml += '<table border="1">'
	cHtml += '<tr>'
	cHtml += '<th>Description</th>'
	cHtml += '<th>Value</th>'	
	cHtml += '</tr>'

	for n := 1 to nLen 
		cHtml += '<tr>'
		
		aPair := HB_HPairAt( hInfo, n )		
		
		cHtml += '<td>' + hb_CStr( aPair[1] ) + '</td>'
		cHtml += '<td>' + hb_CStr( aPair[2] ) + '</td>'

		cHtml += '</tr>'	
	next	

	cHtml 	+= '</table>'	

	
	oDom:SetMsg( cHtml )
	oDom:Set( 'btn', 'Done !' )


return nil  