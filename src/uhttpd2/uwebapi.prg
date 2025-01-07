

FUNCTION UWebApi( oServer, o )

   LOCAL oUDom, bCode
   LOCAL cHtml

// Controles

   IF hb_HHasKey( oServer[ 'post' ], 'controls' )
      oServer[ 'post' ][ 'controls' ] := hb_jsonDecode( oServer[ 'post' ][ 'controls' ] )
   ELSE
      oServer[ 'post' ][ 'controls' ] := { => }
   ENDIF

   IF ! hb_HHasKey( oServer[ 'post' ], 'api' )
      UDo_Error( 'data-api not defined', NIL, 100 )
      RETU NIL
   ENDIF

  /*
  if hb_HHaskey( oServer[ 'server' ], 'HTTP_COOKIE' )
   _d( 'HAY COOKIE => ' + oServer[ 'server' ]['HTTP_COOKIE'] )
  else
   _d( 'NO COOKIE*!' )
  endif
  */


// Function API


   IF hb_IsFunction( oServer[ 'post' ][ 'api' ] )

      USetFilePrg( 'API ' + oServer[ 'post' ][ 'api' ] )

      oUDom := UDom():New( oServer[ 'post' ], oServer[ 'files' ] )

      bCode := &( "{|o,oSrv| " + oServer[ 'post' ][ 'api' ] + "(o,oSrv) }" )

// _d( 'UWebApi INI', oServer )
// _d( 'UWebApi INI' )

      cHtml := Eval( bCode, oUDom, oServer  )

// If exist session, we can save data values...

      USessionWrite() // Write Session if exist

      USessionDelete() // Delete static Session if exist

      IF ValType( o:bPostRun ) == 'B'
         Eval( o:bPostRun )
      ENDIF

// _d( 'WEBAPI END' )

   ELSE

      cHtml := "Api function doesn't exist.... => " + oServer[ 'post' ][ 'api' ]

   ENDIF

   RETU cHtml

// ----------------------------------------------------------------------------//

FUNCTION UMyError( oErr, cErr )

   uWrite( cErr )
   uWrite( oErr )

   RETU NIL
