/*
    Credentials recover: token, user/psw

    - UGetTokenApiKey( cKey )   --> cToken
    - UGetTokenBearer()         --> cToken
    - UGetTokenBasic()          --> aPair -> aPair[ 'user' ], aPair[ 'password' ]
*/
// ----------------------------------------------------------- //

FUNCTION UGetTokenBearer()

   LOCAL hParams  := UGetParams()
   LOCAL cName     := 'HTTP_AUTHORIZATION'
   LOCAL hAuth     := nil
   LOCAL nPos, cToken

   cToken  := hb_HGetDef( hParams, cName, '' )
   nPos  := At( 'Bearer', cToken )

   IF nPos > 0

      cToken  := AllTrim( SubStr( cToken, 7 ) )

   ENDIF

   RETU cToken

// ----------------------------------------------------------- //

FUNCTION UGetTokenBasic()

   LOCAL hParams  := UGetParams()
   LOCAL cName     := 'HTTP_AUTHORIZATION'
   LOCAL hAuth     := nil
   LOCAL nPos, aTokens, cToken

   cToken  := hb_HGetDef( hParams, cName, '' )
   nPos  := At( 'Basic', cToken )

   IF nPos > 0

      cToken  := AllTrim( SubStr( cToken, 6 ) )
      cToken  := hb_base64Decode( cToken )

      aTokens := hb_ATokens( cToken, ':' )

      IF ValType( aTokens ) == 'A' .AND. Len( aTokens ) == 2
         hAuth := { => }
         hAuth[ 'username' ] := aTokens[ 1 ]
         hAuth[ 'password' ] := aTokens[ 2 ]

      ENDIF

   ENDIF

   RETU hAuth

// ----------------------------------------------------------- //

FUNCTION UGetTokenApiKey( cKey )

   LOCAL hParams  := UGetParams()
   LOCAL cName

   hb_default( @cKey, '' )

   cName := 'HTTP_' + Upper( cKey )

   RETU hb_HGetDef( hParams, cName, NIL )

// ----------------------------------------------------------- //
