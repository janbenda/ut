#define U_PSW  'mEmorY@2022!'

STATIC _cPsw := U_PSW

FUNCTION USetPsw( cNewPsw )

   _cPsw := cNewPsw
   RETU NIL

FUNCTION UGetPsw() ; RETU _cPsw

FUNCTION USetToken( uData, cPsw )

   LOCAL cKey  := hb_blowfishKey( if( ValType( cPsw ) == 'C', cPsw, UGetPsw() ) )
   LOCAL hData  := { => }
   LOCAL cToken, cData

   hData[ 'data' ] := uData

   cData := hb_jsonEncode( hData )

   cToken  := hb_base64Encode( hb_blowfishEncrypt( cKey, cData ) )
   cToken  := hb_StrReplace( cToken, '+/', '-_' )

   RETU cToken

// lAuth by reference

FUNCTION UGetToken( cToken, lAuth, cPsw  )

   LOCAL cRealToken  := hb_StrReplace( cToken, '-_',  '+/' )
   LOCAL hData   := { => }
   LOCAL cData

   cRealToken := hb_base64Decode( cRealToken )

   cData := hb_blowfishDecrypt( hb_blowfishKey( if( ValType( cPsw ) == 'C', cPsw, UGetPsw() ) ), cRealToken )

   lAuth := if( cData == NIL, .F., .T. )

   IF lAuth
      hData := hb_jsonDecode( cData )
   ENDIF

   RETU if( !Empty( hData ), hData[ 'data' ], NIL )
