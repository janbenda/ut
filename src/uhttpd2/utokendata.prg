#include 'hbclass.ch'

CLASS UTokenData

   DATA cKey        INIT 'UT!2019@v1'
   DATA lValid       INIT .F.
   DATA uData
   DATA cError       INIT ''

   METHOD New()       CONSTRUCTOR

   METHOD Encode( uData )
   METHOD Decode()
   METHOD Valid( cToken )
   METHOD GetData()     INLINE ::uData
   METHOD SetData( uData )   INLINE ::uData := uData

   METHOD SetKey( cKey )

ENDCLASS

// ----------------------------------------------- //

METHOD New( cKey ) CLASS UTokenData

   ::SetKey( cKey )

   ::uData := nil

   RETU SELF

// ----------------------------------------------- //

METHOD SetKey( cKey ) CLASS UTokenData

   hb_default( @cKey, '' )

   IF !Empty( cKey )
      ::cKey := cKey
   ENDIF

   RETU NIL

// ----------------------------------------------- //

METHOD Encode( uData ) CLASS UTokenData

   LOCAL cToken
   LOCAL cKey   := hb_blowfishKey( ::cKey )

   IF uData == nil
      uData := ::uData
   ENDIF

// uData := hb_jsonencode( uData )
   uData := hb_Serialize( uData )

   cToken  := hb_base64Encode( hb_blowfishEncrypt( cKey, uData ) )
   cToken  := hb_StrReplace( cToken, '+/', '-_' )

   RETU cToken

// ----------------------------------------------- //

METHOD Valid( cToken ) CLASS UTokenData

   LOCAL cRealToken, cData

   hb_default( @cToken, '' )

   ::lValid  := .F.
   ::uData  := nil
   ::cError   := ''

   IF  Empty( cToken )
      RETU .F.
   ENDIF

   cRealToken := hb_StrReplace( cToken, '-_',  '+/' )
   cRealToken := hb_base64Decode( cRealToken )

   cData := hb_blowfishDecrypt( hb_blowfishKey( ::cKey ), cRealToken )

   ::lValid := if( cData == NIL, .F., .T. )

   IF ::lValid
// ::uData := hb_jsondecode( cData )
      ::uData := hb_Deserialize( cData )
   ELSE
      ::cError   := 'Verificacion de firma ha fallado'
   ENDIF

   RETU ::lValid

// ----------------------------------------------- //

METHOD Decode( cToken ) CLASS UTokenData

   IF ! ::Valid( cToken )
      RETU NIL
   ENDIF

   RETU ::uData

// ----------------------------------------------- //
