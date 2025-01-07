// ----------------------------------------------------------------

FUNCTION GetMsgServer()

   LOCAL hParam  := UPost()
   LOCAL uValue

   IF hb_HHasKey( hParam, 'type' )

      DO CASE
      CASE hParam[ 'type' ] == 'C';  uValue := hParam[ 'value' ]
      CASE hParam[ 'type' ] == 'H';  uValue := hb_jsonDecode( hParam[ 'value' ] )
      CASE hParam[ 'type' ] == 'N';  uValue := Val( hParam[ 'value' ] )
      CASE hParam[ 'type' ] == 'L'; uValue := if( hParam[ 'value' ] == 'true', .T., .F.  )
      OTHERWISE
         uValue := hParam[ 'value' ]
      ENDCASE

   ELSE

      uValue := hParam

   ENDIF

   RETU uValue
