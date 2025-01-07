
FUNCTION UParse( aData, cFileName )
   RETURN parse_data( aData, compile_file( cFileName ) )


STATIC FUNCTION parse_data( aData, aCode )

   LOCAL aInstr, aData2, cRet, xValue, aValue, cExtend := ""

   DO WHILE cExtend != NIL
      cExtend := NIL
      cRet := ""
      FOR EACH aInstr IN aCode
         SWITCH aInstr[ 1 ]
         CASE "txt"
            cRet += aInstr[ 2 ]
            EXIT

         CASE "="
            IF hb_HHasKey( aData, aInstr[ 2 ] )
               xValue := aData[ aInstr[ 2 ] ]
               IF HB_ISCHAR( xValue )
                  cRet += UHtmlEncode( xValue )
               ELSEIF HB_ISNUMERIC( xValue )
                  cRet += UHtmlEncode( Str( xValue ) )
               ELSEIF HB_ISDATE( xValue )
                  cRet += UHtmlEncode( DToC( xValue ) )
               ELSEIF HB_ISTIMESTAMP( xValue )
                  cRet += UHtmlEncode( hb_TToC( xValue ) )
               ELSEIF HB_ISOBJECT( xValue )
                  cRet += UHtmlEncode( xValue:Output() )
               ELSE
                  ? hb_StrFormat( "Template error: invalid type '%s'", ValType( xValue ) )
               ENDIF
            ELSE
               ? hb_StrFormat( "Template error: variable '%s' not found", aInstr[ 2 ] )
            ENDIF
            EXIT

         CASE ":"
            IF hb_HHasKey( aData, aInstr[ 2 ] )
               xValue := aData[ aInstr[ 2 ] ]
               IF HB_ISCHAR( xValue )
                  cRet += xValue
               ELSEIF HB_ISNUMERIC( xValue )
                  cRet += Str( xValue )
               ELSEIF HB_ISDATE( xValue )
                  cRet += DToC( xValue )
               ELSEIF HB_ISTIMESTAMP( xValue )
                  cRet += hb_TToC( xValue )
               ELSEIF HB_ISOBJECT( xValue )
                  cRet += xValue:Output()
               ELSE
                  ? hb_StrFormat( "Template error: invalid type '%s'", ValType( xValue ) )
               ENDIF
            ELSE
               ? hb_StrFormat( "Template error: variable '%s' not found", aInstr[ 2 ] )
            ENDIF
            EXIT

         CASE "if"
            xValue := iif( hb_HHasKey( aData, aInstr[ 2 ] ), aData[ aInstr[ 2 ] ], NIL )
            IF ! Empty( xValue )
               cRet += parse_data( aData, aInstr[ 3 ] )
            ELSE
               cRet += parse_data( aData, aInstr[ 4 ] )
            ENDIF
            EXIT

         CASE "loop"
            IF hb_HHasKey( aData, aInstr[ 2 ] ) .AND. ValType( aValue := aData[ aInstr[ 2 ] ] ) == "A"
               FOR EACH xValue IN aValue
                  aData2 := hb_HClone( aData )
                  hb_HEval( xValue, {| k, v | aData2[ aInstr[ 2 ] + "." + k ] := v } )
                  aData2[ aInstr[ 2 ] + ".__index" ] := xValue:__enumIndex
                  cRet += parse_data( aData2, aInstr[ 3 ] )
                  aData2 := NIL
               NEXT
            ELSE
               ? hb_StrFormat( "Template error: loop variable '%s' not found", aInstr[ 2 ] )
            ENDIF
            EXIT

         CASE "extend"
            cExtend := aInstr[ 2 ]
            EXIT

         CASE "include"
            cRet += parse_data( aData, compile_file( aInstr[ 2 ] ) )
            EXIT
         ENDSWITCH
      NEXT
      IF cExtend != NIL
         aData[ "" ] := cRet
         cRet := ""
         aCode := compile_file( cExtend )
      ENDIF
   ENDDO

   RETURN cRet


STATIC FUNCTION compile_file( cFileName )

   LOCAL nPos, cTpl, aCode := {}

   IF cFileName == NIL
      cFileName := MEMVAR->server[ "SCRIPT_NAME" ]
   ENDIF
   cFileName := UOsFileName( hb_DirBase() + "/tpl/" + cFileName + ".tpl" )
   IF hb_FileExists( cFileName )
      cTpl := hb_MemoRead( cFileName )
      BEGIN SEQUENCE
         IF ( nPos := compile_buffer( cTpl, 1, aCode ) ) < Len( cTpl ) + 1
            Break( nPos )
         ENDIF
      RECOVER USING nPos
         ? hb_StrFormat( "Template error: syntax at %s(%d,%d)", cFileName, SUBSTRCOUNT( Chr( 10 ), cTpl,, nPos ) + 1, nPos - hb_RAt( Chr( 10 ), Left( cTpl, nPos - 1 ) ) )
         aCode := {}
      END SEQUENCE
   ELSE
      ? hb_StrFormat( "Template error: file '%s' not found", cFileName )
   ENDIF

   RETURN aCode


STATIC FUNCTION compile_buffer( cTpl, nStart, aCode )

   LOCAL nI, nS, nE, cTag, cParam

   DO WHILE ( nS := hb_At( "{{", cTpl, nStart ) ) > 0
      IF nS > nStart
         AAdd( aCode, { "txt", SubStr( cTpl, nStart, nS - nStart ) } )
      ENDIF
      nE := hb_At( "}}", cTpl, nS )
      IF nE > 0
         IF ( nI := hb_At( " ", cTpl, nS, nE ) ) == 0
            nI := nE
         ENDIF
         cTag := SubStr( cTpl, nS + 2, nI - nS - 2 )
         cParam := SubStr( cTpl, nI + 1, nE - nI - 1 )

         SWITCH cTag
         CASE "="
         CASE ":"
            AAdd( aCode, { cTag, cParam } )
            nStart := nE + 2
            EXIT

         CASE "if"
            AAdd( aCode, { "if", cParam, {}, {} } )
            nI := compile_buffer( cTpl, nE + 2, ATail( aCode )[ 3 ] )
            IF SubStr( cTpl, nI, 8 ) == "{{else}}"
               nI := compile_buffer( cTpl, nI + 8, ATail( aCode )[ 4 ] )
            ENDIF
            IF SubStr( cTpl, nI, 9 ) == "{{endif}}"
               nStart := nI + 9
            ELSE
               Break( nI )
            ENDIF
            EXIT

         CASE "loop"
            AAdd( aCode, { "loop", cParam, {} } )
            nI := compile_buffer( cTpl, nE + 2, ATail( aCode )[ 3 ] )
            IF SubStr( cTpl, nI, 11 ) == "{{endloop}}"
               nStart := nI + 11
            ELSE
               Break( nI )
            ENDIF
            EXIT

         CASE "extend"
            AAdd( aCode, { "extend", cParam } )
            nStart := nE + 2
            EXIT

         CASE "include"
            AAdd( aCode, { "include", cParam } )
            nStart := nE + 2
            EXIT

         OTHERWISE
            RETURN nS

         ENDSWITCH
      ELSE
         Break( nS )
      ENDIF
   ENDDO
   IF nStart < Len( cTpl )
      AAdd( aCode, { "txt", SubStr( cTpl, nStart ) } )
   ENDIF

   RETURN Len( cTpl ) + 1


STATIC FUNCTION SUBSTRCOUNT( cSub, cString, nStart, nEnd )

   LOCAL nCount := 0

   IF nStart == NIL;  nStart := 1
   ENDIF
   DO WHILE ( nStart := hb_At( cSub, cString, nStart, nEnd ) ) > 0
      nCount++
      nStart++
   ENDDO

   RETURN nCount
