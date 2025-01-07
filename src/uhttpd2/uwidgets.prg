#include "hbclass.ch"

#pragma -kM+

MEMVAR server

// ============================================================
CLASS UWBrowse

   DATA aColumns   INIT {}
   DATA nPageSize  INIT 0
   DATA nPos       INIT 0

   METHOD AddColumn( nID, cTitle, cField, lRaw )
   METHOD Output()

ENDCLASS


FUNCTION UWBrowseNew()

   LOCAL oW := UWBrowse()

   RETURN oW


METHOD AddColumn( nID, cTitle, cField, lRaw ) CLASS UWBrowse

   AAdd( Self:aColumns, { nID, cTitle, cField, !Empty( lRaw ) } )

   RETURN Self


METHOD Output() CLASS UWBrowse

   LOCAL cRet := "", nI, xI, xField, nPos, cUrl, cI, lValidate

   cRet += '<table class="ubr"><tr>'

   // Header
   cRet += '<tr>'
   FOR nI := 1 TO Len( Self:aColumns )
      cRet += '<th>' + UHtmlEncode( Self:aColumns[ nI, 2 ] ) + '</th>'
   NEXT
   cRet += '</tr>'

   // Body
   nPos := 0
   dbGoTop()
   IF Self:nPageSize > 0 .AND. Self:nPos > 0
      dbSkip( Self:nPos )
   ENDIF
   DO WHILE ! Eof()
      cRet += '<tr>'
      FOR nI := 1 TO Len( Self:aColumns )
         xField := Self:aColumns[ nI, 3 ]
         IF ValType( xField ) == "C"
            xI := FieldGet( FieldPos( xField ) )
         ELSEIF ValType( xField ) == "B"
            xI := Eval( xField )
         ENDIF
         IF     ValType( xI ) == "C";  xI := Trim( xI )
         ELSEIF ValType( xI ) == "N";  xI := Str( xI )
         ELSEIF ValType( xI ) == "D";  xI := DToC( xI )
         ELSE ;  xI := "VALTYPE()==" + ValType( xI )
         ENDIF
         IF ! Self:aColumns[ nI, 4 ]
            xI := UHtmlEncode( xI )
         ENDIF
         cRet += '<td><nobr>' + xI + '</nobr></td>'
      NEXT
      cRet += '</tr>'
      dbSkip()
      IF ++nPos >= Self:nPageSize
         EXIT
      ENDIF
   ENDDO
   cRet += '</table>'
   IF ! Eof() .OR. Self:nPos > 0
      cUrl := server[ "REQUEST_URI" ]
      IF ( nI := At( "?_ucs=", cUrl ) ) == 0
         nI := At( "&_ucs=", cUrl )
      ENDIF
      IF ( lValidate := nI > 0 )
         cUrl := Left( cUrl, nI - 1 )
      ENDIF
      IF ( nI := At( "?_pos=", cUrl ) ) == 0
         nI := At( "&_pos=", cUrl )
      ENDIF
      IF nI > 0
         cUrl := Left( cUrl, nI - 1 )
      ENDIF
      cUrl += iif( "?" $ cUrl, "&", "?" ) + "_pos="
      cRet := '<br>' + cRet
      IF ! Eof()
         cI := cUrl + LTrim( Str( Self:nPos + Self:nPageSize ) )
         cRet := '<a href="' + iif( lValidate, UUrlChecksum( cI ), cI ) + '">&gt;&gt;</a>' + cRet
      ENDIF
      IF Self:nPos > 0
         cI := cUrl + LTrim( Str( Max( 0, Self:nPos - Self:nPageSize ) ) )
         cRet := '<a href="' + iif( lValidate, UUrlChecksum( cI ), cI ) + '">&lt;&lt;</a>&nbsp;&nbsp;' + cRet
      ENDIF
   ENDIF

   RETURN cRet


// ============================================================
CLASS UWOption

   DATA aOption   INIT {}
   DATA cValue

   METHOD Add( cTitle, cCode, lRaw )
   METHOD Output()

ENDCLASS


FUNCTION UWOptionNew()

   LOCAL oW := UWOption()

   RETURN oW


METHOD Add( cTitle, cCode, lRaw ) CLASS UWOption

   AAdd( Self:aOption, { iif( ! Empty( lRaw ), cTitle, UHtmlEncode( cTitle ) ), cCode } )

   RETURN Self


METHOD Output() CLASS UWOption

   LOCAL cRet := ""

   AEval( Self:aOption, {| X | cRet += hb_StrFormat( '<option value="%s"%s>%s</option>', UHtmlEncode( X[ 2 ] ), iif( X[ 2 ] == Self:cValue, " selected", "" ), X[ 1 ] ) } )

   RETURN cRet
