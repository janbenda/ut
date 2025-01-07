
CLASS TWebCard FROM TWebForm

   DATA lFluid      INIT .F.


   METHOD New( oParent, cId, cClass, cStyle )   CONSTRUCTOR

   METHOD AddControl( uValue )  INLINE AAdd( ::aControls, uValue )
   METHOD Html( cCode )    INLINE AAdd( ::aControls, cCode )

   METHOD AddHeader( cCode )
   METHOD EndHeader()     INLINE ::Html( '</div>'  + CRLF )

   METHOD AddBody( cCode )
   METHOD EndBody()     INLINE ::Html( '</div>'  + CRLF )


   METHOD AddFooter( cCode )

   METHOD EndCard()    INLINE ::Html( '</div>'  + CRLF )   // ::Html( '</div></div>' )

   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cClass, cStyle ) CLASS TWebCard

   hb_default( @cId, '' )
   hb_default( @cClass, '' )
   hb_default( @cStyle, '' )

   ::oParent   := oParent
   ::cId    := cId
   ::cClass   := cClass
   ::cStyle  := cStyle



   IF ValType( oParent ) == 'O'

      oParent:AddControl( SELF )

      ::lDessign := oParent:lDessign
      ::cSizing  := oParent:cSizing
      ::cType    := oParent:cType
      ::lFluid   := oParent:lFluid

// We're looking for a TWEBFORM. We need cId_Dialog
      ::cId_Dialog  := UIdFormParent( oParent )

   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebCard

   LOCAL cHtml := ''
   LOCAL nI

// cHtml += '<div class="card-deck" >'

   cHtml += '<div class="card '

   IF !Empty( ::cClass )
      cHtml += ::cClass
   ENDIF

   cHtml += '" '

   IF !Empty( ::cStyle )
      cHtml += 'style="' + ::cStyle + '" '
   ENDIF


   cHtml += '>' + CRLF

   FOR nI := 1 TO Len( ::aControls )

      IF ValType( ::aControls[ nI ] ) == 'O'

         cHtml += ::aControls[ nI ]:Activate()
      ELSE

         cHtml += ::aControls[ nI ]
      ENDIF

   NEXT

   RETU cHtml

METHOD AddHeader( cCode, cClass, cStyle ) CLASS TWebCard

   LOCAL cHtml, oHeader

   hb_default( @cCode, '' )
   hb_default( @cClass, '' )
   hb_default( @cStyle, '' )

   cHtml := '<div class="card-header ' + cClass + '" '

   IF !Empty( cStyle )
      cHtml += 'style="' + cStyle + '" >'
   ENDIF

   cHtml += '>' + CRLF

   IF !Empty( cCode )

      cHtml += cCode + '</div>' + CRLF

      ::Html( cHtml )

   ELSE

      oHeader := TCardContainer():New( SELF )

      oHeader:Html( cHtml )

   ENDIF

   RETU oHeader

METHOD AddBody( cCode, cClass, cStyle ) CLASS TWebCard

   LOCAL cHtml, oBody

   hb_default( @cCode, '' )
   hb_default( @cClass, '' )
   hb_default( @cStyle, '' )

   cHtml := '<div class="card-body ' + cClass + '" '

   IF !Empty( cStyle )
      cHtml += 'style="' + cStyle + '" >'
   ENDIF

   cHtml += '>' + CRLF

   IF !Empty( cCode )

      cHtml += cCode + '</div>' + CRLF

      ::Html( cHtml )

   ELSE

      oBody := TCardContainer():New( SELF )

      oBody:Html( cHtml )

   ENDIF

   RETU oBody

METHOD AddFooter( cCode, cClass, cStyle ) CLASS TWebCard

   LOCAL cHtml, oFooter

   hb_default( @cCode, '' )
   hb_default( @cClass, '' )
   hb_default( @cStyle, '' )

   cHtml := '<div class="card-footer ' + cClass + '" '

   IF !Empty( cStyle )
      cHtml += 'style="' + cStyle + '" >'
   ENDIF

   cHtml += '>' + CRLF

   IF !Empty( cCode )

      cHtml += '<small class="text-muted">' + cCode + '</small></div>' + CRLF

      ::Html( cHtml )

   ELSE

      oFooter := TCardContainer():New( SELF )

      oFooter:Html( cHtml )

   ENDIF

   RETU oFooter


// --------------------------------------------------- //

CLASS TCardContainer FROM TWebForm

   DATA cSizing     INIT ''
   DATA cId_Dialog    INIT ''
   DATA lFluid     INIT ''


   METHOD New( oParent )   CONSTRUCTOR

   METHOD End()      INLINE ::Html( '</div>' )

   METHOD Activate()

ENDCLASS

METHOD New( oParent ) CLASS TCardContainer

   LOCAL lFound := .F.

   ::oParent   := oParent

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )

      ::lDessign  := oParent:lDessign
      ::cSizing    := oParent:cSizing
      ::cType      := oParent:cType
      ::lFluid      := oParent:lFluid

// We're looking for a TWEBFORM. We need cId_Dialog
      ::cId_Dialog  := UIdFormParent( oParent )

   ENDIF

   RETU SELF

METHOD Activate() CLASS TCardContainer

   LOCAL cHtml := ''
   LOCAL nI

   FOR nI := 1 TO Len( ::aControls )

      IF ValType( ::aControls[ nI ] ) == 'O'
         cHtml += ::aControls[ nI ]:Activate()
      ELSE
         cHtml += ::aControls[ nI ]
      ENDIF

   NEXT

   RETU cHtml
