
CLASS TWebAccordion FROM TWebForm

   DATA aSections      INIT {}
   DATA cId_Dialog     INIT ''
   DATA cClass_Section     INIT ''
   DATA lUnique       INIT .F.


   METHOD New( oParent, cId, cClass, cStyle, lUnique )   CONSTRUCTOR

   METHOD AddSection( cId_Header, cId_Body, cClass )

   METHOD Activate()

   METHOD End()      INLINE ::oParent:Html( '</div>' + CRLF )
// METHOD EndAccordion()    INLINE NIL

ENDCLASS

METHOD New( oParent, cId, cClass, cStyle, lUnique ) CLASS TWebAccordion

   LOCAL oForm

// hb_default( @cId, ltrim(str(hb_milliseconds())) )
   hb_default( @cId, '' )
   hb_default( @cClass, 'accordion' )
   hb_default( @cStyle, '' )
   hb_default( @lUnique, .F.  )

   ::oParent   := oParent
   ::cId    := cId
   ::cClass   := cClass
   ::cStyle  := cStyle
   ::lUnique   := lUnique


   IF ::lUnique .AND. Empty( ::cId )
      UDo_ErrorMsg( 'TAccordion: Clausule UNIQUE need ID', 'TWeb' )
      RETURN NIL
   ENDIF



   IF ValType( oParent ) == 'O'

      oParent:AddControl( SELF )

      ::lDessign := oParent:lDessign
      ::cSizing  := oParent:cSizing
      ::cType    := oParent:cType

// We're looking for a TWEBFORM. We need cId_Dialog
      ::cId_Dialog  := UIdFormParent( oParent )

      oForm  := UOFormParent( oParent )

      IF ValType( oForm ) == 'O' .AND. oForm:ClassName() == 'TWEBFORM'
         ::lDessign := oForm:lDessign
         ::cSizing  := oForm:cSizing
         ::cType    := oForm:cType
         ::lFluid   := oForm:lFluid

      ENDIF


   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebAccordion

   LOCAL cHtml := ''
   LOCAL nI

   cHtml += CRLF
   cHtml += '<div '

   IF !Empty( ::cId )
      cHtml += 'id="' + ::cId + '" '
   ENDIF

   IF !Empty( ::cClass )
      cHtml += 'class="' + ::cClass + '" '
   ENDIF

   IF !Empty( ::cStyle )
      cHtml += 'style="' + ::cStyle + '" '
   ENDIF

   cHtml += '>' + CRLF + CRLF


   FOR nI := 1 TO Len( ::aSections )

      cHtml += ::aSections[ nI ]:Activate()

   NEXT



// Este ultimo div, es lo que hace::EndAccordion()
// cHtml += '</div><!-- ENDACORDION -->'  + CRLF
// Pero, es mejor que cierren el accordion con comando

   RETU cHtml

METHOD AddSection( cId_Header, cId_Body, cClass ) CLASS TWebAccordion

   LOCAL oSection := nil

   hb_default( @cClass, '' )

   ::cClass_Section := cClass

   oSection := TAccordionSection():New( SELF, cId_Header, cId_Body )

   AAdd( ::aSections, oSection )

   RETU oSection

// --------------------------------------------------- //

CLASS TAccordionSection FROM TWebControl

   DATA oHeader
   DATA oBody
   DATA cId_Dialog      INIT ''
   DATA cId_Header
   DATA cId_Body
   DATA lShow         INIT .F.

   METHOD New( oParent, cId_Header, cId_Body )    CONSTRUCTOR

   METHOD Header()
   METHOD Body()

   METHOD Activate()

   METHOD End()    INLINE  ::Html( '</div>' + CRLF )

ENDCLASS

METHOD New( oParent, cId_Header, cId_Body ) CLASS TAccordionSection

   hb_default( @cId_Header, '' )
   hb_default( @cId_Body, '' )

   ::oParent   := oParent
   ::cId_Header  := cId_Header
   ::cId_Body  := cId_Body


   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )

      ::lDessign  := oParent:lDessign
      ::cSizing    := oParent:cSizing
      ::cType      := oParent:cType

      ::cId_Dialog  := UIdFormParent( oParent )

   ENDIF

   RETU SELF

METHOD Header( cCode, cClass ) CLASS TAccordionSection

   LOCAL cHtml  := ''
   LOCAL oHeader

   hb_default( @cCode, '' )
   hb_default( @cClass, '' )

   cHtml += '<div '

   IF !Empty( ::cId_Header )
      cHtml += 'id="' + ::cId_Header + '" '
   ENDIF

   cHtml += 'class="card-header p-0 '

   IF !Empty( cClass )
      cHtml += cClass
   ENDIF

   cHtml += '" >'  + CRLF


// cHtml += '<button class="btn btn-block text-left header_title" type="button" data-toggle="collapse" data-target="#collapseOne" >'
   cHtml += '<button class="btn btn-block text-left " type="button" data-toggle="collapse" '
   cHtml += 'data-target="#' + ::cId_Body + '" >' + CRLF



   IF !Empty( cCode )

      cHtml += cCode + CRLF
      cHtml += '</button>'   + CRLF
      cHtml += '</div>' + CRLF

      ::Html( cHtml )

   ELSE

      ::Html( cHtml )

      oHeader := TContainer():New( ::oParent )
// oHeader := TWebPanel():New( ::oParent )

      oHeader:cHtml_End  := '</button></div>' + CRLF

      ::Html( oHeader )

   ENDIF

   RETU oHeader

METHOD Body( cCode, cClass, cStyle, lShow ) CLASS TAccordionSection

   LOCAL cHtml  := ''
   LOCAL oBody

   hb_default( @cCode, '' )
   hb_default( @cClass, '' )
   hb_default( @cStyle, '' )
   hb_default( @lShow, .F. )


   cHtml += '<div id="' + ::cId_Body + '" class="collapse '

   IF lShow
      cHtml += ' show '
   ENDIF


   cHtml += '" aria-labelledby="' + ::cId_Header   + '" '

   IF ::oParent:lUnique
      cHtml += ' data-parent="#' + ::oParent:cId + '" '
   ENDIF

   cHtml += '>' + CRLF


// ---------------------------

   cHtml += '  <div class="card-body"> ' + CRLF

   IF !Empty( cCode )

      cHtml += cCode + CRLF

      cHtml += '  </div>' + CRLF
      cHtml += '</div>' + CRLF


      ::Html( cHtml )


   ELSE

      ::Html( cHtml )

      oBody := TContainer():New( ::oParent )
// oBody := TWebPanel():New( ::oParent )


      oBody:cHtml_End  := '</div>' + CRLF

      ::Html( oBody )

      cHtml := '  </div>' + CRLF

      ::Html( cHtml )

   ENDIF


   RETU oBody

METHOD Activate() CLASS TAccordionSection

   LOCAL cHtml := ''
   LOCAL nI

   cHtml += '<div class="card '


   IF !Empty( ::oParent:cClass_Section )
      cHtml += ::oParent:cClass_Section
   ENDIF

 /*
 cHtml += '" '

 if !empty( ::cStyle )
  cHtml += 'style="' + ::cStyle + '" '
 endif

*/

   cHtml += '">' + CRLF




   FOR nI := 1 TO Len( ::aControls )


      IF ValType( ::aControls[ nI ] ) == 'O'


         cHtml += ::aControls[ nI ]:Activate()

      ELSE

         cHtml += ::aControls[ nI ]
      ENDIF

   NEXT



   RETU cHtml

// --------------------------------------------------- //

CLASS TContainer FROM TWebForm

   DATA cHtml_End     INIT '</div>' + CRLF

   DATA cSizing     INIT ''
   DATA cId_Dialog     INIT ''
   DATA lFluid      INIT .F.


   METHOD New( oParent )    CONSTRUCTOR

   METHOD AddControl( uValue )  INLINE AAdd( ::aControls, uValue )


   METHOD End()      INLINE ::Html( ::cHtml_End )

   METHOD Activate()

ENDCLASS

METHOD New( oParent ) CLASS TContainer

   ::oParent   := oParent

   ::cId_Dialog  := UIdFormParent( oParent )

   ::lFluid  := .F.

   RETU SELF

METHOD Activate() CLASS TContainer

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
