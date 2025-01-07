// -------------------------------------------------------------

CLASS TWebFont FROM TWebControl

   DATA cColor      INIT ''
   DATA cbackground    INIT ''
   DATA nSize       INIT 0
   DATA lBold       INIT .F.
   DATA lItalic      INIT .F.
   DATA cFamily     INIT ''

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cColor, cbackGround, nSize, lBold, lItalic, cFamily ) CLASS TWebFont

   DEFAULT cId    TO ''
   DEFAULT cColor   TO ''
   DEFAULT cBackGround  TO ''
   DEFAULT nSize   TO 0
   DEFAULT lBold   TO .F.
   DEFAULT lItalic  TO .F.
   DEFAULT cFamily   TO ''


   ::oParent   := oParent
   ::cId   := cId
   ::cColor  := cColor
   ::cBackGround := cBackGround
   ::nSize   := nSize
   ::lBold   := lBold
   ::lItalic   := lItalic
   ::cFamily   := cFamily

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebFont

   LOCAL cHtml := ''

   cHtml += '<style>'
   cHtml += '    .' + ::cId + '{ '

   IF !Empty( ::cColor )
      cHtml += 'color: ' + ::cColor + ' !important;'
   ENDIF

   IF !Empty( ::cBackGround )
      cHtml += 'background: ' + ::cBackGround + ' !important;'
   ENDIF

   IF ::nSize > 0
      cHtml += 'font-size: ' + LTrim( Str( ::nSize ) ) + 'px  !important;'
   ENDIF

   IF ::lBold
      cHtml += 'font-weight: bold  !important;'
   ENDIF

   IF ::lItalic
      cHtml += 'font-style: italic  !important;'
   ENDIF

   IF !Empty( ::cFamily )
      cHtml += 'font-family: ' + ::cFamily + ' !important;'
   ENDIF

   cHtml += '} '
   cHtml += '</style>'

   RETU cHtml
