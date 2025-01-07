CLASS TWebPanel FROM TWebForm

   DATA cSizing    INIT ''
   DATA cId_Dialog   INIT ''
   DATA lFluid     INIT ''
   DATA cHtml_End    INIT '</div>' + CRLF


   METHOD New()     CONSTRUCTOR

   METHOD End()     INLINE ::Html( ::cHtml_End )

   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, cClass, cStyle, cProp, lHidden ) CLASS TWebPanel

   LOCAL lFound := .F.
   LOCAL oForm

   DEFAULT cId TO ::GetId()
   DEFAULT cClass TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lHidden TO .F.

   ::cId    := cId
   ::cClass  := cClass
   ::cStyle  := cStyle
   ::cProp   := cProp
   ::lHidden  := lHidden

   ::oParent   := oParent



   IF ValType( oParent ) == 'O'

      oParent:AddControl( SELF )

      ::lDessign  := oParent:lDessign
      ::cSizing   := oParent:cSizing
      ::cType     := oParent:cType
      ::lFluid    := oParent:lFluid

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

METHOD Activate() CLASS TWebPanel

   LOCAL cHtml  := ''
   LOCAL cStyle := ''
   LOCAL nI

   cHtml += '<div id="' + ::cId_Dialog + '-' + ::cId + '" '

   IF !Empty( ::cClass )
      cHtml += ' class="' + ::cClass + '" '
   ENDIF

   IF ::lDessign
      cStyle += ";border:1px solid red;"
   ENDIF

   IF ::lHidden
      cStyle +=  'display:none;'
   ENDIF

   IF !Empty( ::cStyle )
      cStyle += ::cStyle
   ENDIF

   IF !Empty( cStyle )
      cHtml += ' style="' + cStyle + '" '
   ENDIF

   IF !Empty( ::cProp )
      cHtml += ' ' + ::cProp + ' '
   ENDIF


   cHtml += '>'  + CRLF

// ::Html( cHtml )

   FOR nI := 1 TO Len( ::aControls )

      IF ValType( ::aControls[ nI ] ) == 'O'
         cHtml += ::aControls[ nI ]:Activate()
      ELSE
         cHtml += ::aControls[ nI ]
      ENDIF

   NEXT

// cHtml += '</div>' + CRLF

   RETU cHtml
