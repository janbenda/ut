// -------------------------------------------------------------

CLASS TWebSelect FROM TWebControl

   DATA aItems      INIT {}
   DATA aValues     INIT {}
   DATA lParKeyValue    INIT .F.
   DATA cWidth

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, uValue, aItems, aValues, aKeyValue, nGrid, cAction, cLabel, cClass, cFont, cGroup, cStyle, cProp, lReadOnly, lHidden, cWidth ) CLASS TWebSelect

   DEFAULT cId TO ::GetId()
   DEFAULT aItems TO {}
   DEFAULT aValues TO {}
   DEFAULT aKeyValue TO NIL
   DEFAULT nGrid TO 4
   DEFAULT cAction TO ''
   DEFAULT cLabel TO ''
   DEFAULT uValue TO ''
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cGroup TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lReadonly TO .F.
   DEFAULT lHidden TO .F.
   DEFAULT cWidth TO ''


   ::oParent   := oParent
   ::cId   := cId

   ::aValues  := aValues  // IF( valtype( aValues ) == 'A' .AND. len( aValues ) == len( aItems ), aValues, aItems )
   ::nGrid   := nGrid
   ::cAction  := cAction
   ::cLabel  := cLabel
   ::uValue  := uValue
   ::cClass   := cClass
   ::cFont   := cFont
   ::cGroup   := cGroup
   ::cStyle   := cStyle
   ::cProp   := cProp
   ::lReadOnly  := lReadOnly
   ::lHidden   := lHidden
   ::cWidth   := cWidth


   IF ValType( aKeyValue ) == 'H'
      ::lParKeyValue  := .T.
      ::aValues   := aKeyValue

   ELSE

      IF ValType( aItems ) == 'A' .AND. Len( aItems ) > 0

         IF ValType( aItems[ 1 ] ) == 'A'
            ::aItems := aItems[ 1 ]
         ELSE
            ::aItems := aItems
         ENDIF

      ELSE

         ::aItems := {}
      ENDIF

      IF ValType( aValues ) == 'A' .AND. Len( aValues ) > 0

         IF ValType( aValues[ 1 ] ) == 'A'
            ::aValues := aValues[ 1 ]
         ELSE
            ::aValues := aValues
         ENDIF

      ELSE
         ::aValues := {}
      ENDIF

      IF Len( ::aItems ) <> Len( ::aValues )
         ::aValues := ::aItems
      ENDIF

   ENDIF

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebSelect

   LOCAL cHtml  := ''
   LOCAL cChecked := ''
   LOCAL nI
   LOCAL cSize := ''
   LOCAL cSizeLabel := ''
   LOCAL aPar
   LOCAL lArrayPar := .F.
   LOCAL cIdPrefix
   LOCAL cSt := ''
   LOCAL cGrid := ''

   DO CASE
   CASE Upper( ::oParent:cSizing ) == 'SM'
      cSize := 'form-control-sm'
      cSizeLabel := 'col-form-label-sm'
   CASE Upper( ::oParent:cSizing ) == 'LG'
      cSize := 'form-control-lg'
      cSizeLabel := 'col-form-label-lg'
   ENDCASE

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF

   IF ValType( ::nGrid ) == 'N'
      cGrid := LTrim( Str( ::nGrid ) )
   ELSE
      cGrid := ::nGrid
   ENDIF

   IF !Empty( ::cWidth )
      cGrid := '0'
      cSt += 'width:' + ::cWidth + ';'
   ENDIF

   cHtml := '<div class="col-' + cGrid + IF( ::oParent:lDessign, ' tweb_dessign', '' ) + '" ' + IF( ::oParent:lDessign, 'style="border:1px solid blue;"', '' )

   IF  ::oParent:lDessign
      cSt += 'border:1px solid blue;'
   ENDIF

   IF ::lHidden
      cSt += 'display:none;'
   ENDIF

   IF !Empty( cSt )
      cHtml += ' style="' + cSt + '" '
   ENDIF

   cHtml += ' data-group="' + cIdPrefix + ::cId   + '" >'


   IF !Empty( ::cLabel )

// cHtml += '<label for="' + ::cId + '">' + ::cLabel + '</label>'
      cHtml += '<label class="' + cSizeLabel + ' ' + ::cFontLabel + ' "for="' + cIdPrefix + ::cId + '">' + ::cLabel + '</label>'

   ENDIF

   cHtml += '<div class="input-group">'


   cHtml += '<select data-control="tcombobox" '

 /*
 if !empty( ::cGroup )
  cHtml += 'data-group="' + ::cGroup + '" '
 endif
 */


   cHtml += 'class="col-12 form-control ' + cSize + IF( ::oParent:lDessign, ' tweb_dessign', '' )


   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   ENDIF

   IF !Empty( ::cFont )
      cHtml += ' ' + ::cFont
   ENDIF


   cHtml += '" '

   IF ::oParent:lDessign
      ::cStyle += "border:1px solid blue;"
   ENDIF

   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   IF !Empty( ::cProp )
      cHtml += ' ' + ::cProp + ' '
   ENDIF



   cHtml += 'id="' + cIdPrefix + ::cId + '" name="' + cIdPrefix + ::cId + '" '

   cHtml += ' data-live '

   IF !Empty( ::cAction )
      IF At( '(', ::cAction ) >  0   // Exist function ?
         cHtml += ' onchange="' + ::cAction + '" '
      ELSE
         cHtml += ' data-onchange="' + ::cAction + '" '
      ENDIF

   ENDIF

   IF ::lReadOnly
      cHtml += ' disabled '
   ENDIF


   cHtml += '>'


   FOR nI := 1 TO Len( ::aValues )

      IF ::lParKeyValue
         aPar := hb_HPairAt( ::aValues, nI )
      ELSE
         aPar := { ::aValues[ nI ], ::aItems[ nI ] }

      ENDIF

      cHtml += '<option value="' + aPar[ 1 ] + '" '

      IF ::uValue == aPar[ 1 ]
         cHtml += ' selected '
      ENDIF

      cHtml += '>' + aPar[ 2 ]
      cHtml += '</option>'

   NEXT

   cHtml += '</select>'

   cHtml += ' </div>'
   cHtml += '</div>'

// cHtml += ::Properties( cIdPrefix + ::cId, ::hProp )


   RETU cHtml
