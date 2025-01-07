// -------------------------------------------------------------

CLASS TWebGet FROM TWebControl

   DATA cType       INIT 'text'
   DATA cPlaceHolder     INIT ''
   DATA aBtnLabel      INIT {}
   DATA aBtnAction     INIT {}
   DATA aBtnId      INIT {}
   DATA uSource      INIT ''
   DATA cSelect      INIT ''
   DATA cLink       INIT ''
   DATA aSpan      INIT {}
   DATA aSpanId      INIT {}
   DATA hConfig      INIT { => }
   DATA hParam      INIT { => }
   DATA cBtnClass     INIT ''
   DATA lReturn     INIT .F.
   DATA cConfirm      INIT ''


   METHOD New()      CONSTRUCTOR
   METHOD Activate()

ENDCLASS

METHOD New( oParent, cId, uValue, nGrid, cLabel, cAlign, lReadOnly, cType, cPlaceHolder, aBtnLabel, aBtnAction, aBtnId, lRequired, uSource, cSelect, cChange, cClass, cFont, cFontLabel, cLink, cGroup, cDefault, aSpan, aSpanId, cStyle, cProp, lHidden, hConfig, hParam, cBtnClass, lReturn, cConfirm ) CLASS TWebGet

   DEFAULT cId TO ::GetId()
   DEFAULT uValue TO ''
   DEFAULT nGrid TO 4
   DEFAULT cLabel TO ''
   DEFAULT cAlign TO ''
   DEFAULT lReadOnly TO .F.
   DEFAULT cType TO 'text'
   DEFAULT cPlaceHolder TO ''
   DEFAULT aBtnLabel TO {}
   DEFAULT aBtnAction TO {}
   DEFAULT aBtnId   TO {}
   DEFAULT lRequired TO .F.
   DEFAULT uSource TO ''
   DEFAULT cSelect TO ''
   DEFAULT cChange TO ''
   DEFAULT cClass TO ''
   DEFAULT cFont TO ''
   DEFAULT cFontLabel TO ''
   DEFAULT cLink TO ''
   DEFAULT cGroup TO ''
   DEFAULT cDefault TO ''
   DEFAULT aSpan TO {}
   DEFAULT aSpanId TO {}
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT lHidden TO .F.
   DEFAULT hConfig TO { => }
   DEFAULT hParam TO { => }
   DEFAULT cBtnClass TO ''
   DEFAULT lReturn TO .F.
   DEFAULT cConfirm TO ''

   ::oParent   := oParent
   ::cId   := cId
   ::uValue  := uValue
   ::nGrid   := nGrid
   ::cLabel   := cLabel
   ::cAlign   := cAlign
   ::lReadOnly  := lReadOnly
   ::cType   := cType
   ::cPlaceHolder := cPlaceHolder
   ::aBtnLabel  := aBtnLabel
   ::aBtnAction := aBtnAction
   ::aBtnId  := aBtnId
   ::lRequired  := lRequired
   ::uSource   := uSource
   ::cSelect   := cSelect
   ::cChange   := cChange
   ::cClass   := cClass
   ::cFont   := cFont
   ::cFontLabel := cFontLabel
   ::cLink   := cLink
   ::cGroup  := cGroup
   ::cDefault  := cDefault
   ::aSpan   := aSpan
   ::aSpanId  := aSpanId
   ::cStyle  := cStyle
   ::cProp   := cProp
   ::lHidden  := lHidden
   ::hConfig  := hConfig
   ::hParam  := hParam
   ::cBtnClass := cBtnClass
   ::lReturn   := lReturn
   ::cConfirm   := cConfirm

   IF ValType( oParent ) == 'O'
      oParent:AddControl( SELF )
   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebGet

   LOCAL cHtml, hSource
   LOCAL cSize   := ''
   LOCAL cAlign   := ''
   LOCAL cSizeLabel := 'col-form-label'
   LOCAL cBtnSize   := ''
   LOCAL nI, nBtn, cLabel, cAction, cBtnId, nSpan, cGrid, cCmd
   LOCAL cIdPrefix, hParam, hConfig
   LOCAL cSt := ''

   DO CASE
   CASE ::cAlign == 'center' ; cAlign := 'text-center'
   CASE ::cAlign == 'right'  ; cAlign := 'text-right'
   ENDCASE

   DO CASE
   CASE Upper( ::oParent:cSizing ) == 'SM'
      cSize   := 'form-control-sm'
      cSizeLabel := 'col-form-label-sm'
      cBtnSize  := 'btn-sm'
   CASE Upper( ::oParent:cSizing ) == 'LG'
      cSize   := 'form-control-lg'
      cSizeLabel := 'col-form-label-lg'
      cBtnSize  := 'btn-lg'
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

// cHtml := '<div class="col-' + ltrim(str(::nGrid))
   cHtml := '<div class="col-' + cGrid

   cHtml += IF( ::oParent:lDessign, ' tweb_dessign', '' )
/*
 if !empty( ::cClass )
     cHtml += ' ' + ::cClass
 endif
*/

   cHtml += '" '

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

      cHtml += '<label class="' + cSizeLabel + ' ' + ::cFontLabel + ' " for="' + cIdPrefix + ::cId + '">' + ::cLabel + '</label>'

   ENDIF

   cHtml += '<div class="input-group">'

   cHtml += '<input data-control="tget" '

   IF !Empty( ::cGroup )
      cHtml += 'data-group="' + ::cGroup + '" '
   ENDIF

   IF !Empty( ::cDefault )
      cHtml += 'data-default="' + ::cDefault + '" '
   ENDIF



   cHtml += ' type="' + ::cType + '" class="form-control ' + cSize + ' ' + cAlign

   IF !Empty( ::cClass )
      cHtml += ' ' + ::cClass
   ENDIF

   IF !Empty( ::cFont )
      cHtml += ' ' + ::cFont
   ENDIF

   cHtml += '" '



   cHtml += 'id="' + cIdPrefix + ::cId + '"  name="' + cIdPrefix + ::cId + '" '

   cHtml += 'placeholder="' + ::cPlaceHolder + '" '

   IF ::lReadOnly
// cHtml += ' readonly '
      cHtml += ' disabled '
   ENDIF

   IF ::lRequired
      cHtml += ' required '
   ENDIF



   IF !Empty( ::cStyle )
      cHtml += ' style="' + ::cStyle + '" '
   ENDIF

   IF !Empty( ::cProp )
      cHtml += ' ' + ::cProp + ' '
   ENDIF

   cHtml += ' data-live '

   IF !Empty( ::cChange )
      IF At( '(', ::cChange ) >  0   // Exist function ?
         cHtml += ' onchange="' + ::cChange + '" '
      ELSE
         cHtml += ' data-onchange="' + ::cChange + '" '
      ENDIF
// cHtml += ' onfocusout="' + ::cChange + '" '
   ENDIF

   cHtml += ' value="' + ::uValue + '">'


   nSpan := Len( ::aSpan )

   IF nSpan > 0

      cHtml += '<div class="input-group-append">'

      FOR nI := 1 TO nSpan

         cHtml += '<span'

         IF Len( ::aSpanId ) == Len( ::aSpan )
            cHtml += ' id="' + cIdPrefix + ::aSpanId[ nI ] + '" '
         ENDIF

         cHtml += ' class="input-group-text" >' + ::aSpan[ nI ] + '</span>'

      NEXT

      cHtml += '</div>'

   ENDIF


   nBtn := Len( ::aBtnLabel )


   IF nBtn > 0

      cHtml += '<div class="input-group-append">'

      FOR nI := 1 TO nBtn

         cLabel  := ::aBtnLabel[ nI ]

         IF Len( ::aBtnId ) == nBtn
            cBtnId := cIdPrefix + ::aBtnId[ nI ]
         ELSE
            cBtnId := cIdPrefix + 'btn_' + ::cId + '_' + LTrim( Str( nI ) )
         ENDIF

         IF Empty( ::cBtnClass )
            ::cBtnClass := 'btn btn-outline-secondary '
         ENDIF

         cHtml += '<button id="' + cBtnId + '" class="' + ::cBtnClass + ' ' + cBtnSize + '" type="button" '

         IF Empty( ::cLink )

            IF ValType( ::aBtnAction ) == 'A' .AND. Len( ::aBtnAction ) >= nI
               cAction := ::aBtnAction[ nI ]

               IF At( '(', cAction ) >  0   // Exist function ?
                  cHtml += 'onclick="' + cAction + '" '
               ELSE
                  cHtml += 'data-live data-onclick="' + cAction + '" '
               ENDIF

               IF !Empty( ::cConfirm )
                  cHtml += ' data-confirm="' + ::cConfirm + '" '
               ENDIF

            ENDIF

         ELSE
            cHtml += 'style="border-radius: 0px 5px 5px 0px;" '
         ENDIF

         IF ::lReadOnly
            cHtml += ' disabled '
         ENDIF

         cHtml += ' >'

         IF !Empty( ::cLink )
            cHtml += '<a href="' + ::cLink + '" >'
         ENDIF

         cHtml += cLabel

         IF !Empty( ::cLink )
            cHtml += '</a>'
         ENDIF

         cHtml += '</button>'


      NEXT

      cHtml += '</div>'

   ENDIF





   cHtml += ' </div>'
   cHtml += '</div>'



/*
 DO CASE
  CASE hCtrl[ 'align' ] == 'center' ; cHtml += ' text-center '
  CASE hCtrl[ 'align' ] == 'right'  ; cHtml += ' text-right '
 ENDCASE
*/

// cHtml += ::Properties( cIdPrefix + ::cId, ::hProp )

   IF !Empty( ::uSource )

      cHtml += '<script>'
      cHtml += "$(document).ready( function() {"

      DO CASE

      CASE ValType( ::uSource ) == 'A' // Tabla Array

         hSource := hb_jsonEncode( ::uSource )

         cHtml += "  var _uSource = JSON.parse( '" + hSource + "' );"


      CASE ValType( ::uSource ) == 'C' // Url

         cHtml += "  var _uSource = '" + ::uSource + "';"

      ENDCASE

      IF !Empty( ::hConfig )

         hConfig := hb_jsonEncode( ::hConfig )

         cHtml += "  var _hConfig = JSON.parse( '" + hConfig + "' );"

      ELSE

         cHtml += "  var _hConfig = null;"

      ENDIF

      IF !Empty( ::hParam )

         hParam := hb_jsonEncode( ::hParam )

         cHtml += "  var _hParam = JSON.parse( '" + hParam + "' );"

      ELSE

         cHtml += "  var _hParam = null;"

      ENDIF

      IF Empty( ::cSelect )
         ::cSelect := 'null'
      ENDIF

      cHtml += "   var _oGAC = new TWebGetAutocomplete( '" + cIdPrefix + ::cId + "', _uSource, '" + ::cSelect + "', _hConfig,  _hParam ); "
      cHtml += "_oGAC.Init(); "

      cHtml += '})'
      cHtml += '</script>'

   ENDIF



   IF nBtn > 0 .AND. ::lReturn

      IF Len( ::aBtnId ) == nBtn
         cBtnId := cIdPrefix + ::aBtnId[ 1 ]
      ELSE
         cBtnId := cIdPrefix + 'btn_' + ::cId + '_1'
      ENDIF

      cCmd := "<script>"
      cCmd += "TWebIntroGetBtn( '" + cIdPrefix + ::cId + "', '" + cBtnID + "'  )"
      cCmd += "</script>"

// cHtml += JS( cCmd )
      cHtml += cCmd

   ENDIF



   RETU cHtml
