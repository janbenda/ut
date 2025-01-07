// -------------------------------------------------------------

/*
 The Bootstrap grid system has four classes:

  xs (for phones - screens less than 768px wide)
  sm (for tablets - screens equal to or greater than 768px wide)
  md (for small laptops - screens equal to or greater than 992px wide)
  lg (for laptops and desktops - screens equal to or greater than 1200px wide)

 Controls
  form-group-sm/lg  (get,select)
  btn-sm/lg   (button)
*/



CLASS TWebForm FROM TWebControl

   DATA oWeb      INIT {}
// DATA aControls     INIT {}
   DATA cId      INIT ''
   DATA cId_Dialog    INIT ''
   DATA cAction     INIT ''
   DATA cMethod     INIT 'POST'
   DATA cApi      INIT ''
   DATA cProc      INIT ''
   DATA lFluid     INIT .F.
   DATA lHttpd2Api    INIT .F.
   DATA lActivated     INIT .F.

// DATA cType      INIT ''  // sm, md, lg, xl, xs
// DATA cSizing     INIT ''  // sm, lg

   METHOD New()      CONSTRUCTOR

   METHOD AddControl( uValue )  INLINE AAdd( ::aControls, uValue )
   METHOD InitForm()
   METHOD Activate()


   METHOD Col()
   METHOD Row()
   METHOD RowGroup()


 /* to TWebControl
 METHOD Html( cCode )    INLINE Aadd( ::aControls, cCode )
 METHOD End()      INLINE ::Html( '</div>' + CRLF  )


 METHOD Html( cCode )    INLINE Aadd( ::aControls, cCode )


 METHOD Caption()
 METHOD Separator()
 METHOD Small()
 METHOD Echo()
 */


   METHOD Div()
   METHOD End()      INLINE ::Html( '</div>' + CRLF  )

ENDCLASS


METHOD New( oWeb, cId, cAction, cMethod, cApi, cProc ) CLASS TWebForm

   DEFAULT cId  TO ''
   DEFAULT cAction TO ''
   DEFAULT cMethod TO 'POST'
   DEFAULT cApi TO ''
   DEFAULT cProc TO ''

   ::oWeb    := oWeb
   ::cId    := cId
   ::cId_Dialog := cId
   ::cAction   := cAction
   ::cMethod   := cMethod
   ::cApi    := cApi
   ::cProc   := cProc

   ::oWeb:AddControl( SELF )

   RETU SELF

METHOD InitForm( cClass ) CLASS TWebForm

   LOCAL cClassForm := IF( ::lFluid, 'container-fluid', 'container' )

   DEFAULT cClass TO ''

   ::Html( '<div class="' + cClassForm + ' ' + cClass + '" ' + ;
      IF( ::lDessign, 'style="border:2px solid green;"', '' ) + ;
      IF( !Empty( ::cId ), ' id="' + ::cId + '" ', '' ) + ;
      IF( !Empty( ::cApi ), ' data-dialog data-api="' + ::cApi + '" ', '' ) + ;
      IF( !Empty( ::cProc ), ' data-oninit="' + ::cProc + '" ', '' ) + ;
      '>' + CRLF   )

   IF !Empty( ::cId ) .AND. !Empty( ::cApi )
      ::lHttpd2Api := .T.
   ENDIF

   IF !Empty( ::cAction )

      ::Html( '<form action="' + ::cAction + '" method="' + ::cMethod + '">'  + CRLF  )

   ENDIF


   RETU NIL

METHOD Col( cId, nCol, cType, cClass, cStyle, lHidden ) CLASS TWebForm

   LOCAL cHtml := ''
   LOCAL cPrefix := IF( Empty( ::cType ), '', ::cType + '-' )

   DEFAULT nCol TO 12
   DEFAULT cType TO ''
   DEFAULT cClass TO ''
   DEFAULT cId TO ''
   DEFAULT cStyle TO ''
   DEFAULT lHidden TO .F.

   IF !Empty( cType )
      cPrefix := cType + '-'
   ELSE
      cPrefix := IF( Empty( ::cType ), '', ::cType + '-' )
   ENDIF


// Si ponemos e- -sm, responsive y pone 1 debajo de otro...
// ::Html ( '<div class="col-sm-' + ltrim(str(nCol)) + '"' + IF( ::lDessign, 'style="border:1px solid blue;"', '' ) + '>' )

   cHtml := '<div '

   IF !Empty( cId )
      cHtml += 'id="' + ::cId_Dialog + '-' + cId + '" '
   ENDIF


   cHtml += ' class="col-' + cPrefix + LTrim( Str( nCol ) )

   IF !Empty( cClass )
      cHtml += ' ' + cClass
   ENDIF

   cHtml += '" '

   IF ::lDessign
      cStyle +=  ';border:1px solid blue;'
   ENDIF

   IF lHidden
      cStyle +=  'display:none;'
   ENDIF

   IF !Empty( cStyle )
      cHtml += ' style="' + cStyle + '" '
   ENDIF

   cHtml += '>' + CRLF

// ::Html ( '<div class="col-' + cPrefix + ltrim(str(nCol)) + '"' + IF( ::lDessign, 'style="border:1px solid blue;"', '' ) + '>' )
   ::Html ( cHtml )

   RETU NIL

METHOD Div( cId, cClass, cStyle, cProp, cCode, lHidden ) CLASS TWebForm

   LOCAL cHtml := ''
   LOCAL cId_Dialog := ''

   DEFAULT cId TO ''
   DEFAULT cClass TO ''
   DEFAULT cStyle TO ''
   DEFAULT cProp TO ''
   DEFAULT cCode TO ''
   DEFAULT lHidden TO .F.

   IF !Empty( ::cId_Dialog )
      cId_Dialog := ::cId_Dialog + '-'
   ENDIF


   cHtml += '<div id="' + cId_Dialog + cId + '" '

   IF !Empty( cClass )
      cHtml += ' class="' + cClass + '" '
   ENDIF

   IF ::lDessign
      cStyle += ";border:1px solid red;"
   ENDIF

   IF lHidden
      cStyle +=  'display:none;'
   ENDIF

   IF !Empty( cStyle )
      cHtml += ' style="' + cStyle + '" '
   ENDIF

   IF !Empty( cProp )
      cHtml += ' ' + cProp + ' '
   ENDIF


   cHtml += '>'  + CRLF
   cHtml += cCode + CRLF

   ::Html( cHtml )

   RETU NIL

METHOD Row( cId, cVAlign, cHAlign, cClass, cTop, cBottom, lHidden ) CLASS TWebForm

   LOCAL cHtml := ''

   DEFAULT cId TO ''
   DEFAULT cVAlign TO 'center'
   DEFAULT cHAlign TO 'left'
   DEFAULT cClass TO ''
   DEFAULT cTop TO ''
   DEFAULT cBottom TO ''
   DEFAULT lHidden TO .F.

   cVAlign  := Lower( cVAlign )
   cHAlign  := Lower( cHAlign )

   DO CASE
   CASE cVAlign == 'top'   ; cVAlign := 'align-items-start'
   CASE cVAlign == 'center'  ; cVAlign := 'align-items-center'
   CASE cVAlign == 'bottom'  ; cVAlign := 'align-items-end'
   ENDCASE

   DO CASE
   CASE cHAlign == 'left'   ; cHAlign := 'justify-content-start'
   CASE cHAlign == 'center' ; cHAlign := 'justify-content-center'
   CASE cHAlign == 'right'  ; cHAlign := 'justify-content-end'
   ENDCASE


   cHtml += '<div id="' + cId + '" class="row ' + cVAlign + ' ' + cHAlign

   IF !Empty( cClass )
      cHtml += ' ' + cClass
   ENDIF

   cHtml += '" '  // End class

   cHtml += ' style="'

   cHtml += IF( ::lDessign, 'border:1px solid red;', '' )

   IF lHidden
      cHtml +=  'display:none;'
   ENDIF

   IF !Empty( cTop )
      cTop := mh_valtochar( cTop )
      cTop := 'margin-top: ' + cTop + ';'
   ENDIF

   IF !Empty( cBottom )
      cBottom := mh_valtochar( cBottom )
      cBottom := 'margin-bottom: ' + cBottom + ';'
   ENDIF

   cHtml += cTop + cBottom + '" '  // End Style

   cHtml += '>'  + CRLF

   ::Html( cHtml )

   RETU NIL

METHOD RowGroup( cId, cVAlign, cHAlign, cClass, cStyle, lHidden ) CLASS TWebForm

   LOCAL cHtml := ''

   DEFAULT cId  TO ''
   DEFAULT cVAlign TO 'center'
   DEFAULT cHAlign TO 'left'
   DEFAULT cClass TO ''
   DEFAULT cStyle TO ''
   DEFAULT lHidden TO .F.

   cVAlign  := Lower( cVAlign )
   cHAlign  := Lower( cHAlign )

   DO CASE
   CASE cVAlign == 'top'   ; cVAlign := 'align-items-start'
   CASE cVAlign == 'center'  ; cVAlign := 'align-items-center'
   CASE cVAlign == 'bottom'  ; cVAlign := 'align-items-end'
   ENDCASE

   DO CASE
   CASE cHAlign == 'left'   ; cHAlign := 'justify-content-start'
   CASE cHAlign == 'center' ; cHAlign := 'justify-content-center'
   CASE cHAlign == 'right'  ; cHAlign := 'justify-content-end'
   ENDCASE


   cHtml += '<div '

   IF !Empty( cId )
      cHtml += 'id="' + ::cId_Dialog + '-' + cId + '" '
   ENDIF

   cHtml += ' class="form-group row ' + cVAlign + ' ' + cHAlign

   IF !Empty( cClass )
      cHtml += ' ' + cClass
   ENDIF

   cHtml += '" '

   IF ::lDessign
      cStyle +=  ';border:1px solid red;'
   ENDIF

   IF lHidden
      cStyle +=  'display:none;'
   ENDIF

   IF !Empty( cStyle )
      cHtml += ' style="' + cStyle + '" '
   ENDIF

   cHtml += ' >'  + CRLF

   ::Html( cHtml )



   RETU NIL

METHOD Activate( fOnInit ) CLASS TWebForm

   LOCAL cHtml := ''
   LOCAL nI, c

   DEFAULT fOnInit TO ''

   IF !Empty( ::cAction )

      ::Html( '</form>' + CRLF )

   ENDIF

   ::Html( '</div>' + CRLF )

   IF !Empty( fOnInit )

      ::Html( JSReady( fOnInit ) )

   ENDIF


   IF ::lHttpd2API

      c := 'UInitDialog("' + ::cId + '");'
      ::Html( JSReady( c ) )

   ELSE

      ::Html( JSReady( "console.warn( 'Warning: Form without id or api')" ) )

   ENDIF



   FOR nI := 1 TO Len( ::aControls )

      IF ValType( ::aControls[ nI ] ) == 'O'
         cHtml += ::aControls[ nI ]:Activate()
      ELSE
         cHtml += ::aControls[ nI ]
      ENDIF

   NEXT

   RETU cHtml
