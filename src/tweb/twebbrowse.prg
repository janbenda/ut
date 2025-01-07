// -------------------------------------------------------------

CLASS TWebBrowse FROM TWebControl

   DATA aCols       INIT {}
   DATA aData       INIT {}
   DATA aEvents     INIT {}
   DATA aFilter     INIT {}
   DATA cFilter_Id    INIT ''
   DATA hOptions     INIT { => }
   DATA cLabel      INIT ''
   DATA lAll      INIT .F.

   METHOD New()      CONSTRUCTOR
   METHOD Activate()

   METHOD AddCol( h )
   METHOD SetOptions( h )  INLINE ::hOptions :=  h
   METHOD Init( aData )

ENDCLASS

METHOD New( oParent, cId, hOptions, aEvents, aFilter, cFilter_Id, cLabel, lAll, cClass, cStyle, lHidden ) CLASS TWebBrowse

   DEFAULT cId TO ''
   DEFAULT hOptions TO { => }
   DEFAULT aFilter TO {}
   DEFAULT cFilter_Id TO ''
   DEFAULT cLabel TO ''
   DEFAULT lAll TO .F.
   DEFAULT cClass TO ''
   DEFAULT cStyle TO ''
   DEFAULT lHidden TO .F.

   ::oParent  := oParent
   ::cId   := cId
   ::hOptions  := hOptions
   ::aEvents  := aEvents
   ::aFilter  := aFilter
   ::cFilter_Id := cFilter_Id
   ::cLabel   := cLabel
   ::lAll    := lAll
   ::cClass   := cClass
   ::cStyle   := cStyle
   ::lHidden   := lHidden

   IF ValType( oParent ) == 'O'

      oParent:AddControl( SELF )

   ENDIF

   RETU SELF

METHOD Activate() CLASS TWebBrowse

   LOCAL cHtml  := ''
   LOCAL cChecked := ''
   LOCAL cIdPrefix, n, nPos
   LOCAL aNewFilter := {}
   LOCAL cSt   := ''

// Check Filter...

   FOR n := 1 TO Len( ::aFilter )

// Podemos tener una columna sin field p.e.
// COL oCol TO oBrw CONFIG { 'formatter' => "rowSelection", 'align' => "center", 'headerSort' => .F. }

      nPos := AScan( ::aCols, {| x | hb_HHasKey( x, 'field' ) .AND. x[ 'field' ] == ::aFilter[ n ] } )

      IF nPos > 0

         AAdd( aNewFilter, { ::aFilter[ n ],  ::aCols[ nPos ][ 'title' ] } )

      ENDIF

   NEXT

// --------------------

   IF !Empty( ::oParent:cId_Dialog )
      cIdPrefix :=  ::oParent:cId_Dialog + '-'
   ELSE
      cIdPrefix :=  ''
   ENDIF

   cHtml := '<div class="card '

   IF !Empty( ::cClass )
      cHtml += ::cClass
   ENDIF

   cHtml += '" '

   cSt := 'style="'
   IF ::lHidden
      cSt += 'display:none;'
   ENDIF

   IF ::oParent:lDessign
      cSt += "border:1px solid blue;"
   ENDIF

   cHtml += cSt + '" >'

   IF !Empty( ::cLabel )
      cHtml += '<div id="' + cIdPrefix + ::cId + '_title' + '" class="card-header">' + ::cLabel + '</div>'
   ENDIF


   IF !Empty( aNewFilter ) .AND. Empty( ::cFilter_Id )
      ::cFilter_Id := '_filter'
      cHtml += '  <div id="' + cIdPrefix + ::cFilter_Id + '" class="card-header"></div>'

   ENDIF

   cHtml += '  <div id="' + cIdPrefix + ::cId + '" data-live class="card-body--- tabulator" '

   IF ::lAll
      cHtml += '  data-all '
   ENDIF
   cHtml += '>'

   cHtml += ' </div>'

   cHtml += '</div>'




   cHtml += '<script>'
   cHtml += '$( document ).ready(function() {'

// cHtml += ' console.log( ' + hb_jsonEncode( ::aCols, .T. ) + '); '

   cHtml += ' var aCols =  ' + hb_jsonEncode( ::aCols, .T. ) + "; "
   cHtml += ' var Options =  ' + hb_jsonEncode( ::hOptions, .T. ) + "; "

   cHtml += " Options[ 'columns' ] = aCols;  "
   cHtml += ' UTabulatorValidOptions( Options ); '

   IF !Empty( ::aEvents )
      cHtml += ' var aEvents =  ' + hb_jsonEncode( ::aEvents, .T. ) + "; "
   ELSE
      cHtml += " var aEvents = null; "
   ENDIF


   IF !Empty( ::aData )
      cHtml += ' var aData =  ' + hb_jsonEncode( ::aData, .T. ) + "; "
      cHtml += " Options[ 'data' ] = aData;  "
   ENDIF

   IF !Empty( aNewFilter ) .AND. !Empty( ::cFilter_Id )
      cHtml += ' var oFilter = new Object(); '
      cHtml += " oFilter[ 'id']     = '" + cIdPrefix + ::cFilter_Id  + "'; "
      cHtml += " oFilter[ 'fields'] = " + hb_jsonEncode( aNewFilter, .T. ) + "; "
   ELSE
      cHtml += ' var oFilter =  null; '
   ENDIF

   cHtml += " var o = new UTabulator( '" + cIdPrefix + ::cId  + "' ); "
   cHtml += " o.Init( Options, aEvents, oFilter ); "

   cHtml += '}) '
   cHtml += '</script>'


   RETU cHtml

METHOD AddCol( h ) CLASS TWebBrowse

   AAdd( ::aCols, h )

   RETU NIL

METHOD Init( aData ) CLASS TWebBrowse

   hb_default( @aData, {} )

   IF !Empty( aData )
      ::aData := aData
   ENDIF


   RETU NIL
