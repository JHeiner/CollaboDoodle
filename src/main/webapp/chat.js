
function ChatAppend( user, command, nick, text ) {
	var array = [];
	function append( user, command, nick, text ) {
		array.push( [ user, command, nick, text ] ); }
	append( user, command, nick, text );
	append.Loaded = array;
	// do the above setup once, from then on just do append
	ChatAppend = append; }

function ChatCheck() {}

$(function(){

	function toBottom() {
		if ( ChatAutoScroll.checked )
			ChatHistory.scrollTop = ChatHistory.scrollHeight; }
	$(ChatAutoScroll).click(toBottom);

	var events = new Doodles.Events(ChatSVG,window);
	events.attach();
	var shapes = events.dom.shapes;
	ChatDoodles = { mine: events };

	var list = $(ChatList);
	var recognizer =
		/^doodle(?::(?:u|[dsm]-?\d+,-?\d+|[rh][ft][ft]\d+(,\d+)*))+$/;

	function delta( user, command, nick, text ) {
		var mine = ( user == ChatUser );
		var matched = recognizer.test( text );
		if ( ! command ) { if ( nick ) command = '<i>'; }
		else command = '<span class=left>'+command+'&nbsp;</span>';
		list.append(
			'<tr class='+( mine ? 'mine' : 'other' )+'>'+
				'<td class=right>'+user+
				'<td class=right>'+command+nick+
				'<td>'+( matched ? text.small() : text ) );
		toBottom();
		if ( ! matched ) return null;
		if ( mine ) return text;
		var doodles = ChatDoodles[user];
		if ( ! doodles ) {
			doodles = new Doodles.Events(ChatSVG,window);
			ChatSVG.insertBefore( doodles.dom.undoCTM, events.dom.undoCTM );
			doodles.color.normal = '#666';
			ChatDoodles[user] = doodles; }
		doodles.dom.shapes.interpret( text );
		return null; }

	function makePoint( x, y ) {
		return { x:x, y:y }; }
	function makeSegment( prefix, x, y ) {
		return prefix + x + ',' + y; }
	function pointToSegment( prefix, point ) {
		return makeSegment( prefix, point.x, point.y ); }
	function segmentToPoint( segment ) {
		var comma = segment.indexOf(',');
		var x = segment.substring( 1, comma );
		var y = segment.substring( comma + 1 );
		return makePoint( Number(x), Number(y) ); }

	var methods = {};
	methods[100]/*d*/ = function( segment ) {
		this.dot( segmentToPoint( segment ) ); };
	methods[115]/*s*/ = function( segment ) {
		var point = segmentToPoint( segment );
		this.currentLine = this.lineStart( point );
		this.previousPoint = point; };
	methods[109]/*m*/ = function( segment ) {
		var comma = segment.indexOf(',');
		var x = segment.substring( 1, comma );
		var y = segment.substring( comma + 1 );
		var point = this.previousPoint;
		point.x += Number(x); point.y += Number(y);
		this.lineMore( this.currentLine, point ); };
	methods[117]/*u*/ = function( segment ) {
		this.unhilight(); };
	methods[114]/*r*/ = function( segment ) {
		var action = (segment.charCodeAt(0)==114/*'r'*/) ?
			this.removeShape : this.hilightShape;
		var subsequentSiblings = (segment.charCodeAt(1)==116/*'t'*/);
		var sorted = (segment.charCodeAt(2)==116/*'t'*/);
		var list = segment.substring(3).split(',');
		var children = this.doodles;
		for ( var i = list.length - 1 ; i >= 0 ; -- i )
			list[i] = children[list[i]];
		this.forSelected(action,subsequentSiblings,sorted,list); };
	methods[104]/*h*/ = methods[114]/*r*/;

	Doodles.Shapes.prototype.interpret = function( text ) {
		if ( ! text ) return;
		var array = text.split(':');
		for ( var i = 1 ; i < array.length ; ++ i ) {
			var segment = array[i];
			var m = methods[segment.charCodeAt(0)];
			if ( m ) m.call( this, segment );
			else console.error( 'interpret', segment, this ); } }

	list.empty();
	if ( ChatAppend.Loaded )
		ChatAppend.Loaded.forEach( function( element, index, array ) {
			shapes.interpret( delta.apply( null, element ) ); });

	var timer = null;
	var buffer = '';
	var limit = 200;
	function push(segment) {
		buffer += ':'+segment;
		ChatCheck(); }
	function send() {
		if ( timer ) { clearTimeout( timer ); timer = null; }
		if ( buffer.length <= 0 ) return;
		var chop = ( buffer.length < limit ) ? buffer.length
			: buffer.lastIndexOf( ':', limit );
		ChatHidden.value = 'doodle' + buffer.substring(0,chop);
		buffer = buffer.substring(chop);
		ChatHidden.form.onsubmit(); }
	ChatCheck = function() {
		ChatHidden.value = '';
		if ( buffer.length >= limit ) { send(); return; }
		if ( timer ) { clearTimeout( timer ); timer = null; }
		if ( buffer.length > 0 ) timer = setTimeout( send, 500 ); }

	var lastPoint = null;
	var unhilighted = true;
	var lastHilighted = '';
	shapes.dot = function(center) {
		push( pointToSegment( 'd', center ) );
		Doodles.Shapes.prototype.dot.call(this,center); };
	// lineStart calls lineMore to insert the first point
	shapes.lineMore = function(line,more) {
		push( lastPoint
			? makeSegment( 'm', (more.x-lastPoint.x), (more.y-lastPoint.y) )
			: pointToSegment( 's', more ) );
		lastPoint = makePoint( more.x, more.y );
		Doodles.Shapes.prototype.lineMore.call(this,line,more); };
	events.pointer.pathEnd = function() {
		lastPoint = null;
		Doodles.Pointer.prototype.pathEnd.call(this); };
	shapes.unhilight = function() {
		if ( !unhilighted ) { push( 'u' ); unhilighted = true; }
		Doodles.Shapes.prototype.unhilight.call(this); };
	shapes.forSelected = function(action,subsequentSiblings,sorted,list) {
		if ( list.length > 0 ) {
			var remove = ( action == Doodles.Shapes.prototype.removeShape );
			var array = ''; var children = this.doodles;
			var i = (sorted && subsequentSiblings ? 0 : list.length - 1);
			for ( ; i >= 0 ; -- i ) {
				var at = Array.prototype.indexOf.call(children,list[i]);
				if (at < 0) throw new Error("non-child:"+list[i]);
				array = ','+at+array; }
			var segment = ( (remove?'r':'h')
				+ (subsequentSiblings?'t':'f') + (sorted?'t':'f')
				+ array.substring(1) );
			if ( remove ) push( segment ); else {
				if ( /:u$/.test(buffer) && lastHighlighted == array )
					buffer = buffer.substring( 0, buffer.length - 2 );
				else { push( segment ); lastHighlighted = array; }
				unhilighted = false; } }
		return Doodles.Shapes.prototype.forSelected.call(
			this,action,subsequentSiblings,sorted,list); };

	ChatAppend = function( user, command, nick, text ) {
		delta( user, command, nick, text ); } // note: no return

});
