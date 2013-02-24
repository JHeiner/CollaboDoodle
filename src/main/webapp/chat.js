
function ChatAppend( user, text, extra ) {
	var array = [];
	function append( user, text, extra ) {
		if ( ! extra ) array.push( [ user, text ] )
		else array.push( [ user, text, extra ] ); }
	append( user, text, extra );
	append.cache = array;
	// do the above setup once, from then on just do append
	ChatAppend = append; }

function ChatCheck() {}

$(function(){

	function toBottom() {
		if ( ChatAutoScroll.checked )
			ChatHistory.scrollTop = ChatHistory.scrollHeight; }
	$(ChatAutoScroll).click(toBottom);

	// call 'delta' to process any incoming chat traffic.
	// at page load time the chat history is delivered inside the HTML,
	// and 'ChatAppend' above gathers the deltas so we can replay them.
	// from that point on deltas come from the server via ajax.

	var list = $(ChatList);
	var data = {};

	var events = new Doodles.Events(ChatSVG,window);
	var shapes = events.dom.shapes;

	var isColor = /%#?[0-9A-Za-z]+$/;
	var isDoodle =
		/^doodle(?::(?:u|[dsm]-?\d+,-?\d+|[rh][ft][ft]\d+(,\d+)*))+$/;

	function delta( user, text, extra ) {
		if ( extra == "/time" && user == "0") {
			for ( var info in data )
				if ( (info != ChatUser) && data[info].events ) {
console.log('reset',info,data[info]);
					data[info].events.destroy(); }
			data = {}; data[ChatUser] = { events: events };
			$(shapes.svg).empty();
			list.empty(); }
		var mine = ( user == ChatUser );
		var info = data[user];
		var nick = info ? info.nick : null; if ( ! nick ) nick = '';
		var color = info ? info.color : null;
		switch ( extra ) {
		case '/nick':
			nick = text.indexOf('\u27a1');
			if ( ! info ) info = data[user] = {};
			info.nick = nick = text.substring(nick+2);
			color = isColor.exec( nick );
			if ( ! color ) {
				delete info.color; color = mine ? '#000' : '#666'; }
			else {
				info.nick = nick = nick.substring(0,color.index);
				info.color = color = color[0].substring(1); }
			if ( info.events ) info.events.color.normal = color;
			break;
		case '/kick':
			var baddie = text.split(' ')[0];
			if (data[baddie]) {
				baddie = data[baddie]; delete data[baddie];
				if ( baddie.events ) baddie.events.destroy();
				if ( events == baddie.events ) {
					ChatInput.disabled = true; buffer = '';
					events = shapes = null; } } }
		var matched = isDoodle.test( text );
		extra = extra ? '<span class=ChatCommand>'+extra+'</span>' : nick;
		list.append(
			'<tr class='+( mine ? 'mine' : 'other' )
				+( color ? ' style=color:'+color : '' )+'>'
				+'<td class="col1">'+user
				+'<td class="col2">'+extra
				+'<td class="col3">'+( matched ? text.small() : text ) );
		toBottom();
		if ( ! matched ) return null;
		if ( mine ) return text;
		if ( ! info ) info = data[user] = {}; // never [nick] if matched
		if ( ! info.events ) {
			info.events = new Doodles.Events(ChatSVG,window);
			ChatSVG.insertBefore( info.events.dom.undoCTM, events.dom.undoCTM );
			info.events.color.normal = color ? color : '#666'; }
		info.events.dom.shapes.interpret( text );
		return null; }

	// use 'send' to send a doodle segment to the server.
	// it takes care of buffering them: if a bunch happen all at once
	// we bundle them into packets instead of sending individual segments.

	var timer = null;
	var buffer = '';
	var limit = 200;
	function send(segment) {
		buffer += ':'+segment;
		ChatCheck(); }
	function sendNow() {
		if ( timer ) { clearTimeout( timer ); timer = null; }
		if ( buffer.length <= 0 ) return;
		var chop = ( buffer.length < limit ) ? buffer.length
			: buffer.lastIndexOf( ':', limit );
		ChatHidden.value = 'doodle' + buffer.substring(0,chop);
		buffer = buffer.substring(chop);
		ChatHidden.form.onsubmit(); }
	ChatCheck = function() {
		ChatHidden.value = '';
		if ( buffer.length >= limit ) { sendNow(); return; }
		if ( timer ) { clearTimeout( timer ); timer = null; }
		if ( buffer.length > 0 ) timer = setTimeout( sendNow, 500 ); }

	// the interpreter chops up packets of doodle segments,
	// then dispatches each segment to a method that handles that segment type.
	// each handler method makes calls to the appropriate Doodles function.

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

	// now that interpretation is all set up we can replay the history.
	// note that we call shapes.interpret to do the local user's history.
	// delta normally ignores the local user's doodling commands.
	// important that we not see this replay as the user really doodling.

	if ( ChatAppend.cache )
		ChatAppend.cache.forEach( function( element, index, array ) {
			shapes.interpret( delta.apply( null, element ) ); });

	// done with the history replay, so now we put our hooks into our
	// local Doodles objects to intercept real doodling, format the event
	// data into segments, and send those to the server.

	var lastPoint = null;
	var unhilighted = true;
	var lastHilighted = '';
	shapes.dot = function(center) {
		send( pointToSegment( 'd', center ) );
		Doodles.Shapes.prototype.dot.call(this,center); };
	// lineStart calls lineMore to insert the first point
	shapes.lineMore = function(line,more) {
		send( lastPoint
			? makeSegment( 'm', (more.x-lastPoint.x), (more.y-lastPoint.y) )
			: pointToSegment( 's', more ) );
		lastPoint = makePoint( more.x, more.y );
		Doodles.Shapes.prototype.lineMore.call(this,line,more); };
	events.pointer.pathEnd = function() {
		lastPoint = null;
		Doodles.Pointer.prototype.pathEnd.call(this); };
	shapes.unhilight = function() {
		if ( !unhilighted ) { send( 'u' ); unhilighted = true; }
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
			if ( remove ) send( segment ); else {
				if ( /:u$/.test(buffer) && lastHighlighted == array )
					buffer = buffer.substring( 0, buffer.length - 2 );
				else { send( segment ); lastHighlighted = array; }
				unhilighted = false; } }
		return Doodles.Shapes.prototype.forSelected.call(
			this,action,subsequentSiblings,sorted,list); };

	// almost done, just need to hook up our event handlers...

	ChatAppend = function( user, command, nick, text ) {
		delta( user, command, nick, text ); } // note: no return

	events.attach();

});
