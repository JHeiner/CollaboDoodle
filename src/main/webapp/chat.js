
function ChatAppend( user, command, nick, text ) {
    var array = [];
    function append( user, command, nick, text ) {
    	array.push( [ user, command, nick, text ] ); }
    append( user, command, nick, text );
    append.Loaded = array;
    // do the above setup once, from then on just do append
    ChatAppend = append; }

$(function(){

    var loaded = ChatAppend.Loaded;
    var list = $(ChatList);

    var input = {
 	value:"", start:0, end:0, element:ChatInput,
	submit:function(temporary) {
	    this.value = this.element.value;
	    this.start = this.element.selectionStart;
	    this.end = this.element.selectionEnd;
	    this.element.value = temporary;
	    this.element.form.onsubmit(); },
	restore:function() {
	    this.element.value = this.value;
	    this.element.setSelectionRange( this.start, this.end );
	    this.value = ""; this.start = this.end = 0; } };

    var draw = {
	timer:undefined, buffer:[], line:undefined,
	push:function(shape) {
    	    this.buffer.push( shape );
    	    if ( this.timer ) clearTimeout( this.timer );
    	    this.timer = setTimeout( this.send, 1000 ); },
	sendUnbound:function() {
    	    this.timer = undefined;
	    input.submit( '['+this.buffer.join(',')+']' );
    	    this.buffer.length = 0; } };
    draw.send = draw.sendUnbound.bind(draw);

    var events = new Doodles.Events(ChatSVG,window);
    events.attach();

    function toBottom() {
      	if ( ChatAutoScroll.checked )
            ChatHistory.scrollTop = ChatHistory.scrollHeight; }

    $(ChatAutoScroll).click(toBottom);

    ChatDoodles = { mine: events };

    function interpret(user,text) { try {
	text = text.replace(/&quot;/g,'"');
	var json = JSON.parse(text);
	var doodles = ChatDoodles[user];
	if ( ! doodles )
	    doodles = ChatDoodles[user] = new Doodles.Events(ChatSVG,window);
	var line;
	json.forEach(function(element,index,array){
	    switch(element[0]) {
	    case "d":
 		doodles.dom.shapes.dot({x:element[1],y:element[2]});
		break;
	    case "s":
		line = undefined;
		break;
	    case "m":
		if (!line)
		    line = doodles.dom.shapes.lineStart({x:element[1],y:element[2]});
		else doodles.dom.shapes.lineMore(line,{x:element[1],y:element[2]});
		break; } });
	} catch ( e ) { console.log(e,text); } }


    // change again to be what should happen for deltas...
    ChatAppend = function( user, command, nick, text ) {
	var mine = ( user == ChatUser );
	var small = ( text.charAt(0) == '[' );
    	if ( !command ) { if ( nick ) command = '<i>'; }
    	else command = '<span class=left>'+command+'&nbsp;</span>';
    	list.append(
            '<tr class='+( mine ? 'mine' : 'other' )+'>'+
            	'<td class=right>'+user+
            	'<td class=right>'+command+nick+
            	'<td>'+( small ? text.small() : text ) );
	if ( small && ! mine ) interpret( user, text );
    	toBottom();
	input.restore(); }

    ChatDoodles[ChatUser] = events;

    list.empty();
    if (loaded) loaded.forEach(function(element,index,array){
	ChatAppend.apply(null,element);
	var user = element[0];
	var text = element[3];
	var mine = ( user == ChatUser );
	var small = ( text.charAt(0) == '[' );
	if (small && mine ) interpret(user,text); });
    loaded = undefined;

    events.dom.shapes.dot = function(center) {
        draw.push( '["d",'+center.x+','+center.y+']' );
        this.__proto__.dot.call(this,center); }
    events.dom.shapes.lineMore = function(line,more) {
        if ( line != draw.line ) {
            draw.push( '"s"' );
            draw.line = line; }
        draw.push( '["m",'+more.x+','+more.y+']' );
        this.__proto__.lineMore.call(this,line,more); }

});
