CodeMirror.defineMode("smalltalk", function(config, parserConfig) {
  var indentUnit = config.indentUnit;

  function chain(stream, state, f) {
    state.tokenize = f;
    return f(stream, state);
  }

  var type;
  function ret(tp, style) {
    type = tp;
    return style;
  }

  function tokenBase(stream, state) {
	var identifierTest = /[A-Za-z0-9_]/;
    var ch = stream.next();
	if (/[A-Za-z]/.test(ch) || (ch === '_' && identifierTest.test(stream.peek()))) {
		stream.eatWhile(identifierTest);
		if (!stream.eol() && stream.match(':')) {
			if (!state.context.keywordSelector) {
				state.context.keywordSelector = stream.current();
			} else {
				state.context.keywordSelector += stream.current();
			}
			return ret('word', 'st-keyword');
		}
	    var current = stream.current();
	    if (current === 'nil') { return ret('word', 'st-nil'); };
	    if (current === 'self') { return ret('word', 'st-self'); };
	    if (current === 'super') { return ret('word', 'st-super'); };
	    if (current === 'thisContext') { return ret('word', 'st-thisContext'); };
	    if (current === 'true' || current === 'false') { return ret('word', 'st-boolean'); };
		if (/^[A-Z]/.test(current)) { return ret('word', 'st-global'); };
		return ret('word', 'st-identifier'); } 
	else if (ch === '.') { 
		state.context.keywordSelector = null;
		return ret('other', 'st-period'); }
	else if (ch === '$') {
		if (stream.eol()) return ret('error', 'error');
		stream.next();
		return ret('character', 'st-character'); }
	else if (ch === '^') return ret('character', 'st-return');
	else if (ch === '"')
      return chain(stream, state, tokenString(ch, 'comment', 'st-comment'));
	else if (ch === "'")
      return chain(stream, state, tokenString(ch, 'string', 'st-string'));
    else if (ch == "#") {
	  if (stream.eat("'")) {
	    return chain(stream, state, tokenString( "'", 'string', 'st-symbol'));
	  }
      stream.eatWhile(/[A-Za-z0-9_:]/);
      return ret("string", "st-symbol"); }
    else if (/\d/.test(ch)) {
      stream.eatWhile(/[\w\.]/)
      return ret("number", "st-number"); }
    else if (/[\[\](){}]/.test(ch)) {
      return ret(ch, null); }
	else if (stream.eatWhile('[\/\[\+\-\\\*\~\<\>\=\|\/\&\@\%\,\?\!]')) { 
		return ret('selector', 'st-binary'); }
    else {
      stream.eatWhile(/[\w\$_]/);
      return ret("other", "st-other");
    }
  }

  function tokenString(endChar, tp, style) {
    return function(stream, state) {
      while (true) {
	    if (stream.eol()) return ret(tp, style);
		if (stream.next() === endChar && !stream.eat(endChar)) {
			state.tokenize = tokenBase;
			return ret(tp, style);
		}
      }
    };
  }

  function Context(indented, column, type, align, prev) {
    this.indented = indented;
    this.column = column;
    this.type = type;
    this.align = align;
    this.prev = prev;
  }

  function pushContext(state, col, type) {
    return state.context = new Context(state.indented, col, type, null, state.context);
  }
  function popContext(state) {
    return state.context = state.context.prev;
  }

  // Interface

  return {
	compareStates: function(x, y) { return false; },	// force full parse to get keyword selectors that span multiple lines

    startState: function(basecolumn) {
      return {
        tokenize: tokenBase,
        context: new Context((basecolumn || 0) - indentUnit, 0, "top", false),
        indented: 0,
        startOfLine: true
      };
    },

    token: function(stream, state) {
      var ctx = state.context;
      if (stream.sol()) {
        if (ctx.align == null) ctx.align = false;
        state.indented = stream.indentation();
        state.startOfLine = true;
      }
      if (stream.eatSpace()) return null;
      var style = state.tokenize(stream, state);
      if (type == "comment") return style;
      if (ctx.align == null) ctx.align = true;

      if (type == "[") pushContext(state, stream.column(), "]");
      else if (type == "(") pushContext(state, stream.column(), ")");
      else if (type == "{") pushContext(state, stream.column(), "}");
      else if (type == ctx.type) popContext(state);
      state.startOfLine = false;
      return style;
    },

    indent: function(state, textAfter) {
      if (state.tokenize != tokenBase) return 0;
      var firstChar = textAfter && textAfter.charAt(0), ctx = state.context, closing = firstChar == ctx.type;
      if (ctx.align) return ctx.column + (closing ? 0 : 1);
      else return ctx.indented + (closing ? 0 : indentUnit);
    },

    electricChars: "]"
  };
});

CodeMirror.defineMIME("text/x-stsrc", {name: "smalltalk"});
