(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS
//
// For some reason, tabs can appear in href protocols and it still works.
// So '\tjava\tSCRIPT:alert("!!!")' and 'javascript:alert("!!!")' are the same
// in practice. That is why _VirtualDom_RE_js and _VirtualDom_RE_js_html look
// so freaky.
//
// Pulling the regular expressions out to the top level gives a slight speed
// boost in small benchmarks (4-10%) but hoisting values to reduce allocation
// can be unpredictable in large programs where JIT may have a harder time with
// functions are not fully self-contained. The benefit is more that the js and
// js_html ones are so weird that I prefer to see them near each other.


var _VirtualDom_RE_script = /^script$/i;
var _VirtualDom_RE_on_formAction = /^(on|formAction$)/i;
var _VirtualDom_RE_js = /^\s*j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:/i;
var _VirtualDom_RE_js_html = /^\s*(j\s*a\s*v\s*a\s*s\s*c\s*r\s*i\s*p\s*t\s*:|d\s*a\s*t\s*a\s*:\s*t\s*e\s*x\s*t\s*\/\s*h\s*t\s*m\s*l\s*(,|;))/i;


function _VirtualDom_noScript(tag)
{
	return _VirtualDom_RE_script.test(tag) ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return _VirtualDom_RE_on_formAction.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return _VirtualDom_RE_js.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return _VirtualDom_RE_js_html.test(value)
		? /**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlJson(value)
{
	return (typeof _Json_unwrap(value) === 'string' && _VirtualDom_RE_js_html.test(_Json_unwrap(value)))
		? _Json_wrap(
			/**_UNUSED/''//*//**/'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'//*/
		) : value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}


// BYTES

function _Bytes_width(bytes)
{
	return bytes.byteLength;
}

var _Bytes_getHostEndianness = F2(function(le, be)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(new Uint8Array(new Uint32Array([1]))[0] === 1 ? le : be));
	});
});


// ENCODERS

function _Bytes_encode(encoder)
{
	var mutableBytes = new DataView(new ArrayBuffer($elm$bytes$Bytes$Encode$getWidth(encoder)));
	$elm$bytes$Bytes$Encode$write(encoder)(mutableBytes)(0);
	return mutableBytes;
}


// SIGNED INTEGERS

var _Bytes_write_i8  = F3(function(mb, i, n) { mb.setInt8(i, n); return i + 1; });
var _Bytes_write_i16 = F4(function(mb, i, n, isLE) { mb.setInt16(i, n, isLE); return i + 2; });
var _Bytes_write_i32 = F4(function(mb, i, n, isLE) { mb.setInt32(i, n, isLE); return i + 4; });


// UNSIGNED INTEGERS

var _Bytes_write_u8  = F3(function(mb, i, n) { mb.setUint8(i, n); return i + 1 ;});
var _Bytes_write_u16 = F4(function(mb, i, n, isLE) { mb.setUint16(i, n, isLE); return i + 2; });
var _Bytes_write_u32 = F4(function(mb, i, n, isLE) { mb.setUint32(i, n, isLE); return i + 4; });


// FLOATS

var _Bytes_write_f32 = F4(function(mb, i, n, isLE) { mb.setFloat32(i, n, isLE); return i + 4; });
var _Bytes_write_f64 = F4(function(mb, i, n, isLE) { mb.setFloat64(i, n, isLE); return i + 8; });


// BYTES

var _Bytes_write_bytes = F3(function(mb, offset, bytes)
{
	for (var i = 0, len = bytes.byteLength, limit = len - 4; i <= limit; i += 4)
	{
		mb.setUint32(offset + i, bytes.getUint32(i));
	}
	for (; i < len; i++)
	{
		mb.setUint8(offset + i, bytes.getUint8(i));
	}
	return offset + len;
});


// STRINGS

function _Bytes_getStringWidth(string)
{
	for (var width = 0, i = 0; i < string.length; i++)
	{
		var code = string.charCodeAt(i);
		width +=
			(code < 0x80) ? 1 :
			(code < 0x800) ? 2 :
			(code < 0xD800 || 0xDBFF < code) ? 3 : (i++, 4);
	}
	return width;
}

var _Bytes_write_string = F3(function(mb, offset, string)
{
	for (var i = 0; i < string.length; i++)
	{
		var code = string.charCodeAt(i);
		offset +=
			(code < 0x80)
				? (mb.setUint8(offset, code)
				, 1
				)
				:
			(code < 0x800)
				? (mb.setUint16(offset, 0xC080 /* 0b1100000010000000 */
					| (code >>> 6 & 0x1F /* 0b00011111 */) << 8
					| code & 0x3F /* 0b00111111 */)
				, 2
				)
				:
			(code < 0xD800 || 0xDBFF < code)
				? (mb.setUint16(offset, 0xE080 /* 0b1110000010000000 */
					| (code >>> 12 & 0xF /* 0b00001111 */) << 8
					| code >>> 6 & 0x3F /* 0b00111111 */)
				, mb.setUint8(offset + 2, 0x80 /* 0b10000000 */
					| code & 0x3F /* 0b00111111 */)
				, 3
				)
				:
			(code = (code - 0xD800) * 0x400 + string.charCodeAt(++i) - 0xDC00 + 0x10000
			, mb.setUint32(offset, 0xF0808080 /* 0b11110000100000001000000010000000 */
				| (code >>> 18 & 0x7 /* 0b00000111 */) << 24
				| (code >>> 12 & 0x3F /* 0b00111111 */) << 16
				| (code >>> 6 & 0x3F /* 0b00111111 */) << 8
				| code & 0x3F /* 0b00111111 */)
			, 4
			);
	}
	return offset;
});


// DECODER

var _Bytes_decode = F2(function(decoder, bytes)
{
	try {
		return $elm$core$Maybe$Just(A2(decoder, bytes, 0).b);
	} catch(e) {
		return $elm$core$Maybe$Nothing;
	}
});

var _Bytes_read_i8  = F2(function(      bytes, offset) { return _Utils_Tuple2(offset + 1, bytes.getInt8(offset)); });
var _Bytes_read_i16 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 2, bytes.getInt16(offset, isLE)); });
var _Bytes_read_i32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getInt32(offset, isLE)); });
var _Bytes_read_u8  = F2(function(      bytes, offset) { return _Utils_Tuple2(offset + 1, bytes.getUint8(offset)); });
var _Bytes_read_u16 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 2, bytes.getUint16(offset, isLE)); });
var _Bytes_read_u32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getUint32(offset, isLE)); });
var _Bytes_read_f32 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 4, bytes.getFloat32(offset, isLE)); });
var _Bytes_read_f64 = F3(function(isLE, bytes, offset) { return _Utils_Tuple2(offset + 8, bytes.getFloat64(offset, isLE)); });

var _Bytes_read_bytes = F3(function(len, bytes, offset)
{
	return _Utils_Tuple2(offset + len, new DataView(bytes.buffer, bytes.byteOffset + offset, len));
});

var _Bytes_read_string = F3(function(len, bytes, offset)
{
	var string = '';
	var end = offset + len;
	for (; offset < end;)
	{
		var byte = bytes.getUint8(offset++);
		string +=
			(byte < 128)
				? String.fromCharCode(byte)
				:
			((byte & 0xE0 /* 0b11100000 */) === 0xC0 /* 0b11000000 */)
				? String.fromCharCode((byte & 0x1F /* 0b00011111 */) << 6 | bytes.getUint8(offset++) & 0x3F /* 0b00111111 */)
				:
			((byte & 0xF0 /* 0b11110000 */) === 0xE0 /* 0b11100000 */)
				? String.fromCharCode(
					(byte & 0xF /* 0b00001111 */) << 12
					| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
					| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
				)
				:
				(byte =
					((byte & 0x7 /* 0b00000111 */) << 18
						| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 12
						| (bytes.getUint8(offset++) & 0x3F /* 0b00111111 */) << 6
						| bytes.getUint8(offset++) & 0x3F /* 0b00111111 */
					) - 0x10000
				, String.fromCharCode(Math.floor(byte / 0x400) + 0xD800, byte % 0x400 + 0xDC00)
				);
	}
	return _Utils_Tuple2(offset, string);
});

var _Bytes_decodeFailure = F2(function() { throw 0; });



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



// SEND REQUEST

var _Http_toTask = F3(function(router, toTask, request)
{
	return _Scheduler_binding(function(callback)
	{
		function done(response) {
			callback(toTask(request.expect.a(response)));
		}

		var xhr = new XMLHttpRequest();
		xhr.addEventListener('error', function() { done($elm$http$Http$NetworkError_); });
		xhr.addEventListener('timeout', function() { done($elm$http$Http$Timeout_); });
		xhr.addEventListener('load', function() { done(_Http_toResponse(request.expect.b, xhr)); });
		$elm$core$Maybe$isJust(request.tracker) && _Http_track(router, xhr, request.tracker.a);

		try {
			xhr.open(request.method, request.url, true);
		} catch (e) {
			return done($elm$http$Http$BadUrl_(request.url));
		}

		_Http_configureRequest(xhr, request);

		request.body.a && xhr.setRequestHeader('Content-Type', request.body.a);
		xhr.send(request.body.b);

		return function() { xhr.c = true; xhr.abort(); };
	});
});


// CONFIGURE

function _Http_configureRequest(xhr, request)
{
	for (var headers = request.headers; headers.b; headers = headers.b) // WHILE_CONS
	{
		xhr.setRequestHeader(headers.a.a, headers.a.b);
	}
	xhr.timeout = request.timeout.a || 0;
	xhr.responseType = request.expect.d;
	xhr.withCredentials = request.allowCookiesFromOtherDomains;
}


// RESPONSES

function _Http_toResponse(toBody, xhr)
{
	return A2(
		200 <= xhr.status && xhr.status < 300 ? $elm$http$Http$GoodStatus_ : $elm$http$Http$BadStatus_,
		_Http_toMetadata(xhr),
		toBody(xhr.response)
	);
}


// METADATA

function _Http_toMetadata(xhr)
{
	return {
		url: xhr.responseURL,
		statusCode: xhr.status,
		statusText: xhr.statusText,
		headers: _Http_parseHeaders(xhr.getAllResponseHeaders())
	};
}


// HEADERS

function _Http_parseHeaders(rawHeaders)
{
	if (!rawHeaders)
	{
		return $elm$core$Dict$empty;
	}

	var headers = $elm$core$Dict$empty;
	var headerPairs = rawHeaders.split('\r\n');
	for (var i = headerPairs.length; i--; )
	{
		var headerPair = headerPairs[i];
		var index = headerPair.indexOf(': ');
		if (index > 0)
		{
			var key = headerPair.substring(0, index);
			var value = headerPair.substring(index + 2);

			headers = A3($elm$core$Dict$update, key, function(oldValue) {
				return $elm$core$Maybe$Just($elm$core$Maybe$isJust(oldValue)
					? value + ', ' + oldValue.a
					: value
				);
			}, headers);
		}
	}
	return headers;
}


// EXPECT

var _Http_expect = F3(function(type, toBody, toValue)
{
	return {
		$: 0,
		d: type,
		b: toBody,
		a: toValue
	};
});

var _Http_mapExpect = F2(function(func, expect)
{
	return {
		$: 0,
		d: expect.d,
		b: expect.b,
		a: function(x) { return func(expect.a(x)); }
	};
});

function _Http_toDataView(arrayBuffer)
{
	return new DataView(arrayBuffer);
}


// BODY and PARTS

var _Http_emptyBody = { $: 0 };
var _Http_pair = F2(function(a, b) { return { $: 0, a: a, b: b }; });

function _Http_toFormData(parts)
{
	for (var formData = new FormData(); parts.b; parts = parts.b) // WHILE_CONS
	{
		var part = parts.a;
		formData.append(part.a, part.b);
	}
	return formData;
}

var _Http_bytesToBlob = F2(function(mime, bytes)
{
	return new Blob([bytes], { type: mime });
});


// PROGRESS

function _Http_track(router, xhr, tracker)
{
	// TODO check out lengthComputable on loadstart event

	xhr.upload.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Sending({
			sent: event.loaded,
			size: event.total
		}))));
	});
	xhr.addEventListener('progress', function(event) {
		if (xhr.c) { return; }
		_Scheduler_rawSpawn(A2($elm$core$Platform$sendToSelf, router, _Utils_Tuple2(tracker, $elm$http$Http$Receiving({
			received: event.loaded,
			size: event.lengthComputable ? $elm$core$Maybe$Just(event.total) : $elm$core$Maybe$Nothing
		}))));
	});
}


function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}


function _Url_percentEncode(string)
{
	return encodeURIComponent(string);
}

function _Url_percentDecode(string)
{
	try
	{
		return $elm$core$Maybe$Just(decodeURIComponent(string));
	}
	catch (e)
	{
		return $elm$core$Maybe$Nothing;
	}
}

// CREATE

var _Regex_never = /.^/;

var _Regex_fromStringWith = F2(function(options, string)
{
	var flags = 'g';
	if (options.multiline) { flags += 'm'; }
	if (options.caseInsensitive) { flags += 'i'; }

	try
	{
		return $elm$core$Maybe$Just(new RegExp(string, flags));
	}
	catch(error)
	{
		return $elm$core$Maybe$Nothing;
	}
});


// USE

var _Regex_contains = F2(function(re, string)
{
	return string.match(re) !== null;
});


var _Regex_findAtMost = F3(function(n, re, str)
{
	var out = [];
	var number = 0;
	var string = str;
	var lastIndex = re.lastIndex;
	var prevLastIndex = -1;
	var result;
	while (number++ < n && (result = re.exec(string)))
	{
		if (prevLastIndex == re.lastIndex) break;
		var i = result.length - 1;
		var subs = new Array(i);
		while (i > 0)
		{
			var submatch = result[i];
			subs[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		out.push(A4($elm$regex$Regex$Match, result[0], result.index, number, _List_fromArray(subs)));
		prevLastIndex = re.lastIndex;
	}
	re.lastIndex = lastIndex;
	return _List_fromArray(out);
});


var _Regex_replaceAtMost = F4(function(n, re, replacer, string)
{
	var count = 0;
	function jsReplacer(match)
	{
		if (count++ >= n)
		{
			return match;
		}
		var i = arguments.length - 3;
		var submatches = new Array(i);
		while (i > 0)
		{
			var submatch = arguments[i];
			submatches[--i] = submatch
				? $elm$core$Maybe$Just(submatch)
				: $elm$core$Maybe$Nothing;
		}
		return replacer(A4($elm$regex$Regex$Match, match, arguments[arguments.length - 2], count, _List_fromArray(submatches)));
	}
	return string.replace(re, jsReplacer);
});

var _Regex_splitAtMost = F3(function(n, re, str)
{
	var string = str;
	var out = [];
	var start = re.lastIndex;
	var restoreLastIndex = re.lastIndex;
	while (n--)
	{
		var result = re.exec(string);
		if (!result) break;
		out.push(string.slice(start, result.index));
		start = re.lastIndex;
	}
	out.push(string.slice(start));
	re.lastIndex = restoreLastIndex;
	return _List_fromArray(out);
});

var _Regex_infinity = Infinity;



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}

var $author$project$Main$ChangedUrl = function (a) {
	return {$: 'ChangedUrl', a: a};
};
var $author$project$Main$ClickedLink = function (a) {
	return {$: 'ClickedLink', a: a};
};
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$application = _Browser_application;
var $author$project$Main$GotTime = function (a) {
	return {$: 'GotTime', a: a};
};
var $author$project$Credentials$Guest = {$: 'Guest'};
var $author$project$Credentials$LoggedIn = function (a) {
	return {$: 'LoggedIn', a: a};
};
var $elm$core$Result$andThen = F2(
	function (callback, result) {
		if (result.$ === 'Ok') {
			var value = result.a;
			return callback(value);
		} else {
			var msg = result.a;
			return $elm$core$Result$Err(msg);
		}
	});
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$json$Json$Decode$decodeValue = _Json_run;
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$core$Result$toMaybe = function (result) {
	if (result.$ === 'Ok') {
		var v = result.a;
		return $elm$core$Maybe$Just(v);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Credentials$Token = function (a) {
	return {$: 'Token', a: a};
};
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom = $elm$json$Json$Decode$map2($elm$core$Basics$apR);
var $elm$json$Json$Decode$field = _Json_decodeField;
var $NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required = F3(
	function (key, valDecoder, decoder) {
		return A2(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$custom,
			A2($elm$json$Json$Decode$field, key, valDecoder),
			decoder);
	});
var $author$project$Credentials$tokenDecoder = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'token',
	$elm$json$Json$Decode$string,
	$elm$json$Json$Decode$succeed($author$project$Credentials$Token));
var $author$project$Credentials$decodeToSession = F2(
	function (key, value) {
		var _v0 = $elm$core$Result$toMaybe(
			A2(
				$elm$core$Result$andThen,
				$elm$json$Json$Decode$decodeString($author$project$Credentials$tokenDecoder),
				A2($elm$json$Json$Decode$decodeValue, $elm$json$Json$Decode$string, value)));
		if (_v0.$ === 'Just') {
			var token = _v0.a;
			return $author$project$Credentials$LoggedIn(token);
		} else {
			return $author$project$Credentials$Guest;
		}
	});
var $author$project$Main$ChatPage = function (a) {
	return {$: 'ChatPage', a: a};
};
var $author$project$Main$GotChatMsg = function (a) {
	return {$: 'GotChatMsg', a: a};
};
var $author$project$Main$GotHomeMsg = function (a) {
	return {$: 'GotHomeMsg', a: a};
};
var $author$project$Main$GotLoginMsg = function (a) {
	return {$: 'GotLoginMsg', a: a};
};
var $author$project$Main$GotProfileMsg = function (a) {
	return {$: 'GotProfileMsg', a: a};
};
var $author$project$Main$GotSignupMsg = function (a) {
	return {$: 'GotSignupMsg', a: a};
};
var $author$project$Main$GotVerificationMsg = function (a) {
	return {$: 'GotVerificationMsg', a: a};
};
var $author$project$Main$HomePage = function (a) {
	return {$: 'HomePage', a: a};
};
var $author$project$Main$LoginPage = function (a) {
	return {$: 'LoginPage', a: a};
};
var $author$project$Main$NotFoundPage = {$: 'NotFoundPage'};
var $author$project$Main$ProfilePage = function (a) {
	return {$: 'ProfilePage', a: a};
};
var $author$project$Main$SignupPage = function (a) {
	return {$: 'SignupPage', a: a};
};
var $author$project$Main$VerificationPage = function (a) {
	return {$: 'VerificationPage', a: a};
};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $elm$json$Json$Encode$string = _Json_wrap;
var $author$project$Credentials$addUserToRoom = _Platform_outgoingPort(
	'addUserToRoom',
	function ($) {
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'userId',
					$elm$json$Json$Encode$string($.userId)),
					_Utils_Tuple2(
					'userName',
					$elm$json$Json$Encode$string($.userName))
				]));
	});
var $simonh1000$elm_jwt$Jwt$TokenDecodeError = function (a) {
	return {$: 'TokenDecodeError', a: a};
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $simonh1000$elm_jwt$Jwt$TokenHeaderError = {$: 'TokenHeaderError'};
var $simonh1000$elm_jwt$Jwt$TokenProcessingError = function (a) {
	return {$: 'TokenProcessingError', a: a};
};
var $elm$core$String$concat = function (strings) {
	return A2($elm$core$String$join, '', strings);
};
var $elm$core$Basics$modBy = _Basics_modBy;
var $simonh1000$elm_jwt$Jwt$fixlength = function (s) {
	var _v0 = A2(
		$elm$core$Basics$modBy,
		4,
		$elm$core$String$length(s));
	switch (_v0) {
		case 0:
			return $elm$core$Result$Ok(s);
		case 2:
			return $elm$core$Result$Ok(
				$elm$core$String$concat(
					_List_fromArray(
						[s, '=='])));
		case 3:
			return $elm$core$Result$Ok(
				$elm$core$String$concat(
					_List_fromArray(
						[s, '='])));
		default:
			return $elm$core$Result$Err(
				$simonh1000$elm_jwt$Jwt$TokenProcessingError('Wrong length'));
	}
};
var $elm$core$Result$fromMaybe = F2(
	function (err, maybe) {
		if (maybe.$ === 'Just') {
			var v = maybe.a;
			return $elm$core$Result$Ok(v);
		} else {
			return $elm$core$Result$Err(err);
		}
	});
var $elm$core$Result$map = F2(
	function (func, ra) {
		if (ra.$ === 'Ok') {
			var a = ra.a;
			return $elm$core$Result$Ok(
				func(a));
		} else {
			var e = ra.a;
			return $elm$core$Result$Err(e);
		}
	});
var $elm$core$Result$map2 = F3(
	function (func, ra, rb) {
		if (ra.$ === 'Err') {
			var x = ra.a;
			return $elm$core$Result$Err(x);
		} else {
			var a = ra.a;
			if (rb.$ === 'Err') {
				var x = rb.a;
				return $elm$core$Result$Err(x);
			} else {
				var b = rb.a;
				return $elm$core$Result$Ok(
					A2(func, a, b));
			}
		}
	});
var $elm$core$Result$mapError = F2(
	function (f, result) {
		if (result.$ === 'Ok') {
			var v = result.a;
			return $elm$core$Result$Ok(v);
		} else {
			var e = result.a;
			return $elm$core$Result$Err(
				f(e));
		}
	});
var $elm$bytes$Bytes$Encode$getWidth = function (builder) {
	switch (builder.$) {
		case 'I8':
			return 1;
		case 'I16':
			return 2;
		case 'I32':
			return 4;
		case 'U8':
			return 1;
		case 'U16':
			return 2;
		case 'U32':
			return 4;
		case 'F32':
			return 4;
		case 'F64':
			return 8;
		case 'Seq':
			var w = builder.a;
			return w;
		case 'Utf8':
			var w = builder.a;
			return w;
		default:
			var bs = builder.a;
			return _Bytes_width(bs);
	}
};
var $elm$bytes$Bytes$LE = {$: 'LE'};
var $elm$bytes$Bytes$Encode$write = F3(
	function (builder, mb, offset) {
		switch (builder.$) {
			case 'I8':
				var n = builder.a;
				return A3(_Bytes_write_i8, mb, offset, n);
			case 'I16':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_i16,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'I32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_i32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'U8':
				var n = builder.a;
				return A3(_Bytes_write_u8, mb, offset, n);
			case 'U16':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_u16,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'U32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_u32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'F32':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_f32,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'F64':
				var e = builder.a;
				var n = builder.b;
				return A4(
					_Bytes_write_f64,
					mb,
					offset,
					n,
					_Utils_eq(e, $elm$bytes$Bytes$LE));
			case 'Seq':
				var bs = builder.b;
				return A3($elm$bytes$Bytes$Encode$writeSequence, bs, mb, offset);
			case 'Utf8':
				var s = builder.b;
				return A3(_Bytes_write_string, mb, offset, s);
			default:
				var bs = builder.a;
				return A3(_Bytes_write_bytes, mb, offset, bs);
		}
	});
var $elm$bytes$Bytes$Encode$writeSequence = F3(
	function (builders, mb, offset) {
		writeSequence:
		while (true) {
			if (!builders.b) {
				return offset;
			} else {
				var b = builders.a;
				var bs = builders.b;
				var $temp$builders = bs,
					$temp$mb = mb,
					$temp$offset = A3($elm$bytes$Bytes$Encode$write, b, mb, offset);
				builders = $temp$builders;
				mb = $temp$mb;
				offset = $temp$offset;
				continue writeSequence;
			}
		}
	});
var $elm$bytes$Bytes$Decode$decode = F2(
	function (_v0, bs) {
		var decoder = _v0.a;
		return A2(_Bytes_decode, decoder, bs);
	});
var $elm$bytes$Bytes$Decode$Decoder = function (a) {
	return {$: 'Decoder', a: a};
};
var $elm$bytes$Bytes$Decode$string = function (n) {
	return $elm$bytes$Bytes$Decode$Decoder(
		_Bytes_read_string(n));
};
var $elm$bytes$Bytes$Encode$encode = _Bytes_encode;
var $elm$bytes$Bytes$BE = {$: 'BE'};
var $danfishgold$base64_bytes$Encode$isValidChar = function (c) {
	if ($elm$core$Char$isAlphaNum(c)) {
		return true;
	} else {
		switch (c.valueOf()) {
			case '+':
				return true;
			case '/':
				return true;
			default:
				return false;
		}
	}
};
var $elm$core$Bitwise$or = _Bitwise_or;
var $elm$bytes$Bytes$Encode$Seq = F2(
	function (a, b) {
		return {$: 'Seq', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$getWidths = F2(
	function (width, builders) {
		getWidths:
		while (true) {
			if (!builders.b) {
				return width;
			} else {
				var b = builders.a;
				var bs = builders.b;
				var $temp$width = width + $elm$bytes$Bytes$Encode$getWidth(b),
					$temp$builders = bs;
				width = $temp$width;
				builders = $temp$builders;
				continue getWidths;
			}
		}
	});
var $elm$bytes$Bytes$Encode$sequence = function (builders) {
	return A2(
		$elm$bytes$Bytes$Encode$Seq,
		A2($elm$bytes$Bytes$Encode$getWidths, 0, builders),
		builders);
};
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $danfishgold$base64_bytes$Encode$unsafeConvertChar = function (_char) {
	var key = $elm$core$Char$toCode(_char);
	if ((key >= 65) && (key <= 90)) {
		return key - 65;
	} else {
		if ((key >= 97) && (key <= 122)) {
			return (key - 97) + 26;
		} else {
			if ((key >= 48) && (key <= 57)) {
				return ((key - 48) + 26) + 26;
			} else {
				switch (_char.valueOf()) {
					case '+':
						return 62;
					case '/':
						return 63;
					default:
						return -1;
				}
			}
		}
	}
};
var $elm$bytes$Bytes$Encode$U16 = F2(
	function (a, b) {
		return {$: 'U16', a: a, b: b};
	});
var $elm$bytes$Bytes$Encode$unsignedInt16 = $elm$bytes$Bytes$Encode$U16;
var $elm$bytes$Bytes$Encode$U8 = function (a) {
	return {$: 'U8', a: a};
};
var $elm$bytes$Bytes$Encode$unsignedInt8 = $elm$bytes$Bytes$Encode$U8;
var $danfishgold$base64_bytes$Encode$encodeCharacters = F4(
	function (a, b, c, d) {
		if ($danfishgold$base64_bytes$Encode$isValidChar(a) && $danfishgold$base64_bytes$Encode$isValidChar(b)) {
			var n2 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(b);
			var n1 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(a);
			if ('=' === d.valueOf()) {
				if ('=' === c.valueOf()) {
					var n = (n1 << 18) | (n2 << 12);
					var b1 = n >> 16;
					return $elm$core$Maybe$Just(
						$elm$bytes$Bytes$Encode$unsignedInt8(b1));
				} else {
					if ($danfishgold$base64_bytes$Encode$isValidChar(c)) {
						var n3 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(c);
						var n = ((n1 << 18) | (n2 << 12)) | (n3 << 6);
						var combined = n >> 8;
						return $elm$core$Maybe$Just(
							A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$BE, combined));
					} else {
						return $elm$core$Maybe$Nothing;
					}
				}
			} else {
				if ($danfishgold$base64_bytes$Encode$isValidChar(c) && $danfishgold$base64_bytes$Encode$isValidChar(d)) {
					var n4 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(d);
					var n3 = $danfishgold$base64_bytes$Encode$unsafeConvertChar(c);
					var n = ((n1 << 18) | (n2 << 12)) | ((n3 << 6) | n4);
					var combined = n >> 8;
					var b3 = n;
					return $elm$core$Maybe$Just(
						$elm$bytes$Bytes$Encode$sequence(
							_List_fromArray(
								[
									A2($elm$bytes$Bytes$Encode$unsignedInt16, $elm$bytes$Bytes$BE, combined),
									$elm$bytes$Bytes$Encode$unsignedInt8(b3)
								])));
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$String$foldr = _String_foldr;
var $elm$core$String$toList = function (string) {
	return A3($elm$core$String$foldr, $elm$core$List$cons, _List_Nil, string);
};
var $danfishgold$base64_bytes$Encode$encodeChunks = F2(
	function (input, accum) {
		encodeChunks:
		while (true) {
			var _v0 = $elm$core$String$toList(
				A2($elm$core$String$left, 4, input));
			_v0$4:
			while (true) {
				if (!_v0.b) {
					return $elm$core$Maybe$Just(accum);
				} else {
					if (_v0.b.b) {
						if (_v0.b.b.b) {
							if (_v0.b.b.b.b) {
								if (!_v0.b.b.b.b.b) {
									var a = _v0.a;
									var _v1 = _v0.b;
									var b = _v1.a;
									var _v2 = _v1.b;
									var c = _v2.a;
									var _v3 = _v2.b;
									var d = _v3.a;
									var _v4 = A4($danfishgold$base64_bytes$Encode$encodeCharacters, a, b, c, d);
									if (_v4.$ === 'Just') {
										var enc = _v4.a;
										var $temp$input = A2($elm$core$String$dropLeft, 4, input),
											$temp$accum = A2($elm$core$List$cons, enc, accum);
										input = $temp$input;
										accum = $temp$accum;
										continue encodeChunks;
									} else {
										return $elm$core$Maybe$Nothing;
									}
								} else {
									break _v0$4;
								}
							} else {
								var a = _v0.a;
								var _v5 = _v0.b;
								var b = _v5.a;
								var _v6 = _v5.b;
								var c = _v6.a;
								var _v7 = A4(
									$danfishgold$base64_bytes$Encode$encodeCharacters,
									a,
									b,
									c,
									_Utils_chr('='));
								if (_v7.$ === 'Nothing') {
									return $elm$core$Maybe$Nothing;
								} else {
									var enc = _v7.a;
									return $elm$core$Maybe$Just(
										A2($elm$core$List$cons, enc, accum));
								}
							}
						} else {
							var a = _v0.a;
							var _v8 = _v0.b;
							var b = _v8.a;
							var _v9 = A4(
								$danfishgold$base64_bytes$Encode$encodeCharacters,
								a,
								b,
								_Utils_chr('='),
								_Utils_chr('='));
							if (_v9.$ === 'Nothing') {
								return $elm$core$Maybe$Nothing;
							} else {
								var enc = _v9.a;
								return $elm$core$Maybe$Just(
									A2($elm$core$List$cons, enc, accum));
							}
						}
					} else {
						break _v0$4;
					}
				}
			}
			return $elm$core$Maybe$Nothing;
		}
	});
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $danfishgold$base64_bytes$Encode$encoder = function (string) {
	return A2(
		$elm$core$Maybe$map,
		A2($elm$core$Basics$composeR, $elm$core$List$reverse, $elm$bytes$Bytes$Encode$sequence),
		A2($danfishgold$base64_bytes$Encode$encodeChunks, string, _List_Nil));
};
var $danfishgold$base64_bytes$Encode$toBytes = function (string) {
	return A2(
		$elm$core$Maybe$map,
		$elm$bytes$Bytes$Encode$encode,
		$danfishgold$base64_bytes$Encode$encoder(string));
};
var $danfishgold$base64_bytes$Base64$toBytes = $danfishgold$base64_bytes$Encode$toBytes;
var $elm$bytes$Bytes$width = _Bytes_width;
var $danfishgold$base64_bytes$Base64$toString = function (b64String) {
	var _v0 = $danfishgold$base64_bytes$Base64$toBytes(b64String);
	if (_v0.$ === 'Nothing') {
		return $elm$core$Maybe$Nothing;
	} else {
		var b64Bytes = _v0.a;
		return A2(
			$elm$bytes$Bytes$Decode$decode,
			$elm$bytes$Bytes$Decode$string(
				$elm$bytes$Bytes$width(b64Bytes)),
			b64Bytes);
	}
};
var $elm$core$String$map = _String_map;
var $simonh1000$elm_jwt$Jwt$unurl = function () {
	var fix = function (c) {
		switch (c.valueOf()) {
			case '-':
				return _Utils_chr('+');
			case '_':
				return _Utils_chr('/');
			default:
				return c;
		}
	};
	return $elm$core$String$map(fix);
}();
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $simonh1000$elm_jwt$Jwt$getTokenParts = function (token) {
	var verifyJson = F2(
		function (errorHandler, str) {
			return A2(
				$elm$core$Result$mapError,
				errorHandler,
				A2(
					$elm$core$Result$map,
					function (_v8) {
						return str;
					},
					A2($elm$json$Json$Decode$decodeString, $elm$json$Json$Decode$value, str)));
		});
	var processor = A2(
		$elm$core$Basics$composeR,
		$simonh1000$elm_jwt$Jwt$unurl,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$String$split('.'),
			$elm$core$List$map($simonh1000$elm_jwt$Jwt$fixlength)));
	var _v0 = processor(token);
	_v0$1:
	while (true) {
		if (((_v0.b && _v0.b.b) && _v0.b.b.b) && (!_v0.b.b.b.b)) {
			if (_v0.a.$ === 'Ok') {
				if (_v0.b.a.$ === 'Ok') {
					var header = _v0.a.a;
					var _v1 = _v0.b;
					var body = _v1.a.a;
					var _v2 = _v1.b;
					var header_ = A2(
						$elm$core$Result$andThen,
						verifyJson(
							function (_v3) {
								return $simonh1000$elm_jwt$Jwt$TokenHeaderError;
							}),
						A2(
							$elm$core$Result$fromMaybe,
							$simonh1000$elm_jwt$Jwt$TokenHeaderError,
							$danfishgold$base64_bytes$Base64$toString(header)));
					var body_ = A2(
						$elm$core$Result$andThen,
						verifyJson($simonh1000$elm_jwt$Jwt$TokenDecodeError),
						A2(
							$elm$core$Result$fromMaybe,
							$simonh1000$elm_jwt$Jwt$TokenProcessingError('Invalid UTF-16'),
							$danfishgold$base64_bytes$Base64$toString(body)));
					return A3(
						$elm$core$Result$map2,
						F2(
							function (a, b) {
								return _Utils_Tuple2(a, b);
							}),
						header_,
						body_);
				} else {
					break _v0$1;
				}
			} else {
				if (_v0.b.a.$ === 'Err') {
					break _v0$1;
				} else {
					var err = _v0.a.a;
					var _v6 = _v0.b;
					var _v7 = _v6.b;
					return $elm$core$Result$Err(err);
				}
			}
		} else {
			return $elm$core$Result$Err(
				$simonh1000$elm_jwt$Jwt$TokenProcessingError('Token has invalid shape'));
		}
	}
	var _v4 = _v0.b;
	var err = _v4.a.a;
	var _v5 = _v4.b;
	return $elm$core$Result$Err(err);
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $simonh1000$elm_jwt$Jwt$decodeToken = F2(
	function (dec, token) {
		return A2(
			$elm$core$Result$andThen,
			A2(
				$elm$core$Basics$composeR,
				$elm$json$Json$Decode$decodeString(dec),
				$elm$core$Result$mapError($simonh1000$elm_jwt$Jwt$TokenDecodeError)),
			A2(
				$elm$core$Result$map,
				$elm$core$Tuple$second,
				$simonh1000$elm_jwt$Jwt$getTokenParts(token)));
	});
var $author$project$Credentials$UserDataFromToken = F6(
	function (id, isverified, email, firstname, verificationstring, profilepicurl) {
		return {email: email, firstname: firstname, id: id, isverified: isverified, profilepicurl: profilepicurl, verificationstring: verificationstring};
	});
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$bool = _Json_decodeBool;
var $author$project$Credentials$UserId = function (a) {
	return {$: 'UserId', a: a};
};
var $author$project$Credentials$idDecoder = A2($elm$json$Json$Decode$map, $author$project$Credentials$UserId, $elm$json$Json$Decode$string);
var $elm$json$Json$Decode$map6 = _Json_map6;
var $author$project$Credentials$VerificationString = function (a) {
	return {$: 'VerificationString', a: a};
};
var $author$project$Credentials$verifyStringDecoder = A2($elm$json$Json$Decode$map, $author$project$Credentials$VerificationString, $elm$json$Json$Decode$string);
var $author$project$Credentials$decodeTokenData = A7(
	$elm$json$Json$Decode$map6,
	$author$project$Credentials$UserDataFromToken,
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['id']),
		$author$project$Credentials$idDecoder),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['isverified']),
		$elm$json$Json$Decode$bool),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['email']),
		$elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['firstname']),
		$elm$json$Json$Decode$string),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['verificationstring']),
		$author$project$Credentials$verifyStringDecoder),
	A2(
		$elm$json$Json$Decode$at,
		_List_fromArray(
			['profilepicurl']),
		$elm$json$Json$Decode$string));
var $author$project$Chat$ChatMessages = function (a) {
	return {$: 'ChatMessages', a: a};
};
var $author$project$Credentials$SocketMessageData = F6(
	function (name, id, clientId, connectionId, timestamp, data) {
		return {clientId: clientId, connectionId: connectionId, data: data, id: id, name: name, timestamp: timestamp};
	});
var $author$project$Credentials$DataMessage = function (message) {
	return {message: message};
};
var $author$project$Credentials$decodeMessage = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'message',
	$elm$json$Json$Decode$string,
	$elm$json$Json$Decode$succeed($author$project$Credentials$DataMessage));
var $elm$json$Json$Decode$int = _Json_decodeInt;
var $author$project$Credentials$decodeSocketMessage = A3(
	$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
	'data',
	$author$project$Credentials$decodeMessage,
	A3(
		$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
		'timestamp',
		$elm$json$Json$Decode$int,
		A3(
			$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
			'connectionId',
			$elm$json$Json$Decode$string,
			A3(
				$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
				'clientId',
				$elm$json$Json$Decode$string,
				A3(
					$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
					'id',
					$elm$json$Json$Decode$string,
					A3(
						$NoRedInk$elm_json_decode_pipeline$Json$Decode$Pipeline$required,
						'name',
						$elm$json$Json$Decode$string,
						$elm$json$Json$Decode$succeed($author$project$Credentials$SocketMessageData)))))));
var $elm$http$Http$BadStatus_ = F2(
	function (a, b) {
		return {$: 'BadStatus_', a: a, b: b};
	});
var $elm$http$Http$BadUrl_ = function (a) {
	return {$: 'BadUrl_', a: a};
};
var $elm$http$Http$GoodStatus_ = F2(
	function (a, b) {
		return {$: 'GoodStatus_', a: a, b: b};
	});
var $elm$http$Http$NetworkError_ = {$: 'NetworkError_'};
var $elm$http$Http$Receiving = function (a) {
	return {$: 'Receiving', a: a};
};
var $elm$http$Http$Sending = function (a) {
	return {$: 'Sending', a: a};
};
var $elm$http$Http$Timeout_ = {$: 'Timeout_'};
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $elm$core$Maybe$isJust = function (maybe) {
	if (maybe.$ === 'Just') {
		return true;
	} else {
		return false;
	}
};
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $elm$http$Http$expectStringResponse = F2(
	function (toMsg, toResult) {
		return A3(
			_Http_expect,
			'',
			$elm$core$Basics$identity,
			A2($elm$core$Basics$composeR, toResult, toMsg));
	});
var $elm$http$Http$BadBody = function (a) {
	return {$: 'BadBody', a: a};
};
var $elm$http$Http$BadStatus = function (a) {
	return {$: 'BadStatus', a: a};
};
var $elm$http$Http$BadUrl = function (a) {
	return {$: 'BadUrl', a: a};
};
var $elm$http$Http$NetworkError = {$: 'NetworkError'};
var $elm$http$Http$Timeout = {$: 'Timeout'};
var $elm$http$Http$resolve = F2(
	function (toResult, response) {
		switch (response.$) {
			case 'BadUrl_':
				var url = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadUrl(url));
			case 'Timeout_':
				return $elm$core$Result$Err($elm$http$Http$Timeout);
			case 'NetworkError_':
				return $elm$core$Result$Err($elm$http$Http$NetworkError);
			case 'BadStatus_':
				var metadata = response.a;
				return $elm$core$Result$Err(
					$elm$http$Http$BadStatus(metadata.statusCode));
			default:
				var body = response.b;
				return A2(
					$elm$core$Result$mapError,
					$elm$http$Http$BadBody,
					toResult(body));
		}
	});
var $elm$http$Http$expectJson = F2(
	function (toMsg, decoder) {
		return A2(
			$elm$http$Http$expectStringResponse,
			toMsg,
			$elm$http$Http$resolve(
				function (string) {
					return A2(
						$elm$core$Result$mapError,
						$elm$json$Json$Decode$errorToString,
						A2($elm$json$Json$Decode$decodeString, decoder, string));
				}));
	});
var $elm$http$Http$emptyBody = _Http_emptyBody;
var $elm$http$Http$Request = function (a) {
	return {$: 'Request', a: a};
};
var $elm$http$Http$State = F2(
	function (reqs, subs) {
		return {reqs: reqs, subs: subs};
	});
var $elm$http$Http$init = $elm$core$Task$succeed(
	A2($elm$http$Http$State, $elm$core$Dict$empty, _List_Nil));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$http$Http$updateReqs = F3(
	function (router, cmds, reqs) {
		updateReqs:
		while (true) {
			if (!cmds.b) {
				return $elm$core$Task$succeed(reqs);
			} else {
				var cmd = cmds.a;
				var otherCmds = cmds.b;
				if (cmd.$ === 'Cancel') {
					var tracker = cmd.a;
					var _v2 = A2($elm$core$Dict$get, tracker, reqs);
					if (_v2.$ === 'Nothing') {
						var $temp$router = router,
							$temp$cmds = otherCmds,
							$temp$reqs = reqs;
						router = $temp$router;
						cmds = $temp$cmds;
						reqs = $temp$reqs;
						continue updateReqs;
					} else {
						var pid = _v2.a;
						return A2(
							$elm$core$Task$andThen,
							function (_v3) {
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A2($elm$core$Dict$remove, tracker, reqs));
							},
							$elm$core$Process$kill(pid));
					}
				} else {
					var req = cmd.a;
					return A2(
						$elm$core$Task$andThen,
						function (pid) {
							var _v4 = req.tracker;
							if (_v4.$ === 'Nothing') {
								return A3($elm$http$Http$updateReqs, router, otherCmds, reqs);
							} else {
								var tracker = _v4.a;
								return A3(
									$elm$http$Http$updateReqs,
									router,
									otherCmds,
									A3($elm$core$Dict$insert, tracker, pid, reqs));
							}
						},
						$elm$core$Process$spawn(
							A3(
								_Http_toTask,
								router,
								$elm$core$Platform$sendToApp(router),
								req)));
				}
			}
		}
	});
var $elm$http$Http$onEffects = F4(
	function (router, cmds, subs, state) {
		return A2(
			$elm$core$Task$andThen,
			function (reqs) {
				return $elm$core$Task$succeed(
					A2($elm$http$Http$State, reqs, subs));
			},
			A3($elm$http$Http$updateReqs, router, cmds, state.reqs));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$http$Http$maybeSend = F4(
	function (router, desiredTracker, progress, _v0) {
		var actualTracker = _v0.a;
		var toMsg = _v0.b;
		return _Utils_eq(desiredTracker, actualTracker) ? $elm$core$Maybe$Just(
			A2(
				$elm$core$Platform$sendToApp,
				router,
				toMsg(progress))) : $elm$core$Maybe$Nothing;
	});
var $elm$http$Http$onSelfMsg = F3(
	function (router, _v0, state) {
		var tracker = _v0.a;
		var progress = _v0.b;
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$filterMap,
					A3($elm$http$Http$maybeSend, router, tracker, progress),
					state.subs)));
	});
var $elm$http$Http$Cancel = function (a) {
	return {$: 'Cancel', a: a};
};
var $elm$http$Http$cmdMap = F2(
	function (func, cmd) {
		if (cmd.$ === 'Cancel') {
			var tracker = cmd.a;
			return $elm$http$Http$Cancel(tracker);
		} else {
			var r = cmd.a;
			return $elm$http$Http$Request(
				{
					allowCookiesFromOtherDomains: r.allowCookiesFromOtherDomains,
					body: r.body,
					expect: A2(_Http_mapExpect, func, r.expect),
					headers: r.headers,
					method: r.method,
					timeout: r.timeout,
					tracker: r.tracker,
					url: r.url
				});
		}
	});
var $elm$http$Http$MySub = F2(
	function (a, b) {
		return {$: 'MySub', a: a, b: b};
	});
var $elm$http$Http$subMap = F2(
	function (func, _v0) {
		var tracker = _v0.a;
		var toMsg = _v0.b;
		return A2(
			$elm$http$Http$MySub,
			tracker,
			A2($elm$core$Basics$composeR, toMsg, func));
	});
_Platform_effectManagers['Http'] = _Platform_createManager($elm$http$Http$init, $elm$http$Http$onEffects, $elm$http$Http$onSelfMsg, $elm$http$Http$cmdMap, $elm$http$Http$subMap);
var $elm$http$Http$command = _Platform_leaf('Http');
var $elm$http$Http$subscription = _Platform_leaf('Http');
var $elm$http$Http$request = function (r) {
	return $elm$http$Http$command(
		$elm$http$Http$Request(
			{allowCookiesFromOtherDomains: false, body: r.body, expect: r.expect, headers: r.headers, method: r.method, timeout: r.timeout, tracker: r.tracker, url: r.url}));
};
var $elm$http$Http$get = function (r) {
	return $elm$http$Http$request(
		{body: $elm$http$Http$emptyBody, expect: r.expect, headers: _List_Nil, method: 'GET', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $author$project$Chat$fetchChatMessages = $elm$http$Http$get(
	{
		expect: A2(
			$elm$http$Http$expectJson,
			$author$project$Chat$ChatMessages,
			$elm$json$Json$Decode$list($author$project$Credentials$decodeSocketMessage)),
		url: '/api/messages'
	});
var $author$project$Chat$FetchUsers = function (a) {
	return {$: 'FetchUsers', a: a};
};
var $author$project$Chat$User = F2(
	function (firstname, email) {
		return {email: email, firstname: firstname};
	});
var $author$project$Chat$userDecoder = A3(
	$elm$json$Json$Decode$map2,
	$author$project$Chat$User,
	A2($elm$json$Json$Decode$field, 'firstname', $elm$json$Json$Decode$string),
	A2($elm$json$Json$Decode$field, 'email', $elm$json$Json$Decode$string));
var $author$project$Chat$usersDecoder = $elm$json$Json$Decode$list($author$project$Chat$userDecoder);
var $author$project$Chat$fetchUsers = $elm$http$Http$get(
	{
		expect: A2($elm$http$Http$expectJson, $author$project$Chat$FetchUsers, $author$project$Chat$usersDecoder),
		url: '/api/socket?roomId=123'
	});
var $author$project$Credentials$fromSessionToToken = function (session) {
	if (session.$ === 'LoggedIn') {
		var token = session.a;
		return $elm$core$Maybe$Just(token);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Credentials$fromTokenToString = function (_v0) {
	var string = _v0.a;
	return string;
};
var $author$project$Chat$initialModel = {error: $elm$core$Maybe$Nothing, message: '', receivedMessages: _List_Nil, users: _List_Nil};
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Chat$takeNameOrEmail = function (_v0) {
	var firstname = _v0.firstname;
	var email = _v0.email;
	return $elm$core$String$isEmpty(firstname) ? email : firstname;
};
var $author$project$Credentials$userIdToString = function (_v0) {
	var id = _v0.a;
	return id;
};
var $author$project$Chat$init = function (session) {
	var _v0 = $author$project$Credentials$fromSessionToToken(session);
	if (_v0.$ === 'Just') {
		var token = _v0.a;
		var _v1 = A2(
			$simonh1000$elm_jwt$Jwt$decodeToken,
			$author$project$Credentials$decodeTokenData,
			$author$project$Credentials$fromTokenToString(token));
		if (_v1.$ === 'Ok') {
			var resultTokenRecord = _v1.a;
			return _Utils_Tuple2(
				$author$project$Chat$initialModel,
				$elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							$author$project$Chat$fetchUsers,
							$author$project$Credentials$addUserToRoom(
							{
								userId: $author$project$Credentials$userIdToString(resultTokenRecord.id),
								userName: $author$project$Chat$takeNameOrEmail(
									{email: resultTokenRecord.email, firstname: resultTokenRecord.firstname})
							}),
							$author$project$Chat$fetchChatMessages
						])));
		} else {
			return _Utils_Tuple2($author$project$Chat$initialModel, $elm$core$Platform$Cmd$none);
		}
	} else {
		return _Utils_Tuple2($author$project$Chat$initialModel, $elm$core$Platform$Cmd$none);
	}
};
var $author$project$Home$initialModel = {};
var $author$project$Home$init = function (_v0) {
	return _Utils_Tuple2($author$project$Home$initialModel, $elm$core$Platform$Cmd$none);
};
var $author$project$Login$Initial = {$: 'Initial'};
var $author$project$Login$initialModel = {formState: $author$project$Login$Initial, storeEmail: '', storePassword: ''};
var $author$project$Login$init = function (_v0) {
	return _Utils_Tuple2($author$project$Login$initialModel, $elm$core$Platform$Cmd$none);
};
var $author$project$Profile$Intruder = {$: 'Intruder'};
var $author$project$Profile$NotVerified = {$: 'NotVerified'};
var $author$project$Profile$Verified = function (a) {
	return {$: 'Verified', a: a};
};
var $author$project$Profile$Initial = {$: 'Initial'};
var $author$project$Profile$initialModel = {formState: $author$project$Profile$Initial, profilePic: $elm$core$Maybe$Nothing, storeName: '', userState: $author$project$Profile$NotVerified};
var $author$project$Profile$init = function (session) {
	var _v0 = $author$project$Credentials$fromSessionToToken(session);
	if (_v0.$ === 'Just') {
		var token = _v0.a;
		var _v1 = A2(
			$simonh1000$elm_jwt$Jwt$decodeToken,
			$author$project$Credentials$decodeTokenData,
			$author$project$Credentials$fromTokenToString(token));
		if (_v1.$ === 'Ok') {
			var userDataFromToken = _v1.a;
			return _Utils_Tuple2(
				_Utils_update(
					$author$project$Profile$initialModel,
					{
						storeName: userDataFromToken.firstname,
						userState: userDataFromToken.isverified ? $author$project$Profile$Verified(session) : $author$project$Profile$NotVerified
					}),
				$elm$core$Platform$Cmd$none);
		} else {
			return _Utils_Tuple2(
				_Utils_update(
					$author$project$Profile$initialModel,
					{userState: $author$project$Profile$Intruder}),
				$elm$core$Platform$Cmd$none);
		}
	} else {
		return _Utils_Tuple2(
			_Utils_update(
				$author$project$Profile$initialModel,
				{userState: $author$project$Profile$Intruder}),
			$elm$core$Platform$Cmd$none);
	}
};
var $author$project$Signup$Initial = {$: 'Initial'};
var $author$project$Signup$initialModel = {formState: $author$project$Signup$Initial, storeConfirmPassword: '', storeEmail: '', storePassword: ''};
var $author$project$Signup$init = function (_v0) {
	return _Utils_Tuple2($author$project$Signup$initialModel, $elm$core$Platform$Cmd$none);
};
var $author$project$Verification$Sessionless = {$: 'Sessionless'};
var $author$project$Verification$VerificationFail = {$: 'VerificationFail'};
var $author$project$Verification$VerificationPending = {$: 'VerificationPending'};
var $author$project$Verification$Verified = {$: 'Verified'};
var $author$project$Verification$VerifyApiCallStart = function (a) {
	return {$: 'VerifyApiCallStart', a: a};
};
var $elm$core$Process$sleep = _Process_sleep;
var $author$project$Verification$apiCallAfterSomeTime = F2(
	function (session, toMsg) {
		return A2(
			$elm$core$Task$perform,
			function (_v0) {
				return toMsg(session);
			},
			$elm$core$Process$sleep(5000));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $elm$core$Basics$not = _Basics_not;
var $author$project$Credentials$verificationToString = function (_v0) {
	var verificationString = _v0.a;
	return verificationString;
};
var $author$project$Verification$init = F2(
	function (session, verificationParam) {
		var _v0 = $author$project$Credentials$fromSessionToToken(session);
		if (_v0.$ === 'Just') {
			var token = _v0.a;
			var _v1 = A2(
				$simonh1000$elm_jwt$Jwt$decodeToken,
				$author$project$Credentials$decodeTokenData,
				$author$project$Credentials$fromTokenToString(token));
			if (_v1.$ === 'Ok') {
				var resultTokenRecord = _v1.a;
				return (!_Utils_eq(
					verificationParam,
					'/verify-email/' + $author$project$Credentials$verificationToString(resultTokenRecord.verificationstring))) ? _Utils_Tuple2(
					{userState: $author$project$Verification$VerificationFail},
					$elm$core$Platform$Cmd$none) : ((!resultTokenRecord.isverified) ? _Utils_Tuple2(
					{userState: $author$project$Verification$VerificationPending},
					A2($author$project$Verification$apiCallAfterSomeTime, session, $author$project$Verification$VerifyApiCallStart)) : _Utils_Tuple2(
					{userState: $author$project$Verification$Verified},
					$elm$core$Platform$Cmd$none));
			} else {
				return _Utils_Tuple2(
					{userState: $author$project$Verification$Sessionless},
					$elm$core$Platform$Cmd$none);
			}
		} else {
			return _Utils_Tuple2(
				{userState: $author$project$Verification$Sessionless},
				$elm$core$Platform$Cmd$none);
		}
	});
var $elm$core$Platform$Cmd$map = _Platform_map;
var $author$project$Main$initCurrentPage = function (_v0) {
	var url = _v0.a;
	var model = _v0.b;
	var existingCmds = _v0.c;
	var _v1 = model.page;
	switch (_v1.$) {
		case 'NotFoundPage':
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{page: $author$project$Main$NotFoundPage}),
				$elm$core$Platform$Cmd$none);
		case 'LoginPage':
			var _v2 = $author$project$Login$init(_Utils_Tuple0);
			var pageModel = _v2.a;
			var pageCmds = _v2.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						page: $author$project$Main$LoginPage(pageModel)
					}),
				A2($elm$core$Platform$Cmd$map, $author$project$Main$GotLoginMsg, pageCmds));
		case 'SignupPage':
			var _v3 = $author$project$Signup$init(_Utils_Tuple0);
			var pageModel = _v3.a;
			var pageCmds = _v3.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						page: $author$project$Main$SignupPage(pageModel)
					}),
				A2($elm$core$Platform$Cmd$map, $author$project$Main$GotSignupMsg, pageCmds));
		case 'HomePage':
			var _v4 = $author$project$Home$init(_Utils_Tuple0);
			var pageModel = _v4.a;
			var pageCmds = _v4.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						page: $author$project$Main$HomePage(pageModel)
					}),
				$elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							A2($elm$core$Platform$Cmd$map, $author$project$Main$GotHomeMsg, pageCmds),
							existingCmds
						])));
		case 'ChatPage':
			var _v5 = $author$project$Chat$init(model.session);
			var pageModel = _v5.a;
			var pageCmds = _v5.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						page: $author$project$Main$ChatPage(pageModel)
					}),
				$elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							A2($elm$core$Platform$Cmd$map, $author$project$Main$GotChatMsg, pageCmds),
							existingCmds
						])));
		case 'VerificationPage':
			var _v6 = A2($author$project$Verification$init, model.session, url.path);
			var pageModel = _v6.a;
			var pageCmds = _v6.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						page: $author$project$Main$VerificationPage(pageModel)
					}),
				A2($elm$core$Platform$Cmd$map, $author$project$Main$GotVerificationMsg, pageCmds));
		default:
			var _v7 = $author$project$Profile$init(model.session);
			var pageModel = _v7.a;
			var pageCmds = _v7.b;
			return _Utils_Tuple2(
				_Utils_update(
					model,
					{
						page: $author$project$Main$ProfilePage(pageModel)
					}),
				$elm$core$Platform$Cmd$batch(
					_List_fromArray(
						[
							A2($elm$core$Platform$Cmd$map, $author$project$Main$GotProfileMsg, pageCmds),
							existingCmds
						])));
	}
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $author$project$Main$Chat = {$: 'Chat'};
var $author$project$Main$Home = {$: 'Home'};
var $author$project$Main$Login = {$: 'Login'};
var $author$project$Main$Profile = function (a) {
	return {$: 'Profile', a: a};
};
var $author$project$Main$Signup = {$: 'Signup'};
var $author$project$Main$Verification = function (a) {
	return {$: 'Verification', a: a};
};
var $elm$url$Url$Parser$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var $elm$url$Url$Parser$State = F5(
	function (visited, unvisited, params, frag, value) {
		return {frag: frag, params: params, unvisited: unvisited, value: value, visited: visited};
	});
var $elm$url$Url$Parser$mapState = F2(
	function (func, _v0) {
		var value = _v0.value;
		var frag = _v0.frag;
		var params = _v0.params;
		var unvisited = _v0.unvisited;
		var visited = _v0.visited;
		return A5(
			$elm$url$Url$Parser$State,
			visited,
			unvisited,
			params,
			frag,
			func(value));
	});
var $elm$url$Url$Parser$map = F2(
	function (subValue, _v0) {
		var parseArg = _v0.a;
		return $elm$url$Url$Parser$Parser(
			function (_v1) {
				var value = _v1.value;
				var frag = _v1.frag;
				var params = _v1.params;
				var unvisited = _v1.unvisited;
				var visited = _v1.visited;
				return A2(
					$elm$core$List$map,
					$elm$url$Url$Parser$mapState(value),
					parseArg(
						A5($elm$url$Url$Parser$State, visited, unvisited, params, frag, subValue)));
			});
	});
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $elm$url$Url$Parser$oneOf = function (parsers) {
	return $elm$url$Url$Parser$Parser(
		function (state) {
			return A2(
				$elm$core$List$concatMap,
				function (_v0) {
					var parser = _v0.a;
					return parser(state);
				},
				parsers);
		});
};
var $elm$url$Url$Parser$s = function (str) {
	return $elm$url$Url$Parser$Parser(
		function (_v0) {
			var value = _v0.value;
			var frag = _v0.frag;
			var params = _v0.params;
			var unvisited = _v0.unvisited;
			var visited = _v0.visited;
			if (!unvisited.b) {
				return _List_Nil;
			} else {
				var next = unvisited.a;
				var rest = unvisited.b;
				return _Utils_eq(next, str) ? _List_fromArray(
					[
						A5(
						$elm$url$Url$Parser$State,
						A2($elm$core$List$cons, next, visited),
						rest,
						params,
						frag,
						value)
					]) : _List_Nil;
			}
		});
};
var $elm$url$Url$Parser$slash = F2(
	function (_v0, _v1) {
		var parseBefore = _v0.a;
		var parseAfter = _v1.a;
		return $elm$url$Url$Parser$Parser(
			function (state) {
				return A2(
					$elm$core$List$concatMap,
					parseAfter,
					parseBefore(state));
			});
	});
var $elm$url$Url$Parser$top = $elm$url$Url$Parser$Parser(
	function (state) {
		return _List_fromArray(
			[state]);
	});
var $elm$url$Url$Parser$custom = F2(
	function (tipe, stringToSomething) {
		return $elm$url$Url$Parser$Parser(
			function (_v0) {
				var value = _v0.value;
				var frag = _v0.frag;
				var params = _v0.params;
				var unvisited = _v0.unvisited;
				var visited = _v0.visited;
				if (!unvisited.b) {
					return _List_Nil;
				} else {
					var next = unvisited.a;
					var rest = unvisited.b;
					var _v2 = stringToSomething(next);
					if (_v2.$ === 'Just') {
						var nextValue = _v2.a;
						return _List_fromArray(
							[
								A5(
								$elm$url$Url$Parser$State,
								A2($elm$core$List$cons, next, visited),
								rest,
								params,
								frag,
								value(nextValue))
							]);
					} else {
						return _List_Nil;
					}
				}
			});
	});
var $author$project$Credentials$userIdParser = A2(
	$elm$url$Url$Parser$custom,
	'USERID',
	function (userId) {
		return A2(
			$elm$core$Maybe$map,
			$author$project$Credentials$UserId,
			$elm$core$Maybe$Just(userId));
	});
var $author$project$Credentials$verifictionStringParser = A2(
	$elm$url$Url$Parser$custom,
	'VERIFICATIONSTRING',
	function (verificationString) {
		return A2(
			$elm$core$Maybe$map,
			$author$project$Credentials$VerificationString,
			$elm$core$Maybe$Just(verificationString));
	});
var $author$project$Main$matchRoute = $elm$url$Url$Parser$oneOf(
	_List_fromArray(
		[
			A2($elm$url$Url$Parser$map, $author$project$Main$Home, $elm$url$Url$Parser$top),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$Login,
			$elm$url$Url$Parser$s('login')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$Profile,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('profile'),
				$author$project$Credentials$userIdParser)),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$Signup,
			$elm$url$Url$Parser$s('signup')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$Chat,
			$elm$url$Url$Parser$s('chat')),
			A2(
			$elm$url$Url$Parser$map,
			$author$project$Main$Verification,
			A2(
				$elm$url$Url$Parser$slash,
				$elm$url$Url$Parser$s('verify-email'),
				$author$project$Credentials$verifictionStringParser))
		]));
var $elm$url$Url$Parser$getFirstMatch = function (states) {
	getFirstMatch:
	while (true) {
		if (!states.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var state = states.a;
			var rest = states.b;
			var _v1 = state.unvisited;
			if (!_v1.b) {
				return $elm$core$Maybe$Just(state.value);
			} else {
				if ((_v1.a === '') && (!_v1.b.b)) {
					return $elm$core$Maybe$Just(state.value);
				} else {
					var $temp$states = rest;
					states = $temp$states;
					continue getFirstMatch;
				}
			}
		}
	}
};
var $elm$url$Url$Parser$removeFinalEmpty = function (segments) {
	if (!segments.b) {
		return _List_Nil;
	} else {
		if ((segments.a === '') && (!segments.b.b)) {
			return _List_Nil;
		} else {
			var segment = segments.a;
			var rest = segments.b;
			return A2(
				$elm$core$List$cons,
				segment,
				$elm$url$Url$Parser$removeFinalEmpty(rest));
		}
	}
};
var $elm$url$Url$Parser$preparePath = function (path) {
	var _v0 = A2($elm$core$String$split, '/', path);
	if (_v0.b && (_v0.a === '')) {
		var segments = _v0.b;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	} else {
		var segments = _v0;
		return $elm$url$Url$Parser$removeFinalEmpty(segments);
	}
};
var $elm$url$Url$Parser$addToParametersHelp = F2(
	function (value, maybeList) {
		if (maybeList.$ === 'Nothing') {
			return $elm$core$Maybe$Just(
				_List_fromArray(
					[value]));
		} else {
			var list = maybeList.a;
			return $elm$core$Maybe$Just(
				A2($elm$core$List$cons, value, list));
		}
	});
var $elm$url$Url$percentDecode = _Url_percentDecode;
var $elm$url$Url$Parser$addParam = F2(
	function (segment, dict) {
		var _v0 = A2($elm$core$String$split, '=', segment);
		if ((_v0.b && _v0.b.b) && (!_v0.b.b.b)) {
			var rawKey = _v0.a;
			var _v1 = _v0.b;
			var rawValue = _v1.a;
			var _v2 = $elm$url$Url$percentDecode(rawKey);
			if (_v2.$ === 'Nothing') {
				return dict;
			} else {
				var key = _v2.a;
				var _v3 = $elm$url$Url$percentDecode(rawValue);
				if (_v3.$ === 'Nothing') {
					return dict;
				} else {
					var value = _v3.a;
					return A3(
						$elm$core$Dict$update,
						key,
						$elm$url$Url$Parser$addToParametersHelp(value),
						dict);
				}
			}
		} else {
			return dict;
		}
	});
var $elm$url$Url$Parser$prepareQuery = function (maybeQuery) {
	if (maybeQuery.$ === 'Nothing') {
		return $elm$core$Dict$empty;
	} else {
		var qry = maybeQuery.a;
		return A3(
			$elm$core$List$foldr,
			$elm$url$Url$Parser$addParam,
			$elm$core$Dict$empty,
			A2($elm$core$String$split, '&', qry));
	}
};
var $elm$url$Url$Parser$parse = F2(
	function (_v0, url) {
		var parser = _v0.a;
		return $elm$url$Url$Parser$getFirstMatch(
			parser(
				A5(
					$elm$url$Url$Parser$State,
					_List_Nil,
					$elm$url$Url$Parser$preparePath(url.path),
					$elm$url$Url$Parser$prepareQuery(url.query),
					url.fragment,
					$elm$core$Basics$identity)));
	});
var $author$project$Main$urlToPage = F2(
	function (url, session) {
		var _v0 = A2($elm$url$Url$Parser$parse, $author$project$Main$matchRoute, url);
		if (_v0.$ === 'Just') {
			switch (_v0.a.$) {
				case 'Login':
					var _v1 = _v0.a;
					return _Utils_eq(
						$author$project$Credentials$fromSessionToToken(session),
						$elm$core$Maybe$Nothing) ? $author$project$Main$LoginPage(
						$author$project$Login$init(_Utils_Tuple0).a) : $author$project$Main$NotFoundPage;
				case 'Signup':
					var _v2 = _v0.a;
					return _Utils_eq(
						$author$project$Credentials$fromSessionToToken(session),
						$elm$core$Maybe$Nothing) ? $author$project$Main$SignupPage(
						$author$project$Signup$init(_Utils_Tuple0).a) : $author$project$Main$NotFoundPage;
				case 'Profile':
					return _Utils_eq(
						$author$project$Credentials$fromSessionToToken(session),
						$elm$core$Maybe$Nothing) ? $author$project$Main$NotFoundPage : $author$project$Main$ProfilePage(
						$author$project$Profile$init(session).a);
				case 'Verification':
					return _Utils_eq(
						$author$project$Credentials$fromSessionToToken(session),
						$elm$core$Maybe$Nothing) ? $author$project$Main$NotFoundPage : $author$project$Main$VerificationPage(
						A2($author$project$Verification$init, session, url.path).a);
				case 'Chat':
					var _v3 = _v0.a;
					return _Utils_eq(
						$author$project$Credentials$fromSessionToToken(session),
						$elm$core$Maybe$Nothing) ? $author$project$Main$NotFoundPage : $author$project$Main$ChatPage(
						$author$project$Chat$init(session).a);
				case 'Home':
					var _v4 = _v0.a;
					return $author$project$Main$HomePage(
						$author$project$Home$init(_Utils_Tuple0).a);
				default:
					var _v5 = _v0.a;
					return $author$project$Main$NotFoundPage;
			}
		} else {
			return $author$project$Main$NotFoundPage;
		}
	});
var $author$project$Main$init = F3(
	function (flags, url, key) {
		var session = A2($author$project$Credentials$decodeToSession, key, flags);
		var model = {
			key: key,
			openDropdown: false,
			page: A2($author$project$Main$urlToPage, url, session),
			session: session,
			time: $elm$core$Maybe$Nothing
		};
		return $author$project$Main$initCurrentPage(
			_Utils_Tuple3(
				url,
				model,
				A2($elm$core$Task$perform, $author$project$Main$GotTime, $elm$time$Time$now)));
	});
var $author$project$Main$GotSubscriptionChangeMsg = function (a) {
	return {$: 'GotSubscriptionChangeMsg', a: a};
};
var $author$project$Main$GotSubscriptionSocketMsg = function (a) {
	return {$: 'GotSubscriptionSocketMsg', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $elm$core$Debug$toString = _Debug_toString;
var $author$project$Credentials$decodeToSocket = F2(
	function (key, value) {
		var result = A2($elm$json$Json$Decode$decodeValue, $author$project$Credentials$decodeSocketMessage, value);
		if (result.$ === 'Ok') {
			var obj = result.a;
			return obj;
		} else {
			var err = result.a;
			return {
				clientId: '',
				connectionId: '',
				data: {message: ''},
				id: '',
				name: $elm$core$Debug$toString(err),
				timestamp: 0
			};
		}
	});
var $author$project$Credentials$listenSocketMessage = _Platform_incomingPort('listenSocketMessage', $elm$json$Json$Decode$value);
var $author$project$Credentials$socketMessageChanges = F2(
	function (toMsg, key) {
		return $author$project$Credentials$listenSocketMessage(
			function (val) {
				return toMsg(
					A2($author$project$Credentials$decodeToSocket, key, val));
			});
	});
var $author$project$Credentials$onSessionChange = _Platform_incomingPort('onSessionChange', $elm$json$Json$Decode$value);
var $author$project$Credentials$subscriptionChanges = F2(
	function (toMsg, key) {
		return $author$project$Credentials$onSessionChange(
			function (val) {
				return toMsg(
					A2($author$project$Credentials$decodeToSession, key, val));
			});
	});
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2($author$project$Credentials$subscriptionChanges, $author$project$Main$GotSubscriptionChangeMsg, model.key),
				A2($author$project$Credentials$socketMessageChanges, $author$project$Main$GotSubscriptionSocketMsg, model.key)
			]));
};
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $elm$core$Basics$round = _Basics_round;
var $simonh1000$elm_jwt$Jwt$getTokenExpirationMillis = function (token) {
	var decodeExp = $elm$json$Json$Decode$oneOf(
		_List_fromArray(
			[
				$elm$json$Json$Decode$int,
				A2($elm$json$Json$Decode$map, $elm$core$Basics$round, $elm$json$Json$Decode$float)
			]));
	return A2(
		$elm$core$Result$map,
		function (exp) {
			return 1000 * exp;
		},
		A2(
			$simonh1000$elm_jwt$Jwt$decodeToken,
			A2($elm$json$Json$Decode$field, 'exp', decodeExp),
			token));
};
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $simonh1000$elm_jwt$Jwt$isExpired = F2(
	function (now, token) {
		return A2(
			$elm$core$Result$map,
			function (millis) {
				return _Utils_cmp(
					$elm$time$Time$posixToMillis(now),
					millis) > 0;
			},
			$simonh1000$elm_jwt$Jwt$getTokenExpirationMillis(token));
	});
var $elm$browser$Browser$Navigation$load = _Browser_load;
var $elm$core$Maybe$destruct = F3(
	function (_default, func, maybe) {
		if (maybe.$ === 'Just') {
			var a = maybe.a;
			return func(a);
		} else {
			return _default;
		}
	});
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $author$project$Credentials$storeSession = _Platform_outgoingPort(
	'storeSession',
	function ($) {
		return A3($elm$core$Maybe$destruct, $elm$json$Json$Encode$null, $elm$json$Json$Encode$string, $);
	});
var $author$project$Credentials$logout = $author$project$Credentials$storeSession($elm$core$Maybe$Nothing);
var $elm$browser$Browser$Navigation$pushUrl = _Browser_pushUrl;
var $elm$browser$Browser$Navigation$reload = _Browser_reload(false);
var $elm$url$Url$addPort = F2(
	function (maybePort, starter) {
		if (maybePort.$ === 'Nothing') {
			return starter;
		} else {
			var port_ = maybePort.a;
			return starter + (':' + $elm$core$String$fromInt(port_));
		}
	});
var $elm$url$Url$addPrefixed = F3(
	function (prefix, maybeSegment, starter) {
		if (maybeSegment.$ === 'Nothing') {
			return starter;
		} else {
			var segment = maybeSegment.a;
			return _Utils_ap(
				starter,
				_Utils_ap(prefix, segment));
		}
	});
var $elm$url$Url$toString = function (url) {
	var http = function () {
		var _v0 = url.protocol;
		if (_v0.$ === 'Http') {
			return 'http://';
		} else {
			return 'https://';
		}
	}();
	return A3(
		$elm$url$Url$addPrefixed,
		'#',
		url.fragment,
		A3(
			$elm$url$Url$addPrefixed,
			'?',
			url.query,
			_Utils_ap(
				A2(
					$elm$url$Url$addPort,
					url.port_,
					_Utils_ap(http, url.host)),
				url.path)));
};
var $author$project$Chat$MessageReceived = function (a) {
	return {$: 'MessageReceived', a: a};
};
var $author$project$Chat$unfoldMessageReceived = function (socketData) {
	return $author$project$Chat$MessageReceived(socketData);
};
var $author$project$Helpers$buildErrorMessage = function (httpError) {
	switch (httpError.$) {
		case 'BadUrl':
			var message = httpError.a;
			return message;
		case 'Timeout':
			return 'Server is taking too long to respond. Please try again later.';
		case 'NetworkError':
			return 'Unable to reach server.';
		case 'BadStatus':
			var statusCode = httpError.a;
			return 'Request failed with status code: ' + $elm$core$String$fromInt(statusCode);
		default:
			var message = httpError.a;
			return message;
	}
};
var $author$project$Credentials$emitTyping = _Platform_outgoingPort('emitTyping', $elm$json$Json$Encode$string);
var $author$project$Credentials$sendMessageToSocket = _Platform_outgoingPort('sendMessageToSocket', $elm$json$Json$Encode$string);
var $author$project$Chat$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'StoreMessage':
				var message = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{message: message}),
					$author$project$Credentials$emitTyping(message));
			case 'MessageSubmit':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{message: ''}),
					$author$project$Credentials$sendMessageToSocket(model.message));
			case 'MessageReceived':
				var message = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							receivedMessages: _Utils_ap(
								model.receivedMessages,
								_List_fromArray(
									[message]))
						}),
					$elm$core$Platform$Cmd$none);
			case 'FetchUsers':
				if (msg.a.$ === 'Ok') {
					var users = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{users: users}),
						$elm$core$Platform$Cmd$none);
				} else {
					var err = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								error: $elm$core$Maybe$Just(
									$author$project$Helpers$buildErrorMessage(err))
							}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				if (msg.a.$ === 'Ok') {
					var messages = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								receivedMessages: _Utils_ap(model.receivedMessages, messages)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var err = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								error: $elm$core$Maybe$Just(
									$author$project$Helpers$buildErrorMessage(err))
							}),
						$elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$Home$update = F2(
	function (msg, model) {
		return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
	});
var $author$project$Login$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Login$HideError = {$: 'HideError'};
var $author$project$Login$Loading = {$: 'Loading'};
var $author$project$Credentials$encodeToken = function (_v0) {
	var token = _v0.a;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'token',
				$elm$json$Json$Encode$string(token))
			]));
};
var $author$project$Login$LoginDone = function (a) {
	return {$: 'LoginDone', a: a};
};
var $author$project$User$fromEmailToString = function (_v0) {
	var validEmail = _v0.a;
	return validEmail;
};
var $author$project$User$fromPasswordToString = function (_v0) {
	var validPassword = _v0.a;
	return validPassword;
};
var $author$project$User$credentialsEncoder = function (_v0) {
	var email = _v0.email;
	var password = _v0.password;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'email',
				$elm$json$Json$Encode$string(
					$author$project$User$fromEmailToString(email))),
				_Utils_Tuple2(
				'password',
				$elm$json$Json$Encode$string(
					$author$project$User$fromPasswordToString(password)))
			]));
};
var $elm$http$Http$jsonBody = function (value) {
	return A2(
		_Http_pair,
		'application/json',
		A2($elm$json$Json$Encode$encode, 0, value));
};
var $elm$http$Http$post = function (r) {
	return $elm$http$Http$request(
		{body: r.body, expect: r.expect, headers: _List_Nil, method: 'POST', timeout: $elm$core$Maybe$Nothing, tracker: $elm$core$Maybe$Nothing, url: r.url});
};
var $author$project$Login$submitLogin = function (credentials) {
	return $elm$http$Http$post(
		{
			body: $elm$http$Http$jsonBody(
				$author$project$User$credentialsEncoder(credentials)),
			expect: A2($elm$http$Http$expectJson, $author$project$Login$LoginDone, $author$project$Credentials$tokenDecoder),
			url: '/api/login'
		});
};
var $author$project$User$ValidCredentials = F2(
	function (email, password) {
		return {email: email, password: password};
	});
var $author$project$User$Email = function (a) {
	return {$: 'Email', a: a};
};
var $elm$regex$Regex$Match = F4(
	function (match, index, number, submatches) {
		return {index: index, match: match, number: number, submatches: submatches};
	});
var $elm$regex$Regex$contains = _Regex_contains;
var $elm$core$String$trim = _String_trim;
var $elm$regex$Regex$fromStringWith = _Regex_fromStringWith;
var $elm$regex$Regex$never = _Regex_never;
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $author$project$Helpers$validEmail = A2(
	$elm$core$Maybe$withDefault,
	$elm$regex$Regex$never,
	A2(
		$elm$regex$Regex$fromStringWith,
		{caseInsensitive: true, multiline: false},
		'^[a-zA-Z0-9.!#$%&\'*+\\/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$'));
var $author$project$User$fromStringToValidEmail = function (email) {
	var trimmedEmail = $elm$core$String$trim(email);
	return $elm$core$String$isEmpty(trimmedEmail) ? $elm$core$Result$Err('Email can\'t be empty') : ((!A2($elm$regex$Regex$contains, $author$project$Helpers$validEmail, trimmedEmail)) ? $elm$core$Result$Err('Email is invalid') : $elm$core$Result$Ok(
		$author$project$User$Email(trimmedEmail)));
};
var $author$project$User$Password = function (a) {
	return {$: 'Password', a: a};
};
var $author$project$User$fromStringToValidPassword = function (password) {
	var trimmedPassword = $elm$core$String$trim(password);
	return $elm$core$String$isEmpty(trimmedPassword) ? $elm$core$Result$Err('Password can\'t be empty') : (($elm$core$String$length(trimmedPassword) < 10) ? $elm$core$Result$Err('Password can\'t be less then 10 characters') : $elm$core$Result$Ok(
		$author$project$User$Password(trimmedPassword)));
};
var $author$project$User$validateCredentials = function (_v0) {
	var email = _v0.email;
	var password = _v0.password;
	return A3(
		$elm$core$Result$map2,
		$author$project$User$ValidCredentials,
		$author$project$User$fromStringToValidEmail(email),
		$author$project$User$fromStringToValidPassword(password));
};
var $author$project$Login$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'StoreEmail':
				var email = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{storeEmail: email}),
					$elm$core$Platform$Cmd$none);
			case 'StorePassword':
				var password = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{storePassword: password}),
					$elm$core$Platform$Cmd$none);
			case 'LoginSubmit':
				var validatedCred = $author$project$User$validateCredentials(
					{email: model.storeEmail, password: model.storePassword});
				if (validatedCred.$ === 'Err') {
					var error = validatedCred.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								formState: $author$project$Login$Error(error)
							}),
						A2(
							$elm$core$Task$perform,
							function (_v2) {
								return $author$project$Login$HideError;
							},
							$elm$core$Process$sleep(4000)));
				} else {
					var validCredentials = validatedCred.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{formState: $author$project$Login$Loading}),
						$author$project$Login$submitLogin(validCredentials));
				}
			case 'HideError':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{formState: $author$project$Login$Initial}),
					$elm$core$Platform$Cmd$none);
			default:
				if (msg.a.$ === 'Ok') {
					var token = msg.a.a;
					var tokenValue = $author$project$Credentials$encodeToken(token);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{formState: $author$project$Login$Initial}),
						$author$project$Credentials$storeSession(
							$elm$core$Maybe$Just(
								A2($elm$json$Json$Encode$encode, 0, tokenValue))));
				} else {
					var error = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								formState: $author$project$Login$Error(
									$author$project$Helpers$buildErrorMessage(error))
							}),
						A2(
							$elm$core$Task$perform,
							function (_v3) {
								return $author$project$Login$HideError;
							},
							$elm$core$Process$sleep(4000)));
				}
		}
	});
var $author$project$Profile$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Profile$FileRead = function (a) {
	return {$: 'FileRead', a: a};
};
var $author$project$Profile$FileRequestProceed = function (a) {
	return {$: 'FileRequestProceed', a: a};
};
var $author$project$Profile$Loading = {$: 'Loading'};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$core$Task$onError = _Scheduler_onError;
var $elm$core$Task$attempt = F2(
	function (resultToMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2(
					$elm$core$Task$onError,
					A2(
						$elm$core$Basics$composeL,
						A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
						$elm$core$Result$Err),
					A2(
						$elm$core$Task$andThen,
						A2(
							$elm$core$Basics$composeL,
							A2($elm$core$Basics$composeL, $elm$core$Task$succeed, resultToMessage),
							$elm$core$Result$Ok),
						task))));
	});
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $author$project$Profile$ProfileDone = function (a) {
	return {$: 'ProfileDone', a: a};
};
var $elm$http$Http$Header = F2(
	function (a, b) {
		return {$: 'Header', a: a, b: b};
	});
var $elm$http$Http$header = $elm$http$Http$Header;
var $author$project$Credentials$addHeader = function (_v0) {
	var tokenString = _v0.a;
	return A2($elm$http$Http$header, 'authorization', 'Token ' + tokenString);
};
var $author$project$Credentials$encodeImageString = function (imageString) {
	return $elm$json$Json$Encode$string(imageString);
};
var $author$project$Profile$profileSubmitDataEncoder = function (_v0) {
	var name = _v0.name;
	var profilePic = _v0.profilePic;
	return $elm$json$Json$Encode$object(
		_List_fromArray(
			[
				_Utils_Tuple2(
				'firstname',
				$elm$json$Json$Encode$string(name)),
				_Utils_Tuple2(
				'imagefile',
				$author$project$Credentials$encodeImageString(profilePic))
			]));
};
var $author$project$Profile$submitProfile = F2(
	function (session, data) {
		var _v0 = $author$project$Credentials$fromSessionToToken(session);
		if (_v0.$ === 'Just') {
			var token = _v0.a;
			return $elm$http$Http$request(
				{
					body: $elm$http$Http$jsonBody(
						$author$project$Profile$profileSubmitDataEncoder(data)),
					expect: A2($elm$http$Http$expectJson, $author$project$Profile$ProfileDone, $author$project$Credentials$tokenDecoder),
					headers: _List_fromArray(
						[
							$author$project$Credentials$addHeader(token)
						]),
					method: 'PUT',
					timeout: $elm$core$Maybe$Nothing,
					tracker: $elm$core$Maybe$Nothing,
					url: '/api/profile'
				});
		} else {
			return $elm$core$Platform$Cmd$none;
		}
	});
var $elm$file$File$toUrl = _File_toUrl;
var $author$project$Profile$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'StoreFirstName':
				var firstName = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{storeName: firstName}),
					$elm$core$Platform$Cmd$none);
			case 'ProfileSubmit':
				var session = msg.a;
				var imageOrNot = function () {
					var _v1 = model.profilePic;
					if (_v1.$ === 'Nothing') {
						return '';
					} else {
						var imageUrl = _v1.a;
						return imageUrl;
					}
				}();
				return $elm$core$String$isEmpty(model.storeName) ? _Utils_Tuple2(
					_Utils_update(
						model,
						{
							formState: $author$project$Profile$Error('Name can\'t be empty')
						}),
					$elm$core$Platform$Cmd$none) : _Utils_Tuple2(
					_Utils_update(
						model,
						{formState: $author$project$Profile$Loading}),
					A2(
						$author$project$Profile$submitProfile,
						session,
						{name: model.storeName, profilePic: imageOrNot}));
			case 'ProfileDone':
				if (msg.a.$ === 'Ok') {
					var token = msg.a.a;
					var tokenValue = $author$project$Credentials$encodeToken(token);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{formState: $author$project$Profile$Initial}),
						$author$project$Credentials$storeSession(
							$elm$core$Maybe$Just(
								A2($elm$json$Json$Encode$encode, 0, tokenValue))));
				} else {
					var error = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								formState: $author$project$Profile$Error(
									$author$project$Helpers$buildErrorMessage(error))
							}),
						function () {
							if (error.$ === 'BadStatus') {
								var statusCode = error.a;
								return (statusCode === 401) ? $author$project$Credentials$logout : $elm$core$Platform$Cmd$none;
							} else {
								return $elm$core$Platform$Cmd$none;
							}
						}());
				}
			case 'FileRequest':
				return _Utils_Tuple2(
					model,
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['image/*']),
						$author$project$Profile$FileRequestProceed));
			case 'FileRequestProceed':
				var file = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$core$Task$attempt,
						$author$project$Profile$FileRead,
						$elm$file$File$toUrl(file)));
			default:
				if (msg.a.$ === 'Ok') {
					var imageFileString = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								profilePic: $elm$core$Maybe$Just(imageFileString)
							}),
						$elm$core$Platform$Cmd$none);
				} else {
					var error = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								formState: $author$project$Profile$Error(
									$author$project$Helpers$buildErrorMessage(error))
							}),
						$elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$Signup$Error = function (a) {
	return {$: 'Error', a: a};
};
var $author$project$Signup$HideError = {$: 'HideError'};
var $author$project$Signup$Loading = {$: 'Loading'};
var $author$project$User$andThenValidateConfirmPassword = F2(
	function (confirmPassword, resultCredential) {
		return A2(
			$elm$core$Result$andThen,
			function (_v0) {
				var email = _v0.email;
				var password = _v0.password;
				var validPassword = $author$project$User$fromPasswordToString(password);
				var trimmedConfirmPassword = $elm$core$String$trim(confirmPassword);
				return ($elm$core$String$length(trimmedConfirmPassword) < 10) ? $elm$core$Result$Err('Confirm password can\'t be less then 10 characters') : ((!_Utils_eq(validPassword, trimmedConfirmPassword)) ? $elm$core$Result$Err('Passwords doesn\'t match') : ($elm$core$String$isEmpty(trimmedConfirmPassword) ? $elm$core$Result$Err('Confirm password can\'t be empty') : $elm$core$Result$Ok(
					{email: email, password: password})));
			},
			resultCredential);
	});
var $author$project$Signup$SignupDone = function (a) {
	return {$: 'SignupDone', a: a};
};
var $author$project$Signup$submitSignup = function (credentials) {
	return $elm$http$Http$post(
		{
			body: $elm$http$Http$jsonBody(
				$author$project$User$credentialsEncoder(credentials)),
			expect: A2($elm$http$Http$expectJson, $author$project$Signup$SignupDone, $author$project$Credentials$tokenDecoder),
			url: '/api/signup'
		});
};
var $author$project$Signup$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'StoreEmail':
				var email = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{storeEmail: email}),
					$elm$core$Platform$Cmd$none);
			case 'StorePassword':
				var password = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{storePassword: password}),
					$elm$core$Platform$Cmd$none);
			case 'StoreConfirmPassword':
				var confirmPassword = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{storeConfirmPassword: confirmPassword}),
					$elm$core$Platform$Cmd$none);
			case 'SignupSubmit':
				var validatedCred = A2(
					$author$project$User$andThenValidateConfirmPassword,
					model.storeConfirmPassword,
					$author$project$User$validateCredentials(
						{email: model.storeEmail, password: model.storePassword}));
				if (validatedCred.$ === 'Err') {
					var error = validatedCred.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								formState: $author$project$Signup$Error(error)
							}),
						A2(
							$elm$core$Task$perform,
							function (_v2) {
								return $author$project$Signup$HideError;
							},
							$elm$core$Process$sleep(4000)));
				} else {
					var validCredentials = validatedCred.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{formState: $author$project$Signup$Loading}),
						$author$project$Signup$submitSignup(validCredentials));
				}
			case 'HideError':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{formState: $author$project$Signup$Initial}),
					$elm$core$Platform$Cmd$none);
			default:
				if (msg.a.$ === 'Ok') {
					var token = msg.a.a;
					var tokenValue = $author$project$Credentials$encodeToken(token);
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{formState: $author$project$Signup$Initial}),
						$author$project$Credentials$storeSession(
							$elm$core$Maybe$Just(
								A2($elm$json$Json$Encode$encode, 0, tokenValue))));
				} else {
					var error = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								formState: $author$project$Signup$Error(
									$author$project$Helpers$buildErrorMessage(error))
							}),
						$elm$core$Platform$Cmd$none);
				}
		}
	});
var $author$project$Verification$TokenToLS = function (a) {
	return {$: 'TokenToLS', a: a};
};
var $author$project$Verification$VerificationDone = {$: 'VerificationDone'};
var $author$project$Verification$VerifyDone = function (a) {
	return {$: 'VerifyDone', a: a};
};
var $author$project$Verification$apiCallToVerify = function (session) {
	var _v0 = $author$project$Credentials$fromSessionToToken(session);
	if (_v0.$ === 'Just') {
		var token = _v0.a;
		return $elm$http$Http$request(
			{
				body: $elm$http$Http$emptyBody,
				expect: A2($elm$http$Http$expectJson, $author$project$Verification$VerifyDone, $author$project$Credentials$tokenDecoder),
				headers: _List_fromArray(
					[
						$author$project$Credentials$addHeader(token)
					]),
				method: 'PUT',
				timeout: $elm$core$Maybe$Nothing,
				tracker: $elm$core$Maybe$Nothing,
				url: '/api/verify'
			});
	} else {
		return $elm$core$Platform$Cmd$none;
	}
};
var $author$project$Verification$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'VerifyApiCallStart':
				var session = msg.a;
				return _Utils_Tuple2(
					model,
					$author$project$Verification$apiCallToVerify(session));
			case 'VerifyDone':
				if (msg.a.$ === 'Ok') {
					var token = msg.a.a;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{userState: $author$project$Verification$VerificationDone}),
						A2(
							$elm$core$Task$perform,
							function (_v1) {
								return $author$project$Verification$TokenToLS(token);
							},
							$elm$core$Process$sleep(5000)));
				} else {
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{userState: $author$project$Verification$VerificationFail}),
						$elm$core$Platform$Cmd$none);
				}
			default:
				var token = msg.a;
				var tokenValue = $author$project$Credentials$encodeToken(token);
				return _Utils_Tuple2(
					model,
					$author$project$Credentials$storeSession(
						$elm$core$Maybe$Just(
							A2($elm$json$Json$Encode$encode, 0, tokenValue))));
		}
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'ClickedLink':
				var urlRequest = msg.a;
				if (urlRequest.$ === 'External') {
					var href = urlRequest.a;
					return _Utils_Tuple2(
						model,
						$elm$browser$Browser$Navigation$load(href));
				} else {
					var url = urlRequest.a;
					var urlPath = url.path;
					return _Utils_Tuple2(
						model,
						(urlPath === '/chat') ? $elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									$elm$browser$Browser$Navigation$reload,
									A2(
									$elm$browser$Browser$Navigation$pushUrl,
									model.key,
									$elm$url$Url$toString(url))
								])) : A2(
							$elm$browser$Browser$Navigation$pushUrl,
							model.key,
							$elm$url$Url$toString(url)));
				}
			case 'ChangedUrl':
				var url = msg.a;
				var newPage = A2($author$project$Main$urlToPage, url, model.session);
				return $author$project$Main$initCurrentPage(
					_Utils_Tuple3(
						url,
						_Utils_update(
							model,
							{page: newPage}),
						$elm$core$Platform$Cmd$none));
			case 'GotLoginMsg':
				var loginMsg = msg.a;
				var _v2 = model.page;
				if (_v2.$ === 'LoginPage') {
					var loginModel = _v2.a;
					var _v3 = A2($author$project$Login$update, loginMsg, loginModel);
					var loginModelFromLogin = _v3.a;
					var loginMsgFromLogin = _v3.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								page: $author$project$Main$LoginPage(loginModelFromLogin)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$GotLoginMsg, loginMsgFromLogin));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'GotProfileMsg':
				var profileMsg = msg.a;
				var _v4 = model.page;
				if (_v4.$ === 'ProfilePage') {
					var profileModel = _v4.a;
					var _v5 = A2($author$project$Profile$update, profileMsg, profileModel);
					var profileModelFromProfile = _v5.a;
					var profileMsgFromProfile = _v5.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								page: $author$project$Main$ProfilePage(profileModelFromProfile)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$GotProfileMsg, profileMsgFromProfile));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'GotHomeMsg':
				var homeMsg = msg.a;
				var _v6 = model.page;
				if (_v6.$ === 'HomePage') {
					var homeModel = _v6.a;
					var _v7 = A2($author$project$Home$update, homeMsg, homeModel);
					var homeModelFromHome = _v7.a;
					var homeMsgFromHome = _v7.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								page: $author$project$Main$HomePage(homeModelFromHome)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$GotHomeMsg, homeMsgFromHome));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'GotVerificationMsg':
				var verificationMsg = msg.a;
				var _v8 = model.page;
				if (_v8.$ === 'VerificationPage') {
					var verificationModel = _v8.a;
					var _v9 = A2($author$project$Verification$update, verificationMsg, verificationModel);
					var verificationModelFromVerification = _v9.a;
					var verificationMsgFromVerification = _v9.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								page: $author$project$Main$VerificationPage(verificationModelFromVerification)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$GotVerificationMsg, verificationMsgFromVerification));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'GotSignupMsg':
				var signupMsg = msg.a;
				var _v10 = model.page;
				if (_v10.$ === 'SignupPage') {
					var signupModel = _v10.a;
					var _v11 = A2($author$project$Signup$update, signupMsg, signupModel);
					var signupModelFromSignup = _v11.a;
					var signupMsgFromSignup = _v11.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								page: $author$project$Main$SignupPage(signupModelFromSignup)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$GotSignupMsg, signupMsgFromSignup));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'GotSubscriptionChangeMsg':
				var session = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{session: session}),
					function () {
						var _v12 = $author$project$Credentials$fromSessionToToken(session);
						if (_v12.$ === 'Just') {
							var token = _v12.a;
							var _v13 = A2(
								$simonh1000$elm_jwt$Jwt$decodeToken,
								$author$project$Credentials$decodeTokenData,
								$author$project$Credentials$fromTokenToString(token));
							if (_v13.$ === 'Ok') {
								var resultTokenRecord = _v13.a;
								return A2(
									$elm$browser$Browser$Navigation$pushUrl,
									model.key,
									'/profile/' + $author$project$Credentials$userIdToString(resultTokenRecord.id));
							} else {
								return A2($elm$browser$Browser$Navigation$pushUrl, model.key, '/login');
							}
						} else {
							return A2($elm$browser$Browser$Navigation$pushUrl, model.key, '/login');
						}
					}());
			case 'GotSubscriptionSocketMsg':
				var socketMsgObj = msg.a;
				var _v14 = model.page;
				if (_v14.$ === 'ChatPage') {
					var chatModel = _v14.a;
					var chatMsg = $author$project$Chat$unfoldMessageReceived(socketMsgObj);
					var _v15 = A2($author$project$Chat$update, chatMsg, chatModel);
					var chatModelFromChat = _v15.a;
					var chatMsgFromChat = _v15.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								page: $author$project$Main$ChatPage(chatModelFromChat)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$GotChatMsg, chatMsgFromChat));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'GotChatMsg':
				var chatMsg = msg.a;
				var _v16 = model.page;
				if (_v16.$ === 'ChatPage') {
					var chatModel = _v16.a;
					var _v17 = A2($author$project$Chat$update, chatMsg, chatModel);
					var chatModelFromChat = _v17.a;
					var chatMsgFromChat = _v17.b;
					return _Utils_Tuple2(
						_Utils_update(
							model,
							{
								page: $author$project$Main$ChatPage(chatModelFromChat)
							}),
						A2($elm$core$Platform$Cmd$map, $author$project$Main$GotChatMsg, chatMsgFromChat));
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'CheckSessionExpired':
				var _v18 = msg.a;
				var session = _v18.a;
				var maybeTime = _v18.b;
				var _v19 = _Utils_Tuple2(
					maybeTime,
					$author$project$Credentials$fromSessionToToken(session));
				if ((_v19.a.$ === 'Just') && (_v19.b.$ === 'Just')) {
					var time = _v19.a.a;
					var token = _v19.b.a;
					var tokenString = $author$project$Credentials$fromTokenToString(token);
					var _v20 = A2($simonh1000$elm_jwt$Jwt$isExpired, time, tokenString);
					if (_v20.$ === 'Ok') {
						var isExpired = _v20.a;
						return isExpired ? _Utils_Tuple2(model, $author$project$Credentials$logout) : _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					} else {
						return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
					}
				} else {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				}
			case 'GetLogout':
				return _Utils_Tuple2(model, $author$project$Credentials$logout);
			case 'ChatNewMessage':
				return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
			case 'GotTime':
				var time = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							time: $elm$core$Maybe$Just(time)
						}),
					$elm$core$Platform$Cmd$none);
			default:
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{openDropdown: !model.openDropdown}),
					$elm$core$Platform$Cmd$none);
		}
	});
var $author$project$Main$CheckSessionExpired = function (a) {
	return {$: 'CheckSessionExpired', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$Node = F3(
	function (a, b, c) {
		return {$: 'Node', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$node = $rtfeldman$elm_css$VirtualDom$Styled$Node;
var $rtfeldman$elm_css$Html$Styled$node = $rtfeldman$elm_css$VirtualDom$Styled$node;
var $rtfeldman$elm_css$Html$Styled$div = $rtfeldman$elm_css$Html$Styled$node('div');
var $rtfeldman$elm_css$VirtualDom$Styled$KeyedNode = F3(
	function (a, b, c) {
		return {$: 'KeyedNode', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$KeyedNodeNS = F4(
	function (a, b, c, d) {
		return {$: 'KeyedNodeNS', a: a, b: b, c: c, d: d};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$NodeNS = F4(
	function (a, b, c, d) {
		return {$: 'NodeNS', a: a, b: b, c: c, d: d};
	});
var $rtfeldman$elm_css$VirtualDom$Styled$Unstyled = function (a) {
	return {$: 'Unstyled', a: a};
};
var $elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var $rtfeldman$elm_css$VirtualDom$Styled$Attribute = F3(
	function (a, b, c) {
		return {$: 'Attribute', a: a, b: b, c: c};
	});
var $elm$virtual_dom$VirtualDom$mapAttribute = _VirtualDom_mapAttribute;
var $rtfeldman$elm_css$VirtualDom$Styled$mapAttribute = F2(
	function (transform, _v0) {
		var prop = _v0.a;
		var isCssStyle = _v0.b;
		var cssTemplate = _v0.c;
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$mapAttribute, transform, prop),
			isCssStyle,
			cssTemplate);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$map = F2(
	function (transform, vdomNode) {
		switch (vdomNode.$) {
			case 'Node':
				var elemType = vdomNode.a;
				var properties = vdomNode.b;
				var children = vdomNode.c;
				return A3(
					$rtfeldman$elm_css$VirtualDom$Styled$Node,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$mapAttribute(transform),
						properties),
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$map(transform),
						children));
			case 'NodeNS':
				var ns = vdomNode.a;
				var elemType = vdomNode.b;
				var properties = vdomNode.c;
				var children = vdomNode.d;
				return A4(
					$rtfeldman$elm_css$VirtualDom$Styled$NodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$mapAttribute(transform),
						properties),
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$map(transform),
						children));
			case 'KeyedNode':
				var elemType = vdomNode.a;
				var properties = vdomNode.b;
				var children = vdomNode.c;
				return A3(
					$rtfeldman$elm_css$VirtualDom$Styled$KeyedNode,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$mapAttribute(transform),
						properties),
					A2(
						$elm$core$List$map,
						function (_v1) {
							var key = _v1.a;
							var child = _v1.b;
							return _Utils_Tuple2(
								key,
								A2($rtfeldman$elm_css$VirtualDom$Styled$map, transform, child));
						},
						children));
			case 'KeyedNodeNS':
				var ns = vdomNode.a;
				var elemType = vdomNode.b;
				var properties = vdomNode.c;
				var children = vdomNode.d;
				return A4(
					$rtfeldman$elm_css$VirtualDom$Styled$KeyedNodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$mapAttribute(transform),
						properties),
					A2(
						$elm$core$List$map,
						function (_v2) {
							var key = _v2.a;
							var child = _v2.b;
							return _Utils_Tuple2(
								key,
								A2($rtfeldman$elm_css$VirtualDom$Styled$map, transform, child));
						},
						children));
			default:
				var vdom = vdomNode.a;
				return $rtfeldman$elm_css$VirtualDom$Styled$Unstyled(
					A2($elm$virtual_dom$VirtualDom$map, transform, vdom));
		}
	});
var $rtfeldman$elm_css$Html$Styled$map = $rtfeldman$elm_css$VirtualDom$Styled$map;
var $rtfeldman$elm_css$Html$Styled$p = $rtfeldman$elm_css$Html$Styled$node('p');
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $rtfeldman$elm_css$VirtualDom$Styled$text = function (str) {
	return $rtfeldman$elm_css$VirtualDom$Styled$Unstyled(
		$elm$virtual_dom$VirtualDom$text(str));
};
var $rtfeldman$elm_css$Html$Styled$text = $rtfeldman$elm_css$VirtualDom$Styled$text;
var $author$project$Chat$MessageSubmit = {$: 'MessageSubmit'};
var $author$project$Chat$StoreMessage = function (a) {
	return {$: 'StoreMessage', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$ApplyStyles = function (a) {
	return {$: 'ApplyStyles', a: a};
};
var $rtfeldman$elm_css$Css$batch = $rtfeldman$elm_css$Css$Preprocess$ApplyStyles;
var $rtfeldman$elm_css$Css$Preprocess$AppendProperty = function (a) {
	return {$: 'AppendProperty', a: a};
};
var $rtfeldman$elm_css$Css$Structure$Property = function (a) {
	return {$: 'Property', a: a};
};
var $rtfeldman$elm_css$Css$property = F2(
	function (key, value) {
		return $rtfeldman$elm_css$Css$Preprocess$AppendProperty(
			$rtfeldman$elm_css$Css$Structure$Property(key + (':' + value)));
	});
var $matheus23$elm_tailwind_modules_base$Tailwind$Color$propertyWithColor = F4(
	function (property, embedColor, opacityVarName, color) {
		if (color.$ === 'Color') {
			var mode = color.a;
			var r = color.b;
			var g = color.c;
			var b = color.d;
			var opacity = color.e;
			var _v1 = _Utils_Tuple2(opacity, opacityVarName);
			if (_v1.a.$ === 'Opacity') {
				var op = _v1.a.a;
				return A2(
					$rtfeldman$elm_css$Css$property,
					property,
					embedColor(mode + ('(' + (r + (' ' + (g + (' ' + (b + (' / ' + (op + ')'))))))))));
			} else {
				if (_v1.b.$ === 'Just') {
					var _v2 = _v1.a;
					var varName = _v1.b.a;
					return $rtfeldman$elm_css$Css$batch(
						_List_fromArray(
							[
								A2($rtfeldman$elm_css$Css$property, varName, '1'),
								A2(
								$rtfeldman$elm_css$Css$property,
								property,
								embedColor(mode + ('(' + (r + (' ' + (g + (' ' + (b + (' / var(' + (varName + '))'))))))))))
							]));
				} else {
					var _v3 = _v1.a;
					var _v4 = _v1.b;
					return A2(
						$rtfeldman$elm_css$Css$property,
						property,
						embedColor(mode + ('(' + (r + (' ' + (g + (' ' + (b + ' / 1.0)'))))))));
				}
			}
		} else {
			var keyword = color.a;
			return A2($rtfeldman$elm_css$Css$property, property, keyword);
		}
	});
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color = function (color) {
	return A4(
		$matheus23$elm_tailwind_modules_base$Tailwind$Color$propertyWithColor,
		'background-color',
		function (c) {
			return c;
		},
		$elm$core$Maybe$Just('--tw-bg-opacity'),
		color);
};
var $rtfeldman$elm_css$Html$Styled$button = $rtfeldman$elm_css$Html$Styled$node('button');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$border = A2($rtfeldman$elm_css$Css$property, 'border-width', '1px');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$border_color = function (color) {
	return A4(
		$matheus23$elm_tailwind_modules_base$Tailwind$Color$propertyWithColor,
		'border-color',
		function (c) {
			return c;
		},
		$elm$core$Maybe$Just('--tw-border-opacity'),
		color);
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$cursor_pointer = A2($rtfeldman$elm_css$Css$property, 'cursor', 'pointer');
var $rtfeldman$elm_css$Css$Preprocess$ExtendSelector = F2(
	function (a, b) {
		return {$: 'ExtendSelector', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$PseudoClassSelector = function (a) {
	return {$: 'PseudoClassSelector', a: a};
};
var $rtfeldman$elm_css$Css$pseudoClass = function (_class) {
	return $rtfeldman$elm_css$Css$Preprocess$ExtendSelector(
		$rtfeldman$elm_css$Css$Structure$PseudoClassSelector(_class));
};
var $rtfeldman$elm_css$Css$hover = $rtfeldman$elm_css$Css$pseudoClass('hover');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4 = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'padding-left', '1rem'),
			A2($rtfeldman$elm_css$Css$property, 'padding-right', '1rem')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1 = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'padding-top', '0.25rem'),
			A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0.25rem')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded = A2($rtfeldman$elm_css$Css$property, 'border-radius', '0.25rem');
var $matheus23$elm_tailwind_modules_base$Tailwind$Color$Color = F5(
	function (a, b, c, d, e) {
		return {$: 'Color', a: a, b: b, c: c, d: d, e: e};
	});
var $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable = {$: 'ViaVariable'};
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_400 = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '56', '189', '248', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_900 = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '12', '74', '110', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color = function (color) {
	return A4(
		$matheus23$elm_tailwind_modules_base$Tailwind$Color$propertyWithColor,
		'color',
		function (c) {
			return c;
		},
		$elm$core$Maybe$Just('--tw-text-opacity'),
		color);
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'font-size', '1.25rem'),
			A2($rtfeldman$elm_css$Css$property, 'line-height', '1.75rem')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$transition_all = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'transition-property', 'all'),
			A2($rtfeldman$elm_css$Css$property, 'transition-timing-function', 'cubic-bezier(0.4, 0, 0.2, 1)'),
			A2($rtfeldman$elm_css$Css$property, 'transition-duration', '150ms')
		]));
var $matheus23$elm_tailwind_modules_base$Tailwind$Color$Opacity = function (a) {
	return {$: 'Opacity', a: a};
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$transparent = A5(
	$matheus23$elm_tailwind_modules_base$Tailwind$Color$Color,
	'rgb',
	'0',
	'0',
	'0',
	$matheus23$elm_tailwind_modules_base$Tailwind$Color$Opacity('0'));
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$white = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '255', '255', '255', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $author$project$GlobalStyles$buttonStyle = _List_fromArray(
	[
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_400),
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$white),
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$border,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$border_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_400),
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$cursor_pointer,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$transition_all,
		$rtfeldman$elm_css$Css$hover(
		_List_fromArray(
			[
				$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_900),
				$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$border_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_900),
				$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$border_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$transparent)
			]))
	]);
var $elm$virtual_dom$VirtualDom$property = F2(
	function (key, value) {
		return A2(
			_VirtualDom_property,
			_VirtualDom_noInnerHtmlOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlJson(value));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$property = F2(
	function (key, value) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$property, key, value),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$class = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('className');
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $elm$core$List$all = F2(
	function (isOkay, list) {
		return !A2(
			$elm$core$List$any,
			A2($elm$core$Basics$composeL, $elm$core$Basics$not, isOkay),
			list);
	});
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $rtfeldman$elm_css$Css$Structure$compactHelp = F2(
	function (declaration, _v0) {
		var keyframesByName = _v0.a;
		var declarations = _v0.b;
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var _v2 = declaration.a;
				var properties = _v2.c;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'MediaRule':
				var styleBlocks = declaration.b;
				return A2(
					$elm$core$List$all,
					function (_v3) {
						var properties = _v3.c;
						return $elm$core$List$isEmpty(properties);
					},
					styleBlocks) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'SupportsRule':
				var otherDeclarations = declaration.b;
				return $elm$core$List$isEmpty(otherDeclarations) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'DocumentRule':
				return _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'PageRule':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'FontFace':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'Keyframes':
				var record = declaration.a;
				return $elm$core$String$isEmpty(record.declaration) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					A3($elm$core$Dict$insert, record.name, record.declaration, keyframesByName),
					declarations);
			case 'Viewport':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			case 'CounterStyle':
				var properties = declaration.a;
				return $elm$core$List$isEmpty(properties) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
			default:
				var tuples = declaration.a;
				return A2(
					$elm$core$List$all,
					function (_v4) {
						var properties = _v4.b;
						return $elm$core$List$isEmpty(properties);
					},
					tuples) ? _Utils_Tuple2(keyframesByName, declarations) : _Utils_Tuple2(
					keyframesByName,
					A2($elm$core$List$cons, declaration, declarations));
		}
	});
var $rtfeldman$elm_css$Css$Structure$Keyframes = function (a) {
	return {$: 'Keyframes', a: a};
};
var $rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations = F2(
	function (keyframesByName, compactedDeclarations) {
		return A2(
			$elm$core$List$append,
			A2(
				$elm$core$List$map,
				function (_v0) {
					var name = _v0.a;
					var decl = _v0.b;
					return $rtfeldman$elm_css$Css$Structure$Keyframes(
						{declaration: decl, name: name});
				},
				$elm$core$Dict$toList(keyframesByName)),
			compactedDeclarations);
	});
var $rtfeldman$elm_css$Css$Structure$compactDeclarations = function (declarations) {
	var _v0 = A3(
		$elm$core$List$foldr,
		$rtfeldman$elm_css$Css$Structure$compactHelp,
		_Utils_Tuple2($elm$core$Dict$empty, _List_Nil),
		declarations);
	var keyframesByName = _v0.a;
	var compactedDeclarations = _v0.b;
	return A2($rtfeldman$elm_css$Css$Structure$withKeyframeDeclarations, keyframesByName, compactedDeclarations);
};
var $rtfeldman$elm_css$Css$Structure$compactStylesheet = function (_v0) {
	var declarations = _v0.declarations;
	var namespaces = _v0.namespaces;
	var imports = _v0.imports;
	var charset = _v0.charset;
	return {
		charset: charset,
		declarations: $rtfeldman$elm_css$Css$Structure$compactDeclarations(declarations),
		imports: imports,
		namespaces: namespaces
	};
};
var $rtfeldman$elm_css$Css$Structure$Output$charsetToString = function (charset) {
	return A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			function (str) {
				return '@charset \"' + (str + '\"');
			},
			charset));
};
var $rtfeldman$elm_css$Css$String$mapJoinHelp = F4(
	function (map, sep, strs, result) {
		mapJoinHelp:
		while (true) {
			if (!strs.b) {
				return result;
			} else {
				if (!strs.b.b) {
					var first = strs.a;
					return result + (map(first) + '');
				} else {
					var first = strs.a;
					var rest = strs.b;
					var $temp$map = map,
						$temp$sep = sep,
						$temp$strs = rest,
						$temp$result = result + (map(first) + (sep + ''));
					map = $temp$map;
					sep = $temp$sep;
					strs = $temp$strs;
					result = $temp$result;
					continue mapJoinHelp;
				}
			}
		}
	});
var $rtfeldman$elm_css$Css$String$mapJoin = F3(
	function (map, sep, strs) {
		return A4($rtfeldman$elm_css$Css$String$mapJoinHelp, map, sep, strs, '');
	});
var $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString = function (expression) {
	return '(' + (expression.feature + (A2(
		$elm$core$Maybe$withDefault,
		'',
		A2(
			$elm$core$Maybe$map,
			$elm$core$Basics$append(': '),
			expression.value)) + ')'));
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString = function (mediaType) {
	switch (mediaType.$) {
		case 'Print':
			return 'print';
		case 'Screen':
			return 'screen';
		default:
			return 'speech';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString = function (mediaQuery) {
	var prefixWith = F3(
		function (str, mediaType, expressions) {
			return str + (' ' + A2(
				$elm$core$String$join,
				' and ',
				A2(
					$elm$core$List$cons,
					$rtfeldman$elm_css$Css$Structure$Output$mediaTypeToString(mediaType),
					A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, expressions))));
		});
	switch (mediaQuery.$) {
		case 'AllQuery':
			var expressions = mediaQuery.a;
			return A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$mediaExpressionToString, ' and ', expressions);
		case 'OnlyQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'only', mediaType, expressions);
		case 'NotQuery':
			var mediaType = mediaQuery.a;
			var expressions = mediaQuery.b;
			return A3(prefixWith, 'not', mediaType, expressions);
		default:
			var str = mediaQuery.a;
			return str;
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString = F2(
	function (name, mediaQuery) {
		return '@import \"' + (name + ($rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString(mediaQuery) + '\"'));
	});
var $rtfeldman$elm_css$Css$Structure$Output$importToString = function (_v0) {
	var name = _v0.a;
	var mediaQueries = _v0.b;
	return A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		$rtfeldman$elm_css$Css$Structure$Output$importMediaQueryToString(name),
		'\n',
		mediaQueries);
};
var $rtfeldman$elm_css$Css$Structure$Output$namespaceToString = function (_v0) {
	var prefix = _v0.a;
	var str = _v0.b;
	return '@namespace ' + (prefix + ('\"' + (str + '\"')));
};
var $rtfeldman$elm_css$Css$Structure$Output$emitProperties = function (properties) {
	return A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		function (_v0) {
			var prop = _v0.a;
			return prop + ';';
		},
		'',
		properties);
};
var $elm$core$String$append = _String_append;
var $rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString = function (_v0) {
	var str = _v0.a;
	return '::' + str;
};
var $rtfeldman$elm_css$Css$Structure$Output$combinatorToString = function (combinator) {
	switch (combinator.$) {
		case 'AdjacentSibling':
			return '+';
		case 'GeneralSibling':
			return '~';
		case 'Child':
			return '>';
		default:
			return '';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString = function (repeatableSimpleSelector) {
	switch (repeatableSimpleSelector.$) {
		case 'ClassSelector':
			var str = repeatableSimpleSelector.a;
			return '.' + str;
		case 'IdSelector':
			var str = repeatableSimpleSelector.a;
			return '#' + str;
		case 'PseudoClassSelector':
			var str = repeatableSimpleSelector.a;
			return ':' + str;
		default:
			var str = repeatableSimpleSelector.a;
			return '[' + (str + ']');
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString = function (simpleSelectorSequence) {
	switch (simpleSelectorSequence.$) {
		case 'TypeSelectorSequence':
			var str = simpleSelectorSequence.a.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return _Utils_ap(
				str,
				A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors));
		case 'UniversalSelectorSequence':
			var repeatableSimpleSelectors = simpleSelectorSequence.a;
			return $elm$core$List$isEmpty(repeatableSimpleSelectors) ? '*' : A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors);
		default:
			var str = simpleSelectorSequence.a;
			var repeatableSimpleSelectors = simpleSelectorSequence.b;
			return _Utils_ap(
				str,
				A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$repeatableSimpleSelectorToString, '', repeatableSimpleSelectors));
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$selectorChainToString = function (_v0) {
	var combinator = _v0.a;
	var sequence = _v0.b;
	return $rtfeldman$elm_css$Css$Structure$Output$combinatorToString(combinator) + (' ' + $rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(sequence));
};
var $rtfeldman$elm_css$Css$Structure$Output$selectorToString = function (_v0) {
	var simpleSelectorSequence = _v0.a;
	var chain = _v0.b;
	var pseudoElement = _v0.c;
	var segments = A2(
		$elm$core$List$cons,
		$rtfeldman$elm_css$Css$Structure$Output$simpleSelectorSequenceToString(simpleSelectorSequence),
		A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$Output$selectorChainToString, chain));
	var pseudoElementsString = A2(
		$elm$core$Maybe$withDefault,
		'',
		A2($elm$core$Maybe$map, $rtfeldman$elm_css$Css$Structure$Output$pseudoElementToString, pseudoElement));
	return A2(
		$elm$core$String$append,
		A2($elm$core$String$join, ' ', segments),
		pseudoElementsString);
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock = function (_v0) {
	var firstSelector = _v0.a;
	var otherSelectors = _v0.b;
	var properties = _v0.c;
	var selectorStr = A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		$rtfeldman$elm_css$Css$Structure$Output$selectorToString,
		',',
		A2($elm$core$List$cons, firstSelector, otherSelectors));
	return selectorStr + ('{' + ($rtfeldman$elm_css$Css$Structure$Output$emitProperties(properties) + '}'));
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration = function (decl) {
	switch (decl.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = decl.a;
			return $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock(styleBlock);
		case 'MediaRule':
			var mediaQueries = decl.a;
			var styleBlocks = decl.b;
			var query = A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$mediaQueryToString, ', ', mediaQueries);
			var blocks = A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$prettyPrintStyleBlock, '\n', styleBlocks);
			return '@media ' + (query + ('{' + (blocks + '}')));
		case 'SupportsRule':
			return 'TODO';
		case 'DocumentRule':
			return 'TODO';
		case 'PageRule':
			return 'TODO';
		case 'FontFace':
			return 'TODO';
		case 'Keyframes':
			var declaration = decl.a.declaration;
			var name = decl.a.name;
			return '@keyframes ' + (name + ('{' + (declaration + '}')));
		case 'Viewport':
			return 'TODO';
		case 'CounterStyle':
			return 'TODO';
		default:
			return 'TODO';
	}
};
var $rtfeldman$elm_css$Css$Structure$Output$prettyPrint = function (_v0) {
	var declarations = _v0.declarations;
	var namespaces = _v0.namespaces;
	var imports = _v0.imports;
	var charset = _v0.charset;
	return $rtfeldman$elm_css$Css$Structure$Output$charsetToString(charset) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$importToString, '\n', imports) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$namespaceToString, '\n', namespaces) + (A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Structure$Output$prettyPrintDeclaration, '\n', declarations) + '')));
};
var $rtfeldman$elm_css$Css$Structure$CounterStyle = function (a) {
	return {$: 'CounterStyle', a: a};
};
var $rtfeldman$elm_css$Css$Structure$FontFace = function (a) {
	return {$: 'FontFace', a: a};
};
var $rtfeldman$elm_css$Css$Structure$PageRule = function (a) {
	return {$: 'PageRule', a: a};
};
var $rtfeldman$elm_css$Css$Structure$Selector = F3(
	function (a, b, c) {
		return {$: 'Selector', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Structure$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var $rtfeldman$elm_css$Css$Structure$SupportsRule = F2(
	function (a, b) {
		return {$: 'SupportsRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$Viewport = function (a) {
	return {$: 'Viewport', a: a};
};
var $rtfeldman$elm_css$Css$Structure$MediaRule = F2(
	function (a, b) {
		return {$: 'MediaRule', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$mapLast = F2(
	function (update, list) {
		if (!list.b) {
			return list;
		} else {
			if (!list.b.b) {
				var only = list.a;
				return _List_fromArray(
					[
						update(only)
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$mapLast, update, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$withPropertyAppended = F2(
	function (property, _v0) {
		var firstSelector = _v0.a;
		var otherSelectors = _v0.b;
		var properties = _v0.c;
		return A3(
			$rtfeldman$elm_css$Css$Structure$StyleBlock,
			firstSelector,
			otherSelectors,
			_Utils_ap(
				properties,
				_List_fromArray(
					[property])));
	});
var $rtfeldman$elm_css$Css$Structure$appendProperty = F2(
	function (property, declarations) {
		if (!declarations.b) {
			return declarations;
		} else {
			if (!declarations.b.b) {
				switch (declarations.a.$) {
					case 'StyleBlockDeclaration':
						var styleBlock = declarations.a.a;
						return _List_fromArray(
							[
								$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
								A2($rtfeldman$elm_css$Css$Structure$withPropertyAppended, property, styleBlock))
							]);
					case 'MediaRule':
						var _v1 = declarations.a;
						var mediaQueries = _v1.a;
						var styleBlocks = _v1.b;
						return _List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Css$Structure$MediaRule,
								mediaQueries,
								A2(
									$rtfeldman$elm_css$Css$Structure$mapLast,
									$rtfeldman$elm_css$Css$Structure$withPropertyAppended(property),
									styleBlocks))
							]);
					default:
						return declarations;
				}
			} else {
				var first = declarations.a;
				var rest = declarations.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$appendProperty, property, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendToLastSelector = F2(
	function (f, styleBlock) {
		if (!styleBlock.b.b) {
			var only = styleBlock.a;
			var properties = styleBlock.c;
			return _List_fromArray(
				[
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, only, _List_Nil, properties),
					A3(
					$rtfeldman$elm_css$Css$Structure$StyleBlock,
					f(only),
					_List_Nil,
					_List_Nil)
				]);
		} else {
			var first = styleBlock.a;
			var rest = styleBlock.b;
			var properties = styleBlock.c;
			var newRest = A2($elm$core$List$map, f, rest);
			var newFirst = f(first);
			return _List_fromArray(
				[
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, first, rest, properties),
					A3($rtfeldman$elm_css$Css$Structure$StyleBlock, newFirst, newRest, _List_Nil)
				]);
		}
	});
var $rtfeldman$elm_css$Css$Structure$applyPseudoElement = F2(
	function (pseudo, _v0) {
		var sequence = _v0.a;
		var selectors = _v0.b;
		return A3(
			$rtfeldman$elm_css$Css$Structure$Selector,
			sequence,
			selectors,
			$elm$core$Maybe$Just(pseudo));
	});
var $rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector = F2(
	function (pseudo, styleBlock) {
		return A2(
			$rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			$rtfeldman$elm_css$Css$Structure$applyPseudoElement(pseudo),
			styleBlock);
	});
var $rtfeldman$elm_css$Css$Structure$CustomSelector = F2(
	function (a, b) {
		return {$: 'CustomSelector', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$TypeSelectorSequence = F2(
	function (a, b) {
		return {$: 'TypeSelectorSequence', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence = function (a) {
	return {$: 'UniversalSelectorSequence', a: a};
};
var $rtfeldman$elm_css$Css$Structure$appendRepeatable = F2(
	function (selector, sequence) {
		switch (sequence.$) {
			case 'TypeSelectorSequence':
				var typeSelector = sequence.a;
				var list = sequence.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$TypeSelectorSequence,
					typeSelector,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			case 'UniversalSelectorSequence':
				var list = sequence.a;
				return $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
			default:
				var str = sequence.a;
				var list = sequence.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$CustomSelector,
					str,
					_Utils_ap(
						list,
						_List_fromArray(
							[selector])));
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator = F2(
	function (selector, list) {
		if (!list.b) {
			return _List_Nil;
		} else {
			if (!list.b.b) {
				var _v1 = list.a;
				var combinator = _v1.a;
				var sequence = _v1.b;
				return _List_fromArray(
					[
						_Utils_Tuple2(
						combinator,
						A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, selector, sequence))
					]);
			} else {
				var first = list.a;
				var rest = list.b;
				return A2(
					$elm$core$List$cons,
					first,
					A2($rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, selector, rest));
			}
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableSelector = F2(
	function (repeatableSimpleSelector, selector) {
		if (!selector.b.b) {
			var sequence = selector.a;
			var pseudoElement = selector.c;
			return A3(
				$rtfeldman$elm_css$Css$Structure$Selector,
				A2($rtfeldman$elm_css$Css$Structure$appendRepeatable, repeatableSimpleSelector, sequence),
				_List_Nil,
				pseudoElement);
		} else {
			var firstSelector = selector.a;
			var tuples = selector.b;
			var pseudoElement = selector.c;
			return A3(
				$rtfeldman$elm_css$Css$Structure$Selector,
				firstSelector,
				A2($rtfeldman$elm_css$Css$Structure$appendRepeatableWithCombinator, repeatableSimpleSelector, tuples),
				pseudoElement);
		}
	});
var $rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector = F2(
	function (selector, styleBlock) {
		return A2(
			$rtfeldman$elm_css$Css$Structure$appendToLastSelector,
			$rtfeldman$elm_css$Css$Structure$appendRepeatableSelector(selector),
			styleBlock);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors = function (declarations) {
	collectSelectors:
	while (true) {
		if (!declarations.b) {
			return _List_Nil;
		} else {
			if (declarations.a.$ === 'StyleBlockDeclaration') {
				var _v1 = declarations.a.a;
				var firstSelector = _v1.a;
				var otherSelectors = _v1.b;
				var rest = declarations.b;
				return _Utils_ap(
					A2($elm$core$List$cons, firstSelector, otherSelectors),
					$rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(rest));
			} else {
				var rest = declarations.b;
				var $temp$declarations = rest;
				declarations = $temp$declarations;
				continue collectSelectors;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Structure$DocumentRule = F5(
	function (a, b, c, d, e) {
		return {$: 'DocumentRule', a: a, b: b, c: c, d: d, e: e};
	});
var $rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock = F2(
	function (update, declarations) {
		_v0$12:
		while (true) {
			if (!declarations.b) {
				return declarations;
			} else {
				if (!declarations.b.b) {
					switch (declarations.a.$) {
						case 'StyleBlockDeclaration':
							var styleBlock = declarations.a.a;
							return A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration,
								update(styleBlock));
						case 'MediaRule':
							if (declarations.a.b.b) {
								if (!declarations.a.b.b.b) {
									var _v1 = declarations.a;
									var mediaQueries = _v1.a;
									var _v2 = _v1.b;
									var styleBlock = _v2.a;
									return _List_fromArray(
										[
											A2(
											$rtfeldman$elm_css$Css$Structure$MediaRule,
											mediaQueries,
											update(styleBlock))
										]);
								} else {
									var _v3 = declarations.a;
									var mediaQueries = _v3.a;
									var _v4 = _v3.b;
									var first = _v4.a;
									var rest = _v4.b;
									var _v5 = A2(
										$rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock,
										update,
										_List_fromArray(
											[
												A2($rtfeldman$elm_css$Css$Structure$MediaRule, mediaQueries, rest)
											]));
									if ((_v5.b && (_v5.a.$ === 'MediaRule')) && (!_v5.b.b)) {
										var _v6 = _v5.a;
										var newMediaQueries = _v6.a;
										var newStyleBlocks = _v6.b;
										return _List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Css$Structure$MediaRule,
												newMediaQueries,
												A2($elm$core$List$cons, first, newStyleBlocks))
											]);
									} else {
										var newDeclarations = _v5;
										return newDeclarations;
									}
								}
							} else {
								break _v0$12;
							}
						case 'SupportsRule':
							var _v7 = declarations.a;
							var str = _v7.a;
							var nestedDeclarations = _v7.b;
							return _List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Css$Structure$SupportsRule,
									str,
									A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, nestedDeclarations))
								]);
						case 'DocumentRule':
							var _v8 = declarations.a;
							var str1 = _v8.a;
							var str2 = _v8.b;
							var str3 = _v8.c;
							var str4 = _v8.d;
							var styleBlock = _v8.e;
							return A2(
								$elm$core$List$map,
								A4($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4),
								update(styleBlock));
						case 'PageRule':
							return declarations;
						case 'FontFace':
							return declarations;
						case 'Keyframes':
							return declarations;
						case 'Viewport':
							return declarations;
						case 'CounterStyle':
							return declarations;
						default:
							return declarations;
					}
				} else {
					break _v0$12;
				}
			}
		}
		var first = declarations.a;
		var rest = declarations.b;
		return A2(
			$elm$core$List$cons,
			first,
			A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, update, rest));
	});
var $elm$core$String$cons = _String_cons;
var $robinheghan$murmur3$Murmur3$HashData = F4(
	function (shift, seed, hash, charsProcessed) {
		return {charsProcessed: charsProcessed, hash: hash, seed: seed, shift: shift};
	});
var $robinheghan$murmur3$Murmur3$c1 = 3432918353;
var $robinheghan$murmur3$Murmur3$c2 = 461845907;
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $robinheghan$murmur3$Murmur3$multiplyBy = F2(
	function (b, a) {
		return ((a & 65535) * b) + ((((a >>> 16) * b) & 65535) << 16);
	});
var $robinheghan$murmur3$Murmur3$rotlBy = F2(
	function (b, a) {
		return (a << b) | (a >>> (32 - b));
	});
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $robinheghan$murmur3$Murmur3$finalize = function (data) {
	var acc = (!(!data.hash)) ? (data.seed ^ A2(
		$robinheghan$murmur3$Murmur3$multiplyBy,
		$robinheghan$murmur3$Murmur3$c2,
		A2(
			$robinheghan$murmur3$Murmur3$rotlBy,
			15,
			A2($robinheghan$murmur3$Murmur3$multiplyBy, $robinheghan$murmur3$Murmur3$c1, data.hash)))) : data.seed;
	var h0 = acc ^ data.charsProcessed;
	var h1 = A2($robinheghan$murmur3$Murmur3$multiplyBy, 2246822507, h0 ^ (h0 >>> 16));
	var h2 = A2($robinheghan$murmur3$Murmur3$multiplyBy, 3266489909, h1 ^ (h1 >>> 13));
	return (h2 ^ (h2 >>> 16)) >>> 0;
};
var $elm$core$String$foldl = _String_foldl;
var $robinheghan$murmur3$Murmur3$mix = F2(
	function (h1, k1) {
		return A2(
			$robinheghan$murmur3$Murmur3$multiplyBy,
			5,
			A2(
				$robinheghan$murmur3$Murmur3$rotlBy,
				13,
				h1 ^ A2(
					$robinheghan$murmur3$Murmur3$multiplyBy,
					$robinheghan$murmur3$Murmur3$c2,
					A2(
						$robinheghan$murmur3$Murmur3$rotlBy,
						15,
						A2($robinheghan$murmur3$Murmur3$multiplyBy, $robinheghan$murmur3$Murmur3$c1, k1))))) + 3864292196;
	});
var $robinheghan$murmur3$Murmur3$hashFold = F2(
	function (c, data) {
		var res = data.hash | ((255 & $elm$core$Char$toCode(c)) << data.shift);
		var _v0 = data.shift;
		if (_v0 === 24) {
			return {
				charsProcessed: data.charsProcessed + 1,
				hash: 0,
				seed: A2($robinheghan$murmur3$Murmur3$mix, data.seed, res),
				shift: 0
			};
		} else {
			return {charsProcessed: data.charsProcessed + 1, hash: res, seed: data.seed, shift: data.shift + 8};
		}
	});
var $robinheghan$murmur3$Murmur3$hashString = F2(
	function (seed, str) {
		return $robinheghan$murmur3$Murmur3$finalize(
			A3(
				$elm$core$String$foldl,
				$robinheghan$murmur3$Murmur3$hashFold,
				A4($robinheghan$murmur3$Murmur3$HashData, 0, seed, 0, 0),
				str));
	});
var $rtfeldman$elm_css$Hash$initialSeed = 15739;
var $elm$core$String$fromList = _String_fromList;
var $rtfeldman$elm_hex$Hex$unsafeToDigit = function (num) {
	unsafeToDigit:
	while (true) {
		switch (num) {
			case 0:
				return _Utils_chr('0');
			case 1:
				return _Utils_chr('1');
			case 2:
				return _Utils_chr('2');
			case 3:
				return _Utils_chr('3');
			case 4:
				return _Utils_chr('4');
			case 5:
				return _Utils_chr('5');
			case 6:
				return _Utils_chr('6');
			case 7:
				return _Utils_chr('7');
			case 8:
				return _Utils_chr('8');
			case 9:
				return _Utils_chr('9');
			case 10:
				return _Utils_chr('a');
			case 11:
				return _Utils_chr('b');
			case 12:
				return _Utils_chr('c');
			case 13:
				return _Utils_chr('d');
			case 14:
				return _Utils_chr('e');
			case 15:
				return _Utils_chr('f');
			default:
				var $temp$num = num;
				num = $temp$num;
				continue unsafeToDigit;
		}
	}
};
var $rtfeldman$elm_hex$Hex$unsafePositiveToDigits = F2(
	function (digits, num) {
		unsafePositiveToDigits:
		while (true) {
			if (num < 16) {
				return A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(num),
					digits);
			} else {
				var $temp$digits = A2(
					$elm$core$List$cons,
					$rtfeldman$elm_hex$Hex$unsafeToDigit(
						A2($elm$core$Basics$modBy, 16, num)),
					digits),
					$temp$num = (num / 16) | 0;
				digits = $temp$digits;
				num = $temp$num;
				continue unsafePositiveToDigits;
			}
		}
	});
var $rtfeldman$elm_hex$Hex$toString = function (num) {
	return $elm$core$String$fromList(
		(num < 0) ? A2(
			$elm$core$List$cons,
			_Utils_chr('-'),
			A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, -num)) : A2($rtfeldman$elm_hex$Hex$unsafePositiveToDigits, _List_Nil, num));
};
var $rtfeldman$elm_css$Hash$fromString = function (str) {
	return A2(
		$elm$core$String$cons,
		_Utils_chr('_'),
		$rtfeldman$elm_hex$Hex$toString(
			A2($robinheghan$murmur3$Murmur3$hashString, $rtfeldman$elm_css$Hash$initialSeed, str)));
};
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$last = function (list) {
	last:
	while (true) {
		if (!list.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!list.b.b) {
				var singleton = list.a;
				return $elm$core$Maybe$Just(singleton);
			} else {
				var rest = list.b;
				var $temp$list = rest;
				list = $temp$list;
				continue last;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration = function (declarations) {
	lastDeclaration:
	while (true) {
		if (!declarations.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!declarations.b.b) {
				var x = declarations.a;
				return $elm$core$Maybe$Just(
					_List_fromArray(
						[x]));
			} else {
				var xs = declarations.b;
				var $temp$declarations = xs;
				declarations = $temp$declarations;
				continue lastDeclaration;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf = function (maybes) {
	oneOf:
	while (true) {
		if (!maybes.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			var maybe = maybes.a;
			var rest = maybes.b;
			if (maybe.$ === 'Nothing') {
				var $temp$maybes = rest;
				maybes = $temp$maybes;
				continue oneOf;
			} else {
				return maybe;
			}
		}
	}
};
var $rtfeldman$elm_css$Css$Structure$FontFeatureValues = function (a) {
	return {$: 'FontFeatureValues', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues = function (tuples) {
	var expandTuples = function (tuplesToExpand) {
		if (!tuplesToExpand.b) {
			return _List_Nil;
		} else {
			var properties = tuplesToExpand.a;
			var rest = tuplesToExpand.b;
			return A2(
				$elm$core$List$cons,
				properties,
				expandTuples(rest));
		}
	};
	var newTuples = expandTuples(tuples);
	return _List_fromArray(
		[
			$rtfeldman$elm_css$Css$Structure$FontFeatureValues(newTuples)
		]);
};
var $elm$core$List$singleton = function (value) {
	return _List_fromArray(
		[value]);
};
var $rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule = F2(
	function (mediaQueries, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var styleBlock = declaration.a;
			return A2(
				$rtfeldman$elm_css$Css$Structure$MediaRule,
				mediaQueries,
				_List_fromArray(
					[styleBlock]));
		} else {
			return declaration;
		}
	});
var $elm$core$List$tail = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(xs);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2($elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var $elm$core$List$takeTailRec = F2(
	function (n, list) {
		return $elm$core$List$reverse(
			A3($elm$core$List$takeReverse, n, list, _List_Nil));
	});
var $elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _v0 = _Utils_Tuple2(n, list);
			_v0$1:
			while (true) {
				_v0$5:
				while (true) {
					if (!_v0.b.b) {
						return list;
					} else {
						if (_v0.b.b.b) {
							switch (_v0.a) {
								case 1:
									break _v0$1;
								case 2:
									var _v2 = _v0.b;
									var x = _v2.a;
									var _v3 = _v2.b;
									var y = _v3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_v0.b.b.b.b) {
										var _v4 = _v0.b;
										var x = _v4.a;
										var _v5 = _v4.b;
										var y = _v5.a;
										var _v6 = _v5.b;
										var z = _v6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _v0$5;
									}
								default:
									if (_v0.b.b.b.b && _v0.b.b.b.b.b) {
										var _v7 = _v0.b;
										var x = _v7.a;
										var _v8 = _v7.b;
										var y = _v8.a;
										var _v9 = _v8.b;
										var z = _v9.a;
										var _v10 = _v9.b;
										var w = _v10.a;
										var tl = _v10.b;
										return (ctr > 1000) ? A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A2($elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											$elm$core$List$cons,
											x,
											A2(
												$elm$core$List$cons,
												y,
												A2(
													$elm$core$List$cons,
													z,
													A2(
														$elm$core$List$cons,
														w,
														A3($elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _v0$5;
									}
							}
						} else {
							if (_v0.a === 1) {
								break _v0$1;
							} else {
								break _v0$5;
							}
						}
					}
				}
				return list;
			}
			var _v1 = _v0.b;
			var x = _v1.a;
			return _List_fromArray(
				[x]);
		}
	});
var $elm$core$List$take = F2(
	function (n, list) {
		return A3($elm$core$List$takeFast, 0, n, list);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule = F5(
	function (str1, str2, str3, str4, declaration) {
		if (declaration.$ === 'StyleBlockDeclaration') {
			var structureStyleBlock = declaration.a;
			return A5($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
		} else {
			return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule = F2(
	function (mediaQueries, declaration) {
		switch (declaration.$) {
			case 'StyleBlockDeclaration':
				var structureStyleBlock = declaration.a;
				return A2(
					$rtfeldman$elm_css$Css$Structure$MediaRule,
					mediaQueries,
					_List_fromArray(
						[structureStyleBlock]));
			case 'MediaRule':
				var newMediaQueries = declaration.a;
				var structureStyleBlocks = declaration.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$MediaRule,
					_Utils_ap(mediaQueries, newMediaQueries),
					structureStyleBlocks);
			case 'SupportsRule':
				var str = declaration.a;
				var declarations = declaration.b;
				return A2(
					$rtfeldman$elm_css$Css$Structure$SupportsRule,
					str,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
						declarations));
			case 'DocumentRule':
				var str1 = declaration.a;
				var str2 = declaration.b;
				var str3 = declaration.c;
				var str4 = declaration.d;
				var structureStyleBlock = declaration.e;
				return A5($rtfeldman$elm_css$Css$Structure$DocumentRule, str1, str2, str3, str4, structureStyleBlock);
			case 'PageRule':
				return declaration;
			case 'FontFace':
				return declaration;
			case 'Keyframes':
				return declaration;
			case 'Viewport':
				return declaration;
			case 'CounterStyle':
				return declaration;
			default:
				return declaration;
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet = function (_v0) {
	var declarations = _v0.a;
	return declarations;
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast = F4(
	function (nestedStyles, rest, f, declarations) {
		var withoutParent = function (decls) {
			return A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$elm$core$List$tail(decls));
		};
		var nextResult = A2(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
			rest,
			A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		var newDeclarations = function () {
			var _v14 = _Utils_Tuple2(
				$elm$core$List$head(nextResult),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$last(declarations));
			if ((_v14.a.$ === 'Just') && (_v14.b.$ === 'Just')) {
				var nextResultParent = _v14.a.a;
				var originalParent = _v14.b.a;
				return _Utils_ap(
					A2(
						$elm$core$List$take,
						$elm$core$List$length(declarations) - 1,
						declarations),
					_List_fromArray(
						[
							(!_Utils_eq(originalParent, nextResultParent)) ? nextResultParent : originalParent
						]));
			} else {
				return declarations;
			}
		}();
		var insertStylesToNestedDecl = function (lastDecl) {
			return $elm$core$List$concat(
				A2(
					$rtfeldman$elm_css$Css$Structure$mapLast,
					$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles(nestedStyles),
					A2(
						$elm$core$List$map,
						$elm$core$List$singleton,
						A2($rtfeldman$elm_css$Css$Structure$concatMapLastStyleBlock, f, lastDecl))));
		};
		var initialResult = A2(
			$elm$core$Maybe$withDefault,
			_List_Nil,
			A2(
				$elm$core$Maybe$map,
				insertStylesToNestedDecl,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$lastDeclaration(declarations)));
		return _Utils_ap(
			newDeclarations,
			_Utils_ap(
				withoutParent(initialResult),
				withoutParent(nextResult)));
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles = F2(
	function (styles, declarations) {
		if (!styles.b) {
			return declarations;
		} else {
			switch (styles.a.$) {
				case 'AppendProperty':
					var property = styles.a.a;
					var rest = styles.b;
					return A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2($rtfeldman$elm_css$Css$Structure$appendProperty, property, declarations));
				case 'ExtendSelector':
					var _v4 = styles.a;
					var selector = _v4.a;
					var nestedStyles = _v4.b;
					var rest = styles.b;
					return A4(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						$rtfeldman$elm_css$Css$Structure$appendRepeatableToLastSelector(selector),
						declarations);
				case 'NestSnippet':
					var _v5 = styles.a;
					var selectorCombinator = _v5.a;
					var snippets = _v5.b;
					var rest = styles.b;
					var chain = F2(
						function (_v9, _v10) {
							var originalSequence = _v9.a;
							var originalTuples = _v9.b;
							var originalPseudoElement = _v9.c;
							var newSequence = _v10.a;
							var newTuples = _v10.b;
							var newPseudoElement = _v10.c;
							return A3(
								$rtfeldman$elm_css$Css$Structure$Selector,
								originalSequence,
								_Utils_ap(
									originalTuples,
									A2(
										$elm$core$List$cons,
										_Utils_Tuple2(selectorCombinator, newSequence),
										newTuples)),
								$rtfeldman$elm_css$Css$Preprocess$Resolve$oneOf(
									_List_fromArray(
										[newPseudoElement, originalPseudoElement])));
						});
					var expandDeclaration = function (declaration) {
						switch (declaration.$) {
							case 'StyleBlockDeclaration':
								var _v7 = declaration.a;
								var firstSelector = _v7.a;
								var otherSelectors = _v7.b;
								var nestedStyles = _v7.c;
								var newSelectors = A2(
									$elm$core$List$concatMap,
									function (originalSelector) {
										return A2(
											$elm$core$List$map,
											chain(originalSelector),
											A2($elm$core$List$cons, firstSelector, otherSelectors));
									},
									$rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations));
								var newDeclarations = function () {
									if (!newSelectors.b) {
										return _List_Nil;
									} else {
										var first = newSelectors.a;
										var remainder = newSelectors.b;
										return _List_fromArray(
											[
												$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
												A3($rtfeldman$elm_css$Css$Structure$StyleBlock, first, remainder, _List_Nil))
											]);
									}
								}();
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, nestedStyles, newDeclarations);
							case 'MediaRule':
								var mediaQueries = declaration.a;
								var styleBlocks = declaration.b;
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
							case 'SupportsRule':
								var str = declaration.a;
								var otherSnippets = declaration.b;
								return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, otherSnippets);
							case 'DocumentRule':
								var str1 = declaration.a;
								var str2 = declaration.b;
								var str3 = declaration.c;
								var str4 = declaration.d;
								var styleBlock = declaration.e;
								return A2(
									$elm$core$List$map,
									A4($rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
									$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
							case 'PageRule':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$PageRule(properties)
									]);
							case 'FontFace':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$FontFace(properties)
									]);
							case 'Viewport':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$Viewport(properties)
									]);
							case 'CounterStyle':
								var properties = declaration.a;
								return _List_fromArray(
									[
										$rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
									]);
							default:
								var tuples = declaration.a;
								return $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
						}
					};
					return $elm$core$List$concat(
						_Utils_ap(
							_List_fromArray(
								[
									A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations)
								]),
							A2(
								$elm$core$List$map,
								expandDeclaration,
								A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets))));
				case 'WithPseudoElement':
					var _v11 = styles.a;
					var pseudoElement = _v11.a;
					var nestedStyles = _v11.b;
					var rest = styles.b;
					return A4(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyNestedStylesToLast,
						nestedStyles,
						rest,
						$rtfeldman$elm_css$Css$Structure$appendPseudoElementToLastSelector(pseudoElement),
						declarations);
				case 'WithKeyframes':
					var str = styles.a.a;
					var rest = styles.b;
					var name = $rtfeldman$elm_css$Hash$fromString(str);
					var newProperty = $rtfeldman$elm_css$Css$Structure$Property('animation-name:' + name);
					var newDeclarations = A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						rest,
						A2($rtfeldman$elm_css$Css$Structure$appendProperty, newProperty, declarations));
					return A2(
						$elm$core$List$append,
						newDeclarations,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Css$Structure$Keyframes(
								{declaration: str, name: name})
							]));
				case 'WithMedia':
					var _v12 = styles.a;
					var mediaQueries = _v12.a;
					var nestedStyles = _v12.b;
					var rest = styles.b;
					var extraDeclarations = function () {
						var _v13 = $rtfeldman$elm_css$Css$Preprocess$Resolve$collectSelectors(declarations);
						if (!_v13.b) {
							return _List_Nil;
						} else {
							var firstSelector = _v13.a;
							var otherSelectors = _v13.b;
							return A2(
								$elm$core$List$map,
								$rtfeldman$elm_css$Css$Structure$styleBlockToMediaRule(mediaQueries),
								A2(
									$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
									nestedStyles,
									$elm$core$List$singleton(
										$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
											A3($rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil)))));
						}
					}();
					return _Utils_ap(
						A2($rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles, rest, declarations),
						extraDeclarations);
				default:
					var otherStyles = styles.a.a;
					var rest = styles.b;
					return A2(
						$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
						_Utils_ap(otherStyles, rest),
						declarations);
			}
		}
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock = function (_v2) {
	var firstSelector = _v2.a;
	var otherSelectors = _v2.b;
	var styles = _v2.c;
	return A2(
		$rtfeldman$elm_css$Css$Preprocess$Resolve$applyStyles,
		styles,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Css$Structure$StyleBlockDeclaration(
				A3($rtfeldman$elm_css$Css$Structure$StyleBlock, firstSelector, otherSelectors, _List_Nil))
			]));
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$extract = function (snippetDeclarations) {
	if (!snippetDeclarations.b) {
		return _List_Nil;
	} else {
		var first = snippetDeclarations.a;
		var rest = snippetDeclarations.b;
		return _Utils_ap(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations(first),
			$rtfeldman$elm_css$Css$Preprocess$Resolve$extract(rest));
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule = F2(
	function (mediaQueries, styleBlocks) {
		var handleStyleBlock = function (styleBlock) {
			return A2(
				$elm$core$List$map,
				$rtfeldman$elm_css$Css$Preprocess$Resolve$toMediaRule(mediaQueries),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		};
		return A2($elm$core$List$concatMap, handleStyleBlock, styleBlocks);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule = F2(
	function (str, snippets) {
		var declarations = $rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
			A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
		return _List_fromArray(
			[
				A2($rtfeldman$elm_css$Css$Structure$SupportsRule, str, declarations)
			]);
	});
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toDeclarations = function (snippetDeclaration) {
	switch (snippetDeclaration.$) {
		case 'StyleBlockDeclaration':
			var styleBlock = snippetDeclaration.a;
			return $rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock);
		case 'MediaRule':
			var mediaQueries = snippetDeclaration.a;
			var styleBlocks = snippetDeclaration.b;
			return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveMediaRule, mediaQueries, styleBlocks);
		case 'SupportsRule':
			var str = snippetDeclaration.a;
			var snippets = snippetDeclaration.b;
			return A2($rtfeldman$elm_css$Css$Preprocess$Resolve$resolveSupportsRule, str, snippets);
		case 'DocumentRule':
			var str1 = snippetDeclaration.a;
			var str2 = snippetDeclaration.b;
			var str3 = snippetDeclaration.c;
			var str4 = snippetDeclaration.d;
			var styleBlock = snippetDeclaration.e;
			return A2(
				$elm$core$List$map,
				A4($rtfeldman$elm_css$Css$Preprocess$Resolve$toDocumentRule, str1, str2, str3, str4),
				$rtfeldman$elm_css$Css$Preprocess$Resolve$expandStyleBlock(styleBlock));
		case 'PageRule':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$PageRule(properties)
				]);
		case 'FontFace':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$FontFace(properties)
				]);
		case 'Viewport':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$Viewport(properties)
				]);
		case 'CounterStyle':
			var properties = snippetDeclaration.a;
			return _List_fromArray(
				[
					$rtfeldman$elm_css$Css$Structure$CounterStyle(properties)
				]);
		default:
			var tuples = snippetDeclaration.a;
			return $rtfeldman$elm_css$Css$Preprocess$Resolve$resolveFontFeatureValues(tuples);
	}
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure = function (_v0) {
	var snippets = _v0.snippets;
	var namespaces = _v0.namespaces;
	var imports = _v0.imports;
	var charset = _v0.charset;
	var declarations = $rtfeldman$elm_css$Css$Preprocess$Resolve$extract(
		A2($elm$core$List$concatMap, $rtfeldman$elm_css$Css$Preprocess$unwrapSnippet, snippets));
	return {charset: charset, declarations: declarations, imports: imports, namespaces: namespaces};
};
var $rtfeldman$elm_css$Css$Preprocess$Resolve$compile = function (sheet) {
	return $rtfeldman$elm_css$Css$Structure$Output$prettyPrint(
		$rtfeldman$elm_css$Css$Structure$compactStylesheet(
			$rtfeldman$elm_css$Css$Preprocess$Resolve$toStructure(sheet)));
};
var $rtfeldman$elm_css$Css$Preprocess$Snippet = function (a) {
	return {$: 'Snippet', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$StyleBlock = F3(
	function (a, b, c) {
		return {$: 'StyleBlock', a: a, b: b, c: c};
	});
var $rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration = function (a) {
	return {$: 'StyleBlockDeclaration', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$makeSnippet = F2(
	function (styles, sequence) {
		var selector = A3($rtfeldman$elm_css$Css$Structure$Selector, sequence, _List_Nil, $elm$core$Maybe$Nothing);
		return $rtfeldman$elm_css$Css$Preprocess$Snippet(
			_List_fromArray(
				[
					$rtfeldman$elm_css$Css$Preprocess$StyleBlockDeclaration(
					A3($rtfeldman$elm_css$Css$Preprocess$StyleBlock, selector, _List_Nil, styles))
				]));
	});
var $rtfeldman$elm_css$Css$Preprocess$stylesheet = function (snippets) {
	return {charset: $elm$core$Maybe$Nothing, imports: _List_Nil, namespaces: _List_Nil, snippets: snippets};
};
var $rtfeldman$elm_css$Css$Structure$ClassSelector = function (a) {
	return {$: 'ClassSelector', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$classnameStandin = '\u0007';
var $rtfeldman$elm_css$VirtualDom$Styled$templateSelector = $rtfeldman$elm_css$Css$Structure$UniversalSelectorSequence(
	_List_fromArray(
		[
			$rtfeldman$elm_css$Css$Structure$ClassSelector($rtfeldman$elm_css$VirtualDom$Styled$classnameStandin)
		]));
var $rtfeldman$elm_css$VirtualDom$Styled$getCssTemplate = function (styles) {
	if (!styles.b) {
		return '';
	} else {
		var otherwise = styles;
		return $rtfeldman$elm_css$Css$Preprocess$Resolve$compile(
			$rtfeldman$elm_css$Css$Preprocess$stylesheet(
				_List_fromArray(
					[
						A2($rtfeldman$elm_css$VirtualDom$Styled$makeSnippet, styles, $rtfeldman$elm_css$VirtualDom$Styled$templateSelector)
					])));
	}
};
var $rtfeldman$elm_css$Html$Styled$Internal$css = function (styles) {
	var cssTemplate = $rtfeldman$elm_css$VirtualDom$Styled$getCssTemplate(styles);
	var classProperty = A2($elm$virtual_dom$VirtualDom$attribute, '', '');
	return A3($rtfeldman$elm_css$VirtualDom$Styled$Attribute, classProperty, true, cssTemplate);
};
var $rtfeldman$elm_css$Html$Styled$Attributes$css = $rtfeldman$elm_css$Html$Styled$Internal$css;
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex = A2($rtfeldman$elm_css$Css$property, 'display', 'flex');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col = A2($rtfeldman$elm_css$Css$property, 'flex-direction', 'column');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_grow = A2($rtfeldman$elm_css$Css$property, 'flex-grow', '1');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_4 = A2($rtfeldman$elm_css$Css$property, 'gap', '1rem');
var $rtfeldman$elm_css$Html$Styled$h3 = $rtfeldman$elm_css$Html$Styled$node('h3');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$block = A2($rtfeldman$elm_css$Css$property, 'display', 'block');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$border_0 = A2($rtfeldman$elm_css$Css$property, 'border-width', '0px');
var $rtfeldman$elm_css$Css$focus = $rtfeldman$elm_css$Css$pseudoClass('focus');
var $rtfeldman$elm_css$Css$Structure$PseudoElement = function (a) {
	return {$: 'PseudoElement', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$WithPseudoElement = F2(
	function (a, b) {
		return {$: 'WithPseudoElement', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$pseudoElement = function (element) {
	return $rtfeldman$elm_css$Css$Preprocess$WithPseudoElement(
		$rtfeldman$elm_css$Css$Structure$PseudoElement(element));
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$form_input = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, '-webkit-appearance', 'none'),
			A2($rtfeldman$elm_css$Css$property, '-moz-appearance', 'none'),
			A2($rtfeldman$elm_css$Css$property, 'appearance', 'none'),
			A2($rtfeldman$elm_css$Css$property, 'background-color', '#fff'),
			A2($rtfeldman$elm_css$Css$property, 'border-color', '#6b7280'),
			A2($rtfeldman$elm_css$Css$property, 'border-width', '1px'),
			A2($rtfeldman$elm_css$Css$property, 'border-radius', '0px'),
			A2($rtfeldman$elm_css$Css$property, 'padding-top', '0.5rem'),
			A2($rtfeldman$elm_css$Css$property, 'padding-right', '0.75rem'),
			A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0.5rem'),
			A2($rtfeldman$elm_css$Css$property, 'padding-left', '0.75rem'),
			A2($rtfeldman$elm_css$Css$property, 'font-size', '1rem'),
			A2($rtfeldman$elm_css$Css$property, 'line-height', '1.5rem'),
			A2($rtfeldman$elm_css$Css$property, '--tw-shadow', '0 0 #0000'),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-meridiem-field',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-millisecond-field',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-second-field',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-minute-field',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-hour-field',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-day-field',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-month-field',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-year-field',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding-top', '0'),
					A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-date-and-time-value',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'min-height', '1.5em')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-webkit-datetime-edit-fields-wrapper',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'padding', '0')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'placeholder',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'color', '#6b7280'),
					A2($rtfeldman$elm_css$Css$property, 'opacity', '1')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoElement,
			'-moz-placeholder',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'color', '#6b7280'),
					A2($rtfeldman$elm_css$Css$property, 'opacity', '1')
				])),
			A2(
			$rtfeldman$elm_css$Css$pseudoClass,
			'focus',
			_List_fromArray(
				[
					A2($rtfeldman$elm_css$Css$property, 'outline', '2px solid transparent'),
					A2($rtfeldman$elm_css$Css$property, 'outline-offset', '2px'),
					A2($rtfeldman$elm_css$Css$property, '--tw-ring-inset', 'var(--tw-empty,/*!*/ /*!*/)'),
					A2($rtfeldman$elm_css$Css$property, '--tw-ring-offset-width', '0px'),
					A2($rtfeldman$elm_css$Css$property, '--tw-ring-offset-color', '#fff'),
					A2($rtfeldman$elm_css$Css$property, '--tw-ring-color', '#2563eb'),
					A2($rtfeldman$elm_css$Css$property, '--tw-ring-offset-shadow', 'var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) var(--tw-ring-offset-color)'),
					A2($rtfeldman$elm_css$Css$property, '--tw-ring-shadow', 'var(--tw-ring-inset) 0 0 0 calc(1px + var(--tw-ring-offset-width)) var(--tw-ring-color)'),
					A2($rtfeldman$elm_css$Css$property, 'box-shadow', 'var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow)'),
					A2($rtfeldman$elm_css$Css$property, 'border-color', '#2563eb')
				]))
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$gray_900 = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '17', '24', '39', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1_dot_5 = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'padding-top', '0.375rem'),
			A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '0.375rem')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_2 = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, '--tw-ring-offset-shadow', 'var(--tw-ring-inset) 0 0 0 var(--tw-ring-offset-width) var(--tw-ring-offset-color)'),
			A2($rtfeldman$elm_css$Css$property, '--tw-ring-shadow', 'var(--tw-ring-inset) 0 0 0 calc(2px + var(--tw-ring-offset-width)) var(--tw-ring-color)'),
			A2($rtfeldman$elm_css$Css$property, 'box-shadow', 'var(--tw-ring-offset-shadow), var(--tw-ring-shadow), var(--tw-shadow, 0 0 #0000)')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_color = function (color) {
	return A4(
		$matheus23$elm_tailwind_modules_base$Tailwind$Color$propertyWithColor,
		'--tw-ring-color',
		function (c) {
			return c;
		},
		$elm$core$Maybe$Just('--tw-ring-opacity'),
		color);
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_inset = A2($rtfeldman$elm_css$Css$property, '--tw-ring-inset', 'inset');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded_md = A2($rtfeldman$elm_css$Css$property, 'border-radius', '0.375rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$shadow_sm = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, '--tw-shadow', '0 1px 2px 0 rgb(0 0 0 / 0.05)'),
			A2($rtfeldman$elm_css$Css$property, '--tw-shadow-colored', '0 1px 2px 0 var(--tw-shadow-color)'),
			A2($rtfeldman$elm_css$Css$property, 'box-shadow', 'var(--tw-ring-offset-shadow, 0 0 #0000), var(--tw-ring-shadow, 0 0 #0000), var(--tw-shadow)')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full = A2($rtfeldman$elm_css$Css$property, 'width', '100%');
var $author$project$GlobalStyles$inputStyle = _List_fromArray(
	[
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$block,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$form_input,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded_md,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$border_0,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1_dot_5,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$gray_900),
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$shadow_sm,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_2,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_inset,
		$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$gray_900),
		$rtfeldman$elm_css$Css$focus(
		_List_fromArray(
			[
				$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_2,
				$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_inset,
				$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ring_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_400)
			]))
	]);
var $rtfeldman$elm_css$Html$Styled$li = $rtfeldman$elm_css$Html$Styled$node('li');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_0 = A2($rtfeldman$elm_css$Css$property, 'margin', '0px');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mt_4 = A2($rtfeldman$elm_css$Css$property, 'margin-top', '1rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mx_2 = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'margin-left', '0.5rem'),
			A2($rtfeldman$elm_css$Css$property, 'margin-right', '0.5rem')
		]));
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $rtfeldman$elm_css$VirtualDom$Styled$on = F2(
	function (eventName, handler) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$on, eventName, handler),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Events$on = F2(
	function (event, decoder) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $rtfeldman$elm_css$Html$Styled$Events$onClick = function (msg) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $rtfeldman$elm_css$Html$Styled$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$rtfeldman$elm_css$VirtualDom$Styled$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $rtfeldman$elm_css$Html$Styled$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $rtfeldman$elm_css$Html$Styled$Events$onInput = function (tagger) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$rtfeldman$elm_css$Html$Styled$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $rtfeldman$elm_css$Html$Styled$Events$targetValue)));
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$p_4 = A2($rtfeldman$elm_css$Css$property, 'padding', '1rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_4 = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'padding-top', '1rem'),
			A2($rtfeldman$elm_css$Css$property, 'padding-bottom', '1rem')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_200 = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '186', '230', '253', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $rtfeldman$elm_css$Html$Styled$textarea = $rtfeldman$elm_css$Html$Styled$node('textarea');
var $rtfeldman$elm_css$Html$Styled$Attributes$type_ = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('type');
var $rtfeldman$elm_css$Html$Styled$ul = $rtfeldman$elm_css$Html$Styled$node('ul');
var $rtfeldman$elm_css$Html$Styled$Attributes$value = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('value');
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$black = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '0', '0', '0', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_1 = A2($rtfeldman$elm_css$Css$property, 'gap', '0.25rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$gray_400 = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '156', '163', '175', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center = A2($rtfeldman$elm_css$Css$property, 'align-items', 'center');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_between = A2($rtfeldman$elm_css$Css$property, 'justify-content', 'space-between');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mb_4 = A2($rtfeldman$elm_css$Css$property, 'margin-bottom', '1rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$p_2 = A2($rtfeldman$elm_css$Css$property, 'padding', '0.5rem');
var $elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return $elm$core$Basics$floor(numerator / denominator);
	});
var $elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var $elm$time$Time$toAdjustedMinutes = F2(
	function (_v0, time) {
		var defaultOffset = _v0.a;
		var eras = _v0.b;
		return A3(
			$elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				$elm$time$Time$flooredDiv,
				$elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var $elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			24,
			A2(
				$elm$time$Time$flooredDiv,
				A2($elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var $elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			$elm$core$Basics$modBy,
			60,
			A2($elm$time$Time$toAdjustedMinutes, zone, time));
	});
var $elm$time$Time$utc = A2($elm$time$Time$Zone, 0, _List_Nil);
var $author$project$Chat$viewMessage = function (messageData) {
	var minute = $elm$core$String$fromInt(
		A2(
			$elm$time$Time$toMinute,
			$elm$time$Time$utc,
			$elm$time$Time$millisToPosix(messageData.timestamp)));
	var hour = $elm$core$String$fromInt(
		A2(
			$elm$time$Time$toHour,
			$elm$time$Time$utc,
			$elm$time$Time$millisToPosix(messageData.timestamp)));
	return A2(
		$rtfeldman$elm_css$Html$Styled$li,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_200),
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$p_2,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mb_4
					]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_between, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_1]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[
												$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$black)
											]))
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(messageData.name + ':')
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text(messageData.data.message)
									]))
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$gray_400)
									]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text(hour + (':' + minute))
							]))
					]))
			]));
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_80 = A2($rtfeldman$elm_css$Css$property, 'width', '20rem');
var $author$project$Chat$view = function (model) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mx_2]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_80,
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_200),
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$p_4,
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded
									]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$h3,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_0]))
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Participants')
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$ul,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mt_4]))
											]),
										A2(
											$elm$core$List$map,
											function (user) {
												return A2(
													$rtfeldman$elm_css$Html$Styled$li,
													_List_fromArray(
														[
															$rtfeldman$elm_css$Html$Styled$Attributes$css(
															_List_fromArray(
																[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mt_4]))
														]),
													_List_fromArray(
														[
															$rtfeldman$elm_css$Html$Styled$text(
															$author$project$Chat$takeNameOrEmail(user))
														]));
											},
											model.users))
									]))
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_grow]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$class('customHeight')
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$ul,
										_List_Nil,
										A2($elm$core$List$map, $author$project$Chat$viewMessage, model.receivedMessages))
									])),
								function () {
								var _v0 = model.error;
								if (_v0.$ === 'Just') {
									var err = _v0.a;
									return A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_Nil,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text(err)
											]));
								} else {
									return A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_4]))
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$textarea,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$inputStyle),
														$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Chat$StoreMessage),
														$rtfeldman$elm_css$Html$Styled$Attributes$value(model.message)
													]),
												_List_Nil),
												A2(
												$rtfeldman$elm_css$Html$Styled$button,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$type_('button'),
														$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$buttonStyle),
														$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Chat$MessageSubmit)
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('Send')
													]))
											]));
								}
							}()
							]))
					]))
			]));
};
var $rtfeldman$elm_css$Html$Styled$h2 = $rtfeldman$elm_css$Html$Styled$node('h2');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20 = A2($rtfeldman$elm_css$Css$property, 'margin', '5rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6 = A2($rtfeldman$elm_css$Css$property, 'margin', '1.5rem');
var $rtfeldman$elm_css$Css$Structure$CustomQuery = function (a) {
	return {$: 'CustomQuery', a: a};
};
var $rtfeldman$elm_css$Css$Preprocess$WithMedia = F2(
	function (a, b) {
		return {$: 'WithMedia', a: a, b: b};
	});
var $rtfeldman$elm_css$Css$Media$withMediaQuery = function (queries) {
	return $rtfeldman$elm_css$Css$Preprocess$WithMedia(
		A2($elm$core$List$map, $rtfeldman$elm_css$Css$Structure$CustomQuery, queries));
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$sm = $rtfeldman$elm_css$Css$Media$withMediaQuery(
	_List_fromArray(
		['(min-width: 640px)']));
var $author$project$Home$view = A2(
	$rtfeldman$elm_css$Html$Styled$div,
	_List_fromArray(
		[
			$rtfeldman$elm_css$Html$Styled$Attributes$css(
			_List_fromArray(
				[
					$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
					$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
					$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
					$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6,
					$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$sm(
					_List_fromArray(
						[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20]))
				]))
		]),
	_List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$h2,
			_List_Nil,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('Hello and welcome to our awesome website !')
				])),
			A2(
			$rtfeldman$elm_css$Html$Styled$p,
			_List_Nil,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$text('which is still under construction')
				]))
		]));
var $author$project$Login$LoginSubmit = {$: 'LoginSubmit'};
var $author$project$Login$StoreEmail = function (a) {
	return {$: 'StoreEmail', a: a};
};
var $author$project$Login$StorePassword = function (a) {
	return {$: 'StorePassword', a: a};
};
var $rtfeldman$elm_css$Html$Styled$a = $rtfeldman$elm_css$Html$Styled$node('a');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$absolute = A2($rtfeldman$elm_css$Css$property, 'position', 'absolute');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_opacity_40 = A2($rtfeldman$elm_css$Css$property, '--tw-bg-opacity', '0.4');
var $rtfeldman$elm_css$Html$Styled$form = $rtfeldman$elm_css$Html$Styled$node('form');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3 = A2($rtfeldman$elm_css$Css$property, 'gap', '0.75rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_5 = A2($rtfeldman$elm_css$Css$property, 'gap', '1.25rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_full = A2($rtfeldman$elm_css$Css$property, 'height', '100%');
var $rtfeldman$elm_css$Html$Styled$Attributes$href = function (url) {
	return A2($rtfeldman$elm_css$Html$Styled$Attributes$stringProperty, 'href', url);
};
var $rtfeldman$elm_css$Html$Styled$input = $rtfeldman$elm_css$Html$Styled$node('input');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_center = A2($rtfeldman$elm_css$Css$property, 'justify-content', 'center');
var $rtfeldman$elm_css$Html$Styled$label = $rtfeldman$elm_css$Html$Styled$node('label');
var $rtfeldman$elm_css$Css$Preprocess$WithKeyframes = function (a) {
	return {$: 'WithKeyframes', a: a};
};
var $rtfeldman$elm_css$Css$prop1 = F2(
	function (key, arg) {
		return A2($rtfeldman$elm_css$Css$property, key, arg.value);
	});
var $rtfeldman$elm_css$Css$animationName = function (arg) {
	return ((arg.value === 'none') || ((arg.value === 'inherit') || ((arg.value === 'unset') || (arg.value === 'initial')))) ? A2($rtfeldman$elm_css$Css$prop1, 'animation-name', arg) : $rtfeldman$elm_css$Css$Preprocess$WithKeyframes(arg.value);
};
var $rtfeldman$elm_css$Css$Structure$Compatible = {$: 'Compatible'};
var $rtfeldman$elm_css$Css$Internal$printKeyframeSelector = function (_v0) {
	var percentage = _v0.a;
	var properties = _v0.b;
	var propertiesStr = A3(
		$rtfeldman$elm_css$Css$String$mapJoin,
		function (_v1) {
			var prop = _v1.a;
			return prop + ';';
		},
		'',
		properties);
	var percentageStr = $elm$core$String$fromInt(percentage) + '%';
	return percentageStr + ('{' + (propertiesStr + '}'));
};
var $rtfeldman$elm_css$Css$Internal$compileKeyframes = function (tuples) {
	return A3($rtfeldman$elm_css$Css$String$mapJoin, $rtfeldman$elm_css$Css$Internal$printKeyframeSelector, '', tuples);
};
var $rtfeldman$elm_css$Css$Animations$keyframes = function (tuples) {
	return $elm$core$List$isEmpty(tuples) ? {keyframes: $rtfeldman$elm_css$Css$Structure$Compatible, none: $rtfeldman$elm_css$Css$Structure$Compatible, value: 'none'} : {
		keyframes: $rtfeldman$elm_css$Css$Structure$Compatible,
		none: $rtfeldman$elm_css$Css$Structure$Compatible,
		value: $rtfeldman$elm_css$Css$Internal$compileKeyframes(tuples)
	};
};
var $rtfeldman$elm_css$Css$Internal$Property = function (a) {
	return {$: 'Property', a: a};
};
var $rtfeldman$elm_css$Css$Animations$property = F2(
	function (key, value) {
		return $rtfeldman$elm_css$Css$Internal$Property(key + (':' + value));
	});
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$animate_ping = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, '-webkit-animation', '1s cubic-bezier(0, 0, 0.2, 1) infinite'),
			$rtfeldman$elm_css$Css$animationName(
			$rtfeldman$elm_css$Css$Animations$keyframes(
				_List_fromArray(
					[
						_Utils_Tuple2(
						75,
						_List_fromArray(
							[
								A2($rtfeldman$elm_css$Css$Animations$property, 'transform', 'scale(2)'),
								A2($rtfeldman$elm_css$Css$Animations$property, 'opacity', '0')
							]))
					]))),
			A2($rtfeldman$elm_css$Css$property, 'animation', '1s cubic-bezier(0, 0, 0.2, 1) infinite'),
			$rtfeldman$elm_css$Css$animationName(
			$rtfeldman$elm_css$Css$Animations$keyframes(
				_List_fromArray(
					[
						_Utils_Tuple2(
						75,
						_List_fromArray(
							[
								A2($rtfeldman$elm_css$Css$Animations$property, 'transform', 'scale(2)'),
								A2($rtfeldman$elm_css$Css$Animations$property, 'opacity', '0')
							]))
					])))
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_5 = A2($rtfeldman$elm_css$Css$property, 'height', '1.25rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$inline_flex = A2($rtfeldman$elm_css$Css$property, 'display', 'inline-flex');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$opacity_75 = A2($rtfeldman$elm_css$Css$property, 'opacity', '0.75');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$relative = A2($rtfeldman$elm_css$Css$property, 'position', 'relative');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded_full = A2($rtfeldman$elm_css$Css$property, 'border-radius', '9999px');
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_500 = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '14', '165', '233', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $rtfeldman$elm_css$Html$Styled$span = $rtfeldman$elm_css$Html$Styled$node('span');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_5 = A2($rtfeldman$elm_css$Css$property, 'width', '1.25rem');
var $author$project$Helpers$loadingElement = A2(
	$rtfeldman$elm_css$Html$Styled$div,
	_List_fromArray(
		[
			$rtfeldman$elm_css$Html$Styled$Attributes$css(
			_List_fromArray(
				[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$relative, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_5, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_5, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex]))
		]),
	_List_fromArray(
		[
			A2(
			$rtfeldman$elm_css$Html$Styled$span,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$animate_ping,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$absolute,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$inline_flex,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_full,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded_full,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_400),
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$opacity_75
						]))
				]),
			_List_Nil),
			A2(
			$rtfeldman$elm_css$Html$Styled$span,
			_List_fromArray(
				[
					$rtfeldman$elm_css$Html$Styled$Attributes$css(
					_List_fromArray(
						[
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$relative,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$inline_flex,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded_full,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_5,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_5,
							$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_500)
						]))
				]),
			_List_Nil)
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$md = $rtfeldman$elm_css$Css$Media$withMediaQuery(
	_List_fromArray(
		['(min-width: 768px)']));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mt_5 = A2($rtfeldman$elm_css$Css$property, 'margin-top', '1.25rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$red_400 = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '248', '113', '113', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_50 = A5($matheus23$elm_tailwind_modules_base$Tailwind$Color$Color, 'rgb', '240', '249', '255', $matheus23$elm_tailwind_modules_base$Tailwind$Color$ViaVariable);
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_3xl = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'font-size', '1.875rem'),
			A2($rtfeldman$elm_css$Css$property, 'line-height', '2.25rem')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_60 = A2($rtfeldman$elm_css$Css$property, 'width', '15rem');
var $author$project$Login$view = function (model) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$relative,
						$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$md(
						_List_fromArray(
							[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20]))
					]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$h2,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_3xl]))
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Login')
					])),
				function () {
				var _v0 = model.formState;
				switch (_v0.$) {
					case 'Loading':
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$absolute,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_full,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_center,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_50),
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_opacity_40
										]))
								]),
							_List_fromArray(
								[$author$project$Helpers$loadingElement]));
					case 'Error':
						var error = _v0.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$p,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$red_400)
										]))
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(error)
								]));
					default:
						return $rtfeldman$elm_css$Html$Styled$text('');
				}
			}(),
				A2(
				$rtfeldman$elm_css$Html$Styled$form,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_5,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full,
								$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$md(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_60]))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$label,
								_List_Nil,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Email')
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$input,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$inputStyle),
										$rtfeldman$elm_css$Html$Styled$Attributes$type_('text'),
										$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Login$StoreEmail),
										$rtfeldman$elm_css$Html$Styled$Attributes$value(model.storeEmail)
									]),
								_List_Nil)
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$label,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(_List_Nil)
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('Password')
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$input,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$inputStyle),
										$rtfeldman$elm_css$Html$Styled$Attributes$type_('password'),
										$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Login$StorePassword),
										$rtfeldman$elm_css$Html$Styled$Attributes$value(model.storePassword)
									]),
								_List_Nil)
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$button,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$buttonStyle),
								$rtfeldman$elm_css$Html$Styled$Attributes$type_('button'),
								$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Login$LoginSubmit)
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Sign in')
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$a,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$href('/forgot-password'),
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mt_5]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Forgot password ?')
							]))
					]))
			]));
};
var $author$project$Profile$FileRequest = {$: 'FileRequest'};
var $author$project$Profile$ProfileSubmit = function (a) {
	return {$: 'ProfileSubmit', a: a};
};
var $author$project$Profile$StoreFirstName = function (a) {
	return {$: 'StoreFirstName', a: a};
};
var $rtfeldman$elm_css$Html$Styled$Attributes$for = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('htmlFor');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_1 = A2($rtfeldman$elm_css$Css$property, 'height', '0.25rem');
var $rtfeldman$elm_css$Html$Styled$Attributes$id = $rtfeldman$elm_css$Html$Styled$Attributes$stringProperty('id');
var $rtfeldman$elm_css$Html$Styled$img = $rtfeldman$elm_css$Html$Styled$node('img');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$opacity_0 = A2($rtfeldman$elm_css$Css$property, 'opacity', '0');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$overflow_hidden = A2($rtfeldman$elm_css$Css$property, 'overflow', 'hidden');
var $rtfeldman$elm_css$Html$Styled$Attributes$src = function (url) {
	return A2($rtfeldman$elm_css$Html$Styled$Attributes$stringProperty, 'src', url);
};
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_sm = $rtfeldman$elm_css$Css$batch(
	_List_fromArray(
		[
			A2($rtfeldman$elm_css$Css$property, 'font-size', '0.875rem'),
			A2($rtfeldman$elm_css$Css$property, 'line-height', '1.25rem')
		]));
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_1 = A2($rtfeldman$elm_css$Css$property, 'width', '0.25rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$z_0 = A2($rtfeldman$elm_css$Css$property, 'z-index', '0');
var $author$project$Profile$view = function (model) {
	var _v0 = model.userState;
	switch (_v0.$) {
		case 'Verified':
			var session = _v0.a;
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$relative,
								$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$sm(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20]))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$h2,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_3xl]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Hello')
							])),
						function () {
						var _v1 = model.formState;
						switch (_v1.$) {
							case 'Loading':
								return A2(
									$rtfeldman$elm_css$Html$Styled$div,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$absolute,
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full,
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_full,
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_center,
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_50),
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_opacity_40
												]))
										]),
									_List_fromArray(
										[$author$project$Helpers$loadingElement]));
							case 'Error':
								var error = _v1.a;
								return A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$Attributes$css(
											_List_fromArray(
												[
													$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$red_400)
												]))
										]),
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text(error)
										]));
							default:
								return $rtfeldman$elm_css$Html$Styled$text('');
						}
					}(),
						A2(
						$rtfeldman$elm_css$Html$Styled$form,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_5,
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl,
										$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full,
										$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$md(
										_List_fromArray(
											[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_60]))
									]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
									]),
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$text('First Name'),
										A2(
										$rtfeldman$elm_css$Html$Styled$input,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$inputStyle),
												$rtfeldman$elm_css$Html$Styled$Attributes$type_('text'),
												$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Profile$StoreFirstName),
												$rtfeldman$elm_css$Html$Styled$Attributes$value(model.storeName)
											]),
										_List_Nil)
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col]))
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$p,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_0]))
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('Upload an avatar')
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$p,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[
																$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_0,
																$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_sm,
																$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$gray_400)
															]))
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text('(Size limit is 3 mb)')
													]))
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$label,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$for('file'),
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_Utils_ap(
													$author$project$GlobalStyles$buttonStyle,
													_List_fromArray(
														[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$overflow_hidden])))
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Choose file'),
												A2(
												$rtfeldman$elm_css$Html$Styled$input,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$overflow_hidden, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$opacity_0, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$absolute, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$z_0])),
														$rtfeldman$elm_css$Html$Styled$Attributes$id('file'),
														$rtfeldman$elm_css$Html$Styled$Attributes$type_('file'),
														$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Profile$FileRequest)
													]),
												_List_Nil)
											]))
									])),
								function () {
								var _v2 = model.profilePic;
								if (_v2.$ === 'Just') {
									var imageString = _v2.a;
									return A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Your avatar preview'),
												A2(
												$rtfeldman$elm_css$Html$Styled$img,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded])),
														$rtfeldman$elm_css$Html$Styled$Attributes$src(imageString)
													]),
												_List_Nil)
											]));
								} else {
									return $rtfeldman$elm_css$Html$Styled$text('');
								}
							}(),
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$button,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$buttonStyle),
												$rtfeldman$elm_css$Html$Styled$Attributes$type_('button'),
												$rtfeldman$elm_css$Html$Styled$Events$onClick(
												$author$project$Profile$ProfileSubmit(session))
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('Submit')
											]))
									]))
							]))
					]));
		case 'NotVerified':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6,
								$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$sm(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20]))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$h2,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Please verify your email ! ')
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$p,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('You can\'t access your profile until you verify your email')
							]))
					]));
		case 'Intruder':
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6,
								$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$sm(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20]))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$h2,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Hmm seems you are not logged in')
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$p,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Please create account or login')
							]))
					]));
		default:
			return A2(
				$rtfeldman$elm_css$Html$Styled$div,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6,
								$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$sm(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20]))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$h2,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Your session have expired')
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$p,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Please login again')
							]))
					]));
	}
};
var $author$project$Signup$SignupSubmit = {$: 'SignupSubmit'};
var $author$project$Signup$StoreConfirmPassword = function (a) {
	return {$: 'StoreConfirmPassword', a: a};
};
var $author$project$Signup$StoreEmail = function (a) {
	return {$: 'StoreEmail', a: a};
};
var $author$project$Signup$StorePassword = function (a) {
	return {$: 'StorePassword', a: a};
};
var $author$project$Signup$view = function (model) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$relative,
						$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$md(
						_List_fromArray(
							[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20]))
					]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$h2,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_3xl]))
					]),
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$text('Signup')
					])),
				function () {
				var _v0 = model.formState;
				switch (_v0.$) {
					case 'Loading':
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$absolute,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_full,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_center,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$sky_50),
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_opacity_40
										]))
								]),
							_List_fromArray(
								[$author$project$Helpers$loadingElement]));
					case 'Error':
						var error = _v0.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$p,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$Attributes$css(
									_List_fromArray(
										[
											$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$red_400)
										]))
								]),
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text(error)
								]));
					default:
						return $rtfeldman$elm_css$Html$Styled$text('');
				}
			}(),
				A2(
				$rtfeldman$elm_css$Html$Styled$form,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$css(
						_List_fromArray(
							[
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_5,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl,
								$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_full,
								$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$md(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_60]))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Email'),
								A2(
								$rtfeldman$elm_css$Html$Styled$input,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$inputStyle),
										$rtfeldman$elm_css$Html$Styled$Attributes$type_('text'),
										$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Signup$StoreEmail),
										$rtfeldman$elm_css$Html$Styled$Attributes$value(model.storeEmail)
									]),
								_List_Nil)
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Password'),
								A2(
								$rtfeldman$elm_css$Html$Styled$input,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$inputStyle),
										$rtfeldman$elm_css$Html$Styled$Attributes$type_('password'),
										$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Signup$StorePassword),
										$rtfeldman$elm_css$Html$Styled$Attributes$value(model.storePassword)
									]),
								_List_Nil)
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$div,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_3]))
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Confirm Password'),
								A2(
								$rtfeldman$elm_css$Html$Styled$input,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$inputStyle),
										$rtfeldman$elm_css$Html$Styled$Attributes$type_('password'),
										$rtfeldman$elm_css$Html$Styled$Events$onInput($author$project$Signup$StoreConfirmPassword),
										$rtfeldman$elm_css$Html$Styled$Attributes$value(model.storeConfirmPassword)
									]),
								_List_Nil)
							])),
						A2(
						$rtfeldman$elm_css$Html$Styled$button,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css($author$project$GlobalStyles$buttonStyle),
								$rtfeldman$elm_css$Html$Styled$Attributes$type_('button'),
								$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Signup$SignupSubmit)
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Sign up')
							]))
					]))
			]));
};
var $author$project$Verification$view = function (model) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center,
						$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_6,
						$matheus23$elm_default_tailwind_modules$Tailwind$Breakpoints$sm(
						_List_fromArray(
							[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$m_20]))
					]))
			]),
		_List_fromArray(
			[
				function () {
				var _v0 = model.userState;
				switch (_v0.$) {
					case 'VerificationPending':
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$h2,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('Give us a moment to verify your account ! ')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('Soon you will have access to a all profile features')
										])),
									$author$project$Helpers$loadingElement
								]));
					case 'VerificationDone':
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$h2,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('Thanks for verifying your email ! ')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('Now you will be redirected to your profile page and have full access to all app\'s features')
										])),
									$author$project$Helpers$loadingElement
								]));
					case 'VerificationFail':
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$h2,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('UPS seems that something is off !')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('Try to re-login or refresh the page')
										]))
								]));
					case 'Verified':
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$h2,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('HMMm seems that you\'re already verified !')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('Please proceed to you profile')
										]))
								]));
					default:
						return A2(
							$rtfeldman$elm_css$Html$Styled$div,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rtfeldman$elm_css$Html$Styled$h2,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('You are not logged in !')
										])),
									A2(
									$rtfeldman$elm_css$Html$Styled$p,
									_List_Nil,
									_List_fromArray(
										[
											$rtfeldman$elm_css$Html$Styled$text('Please proceed to login')
										]))
								]));
				}
			}()
			]));
};
var $author$project$Main$content = function (model) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_Nil,
		_List_fromArray(
			[
				function () {
				var _v0 = model.page;
				switch (_v0.$) {
					case 'LoginPage':
						var loginModel = _v0.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$map,
							$author$project$Main$GotLoginMsg,
							$author$project$Login$view(loginModel));
					case 'SignupPage':
						var signupModel = _v0.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$map,
							$author$project$Main$GotSignupMsg,
							$author$project$Signup$view(signupModel));
					case 'ProfilePage':
						var profileModel = _v0.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$map,
							$author$project$Main$GotProfileMsg,
							$author$project$Profile$view(profileModel));
					case 'HomePage':
						return A2($rtfeldman$elm_css$Html$Styled$map, $author$project$Main$GotHomeMsg, $author$project$Home$view);
					case 'ChatPage':
						var chatModel = _v0.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$map,
							$author$project$Main$GotChatMsg,
							$author$project$Chat$view(chatModel));
					case 'VerificationPage':
						var verificationModel = _v0.a;
						return A2(
							$rtfeldman$elm_css$Html$Styled$map,
							$author$project$Main$GotVerificationMsg,
							$author$project$Verification$view(verificationModel));
					default:
						return A2(
							$rtfeldman$elm_css$Html$Styled$p,
							_List_Nil,
							_List_fromArray(
								[
									$rtfeldman$elm_css$Html$Styled$text('Page not found buddy -_- sorry')
								]));
				}
			}()
			]));
};
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$classList = function (classes) {
	return $rtfeldman$elm_css$Html$Styled$Attributes$class(
		A3(
			$rtfeldman$elm_css$Css$String$mapJoin,
			$elm$core$Tuple$first,
			' ',
			A2($elm$core$List$filter, $elm$core$Tuple$second, classes)));
};
var $rtfeldman$elm_css$Html$Styled$h1 = $rtfeldman$elm_css$Html$Styled$node('h1');
var $author$project$Main$isActive = function (_v0) {
	var link = _v0.link;
	var page = _v0.page;
	var _v1 = _Utils_Tuple2(link, page);
	switch (_v1.a.$) {
		case 'Login':
			if (_v1.b.$ === 'LoginPage') {
				var _v2 = _v1.a;
				return true;
			} else {
				var _v3 = _v1.a;
				return false;
			}
		case 'Signup':
			if (_v1.b.$ === 'SignupPage') {
				var _v4 = _v1.a;
				return true;
			} else {
				var _v5 = _v1.a;
				return false;
			}
		case 'Profile':
			if (_v1.b.$ === 'ProfilePage') {
				return true;
			} else {
				return false;
			}
		case 'Home':
			if (_v1.b.$ === 'HomePage') {
				var _v6 = _v1.a;
				return true;
			} else {
				var _v7 = _v1.a;
				return false;
			}
		case 'Chat':
			if (_v1.b.$ === 'ChatPage') {
				var _v8 = _v1.a;
				return true;
			} else {
				var _v9 = _v1.a;
				return false;
			}
		case 'Verification':
			if (_v1.b.$ === 'VerificationPage') {
				return true;
			} else {
				return false;
			}
		default:
			var _v10 = _v1.a;
			return false;
	}
};
var $rtfeldman$elm_css$Html$Styled$nav = $rtfeldman$elm_css$Html$Styled$node('nav');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$p_5 = A2($rtfeldman$elm_css$Css$property, 'padding', '1.25rem');
var $author$project$Main$GetLogout = {$: 'GetLogout'};
var $author$project$Main$OpenDropdown = {$: 'OpenDropdown'};
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$duration_500 = A2($rtfeldman$elm_css$Css$property, 'transition-duration', '500ms');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_10 = A2($rtfeldman$elm_css$Css$property, 'height', '2.5rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_end = A2($rtfeldman$elm_css$Css$property, 'align-items', 'flex-end');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ml_1 = A2($rtfeldman$elm_css$Css$property, 'margin-left', '0.25rem');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mt_3 = A2($rtfeldman$elm_css$Css$property, 'margin-top', '0.75rem');
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $rtfeldman$elm_css$VirtualDom$Styled$style = F2(
	function (key, val) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$style, key, val),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$style = $rtfeldman$elm_css$VirtualDom$Styled$style;
var $rtfeldman$elm_css$Html$Styled$sup = $rtfeldman$elm_css$Html$Styled$node('sup');
var $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_10 = A2($rtfeldman$elm_css$Css$property, 'width', '2.5rem');
var $rtfeldman$elm_css$VirtualDom$Styled$attribute = F2(
	function (key, value) {
		return A3(
			$rtfeldman$elm_css$VirtualDom$Styled$Attribute,
			A2($elm$virtual_dom$VirtualDom$attribute, key, value),
			false,
			'');
	});
var $rtfeldman$elm_css$Html$Styled$Attributes$width = function (n) {
	return A2(
		$rtfeldman$elm_css$VirtualDom$Styled$attribute,
		'width',
		$elm$core$String$fromInt(n));
};
var $author$project$Main$viewLoggedInHeader = function (_v0) {
	var page = _v0.page;
	var token = _v0.token;
	var openDropdown = _v0.openDropdown;
	return A2(
		$rtfeldman$elm_css$Html$Styled$ul,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_between, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_end]))
			]),
		_List_fromArray(
			[
				function () {
				var _v1 = A2(
					$simonh1000$elm_jwt$Jwt$decodeToken,
					$author$project$Credentials$decodeTokenData,
					$author$project$Credentials$fromTokenToString(token));
				if (_v1.$ === 'Ok') {
					var resultTokenRecord = _v1.a;
					return A2(
						$rtfeldman$elm_css$Html$Styled$li,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$cursor_pointer]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$div,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$css(
										_List_fromArray(
											[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$relative]))
									]),
								_List_fromArray(
									[
										($elm$core$String$length(resultTokenRecord.firstname) > 0) ? A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center])),
												$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$OpenDropdown)
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_10, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$h_10, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$overflow_hidden, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded_full]))
													]),
												_List_fromArray(
													[
														$elm$core$String$isEmpty(resultTokenRecord.profilepicurl) ? $rtfeldman$elm_css$Html$Styled$text('') : A2(
														$rtfeldman$elm_css$Html$Styled$img,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$w_10])),
																$rtfeldman$elm_css$Html$Styled$Attributes$src(resultTokenRecord.profilepicurl)
															]),
														_List_Nil)
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$span,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl]))
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text(resultTokenRecord.firstname),
														A2(
														$rtfeldman$elm_css$Html$Styled$sup,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ml_1]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('⌄')
															]))
													]))
											])) : A2(
										$rtfeldman$elm_css$Html$Styled$div,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$OpenDropdown)
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$span,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$css(
														_List_fromArray(
															[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl]))
													]),
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$text(resultTokenRecord.email),
														A2(
														$rtfeldman$elm_css$Html$Styled$sup,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$ml_1]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('⌄')
															]))
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$div,
												_List_Nil,
												_List_fromArray(
													[
														$elm$core$String$isEmpty(resultTokenRecord.profilepicurl) ? $rtfeldman$elm_css$Html$Styled$text('') : A2(
														$rtfeldman$elm_css$Html$Styled$img,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$src(resultTokenRecord.profilepicurl),
																$rtfeldman$elm_css$Html$Styled$Attributes$width(60)
															]),
														_List_Nil)
													]))
											])),
										A2(
										$rtfeldman$elm_css$Html$Styled$ul,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex,
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$absolute,
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$mt_3,
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex_col,
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_1,
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$overflow_hidden,
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$transition_all,
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$duration_500,
														$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$bg_color($matheus23$elm_default_tailwind_modules$Tailwind$Theme$white)
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$Attributes$style,
												'height',
												openDropdown ? '90px' : '0')
											]),
										_List_fromArray(
											[
												A2(
												$rtfeldman$elm_css$Html$Styled$li,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Attributes$classList(
														_List_fromArray(
															[
																_Utils_Tuple2(
																'active',
																$author$project$Main$isActive(
																	{
																		link: $author$project$Main$Profile(resultTokenRecord.id),
																		page: page
																	}))
															])),
														$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$OpenDropdown)
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$a,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded])),
																$rtfeldman$elm_css$Html$Styled$Attributes$href(
																'/profile/' + $author$project$Credentials$userIdToString(resultTokenRecord.id))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('My profile')
															]))
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$li,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$OpenDropdown)
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$a,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('option2')
															]))
													])),
												A2(
												$rtfeldman$elm_css$Html$Styled$li,
												_List_fromArray(
													[
														$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$OpenDropdown)
													]),
												_List_fromArray(
													[
														A2(
														$rtfeldman$elm_css$Html$Styled$a,
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$Attributes$css(
																_List_fromArray(
																	[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded]))
															]),
														_List_fromArray(
															[
																$rtfeldman$elm_css$Html$Styled$text('option3')
															]))
													]))
											]))
									]))
							]));
				} else {
					var err = _v1.a;
					return A2(
						$rtfeldman$elm_css$Html$Styled$li,
						_List_Nil,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text(
								$elm$core$Debug$toString(err))
							]));
				}
			}(),
				A2(
				$rtfeldman$elm_css$Html$Styled$li,
				_List_fromArray(
					[
						$rtfeldman$elm_css$Html$Styled$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'active',
								$author$project$Main$isActive(
									{link: $author$project$Main$Chat, page: page}))
							]))
					]),
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$a,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex])),
								$rtfeldman$elm_css$Html$Styled$Attributes$href('/chat')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('Chat')
							]))
					])),
				A2(
				$rtfeldman$elm_css$Html$Styled$li,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$a,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex])),
								$rtfeldman$elm_css$Html$Styled$Attributes$href('/'),
								$rtfeldman$elm_css$Html$Styled$Events$onClick($author$project$Main$GetLogout)
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('logout')
							]))
					]))
			]));
};
var $author$project$Main$viewHeader = function (_v0) {
	var page = _v0.page;
	var session = _v0.session;
	var openDropdown = _v0.openDropdown;
	return A2(
		$rtfeldman$elm_css$Html$Styled$nav,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Attributes$css(
				_List_fromArray(
					[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$p_5, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_between, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$items_center]))
			]),
		_List_fromArray(
			[
				A2(
				$rtfeldman$elm_css$Html$Styled$h1,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$rtfeldman$elm_css$Html$Styled$a,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$href('/')
							]),
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$text('My elm app')
							]))
					])),
				function () {
				var _v1 = $author$project$Credentials$fromSessionToToken(session);
				if (_v1.$ === 'Just') {
					var token = _v1.a;
					return $author$project$Main$viewLoggedInHeader(
						{openDropdown: openDropdown, page: page, token: token});
				} else {
					return A2(
						$rtfeldman$elm_css$Html$Styled$ul,
						_List_fromArray(
							[
								$rtfeldman$elm_css$Html$Styled$Attributes$css(
								_List_fromArray(
									[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$justify_between, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$gap_4]))
							]),
						_List_fromArray(
							[
								A2(
								$rtfeldman$elm_css$Html$Styled$li,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$classList(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'active',
												$author$project$Main$isActive(
													{link: $author$project$Main$Home, page: page}))
											]))
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$a,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex])),
												$rtfeldman$elm_css$Html$Styled$Attributes$href('/')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('home')
											]))
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$li,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$classList(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'active',
												$author$project$Main$isActive(
													{link: $author$project$Main$Login, page: page}))
											]))
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$a,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex])),
												$rtfeldman$elm_css$Html$Styled$Attributes$href('/login')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('login')
											]))
									])),
								A2(
								$rtfeldman$elm_css$Html$Styled$li,
								_List_fromArray(
									[
										$rtfeldman$elm_css$Html$Styled$Attributes$classList(
										_List_fromArray(
											[
												_Utils_Tuple2(
												'active',
												$author$project$Main$isActive(
													{link: $author$project$Main$Signup, page: page}))
											]))
									]),
								_List_fromArray(
									[
										A2(
										$rtfeldman$elm_css$Html$Styled$a,
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$Attributes$css(
												_List_fromArray(
													[$matheus23$elm_default_tailwind_modules$Tailwind$Utilities$py_1, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$px_4, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$text_xl, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$rounded, $matheus23$elm_default_tailwind_modules$Tailwind$Utilities$flex])),
												$rtfeldman$elm_css$Html$Styled$Attributes$href('/signup')
											]),
										_List_fromArray(
											[
												$rtfeldman$elm_css$Html$Styled$text('sign up')
											]))
									]))
							]));
				}
			}()
			]));
};
var $author$project$Main$app = function (model) {
	return A2(
		$rtfeldman$elm_css$Html$Styled$div,
		_List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$Events$onClick(
				$author$project$Main$CheckSessionExpired(
					_Utils_Tuple2(model.session, model.time)))
			]),
		_List_fromArray(
			[
				$author$project$Main$viewHeader(model),
				$author$project$Main$content(model)
			]));
};
var $rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles = function (a) {
	return {$: 'UnscopedStyles', a: a};
};
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles = F2(
	function (_v0, styles) {
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				return styles;
			} else {
				return A3(
					$elm$core$Dict$insert,
					cssTemplate,
					$rtfeldman$elm_css$Hash$fromString(cssTemplate),
					styles);
			}
		} else {
			return styles;
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute = F2(
	function (styles, _v0) {
		var val = _v0.a;
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				var classname = _v1.a;
				return A2(
					$elm$virtual_dom$VirtualDom$property,
					'className',
					$elm$json$Json$Encode$string(classname));
			} else {
				return A2(
					$elm$virtual_dom$VirtualDom$property,
					'className',
					$elm$json$Json$Encode$string('_unstyled'));
			}
		} else {
			return val;
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS = F2(
	function (styles, _v0) {
		var val = _v0.a;
		var isCssStyles = _v0.b;
		var cssTemplate = _v0.c;
		if (isCssStyles) {
			var _v1 = A2($elm$core$Dict$get, cssTemplate, styles);
			if (_v1.$ === 'Just') {
				var classname = _v1.a;
				return A2($elm$virtual_dom$VirtualDom$attribute, 'class', classname);
			} else {
				return A2($elm$virtual_dom$VirtualDom$attribute, 'class', '_unstyled');
			}
		} else {
			return val;
		}
	});
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$keyedNodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_keyedNodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$virtual_dom$VirtualDom$nodeNS = F2(
	function (namespace, tag) {
		return A2(
			_VirtualDom_nodeNS,
			namespace,
			_VirtualDom_noScript(tag));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml = F2(
	function (_v6, _v7) {
		var key = _v6.a;
		var html = _v6.b;
		var pairs = _v7.a;
		var styles = _v7.b;
		switch (html.$) {
			case 'Unstyled':
				var vdom = html.a;
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					styles);
			case 'Node':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v9 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v9.a;
				var finalStyles = _v9.b;
				var vdom = A3(
					$elm$virtual_dom$VirtualDom$node,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 'NodeNS':
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v10 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v10.a;
				var finalStyles = _v10.b;
				var vdom = A4(
					$elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			case 'KeyedNode':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v11 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v11.a;
				var finalStyles = _v11.b;
				var vdom = A3(
					$elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v12 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v12.a;
				var finalStyles = _v12.b;
				var vdom = A4(
					$elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(key, vdom),
						pairs),
					finalStyles);
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml = F2(
	function (html, _v0) {
		var nodes = _v0.a;
		var styles = _v0.b;
		switch (html.$) {
			case 'Unstyled':
				var vdomNode = html.a;
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					styles);
			case 'Node':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v2 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v2.a;
				var finalStyles = _v2.b;
				var vdomNode = A3(
					$elm$virtual_dom$VirtualDom$node,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 'NodeNS':
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v3 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v3.a;
				var finalStyles = _v3.b;
				var vdomNode = A4(
					$elm$virtual_dom$VirtualDom$nodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			case 'KeyedNode':
				var elemType = html.a;
				var properties = html.b;
				var children = html.c;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v4 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v4.a;
				var finalStyles = _v4.b;
				var vdomNode = A3(
					$elm$virtual_dom$VirtualDom$keyedNode,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
			default:
				var ns = html.a;
				var elemType = html.b;
				var properties = html.c;
				var children = html.d;
				var combinedStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, styles, properties);
				var _v5 = A3(
					$elm$core$List$foldl,
					$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
					_Utils_Tuple2(_List_Nil, combinedStyles),
					children);
				var childNodes = _v5.a;
				var finalStyles = _v5.b;
				var vdomNode = A4(
					$elm$virtual_dom$VirtualDom$keyedNodeNS,
					ns,
					elemType,
					A2(
						$elm$core$List$map,
						$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(finalStyles),
						properties),
					$elm$core$List$reverse(childNodes));
				return _Utils_Tuple2(
					A2($elm$core$List$cons, vdomNode, nodes),
					finalStyles);
		}
	});
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$String$replace = F3(
	function (before, after, string) {
		return A2(
			$elm$core$String$join,
			after,
			A2($elm$core$String$split, before, string));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$styleToDeclaration = F3(
	function (template, classname, declaration) {
		return declaration + ('\n' + A3($elm$core$String$replace, $rtfeldman$elm_css$VirtualDom$Styled$classnameStandin, classname, template));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toDeclaration = function (dict) {
	return A3($elm$core$Dict$foldl, $rtfeldman$elm_css$VirtualDom$Styled$styleToDeclaration, '', dict);
};
var $rtfeldman$elm_css$VirtualDom$Styled$toScopedDeclaration = F2(
	function (scopingPrefix, dict) {
		return A3(
			$elm$core$Dict$foldl,
			F3(
				function (template, classname, declaration) {
					return declaration + ('\n' + A3($elm$core$String$replace, '.' + $rtfeldman$elm_css$VirtualDom$Styled$classnameStandin, '#' + (scopingPrefix + ('.' + classname)), template));
				}),
			'',
			dict);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toStyleNode = F2(
	function (maybeNonce, accumulatedStyles) {
		var cssText = function () {
			if (accumulatedStyles.$ === 'UnscopedStyles') {
				var allStyles = accumulatedStyles.a;
				return $rtfeldman$elm_css$VirtualDom$Styled$toDeclaration(allStyles);
			} else {
				var scope = accumulatedStyles.a.a;
				var rootStyles = accumulatedStyles.b;
				var descendantStyles = accumulatedStyles.c;
				return A2($rtfeldman$elm_css$VirtualDom$Styled$toScopedDeclaration, scope, rootStyles) + ('\n' + A2($rtfeldman$elm_css$VirtualDom$Styled$toScopedDeclaration, scope + ' ', descendantStyles));
			}
		}();
		return A3(
			$elm$virtual_dom$VirtualDom$node,
			'span',
			_List_fromArray(
				[
					A2($elm$virtual_dom$VirtualDom$attribute, 'style', 'display: none;'),
					A2($elm$virtual_dom$VirtualDom$attribute, 'class', 'elm-css-style-wrapper')
				]),
			_List_fromArray(
				[
					A3(
					$elm$virtual_dom$VirtualDom$node,
					'style',
					function () {
						if (maybeNonce.$ === 'Just') {
							var nonce = maybeNonce.a.a;
							return _List_fromArray(
								[
									A2($elm$virtual_dom$VirtualDom$attribute, 'nonce', nonce)
								]);
						} else {
							return _List_Nil;
						}
					}(),
					$elm$core$List$singleton(
						$elm$virtual_dom$VirtualDom$text(cssText)))
				]));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyle = F4(
	function (maybeNonce, elemType, properties, children) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _v0.a;
		var styles = _v0.b;
		var styleNode = A2(
			$rtfeldman$elm_css$VirtualDom$Styled$toStyleNode,
			maybeNonce,
			$rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles(styles));
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(styles),
			properties);
		return A3(
			$elm$virtual_dom$VirtualDom$node,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				styleNode,
				$elm$core$List$reverse(childNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$containsKey = F2(
	function (key, pairs) {
		containsKey:
		while (true) {
			if (!pairs.b) {
				return false;
			} else {
				var _v1 = pairs.a;
				var str = _v1.a;
				var rest = pairs.b;
				if (_Utils_eq(key, str)) {
					return true;
				} else {
					var $temp$key = key,
						$temp$pairs = rest;
					key = $temp$key;
					pairs = $temp$pairs;
					continue containsKey;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey = F2(
	function (_default, pairs) {
		getUnusedKey:
		while (true) {
			if (!pairs.b) {
				return _default;
			} else {
				var _v1 = pairs.a;
				var firstKey = _v1.a;
				var rest = pairs.b;
				var newKey = '_' + firstKey;
				if (A2($rtfeldman$elm_css$VirtualDom$Styled$containsKey, newKey, rest)) {
					var $temp$default = newKey,
						$temp$pairs = rest;
					_default = $temp$default;
					pairs = $temp$pairs;
					continue getUnusedKey;
				} else {
					return newKey;
				}
			}
		}
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode = F3(
	function (maybeNonce, accumulatedStyles, keyedChildNodes) {
		var styleNodeKey = A2($rtfeldman$elm_css$VirtualDom$Styled$getUnusedKey, '_', keyedChildNodes);
		var finalNode = A2($rtfeldman$elm_css$VirtualDom$Styled$toStyleNode, maybeNonce, accumulatedStyles);
		return _Utils_Tuple2(styleNodeKey, finalNode);
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed = F4(
	function (maybeNonce, elemType, properties, keyedChildren) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _v0.a;
		var styles = _v0.b;
		var keyedStyleNode = A3(
			$rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode,
			maybeNonce,
			$rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles(styles),
			keyedChildNodes);
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttribute(styles),
			properties);
		return A3(
			$elm$virtual_dom$VirtualDom$keyedNode,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				keyedStyleNode,
				$elm$core$List$reverse(keyedChildNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS = F5(
	function (maybeNonce, ns, elemType, properties, keyedChildren) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateKeyedStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			keyedChildren);
		var keyedChildNodes = _v0.a;
		var styles = _v0.b;
		var keyedStyleNode = A3(
			$rtfeldman$elm_css$VirtualDom$Styled$toKeyedStyleNode,
			maybeNonce,
			$rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles(styles),
			keyedChildNodes);
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(styles),
			properties);
		return A4(
			$elm$virtual_dom$VirtualDom$keyedNodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				keyedStyleNode,
				$elm$core$List$reverse(keyedChildNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$unstyleNS = F5(
	function (maybeNonce, ns, elemType, properties, children) {
		var initialStyles = A3($elm$core$List$foldl, $rtfeldman$elm_css$VirtualDom$Styled$accumulateStyles, $elm$core$Dict$empty, properties);
		var _v0 = A3(
			$elm$core$List$foldl,
			$rtfeldman$elm_css$VirtualDom$Styled$accumulateStyledHtml,
			_Utils_Tuple2(_List_Nil, initialStyles),
			children);
		var childNodes = _v0.a;
		var styles = _v0.b;
		var styleNode = A2(
			$rtfeldman$elm_css$VirtualDom$Styled$toStyleNode,
			maybeNonce,
			$rtfeldman$elm_css$VirtualDom$Styled$UnscopedStyles(styles));
		var unstyledProperties = A2(
			$elm$core$List$map,
			$rtfeldman$elm_css$VirtualDom$Styled$extractUnstyledAttributeNS(styles),
			properties);
		return A4(
			$elm$virtual_dom$VirtualDom$nodeNS,
			ns,
			elemType,
			unstyledProperties,
			A2(
				$elm$core$List$cons,
				styleNode,
				$elm$core$List$reverse(childNodes)));
	});
var $rtfeldman$elm_css$VirtualDom$Styled$toUnstyled = function (vdom) {
	switch (vdom.$) {
		case 'Unstyled':
			var plainNode = vdom.a;
			return plainNode;
		case 'Node':
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A4($rtfeldman$elm_css$VirtualDom$Styled$unstyle, $elm$core$Maybe$Nothing, elemType, properties, children);
		case 'NodeNS':
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A5($rtfeldman$elm_css$VirtualDom$Styled$unstyleNS, $elm$core$Maybe$Nothing, ns, elemType, properties, children);
		case 'KeyedNode':
			var elemType = vdom.a;
			var properties = vdom.b;
			var children = vdom.c;
			return A4($rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyed, $elm$core$Maybe$Nothing, elemType, properties, children);
		default:
			var ns = vdom.a;
			var elemType = vdom.b;
			var properties = vdom.c;
			var children = vdom.d;
			return A5($rtfeldman$elm_css$VirtualDom$Styled$unstyleKeyedNS, $elm$core$Maybe$Nothing, ns, elemType, properties, children);
	}
};
var $rtfeldman$elm_css$Html$Styled$toUnstyled = $rtfeldman$elm_css$VirtualDom$Styled$toUnstyled;
var $author$project$Main$view = function (model) {
	return {
		body: _List_fromArray(
			[
				$rtfeldman$elm_css$Html$Styled$toUnstyled(
				$author$project$Main$app(model))
			]),
		title: 'My elm app'
	};
};
var $author$project$Main$main = $elm$browser$Browser$application(
	{init: $author$project$Main$init, onUrlChange: $author$project$Main$ChangedUrl, onUrlRequest: $author$project$Main$ClickedLink, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main($elm$json$Json$Decode$value)(0)}});}(this));