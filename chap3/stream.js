/*********************************************
 * Reinvention of Stream in SICP
 ********************************************/

/************  Helpers  ************/

function dividable(x, y) {
  return (x % y === 0);
}

function flip(f) {
  return function(x, y) {
    return f(y, x);
  };
}

function invert(f) {
  return function() {
    return !f.apply(null, arguments);
  };
}

function add(x, y) {
  return x+y;
}

var dividable_flip =flip(dividable),
    dividable_by = function(x) { return dividable_flip.bind(null, x); },
    dividable_3 = dividable_by(3);

//console.log(dividable_3(3), dividable_3(4));

/************  Sequence  ************/

function cons(val, next) {
  return {
    val: val,
    next: next
  };
}

function car(pair) {
  return pair.val;
}
function cdr(pair) {
  return pair.next;
}
function empty_pair() {
  return {};
}
function is_empty_pair(pair) {
  return !pair || !pair.hasOwnProperty('val');
}

function filter(pred, pair) {
  var val = car(pair);
  if (is_empty_pair(pair)) { return empty_pair(); }

  return pred(val) ? cons(val, filter(pred, cdr(pair))) : filter(pred, cdr(pair));
}

function enumerate_interval(low, high) {
  return low > high ? empty_pair() : cons(low, enumerate_interval(low+1, high));
}

var second = car(cdr(filter(dividable_3, enumerate_interval(10, 20)))); 
console.assert(second===15);
console.log(second+" found in normal list");

/************  Stream  ************/

function cons_stream(val, delay) {
  return {
    val: val, 
    promise: delay
  };
}

function stream_car(stream) {
  return stream.val;
}

function stream_cdr(stream) {
  return force(stream.promise);
}

function empty_stream() {
  return {};
}

function is_null_stream(stream) {
  return !stream || !stream.hasOwnProperty('val');
}

function force(promise) {
  return promise && promise();
}

function stream_enumerate_interval(low, high) {
  return low > high ? empty_stream() : cons_stream(low, function(){
    // `(delay <exp>)` in Scheme.
    // JavaScript does not have a primitive corresponding to it.
    return stream_enumerate_interval(low+1, high);
  });
}

function stream_filter(pred, stream) {
  if (is_null_stream(stream)) {
    return empty_stream();
  }

  if (pred(stream_car(stream))) {
    return cons_stream(stream_car(stream), function() {
      return stream_filter(pred, stream_cdr(stream));
    });
  }
  return stream_filter(pred, stream_cdr(stream));
}

second = stream_car(
  stream_cdr(
    stream_filter(dividable_3,
	      stream_enumerate_interval(10, 20))));

console.assert(second===15);
console.log(second+" found in stream (delayed list)");

// fibonacci

function stream_ref(stream, n) {
  return n == 0 ? stream_car(stream) : stream_ref(stream_cdr(stream), n-1);
}

function fibgen(a, b) {
  return cons_stream(a, function() {
    return fibgen(b, a + b);
  });
}

console.assert(stream_ref(fibgen(0, 1), 6)===8);

// sieve

// number -> stream
function integer_starting_from(n) {
  return cons_stream(n, function() {
    return integer_starting_from(n+1);
  });
}

// stream -> stream
function sieve(stream) {
  var val = stream_car(stream);
  return cons_stream(val, function() {
    return sieve(stream_filter(invert(dividable_by(val)), stream_cdr(stream)));
  });
}

var primes = sieve(integer_starting_from(2));
console.assert(stream_ref(primes, 10)===31);

// recursive fibonacci stream

function stream_map() {
  var args = Array.prototype.slice.call(arguments),
      proc = args.shift();
  
  if (is_null_stream(args[0])) { empty_stream(); }

  return cons_stream(proc.apply(null, args.map(stream_car)), function() {
    return stream_map.apply(null, [proc].concat(args.map(stream_cdr)));
  });
}

var sum_stream = stream_map(function() {
  var args = Array.prototype.slice.call(arguments);
  return args.reduce(add, 0);
}, integer_starting_from(1), integer_starting_from(2), integer_starting_from(3));

console.assert(stream_ref(sum_stream, 2)===12);

function add_stream(s1, s2) {
  return stream_map(add, s1, s2);
}

var ones = cons_stream(1, function() {
  return ones;
});
console.assert(stream_ref(ones, 100)===1);

var integers = cons_stream(1, function() {
  return add_stream(ones, integers);
});
console.assert(stream_ref(integers, 10)===11);

var fibs = cons_stream(0, function() {
  return cons_stream(1, function() {
    return add_stream(stream_cdr(fibs), fibs);
  });
});

console.assert(stream_ref(fibs, 6)===8);


