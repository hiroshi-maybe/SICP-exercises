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
