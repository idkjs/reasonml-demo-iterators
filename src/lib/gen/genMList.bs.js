// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Curry = require("bs-platform/lib/js/curry.js");
var Caml_array = require("bs-platform/lib/js/caml_array.js");
var Caml_option = require("bs-platform/lib/js/caml_option.js");
var CamlinternalOO = require("bs-platform/lib/js/camlinternalOO.js");

function _make(max_chunk_size, gen) {
  return {
          start: {
            contents: {
              TAG: /* Suspend */2,
              _0: gen
            }
          },
          chunk_size: 8,
          max_chunk_size: max_chunk_size
        };
}

function _incr_chunk_size(mlist) {
  if (mlist.chunk_size < mlist.max_chunk_size) {
    mlist.chunk_size = (mlist.chunk_size << 1);
    return ;
  }
  
}

function _read_chunk(mlist, gen) {
  var x = Curry._1(gen, undefined);
  if (x === undefined) {
    return /* Nil */0;
  }
  var x$1 = Caml_option.valFromOption(x);
  if (mlist.max_chunk_size === 1) {
    var tail = {
      contents: {
        TAG: /* Suspend */2,
        _0: gen
      }
    };
    return {
            TAG: /* Cons1 */1,
            _0: x$1,
            _1: tail
          };
  }
  var r = {
    contents: 1
  };
  var a = Caml_array.caml_make_vect(mlist.chunk_size, x$1);
  var tail$1 = {
    contents: {
      TAG: /* Suspend */2,
      _0: gen
    }
  };
  var stop = false;
  var node = {
    TAG: /* Cons */0,
    _0: a,
    _1: r,
    _2: tail$1
  };
  while(!stop && r.contents < mlist.chunk_size) {
    var x$2 = Curry._1(gen, undefined);
    if (x$2 !== undefined) {
      Caml_array.set(a, r.contents, Caml_option.valFromOption(x$2));
      r.contents = r.contents + 1 | 0;
    } else {
      tail$1.contents = /* Nil */0;
      stop = true;
    }
  };
  _incr_chunk_size(mlist);
  return node;
}

function of_gen(gen) {
  var mlist = _make(4096, gen);
  var _fill = function (_prev) {
    while(true) {
      var prev = _prev;
      var node = _read_chunk(mlist, gen);
      if (typeof node === "number") {
        prev.contents = /* Nil */0;
        return ;
      }
      switch (node.TAG | 0) {
        case /* Cons */0 :
            prev.contents = node;
            _prev = node._2;
            continue ;
        case /* Cons1 */1 :
            prev.contents = node;
            _prev = node._1;
            continue ;
        case /* Suspend */2 :
            throw {
                  RE_EXN_ID: "Assert_failure",
                  _1: [
                    "genMList.re",
                    79,
                    20
                  ],
                  Error: new Error()
                };
        
      }
    };
  };
  _fill(mlist.start);
  return mlist;
}

function of_gen_lazy(max_chunk_sizeOpt, cachingOpt, gen) {
  var max_chunk_size = max_chunk_sizeOpt !== undefined ? max_chunk_sizeOpt : 2048;
  var caching = cachingOpt !== undefined ? cachingOpt : true;
  if (!caching) {
    return {
            start: {
              contents: {
                TAG: /* Suspend */2,
                _0: gen
              }
            },
            chunk_size: 1,
            max_chunk_size: 1
          };
  }
  var max_chunk_size$1 = max_chunk_size > 2 ? max_chunk_size : 2;
  return _make(max_chunk_size$1, gen);
}

function to_gen(l) {
  var cur = {
    contents: l.start
  };
  var i = {
    contents: 0
  };
  var next = function (_param) {
    while(true) {
      var gen = cur.contents.contents;
      if (typeof gen === "number") {
        return ;
      }
      switch (gen.TAG | 0) {
        case /* Cons */0 :
            if (i.contents === gen._1.contents) {
              cur.contents = gen._2;
              i.contents = 0;
              _param = undefined;
              continue ;
            }
            var y = Caml_array.get(gen._0, i.contents);
            i.contents = i.contents + 1 | 0;
            return Caml_option.some(y);
        case /* Cons1 */1 :
            cur.contents = gen._1;
            return Caml_option.some(gen._0);
        case /* Suspend */2 :
            var node = _read_chunk(l, gen._0);
            cur.contents.contents = node;
            _param = undefined;
            continue ;
        
      }
    };
  };
  return next;
}

var class_tables = /* Cons */{
  key: undefined,
  data: undefined,
  next: undefined
};

function to_clonable(l) {
  var make = function (node, i) {
    var cur = {
      contents: node
    };
    var i$1 = {
      contents: i
    };
    var next = function (_param) {
      while(true) {
        var gen = cur.contents.contents;
        if (typeof gen === "number") {
          return ;
        }
        switch (gen.TAG | 0) {
          case /* Cons */0 :
              if (i$1.contents === gen._1.contents) {
                cur.contents = gen._2;
                i$1.contents = 0;
                _param = undefined;
                continue ;
              }
              var y = Caml_array.get(gen._0, i$1.contents);
              i$1.contents = i$1.contents + 1 | 0;
              return Caml_option.some(y);
          case /* Cons1 */1 :
              cur.contents = gen._1;
              return Caml_option.some(gen._0);
          case /* Suspend */2 :
              var node = _read_chunk(l, gen._0);
              cur.contents.contents = node;
              _param = undefined;
              continue ;
          
        }
      };
    };
    if (!class_tables.key) {
      var $$class = CamlinternalOO.create_table([
            "clone",
            "gen"
          ]);
      var env = CamlinternalOO.new_variable($$class, "");
      var ids = CamlinternalOO.get_method_labels($$class, [
            "gen",
            "clone"
          ]);
      var gen = ids[0];
      var clone = ids[1];
      CamlinternalOO.set_methods($$class, [
            gen,
            (function (self$1) {
                return self$1[env][3];
              }),
            clone,
            (function (self$1) {
                var env$1 = self$1[env];
                return Curry._2(env$1[0], env$1[1].contents, env$1[2].contents);
              })
          ]);
      var env_init = function (env$1) {
        var self = CamlinternalOO.create_object_opt(undefined, $$class);
        self[env] = env$1;
        return self;
      };
      CamlinternalOO.init_class($$class);
      class_tables.key = env_init;
    }
    return Curry._1(class_tables.key, [
                make,
                cur,
                i$1,
                next
              ]);
  };
  return make(l.start, 0);
}

exports.of_gen = of_gen;
exports.of_gen_lazy = of_gen_lazy;
exports.to_gen = to_gen;
exports.to_clonable = to_clonable;
/* No side effect */
