// type map =
//   {('a => 'a, (int, (int, 'a) => 'b) => unit, int, (int, 'a) => 'b) => unit};

// let map:map<'a,'b> = (f, source, start, sink) => {
//   start !== 0 ? () : source(start, (t, d) => {sink(t, t == 1 ? f(d) : d)});
// };
let map:('a => 'a, (int, (int, 'a) => 'b) => unit, int, (int, 'a) => 'b) => unit = (f, source, start, sink) => {
  start !== 0 ? () : source(start, (t, d) => {sink(t, t == 1 ? f(d) : d)});
};
