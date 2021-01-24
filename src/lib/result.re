/* https://github.com/janestreet/result */

type result('a, 'b) =
  | Ok('a)
  | Error('b);
