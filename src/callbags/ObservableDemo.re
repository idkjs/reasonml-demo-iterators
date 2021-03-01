// Js.Console.clear()|>ignore;

/**
 * A contrived data source to use in our "observable"
 * NOTE: this will clearly never error
 */
open Task;

open Task.Operators;

let timeout = value =>
  Task(
    (_, res) => {
      let timer = Js.Global.setTimeout(() => res(value), value);
      Cancel(() => Js.Global.clearTimeout(timer));
    },
  );
let notTimeout = value =>
  Task(
    (_, res) => {
      res(value);
      NoCancel;
    },
  );
let p =
  ([1, 2, 3, 4, 5, 6, 7, 8, 9] |> List.map(timeout))
  @ (
    Array.make(10000, 1)
    |> Array.mapi((index, _) => index + 10)
    |> Array.map(notTimeout)
    |> Array.to_list
  )
  |> parallel
  >>| List.fold_left((a, b) => a + b, 0);

let makeTask = i =>
  switch (i) {
  | i when i >= 100000 => (i + 1)->Done |> pure
  | i when i < 0 => reject("i must be positive")
  | i => pure(Next(i + 1))
  };

let t =
  p
  >>= chainRec(makeTask)
  >>| (m => m + 10)
  >>= (m => pure(m + 100))
  >>|! (_ => 100)
  >>= reject
  >>=! (m => pure(m + 100))
  >>> (
    fun
    | Rejection(v) => Js.log(v)
    | Success(s) => Js.log(s)
  );
open ReactFrp.React;
// open ReactReact;
// open ReactReact.Utils;
module Timer = {
  let counter = ref(0);
  let (timeS, timeF) = S.create(counter^);
  let timeIncrement = () => {
    counter := counter^ + 1;
    timeF(counter^);
  };
  let timerId = Js.Global.setInterval(timeIncrement, 1000);
  let vdomS = _ =>
    S.map(
      ~eq=(_, _) => false,
      time => {
        let timeMessage = time == 1 ? "second" : "seconds";
        let message = {j|You've spent $time $timeMessage on this page!|j};
        message;
      },
      timeS,
    );
  //  componentFromSignal(vdomS, ());
  vdomS() |> Js.log;
};
let counter = ref(0);
let (timeS, timeF) = S.create(counter^);
let timeIncrement = () => {
  counter := counter^ + 1;
  timeF(counter^);
};
let timerId = Js.Global.setInterval(timeIncrement, 200);

// module DataSource = {
//   let make() {
//     let i = 0;
//     this._id = setInterval(() => timerId, 200);
//     let (timer, setTimer) = React.useState(() => 0);
//   React.useEffect0(() => {
//     let i = 0;
//     let id =
//       Js.Global.setInterval(() => setTimer(prevTimer => prevTimer + 1), 200);

//     Some(() => Js.Global.clearInterval(id));
//   });
//   }

//   emit(n) {
//     const limit = 10;
//     if (this.ondata) {
//       this.ondata(n);
//     }
//     if (n === limit) {
//       if (this.oncomplete) {
//         this.oncomplete();
//       }
//       this.destroy();
//     }
//   }

//   destroy() {
//     clearInterval(this._id);
//   }
// }

// // /**
// //  * our observable
// //  */
// // function myObservable(observer) {
// //     let datasource = new DataSource();
// //     datasource.ondata = (e) => observer.next(e);
// //     datasource.onerror = (err) => observer.error(err);
// //     datasource.oncomplete = () => observer.complete();
// //     return () => {
// //         datasource.destroy();
// //     };
// // }

// // /**
// //  * now let's use it
// //  */
// // const unsub = myObservable({
// //   next(x) { console.log(x); },
// //   error(err) { console.error(err); },
// //   complete() { console.log('done')}
// // });
