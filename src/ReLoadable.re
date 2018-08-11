open DynamicImport;

type nothing;

module type Configuration = {module type t;};

module WithRender = (Config: Configuration) => {
  type childless = array(nothing);
  type renderProp = (module Config.t) => ReasonReact.reactElement;
  type state =
    | Initial
    | Loading
    | Failed(string)
    | Loaded((module Config.t));
  let component = ReasonReact.reducerComponent("Loadable.WithRender");
  /** Our component accept different props :
    *
    * fetch
    * onInitial
    * onFail
    * onLoading
    * delay
    * render
    *
    * Children is unused and enforced to be nothing.
    *
    * As you can see, some of props have default value.
  **/
  let make =
      (
        ~fetch,
        ~onInitial=() => ReasonReact.null,
        ~onFail=_error => ReasonReact.null,
        ~onLoading=() => ReasonReact.null,
        ~delay=2000,
        ~render: renderProp,
        _children: childless,
      ) => {
    ...component,
    initialState: () => Initial,
    reducer: (action, state) =>
      switch (action) {
      | Initial => ReasonReact.Update(Initial)
      | Loading =>
        switch (state) {
        | Initial
        | Loading => ReasonReact.Update(Loading)
        | _ => ReasonReact.NoUpdate
        }
      | Failed(err) => ReasonReact.Update(Failed(err))
      | Loaded(component) => ReasonReact.Update(Loaded(component))
      },
    didMount: self => {
      fetch()
      /* Resolve module (unwrap). */
      |> resolve
      /* Resolve new state, user should refine module himself with correct type on render. */
      <$> (data => self.send(Loaded(data)))
      /* Forward error if some trouble happen. */
      <$!> (err => self.send(Failed(err |> Js.String.make)))
      |> ignore;
      
      let timeoutId = Js.Global.setTimeout(() => self.send(Loading), delay);
      self.onUnmount(() => Js.Global.clearTimeout(timeoutId));
    },
    render: ({state}) =>
      switch (state) {
      | Initial => onInitial()
      | Loading => onLoading()
      | Failed(err) => onFail(err)
      | Loaded(component) => render(component)
      },
  };
};

module WithChildren = (Config: Configuration) => {
  type renderChild = (module Config.t) => ReasonReact.reactElement;
  type state =
    | Initial
    | Loading
    | Failed(string)
    | Loaded((module Config.t));
  let component = ReasonReact.reducerComponent("Loadable.WithChildren");
  /** Our component accept different props :
    *
    * fetch
    * onInitial
    * onFail
    * onLoading
    * delay
    * render
    *
    * Children is enforced to be a render function.
    *
    * As you can see, some of props have default value.
  **/
  let make =
      (
        ~fetch,
        ~onInitial=() => ReasonReact.null,
        ~onFail=_error => ReasonReact.null,
        ~onLoading=() => ReasonReact.null,
        ~delay=200,
        children: renderChild,
      ) => {
    ...component,
    initialState: () => Initial,
    reducer: (action, state) =>
      switch (action) {
      | Initial => ReasonReact.Update(Initial)
      | Loading =>
        switch (state) {
        | Initial
        | Loading => ReasonReact.Update(Loading)
        | _ => ReasonReact.NoUpdate
        }
      | Failed(err) => ReasonReact.Update(Failed(err))
      | Loaded(component) => ReasonReact.Update(Loaded(component))
      },
    didMount: self => {
      fetch()
      /* Resolve module (unwrap). */
      |> resolve
      /* Resolve new state, user should refine module himself with correct type on render. */
      <$> (data => self.send(Loaded(data)))
      /* Forward error if some trouble happen. */
      <$!> (err => self.send(Failed(err |> Js.String.make)))
      |> ignore;
      
      let timeoutId = Js.Global.setTimeout(() => self.send(Loading), delay);
      self.onUnmount(() => Js.Global.clearTimeout(timeoutId));
    },
    render: ({state}) =>
      switch (state) {
      | Initial => onInitial()
      | Loading => onLoading()
      | Failed(err) => onFail(err)
      | Loaded(component) => children(component)
      },
  };
};
