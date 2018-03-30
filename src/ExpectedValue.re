[@bs.send]
external toLocaleStringWithEnAndPercent :
  (float, [@bs.as "en"] _, [@bs.as {json|{"style": "percent"}|json}] _) =>
  string =
  "toLocaleString";

let toPercent = n =>
  toLocaleStringWithEnAndPercent(Js.Float.isNaN(n) ? 0. : n);

let successColor = "#BEDB39";

let failureColor = "#FD7400";

module Title = {
  let component = ReasonReact.statelessComponent("Title");
  let make = (~name, _children) => {
    ...component,
    render: _self =>
      <div
        style=(
          ReactDOMRe.Style.make(
            ~fontSize="5em",
            ~fontWeight="bold",
            ~textTransform="uppercase",
            ~padding="0 8px",
            (),
          )
        )>
        (ReasonReact.stringToElement(name))
      </div>,
  };
};

module SimulationAttributeDetail = {
  let component = ReasonReact.statelessComponent("SimulationAttributeDetail");
  let make = children => {
    ...component,
    render: _self =>
      ReasonReact.createDomElement(
        "div",
        ~props={
          "style":
            ReactDOMRe.Style.make(
              ~borderBottom="1px solid rgba(255,255,255,0.6)",
              ~padding="16px 8px",
              (),
            ),
        },
        children,
      ),
  };
};

module SimulationDetails = {
  module SimulationTitle = {
    let component = ReasonReact.statelessComponent("SimulationTitle");
    let make = (~title, _children) => {
      ...component,
      render: _self =>
        <div
          style=(
            ReactDOMRe.Style.make(
              ~fontWeight="bold",
              ~fontSize="0.8em",
              ~color="rgba(255,255,255,0.6)",
              ~textTransform="uppercase",
              (),
            )
          )>
          (ReasonReact.stringToElement(title))
        </div>,
    };
  };
  module SimulationValue = {
    let component = ReasonReact.statelessComponent("SimulationValue");
    let make = (~value, _children) => {
      ...component,
      render: _self =>
        <div
          style=(
            ReactDOMRe.Style.make(
              ~color="#004358",
              ~textShadow=
                "-1px -1px 0 #FFF, 1px -1px 0 #FFF, -1px 1px 0 #FFF, 1px 1px 0 #FFF",
              ~fontSize="4em",
              (),
            )
          )>
          value
        </div>,
    };
  };
  let component = ReasonReact.statelessComponent("SimulationDetails");
  let make = (~probability, ~successes, ~failures, _children) => {
    ...component,
    render: _self => {
      let probabilityAsString = toPercent(probability);
      let expectedSuccess =
        Js.Math.round(probability *. float_of_int(successes + failures));
      let expectedFailure =
        Js.Math.round(
          (1. -. probability) *. float_of_int(successes + failures),
        );
      <div>
        <SimulationAttributeDetail>
          <SimulationTitle
            title={j|Success Probability - Expected $probabilityAsString|j}
          />
          <SimulationValue
            value=(
              ReasonReact.stringToElement(
                toPercent(
                  float_of_int(successes)
                  /. float_of_int(failures + successes),
                ),
              )
            )
          />
        </SimulationAttributeDetail>
        <SimulationAttributeDetail>
          <SimulationTitle
            title={j|Successful Events - Expected $expectedSuccess|j}
          />
          <SimulationValue
            value=
              <span
                style=(
                  ReactDOMRe.Style.make(
                    ~textShadow=
                      {j| -1px -1px 0 $successColor, 1px -1px 0 $successColor, -1px 1px 0 $successColor, 1px 1px 0 $successColor |j},
                    (),
                  )
                )>
                (ReasonReact.stringToElement(string_of_int(successes)))
              </span>
          />
        </SimulationAttributeDetail>
        <SimulationAttributeDetail>
          <SimulationTitle
            title={j|Failed Events - Expected $expectedFailure|j}
          />
          <SimulationValue
            value=
              <span
                style=(
                  ReactDOMRe.Style.make(
                    ~textShadow=
                      {j| -1px -1px 0 $failureColor, 1px -1px 0 $failureColor, -1px 1px 0 $failureColor, 1px 1px 0 $failureColor |j},
                    (),
                  )
                )>
                (ReasonReact.stringToElement(string_of_int(failures)))
              </span>
          />
        </SimulationAttributeDetail>
      </div>;
    },
  };
};

module SimulationEvent = {
  let component = ReasonReact.statelessComponent("SimulationEvent");
  let make = (~event, _children) => {
    ...component,
    render: _self =>
      <div
        style=(
          ReactDOMRe.Style.make(
            ~backgroundColor=event ? successColor : failureColor,
            ~width="16px",
            ~height="16px",
            ~margin="4px",
            ~display="inline-block",
            (),
          )
        )
      />,
  };
};

module SimulationEventView = {
  let component = ReasonReact.statelessComponent("SimulationEventView");
  let make = (~events, _children) => {
    ...component,
    render: _self =>
      <div>
        (
          List.mapi(
            (key, event) =>
              <SimulationEvent key=(string_of_int(key)) event />,
            events,
          )
          |> Array.of_list
          |> ReasonReact.arrayToElement
        )
      </div>,
  };
};

type state = {events: list(bool)};

type results = {
  successes: int,
  failures: int,
};

type action =
  | Tick;

let component = ReasonReact.reducerComponent("ExpectedValue");

let make = (~name, ~probability, _children) => {
  ...component,
  subscriptions: self => [
    Sub(
      () => Js.Global.setInterval(() => self.send(Tick), 500),
      Js.Global.clearInterval,
    ),
  ],
  initialState: () => {events: []},
  reducer: (action, state) =>
    switch (action) {
    | Tick =>
      ReasonReact.Update({
        events: List.append(state.events, [Js.Math.random() < probability]),
      })
    },
  render: self => {
    let simulation =
      List.fold_left(
        (memo, event) =>
          event ?
            {...memo, successes: memo.successes + 1} :
            {...memo, failures: memo.failures + 1},
        {successes: 0, failures: 0},
        self.state.events,
      );
    <ExpectedValueContainer
      header={<Title name />}
      column={
        <SimulationDetails
          probability
          successes=simulation.successes
          failures=simulation.failures
        />
      }
      body={<SimulationEventView events=self.state.events />}
    />;
  },
};