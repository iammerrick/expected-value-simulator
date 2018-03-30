let component = ReasonReact.statelessComponent("ExpectedValueContainer");

let make = (~header, ~column, ~body, _children) => {
  ...component,
  render: _self =>
    <div
      style=(
        ReactDOMRe.Style.make(
          ~width="100vw",
          ~height="100vh",
          ~padding="16px",
          ~display="grid",
          ~gridTemplate=
            {|
            "header header"
            "column body" 1fr / 1fr 4fr
          |},
          (),
        )
      )>
      <div
        style=(
          ReactDOMRe.Style.make(
            ~gridArea="header",
            ~padding="16px 0",
            ~borderBottom="1px solid rgba(255,255,255,0.6)",
            (),
          )
        )>
        header
      </div>
      <div
        style=(
          ReactDOMRe.Style.make(
            ~gridArea="column",
            ~borderRight="1px solid rgba(255,255,255,0.6)",
            (),
          )
        )>
        column
      </div>
      <div
        style=(ReactDOMRe.Style.make(~gridArea="body", ~padding="16px", ()))>
        body
      </div>
    </div>,
};