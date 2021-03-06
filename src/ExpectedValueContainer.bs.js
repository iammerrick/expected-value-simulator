// Generated by BUCKLESCRIPT VERSION 2.2.3, PLEASE EDIT WITH CARE
'use strict';

var React = require("react");
var ReasonReact = require("reason-react/src/ReasonReact.js");

var component = ReasonReact.statelessComponent("ExpectedValueContainer");

function make(header, column, body, _) {
  var newrecord = component.slice();
  newrecord[/* render */9] = (function () {
      return React.createElement("div", {
                  style: {
                    display: "grid",
                    height: "100vh",
                    padding: "16px",
                    width: "100vw",
                    gridTemplate: "\n            \"header header\"\n            \"column body\" 1fr / 1fr 4fr\n          "
                  }
                }, React.createElement("div", {
                      style: {
                        borderBottom: "1px solid rgba(255,255,255,0.6)",
                        padding: "16px 0",
                        gridArea: "header"
                      }
                    }, header), React.createElement("div", {
                      style: {
                        borderRight: "1px solid rgba(255,255,255,0.6)",
                        gridArea: "column"
                      }
                    }, column), React.createElement("div", {
                      style: {
                        padding: "16px",
                        gridArea: "body"
                      }
                    }, body));
    });
  return newrecord;
}

exports.component = component;
exports.make = make;
/* component Not a pure module */
