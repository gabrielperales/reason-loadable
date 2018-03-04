'use strict';

var React = require("react");
var ReasonReact = require("reason-react/src/ReasonReact.js");
var String$ReasonmlCodeSplit = require("../Core/String.bs.js");

var component = ReasonReact.statelessComponent("Home");

function make() {
  var newrecord = component.slice();
  newrecord[/* render */9] = (function () {
      return React.createElement("h1", undefined, ReasonReact.element(/* None */0, /* None */0, String$ReasonmlCodeSplit.make("Home", /* array */[])));
    });
  return newrecord;
}

exports.component = component;
exports.make = make;
/* component Not a pure module */
