require=function(r,e,n){function t(n,o){function i(r){return t(i.resolve(r))}function f(e){return r[n][1][e]||e}if(!e[n]){if(!r[n]){var c="function"==typeof require&&require;if(!o&&c)return c(n,!0);if(u)return u(n,!0);var l=new Error("Cannot find module '"+n+"'");throw l.code="MODULE_NOT_FOUND",l}i.resolve=f;var s=e[n]=new t.Module(n);r[n][0].call(s.exports,i,s,s.exports)}return e[n].exports}function o(r){this.id=r,this.bundle=t,this.exports={}}var u="function"==typeof require&&require;t.isParcelRequire=!0,t.Module=o,t.modules=r,t.cache=e,t.parent=u;for(var i=0;i<n.length;i++)t(n[i]);return t}({69:[function(require,module,exports) {
"use strict";var e=require("react"),r=require("reason-react/src/ReasonReact.js"),t=require("../Core/String.bs.js"),n=r.statelessComponent("Home");function o(){var o=n.slice();return o[9]=function(){return e.createElement("h1",void 0,r.element(0,0,t.make("Home",[])))},o}exports.component=n,exports.make=o;
},{"react":35,"reason-react/src/ReasonReact.js":33,"../Core/String.bs.js":22}],31:[function(require,module,exports) {
"use strict";var e=require("./Home.bs.js"),r=[e.component,e.make];exports.importable=r;
},{"./Home.bs.js":69}]},{},[31])