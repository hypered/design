webpackHotUpdate("static/development/pages/index.js",{

/***/ "./components/home/NavSection.js":
/*!***************************************!*\
  !*** ./components/home/NavSection.js ***!
  \***************************************/
/*! exports provided: NavSection */
/***/ (function(module, __webpack_exports__, __webpack_require__) {

"use strict";
__webpack_require__.r(__webpack_exports__);
/* harmony export (binding) */ __webpack_require__.d(__webpack_exports__, "NavSection", function() { return NavSection; });
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0__ = __webpack_require__(/*! react */ "./node_modules/react/index.js");
/* harmony import */ var react__WEBPACK_IMPORTED_MODULE_0___default = /*#__PURE__*/__webpack_require__.n(react__WEBPACK_IMPORTED_MODULE_0__);
/* harmony import */ var next_router__WEBPACK_IMPORTED_MODULE_1__ = __webpack_require__(/*! next/router */ "./node_modules/next/dist/client/router.js");
/* harmony import */ var next_router__WEBPACK_IMPORTED_MODULE_1___default = /*#__PURE__*/__webpack_require__.n(next_router__WEBPACK_IMPORTED_MODULE_1__);
/* harmony import */ var next_link__WEBPACK_IMPORTED_MODULE_2__ = __webpack_require__(/*! next/link */ "./node_modules/next/link.js");
/* harmony import */ var next_link__WEBPACK_IMPORTED_MODULE_2___default = /*#__PURE__*/__webpack_require__.n(next_link__WEBPACK_IMPORTED_MODULE_2__);
/* harmony import */ var _components__WEBPACK_IMPORTED_MODULE_3__ = __webpack_require__(/*! ../../components */ "./components/index.js");
var _jsxFileName = "/Users/andyngo/Documents/Source Codes/noteed/design-system/components/home/NavSection.js";
var __jsx = react__WEBPACK_IMPORTED_MODULE_0___default.a.createElement;




var NavItem = Object(next_router__WEBPACK_IMPORTED_MODULE_1__["withRouter"])(function (_ref) {
  var children = _ref.children,
      href = _ref.href,
      router = _ref.router,
      as = _ref.as;
  var pathname = href.pathname,
      query = href.query;
  var active = router.pathname === (pathname || href);
  return __jsx(next_link__WEBPACK_IMPORTED_MODULE_2___default.a, {
    href: href,
    passHref: true,
    as: "" + href,
    __source: {
      fileName: _jsxFileName,
      lineNumber: 13
    },
    __self: this
  }, __jsx(_components__WEBPACK_IMPORTED_MODULE_3__["NavLink"], {
    active: active ? 1 : null,
    __source: {
      fileName: _jsxFileName,
      lineNumber: 14
    },
    __self: this
  }, children));
});

var NavSection = function NavSection(props) {
  return __jsx(_components__WEBPACK_IMPORTED_MODULE_3__["Nav"], {
    __source: {
      fileName: _jsxFileName,
      lineNumber: 20
    },
    __self: this
  }, __jsx("div", {
    __source: {
      fileName: _jsxFileName,
      lineNumber: 21
    },
    __self: this
  }, __jsx(NavItem, {
    href: "/",
    __source: {
      fileName: _jsxFileName,
      lineNumber: 22
    },
    __self: this
  }, "Home"), __jsx(NavItem, {
    href: "/components",
    __source: {
      fileName: _jsxFileName,
      lineNumber: 23
    },
    __self: this
  }, "Components")));
};



/***/ })

})
//# sourceMappingURL=index.js.868c54f8685aecd62071.hot-update.js.map