chai.Assertion.addMethod("position", function(x, y) {
  const $element = this._obj;
  new chai.Assertion($element).to.have.attr("x", x.toString());
  new chai.Assertion($element).to.have.attr("y", y.toString());
});
