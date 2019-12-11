chai.Assertion.addMethod("position", function(pos) {
  new chai.Assertion(this._obj).to.have.attr("x", pos.x.toString());
  new chai.Assertion(this._obj).to.have.attr("y", pos.y.toString());
});

chai.Assertion.addMethod("positionRange", function(pos1, pos2) {
  new chai.Assertion(this._obj).to.have.attr("x", pos1.x.toString());
  new chai.Assertion(this._obj).to.have.attr("y", pos1.y.toString());
  new chai.Assertion(this._obj).to.have.attr(
    "width",
    (pos1.x + pos2.x).toString()
  );
});
