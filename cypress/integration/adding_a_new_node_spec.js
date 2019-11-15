describe("Adding a new node", () => {
  const expectedY = "-8"; // 16 / 2

  function getBody() {
    return cy.get("body");
  }

  function getNodeEditorTextBox() {
    return cy.get("svg text");
  }

  function getNodeEditorCursor() {
    return cy.get("svg [data-testid='cursor']");
  }

  beforeEach(() => {
    cy.visit("/");
    getBody().type("n");
    cy.get("svg")
      .trigger("mousemove", { clientX: 100, clientY: 100 })
      .click();
    cy.get("[data-testid='cursor']").should("be.visible");
  });

  specify("Typing multiple words and having the cursor follow along", () => {
    getBody().type("  this is a tomato  ");
    getNodeEditorTextBox()
      .should($el => $el[0].textContent === "  this is a tomato  ")
      .getPositions()
      .then(positions => {
        getNodeEditorCursor()
          .should(
            "have.attr",
            "x",
            positions[positions.length - 1].end.x.toString()
          )
          .should("have.attr", "y", expectedY);
      });
  });

  specify(
    "Using the arrow keys to navigate to some other part of the line and having the cursor follow along",
    () => {
      getBody().type("  tomato  ");
      getNodeEditorTextBox()
        .should($el => $el[0].textContent === "  tomato  ")
        .getPositions()
        .then(positions => {
          getBody().type("{leftarrow}{leftarrow}");
          getNodeEditorCursor()
            .should(
              "have.attr",
              "x",
              positions[positions.length - 3].end.x.toString()
            )
            .should("have.attr", "y", expectedY);

          getBody().type("{rightarrow}{rightarrow}");
          getNodeEditorCursor()
            .should(
              "have.attr",
              "x",
              positions[positions.length - 1].end.x.toString()
            )
            .should("have.attr", "y", expectedY);
        });
    }
  );

  specify("Pressing the left arrow key at the beginning of the line", () => {
    getBody().type("  x  ");
    getNodeEditorTextBox()
      .should($el => $el[0].textContent === "  x  ")
      .getPositions()
      .then(positions => {
        getBody().type("{meta}{leftarrow}");
        getBody().type("{leftarrow}");
        getNodeEditorCursor()
          .should("have.attr", "x", positions[0].start.x.toString())
          .should("have.attr", "y", expectedY);
      });
  });

  specify("Pressing the right arrow key at the end of the line", () => {
    getBody().type("  x  ");
    getNodeEditorTextBox()
      .should($el => $el[0].textContent === "  x  ")
      .getPositions()
      .then(positions => {
        getBody().type("{meta}{rightarrow}");
        getBody().type("{rightarrow}");
        getNodeEditorCursor()
          .should(
            "have.attr",
            "x",
            positions[positions.length - 1].end.x.toString()
          )
          .should("have.attr", "y", expectedY);
      });
  });

  specify("Using Alt + the arrow keys to skip words", () => {
    getBody().type("  this is a tomato  ");

    getNodeEditorTextBox()
      .should($el => $el[0].innerHTML === "  this is a tomato  ")
      .getPositions()
      .then(positions => {
        getBody().type("{alt}{leftarrow}");
        getNodeEditorCursor()
          .should(
            "have.attr",
            "x",
            positions[positions.length - 9].end.x.toString()
          )
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{leftarrow}");
        getNodeEditorCursor()
          .should(
            "have.attr",
            "x",
            positions[positions.length - 11].end.x.toString()
          )
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{leftarrow}");
        getNodeEditorCursor()
          .should(
            "have.attr",
            "x",
            positions[positions.length - 14].end.x.toString()
          )
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{leftarrow}");
        getNodeEditorCursor()
          .should(
            "have.attr",
            "x",
            positions[positions.length - 19].end.x.toString()
          )
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{leftarrow}");
        getNodeEditorCursor()
          .should("have.attr", "x", positions[0].start.x.toString())
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{rightarrow}");
        getNodeEditorCursor()
          .should("have.attr", "x", positions[5].end.x.toString())
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{rightarrow}");
        getNodeEditorCursor()
          .should("have.attr", "x", positions[8].end.x.toString())
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{rightarrow}");
        getNodeEditorCursor()
          .should("have.attr", "x", positions[10].end.x.toString())
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{rightarrow}");
        getNodeEditorCursor()
          .should("have.attr", "x", positions[17].end.x.toString())
          .should("have.attr", "y", expectedY);

        getBody().type("{alt}{rightarrow}");
        getNodeEditorCursor()
          .should(
            "have.attr",
            "x",
            positions[positions.length - 1].end.x.toString()
          )
          .should("have.attr", "y", expectedY);
      });
  });

  specify(
    "Pressing Meta + the arrow keys to skip to the beginning and end of the line",
    () => {
      getBody().type("  x  ");

      getNodeEditorTextBox()
        .should($el => $el[0].textContent === "  x  ")
        .getPositions()
        .then(positions => {
          getBody().type("{meta}{leftarrow}");
          getNodeEditorCursor()
            .should("have.attr", "x", positions[0].start.x.toString())
            .should("have.attr", "y", expectedY);

          getBody().type("{meta}{rightarrow}");
          getNodeEditorCursor()
            .should(
              "have.attr",
              "x",
              positions[positions.length - 1].end.x.toString()
            )
            .should("have.attr", "y", expectedY);
        });
    }
  );

  specify("Clicking in the middle of the text to move the cursor", () => {
    getBody().type("tomato");

    getNodeEditorTextBox()
      .getPositions()
      .then(positions => {
        const x1 = positions[2].end.x;
        const x2 = positions[3].end.x;
        const x = x1 + (x2 - x1) * 0.3;

        getNodeEditorTextBox().click(x, expectedY);

        getNodeEditorCursor().should(
          "have.position",
          positions[2].start.x,
          expectedY
        );
      });
  });
});
