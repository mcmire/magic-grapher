Cypress.Commands.add(
  "getPositions",
  { prevSubject: true, element: true },
  subject => {
    const el = subject[0];
    const text = el.textContent;

    const positions = [];
    for (let i = 0, len = text.length; i < len; i++) {
      let startPosition = el.getStartPositionOfChar(i);
      let endPosition = el.getEndPositionOfChar(i);
      positions[i] = {
        start: { x: startPosition.x, y: startPosition.y },
        end: { x: endPosition.x, y: endPosition.y }
      };
    }
    return positions;
  }
);
