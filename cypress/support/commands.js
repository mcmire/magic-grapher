Cypress.Commands.add(
  "getPositions",
  { prevSubject: true, element: true },
  subject => {
    // TODO: This is ALL wrong... how the hell did this work before?

    const element = subject[0];
    // relative to the whole page
    const absoluteBbox = element.getBoundingClientRect();
    // relative to its parent
    const relativeBbox = element.getBBox();
    const center = {
      x: absoluteBbox.x - relativeBbox.x,
      y: absoluteBbox.y - relativeBbox.y
    };
    const text = element.textContent;

    const positions = [];
    for (let i = 0, len = text.length; i < len; i++) {
      // relative to itself
      let startPosition = element.getStartPositionOfChar(i);
      let endPosition = element.getEndPositionOfChar(i);
      positions[i] = {
        start: {
          relative: { x: startPosition.x, y: startPosition.y },
          absolute: {
            x: startPosition.x + center.x,
            y: startPosition.y + center.y
          }
        },
        end: {
          relative: { x: endPosition.x, y: endPosition.y },
          absolute: { x: endPosition.x + center.x, y: endPosition.y + center.y }
        }
      };
    }
    return positions;
  }
);
