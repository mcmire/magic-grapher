import "./index.css";

import { Elm } from "./Main.elm";

// TODO: Convert to TypeScript

class GraphNodeEditor {
  constructor(root, graphNodeId, element) {
    this.root = root;
    this.graphNodeId = graphNodeId;
    this.element = element;
  }

  fireInitEvent() {
    console.log("[JS] dispatching init event");
    const event = new CustomEvent("init");
    this.element.dispatchEvent(event);
  }

  fireKeyEvent(originalEvent) {
    console.log("[JS] dispatching key event");
    const event = new CustomEvent("key", { detail: originalEvent });
    this.element.dispatchEvent(event);
  }

  updateCursorPositionFromMouse(mousePosition) {
    const cursorIndex = this._findCursorIndexAt(mousePosition);
    this.calculateMetrics({
      text: this.element.textContent,
      cursor: cursorIndex
    });
  }

  updateCursorPositionFromIndex(cursorIndex) {
    this.calculateMetrics({
      text: this.element.textContent,
      cursor: cursorIndex
    });
  }

  updateSelectionFromMouse(fromCursorIndex, toMousePosition) {
    const toCursorIndex = this._findCursorIndexAt(toMousePosition);
    this.calculateMetrics({
      text: this.element.textContent,
      selection: { start: fromCursorIndex, end: toCursorIndex }
    });
  }

  // request
  // = { text : String, cursor : Int }
  // | { text : String, selection : { start : Int, end : Int } }
  calculateMetrics({ text, cursor, selection }) {
    console.log("[JS] calculating metrics!", text, cursor, selection);

    // Update the text node inside of the <text> element
    // Don't use innerHTML or else it replaces the text node and Elm will get
    //   very confused!
    this.element.childNodes[0].nodeValue = this._normalizeTextForSvgElement(
      text
    );
    const bbox = this.element.getBBox();

    let userLocation;

    // TODO: Don't we already have the position? Why do we have to recalculate
    // this?
    if (selection != null) {
      const normalizedStartIndex = this._normalizeCursorIndex({
        cursorIndex: selection.start,
        text: text
      });

      const startPosition = this._determineCursorPosition({
        cursorIndex: normalizedStartIndex,
        text: text,
        bbox: bbox
      });

      const normalizedEndIndex = this._normalizeCursorIndex({
        cursorIndex: selection.end,
        text: text
      });

      const endPosition = this._determineCursorPosition({
        cursorIndex: normalizedEndIndex,
        text: text,
        bbox: bbox
      });

      userLocation = {
        start: { index: normalizedStartIndex, position: startPosition },
        end: { index: normalizedEndIndex, position: endPosition }
      };
    } else if (cursor != null) {
      const normalizedIndex = this._normalizeCursorIndex({
        cursorIndex: cursor,
        text: text
      });

      const position = this._determineCursorPosition({
        cursorIndex: normalizedIndex,
        text: text,
        bbox: bbox
      });

      userLocation = {
        index: normalizedIndex,
        position: position
      };
    } else {
      throw new Error(
        "Invalid request. Must match { text, cursor } or { text, selection }."
      );
    }

    const detail = {
      graphNodeId: parseInt(this.graphNodeId, 10),
      width: bbox.width,
      height: bbox.height,
      userLocation: userLocation,
      text: text
    };

    console.log("[JS] dispatching metricsRecalculated", detail);
    // TODO: Convert this to a port?
    const event = new CustomEvent("metricsRecalculated", { detail });
    this.element.dispatchEvent(event);
  }

  _findCursorIndexAt(mousePosition) {
    const absoluteBbox = this.element.getBoundingClientRect();
    const relativeBbox = this.element.getBBox();
    const center = {
      x: absoluteBbox.x - relativeBbox.x,
      y: absoluteBbox.y - relativeBbox.y
    };

    const normalizedMousePosition = this._normalizePosition(
      mousePosition,
      absoluteBbox
    );
    const relativizedNormalizedMousePosition = this._relativizePosition(
      normalizedMousePosition,
      center
    );

    console.log(
      "normalizing mouse position to",
      relativizedNormalizedMousePosition.x,
      relativizedNormalizedMousePosition.y
    );

    const point = this.root.createSVGPoint();
    point.x = relativizedNormalizedMousePosition.x;
    point.y = relativizedNormalizedMousePosition.y;

    return this.element.getCharNumAtPosition(point);
  }

  _normalizePosition(position, bbox) {
    const normalizedPosition = {};

    if (position.x > bbox.right) {
      normalizedPosition.x = bbox.right;
    } else if (position.x < bbox.left) {
      normalizedPosition.x = bbox.left;
    } else {
      normalizedPosition.x = position.x;
    }

    if (position.y < bbox.top) {
      normalizedPosition.y = bbox.top;
    } else if (position.y > bbox.bottom) {
      normalizedPosition.y = bbox.bottom;
    } else {
      normalizedPosition.y = position.y;
    }

    return normalizedPosition;
  }

  _relativizePosition(position, bbox) {
    return {
      x: position.x - bbox.x,
      y: position.y - bbox.y
    };
  }

  _normalizeTextForSvgElement(text) {
    // HTML/SVG will strip leading and trailing spaces, which affects how we
    // figure out the positions of each character in the node
    return text.replace(/^[ ]/, " ").replace(/[ ]$/, " ");
  }

  _normalizeCursorIndex({ cursorIndex, text }) {
    if (cursorIndex < 0) {
      return 0;
    } else if (cursorIndex > text.length) {
      return text.length;
    } else {
      return cursorIndex;
    }
  }

  _determineCursorPosition({ cursorIndex, text, bbox }) {
    // cursorIndex can either refer to the start of a character or the end of a
    // character. Usually it refers to the position after a character, but if
    // it's 0, then it refers to the position before the character.
    //
    // See this for more on getStartPositionOfChar and getEndPositionOfChar:
    //
    //   https://www.w3.org/TR/SVG2/text.html#TextSelectionImplementationNotes

    let cursorPosition;

    if (text === "") {
      cursorPosition = bbox.x;
    } else if (cursorIndex === text.length) {
      cursorPosition = this.element.getEndPositionOfChar(cursorIndex - 1).x;
    } else {
      cursorPosition = this.element.getStartPositionOfChar(cursorIndex).x;
    }

    if (isNaN(cursorPosition)) {
      throw new Error("cursorPosition is not a number!");
    }

    return cursorPosition;
  }
}

function findGraphNodeEditorBy(id) {
  const graphNodeEditor = graphNodeEditors[id];

  if (graphNodeEditor == null) {
    throw new Error(`Can't find graph node with node id: ${id}`);
  }

  return graphNodeEditor;
}

function isGraphNodeElement(node) {
  return (
    node.dataset != null &&
    node.dataset.id === "graph-node" &&
    node.dataset.graphNodeId != null
  );
}

function isInterestingKeyEvent(event) {
  return (
    event.key === "Escape" ||
    event.key === "Backspace" ||
    (event.key === "ArrowLeft" &&
      (onlyAltKeyPressed(event) ||
        onlyMetaKeyPressed(event) ||
        noModifierKeysPressed(event))) ||
    (event.key === "ArrowRight" &&
      (onlyAltKeyPressed(event) ||
        onlyMetaKeyPressed(event) ||
        noModifierKeysPressed(event))) ||
    isNonControlCharacter(event)
  );
}

function onlyAltKeyPressed(event) {
  return event.altKey && !event.ctrlKey && !event.metaKey && !event.shiftKey;
}

function onlyMetaKeyPressed(event) {
  return !event.altKey && !event.ctrlKey && event.metaKey && !event.shiftKey;
}

function noModifierKeysPressed(event) {
  return !event.altKey && !event.ctrlKey && !event.metaKey && !event.shiftKey;
}

// <https://stackoverflow.com/questions/12467240/determine-if-javascript-e-keycode-is-a-printable-non-control-character/12467610>
function isNonControlCharacter(event) {
  return event.key.length === 1;
}

const app = Elm.Main.init({
  node: document.querySelector("main")
});

const svgElement = document.querySelector("svg");

const graphNodeEditors = {};
let numMutations = 0;
let keydownEventListener = null;

const svgTextElementAddedObserver = new MutationObserver(mutations => {
  mutations.forEach(mutation => {
    console.log(`[JS] mutation observed (${numMutations})`, mutation);
    numMutations++;

    if (mutation.type === "childList") {
      mutation.removedNodes.forEach(node => {
        if (isGraphNodeElement(node)) {
          console.log(`[JS] removing graph node: ${node.dataset.graphNodeId}`);
          delete graphNodeEditors[node.dataset.graphNodeId];
        }
      });

      mutation.addedNodes.forEach(node => {
        console.log("node", node);

        if (isGraphNodeElement(node)) {
          console.log(
            `[JS] adding graph node editor: ${node.dataset.graphNodeId}`
          );
          const graphNodeEditor = new GraphNodeEditor(
            svgElement,
            node.dataset.graphNodeId,
            node.querySelector("[data-id='graph-node-editor']")
          );
          graphNodeEditors[node.dataset.graphNodeId] = graphNodeEditor;
          graphNodeEditor.fireInitEvent();
        }
      });
    }
  });
});

svgTextElementAddedObserver.observe(svgElement, {
  childList: true,
  subtree: true
});

app.ports.startListeningForNodeEditorKeyEvent.subscribe(graphNodeId => {
  console.log("[JS] start listening for graph node key event");

  if (keydownEventListener != null) {
    throw new Error("Something is already listening to keydown.");
  }

  keydownEventListener = event => {
    const graphNodeEditor = graphNodeEditors[graphNodeId];

    if (graphNodeEditor == null) {
      throw new Error(
        `[startListeningForNodeEditorKeyEvent] ` +
          `Can't find graph node with node id: ${graphNodeId}`
      );
    }

    if (event.metaKey && event.key === "r") {
      window.reload();
    }

    if (isInterestingKeyEvent(event)) {
      event.preventDefault();
    }

    graphNodeEditor.fireKeyEvent(event);
  };

  document.addEventListener("keydown", keydownEventListener);
});

app.ports.stopListeningForNodeEditorKeyEvent.subscribe(() => {
  console.log("[JS] stop listening for node editor key event");

  if (keydownEventListener == null) {
    throw new Error("There doesn't seem to be anything listening for keydown.");
  }

  document.removeEventListener("keydown", keydownEventListener);

  keydownEventListener = null;
});

// TODO: Rename this to calculateNodeEditorMetrics??
/*
app.ports.calculateGraphNodeContentMetrics.subscribe(change => {
  console.log(
    "[JS] receiving request to calculateGraphNodeContentMetrics",
    change
  );

  const graphNodeEditor = findGraphNodeEditorBy(change.graphNodeId);

  graphNodeEditor.calculateMetrics(change);
});
*/

app.ports.updateGraphNodeEditorSelection.subscribe(selectionUpdate => {
  console.log(
    "[JS] receiving request to updateNodeEditorSelection",
    selectionUpdate
  );

  const graphNodeEditor = findGraphNodeEditorBy(selectionUpdate.graphNodeId);

  if (selectionUpdate.type === "UpdateCursorPositionFromMouse") {
    graphNodeEditor.updateCursorPositionFromMouse(selectionUpdate.at);
  } else if (selectionUpdate.type === "UpdateCursorPositionFromIndex") {
    graphNodeEditor.updateCursorPositionFromIndex(selectionUpdate.at);
  } else if (selectionUpdate.type === "UpdateSelectionFromMouse") {
    graphNodeEditor.updateSelectionFromMouse(
      selectionUpdate.from,
      selectionUpdate.to
    );
    /*
  } else if (selectionUpdate.type === "EndSelection") {
    graphNodeEditor.endSelection();
  */
  } else {
    throw new Error(`Invalid update type ${selectionUpdate.type}!`);
  }
});
