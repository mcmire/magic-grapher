import "./index.css";

import { Elm } from "./Main.elm";

// TODO: Convert to TypeScript

class GraphNodeEditor {
  constructor(root, graphNodeId, element) {
    this.root = root;
    this.graphNodeId = graphNodeId;
    this.element = element;

    this.updateQueue = [];
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

  enqueueUpdate(update) {
    this.updateQueue.push(update);
  }

  runUpdateQueue() {
    let update;

    console.log("running number of updates", this.updateQueue.length);

    while ((update = this.updateQueue.shift()) != null) {
      if (update.type === "UpdateCursorPositionFromMouse") {
        this._updateCursorPositionFromMouse(update.at);
      } else if (update.type === "UpdateCursorPositionFromIndex") {
        this._updateCursorPositionFromIndex(update.at, update.text);
      } else if (update.type === "UpdateSelectionFromMouse") {
        this._updateSelectionFromMouse(update.from, update.to);
        /*
      } else if (update.type === "EndSelection") {
        graphNodeEditor.endSelection();
      */
      } else {
        throw new Error(`Invalid update type ${update.type}!`);
      }
    }
  }

  _updateCursorPositionFromMouse(mousePosition) {
    const text = this.element.textContent;
    const cursorIndex = this._findCursorIndexAt(mousePosition, text);

    this._calculateMetrics({
      cursor: cursorIndex,
      text: text
    });
  }

  _updateCursorPositionFromIndex(cursorIndex, text) {
    this._calculateMetrics({
      cursor: cursorIndex,
      text: text
    });
  }

  _updateSelectionFromMouse(fromCursorIndex, toMousePosition) {
    const text = this.element.textContent;
    const toCursorIndex = this._findCursorIndexAt(toMousePosition, text);

    this._calculateMetrics({
      selection: { start: fromCursorIndex, end: toCursorIndex },
      text: text
    });
  }

  _findCursorIndexAt(mousePosition, text) {
    const absoluteBbox = this.element.getBoundingClientRect();
    console.log("absoluteBbox", absoluteBbox);
    const relativeBbox = this.element.getBBox();
    console.log("relativeBbox", relativeBbox);
    const center = {
      x: absoluteBbox.x - relativeBbox.x,
      y: absoluteBbox.y - relativeBbox.y
    };

    const normalizedMousePosition = this._normalizePosition(
      mousePosition,
      absoluteBbox
    );
    console.log(
      "normalizing mouse position to",
      normalizedMousePosition.x,
      normalizedMousePosition.y
    );

    const relativizedNormalizedMousePosition = this._relativizePosition(
      normalizedMousePosition,
      center
    );

    console.log(
      "normalizing + relativizing mouse position to",
      relativizedNormalizedMousePosition.x,
      relativizedNormalizedMousePosition.y
    );

    if (relativizedNormalizedMousePosition.x === relativeBbox.x) {
      return -1;
    } else if (
      relativizedNormalizedMousePosition.x ===
      relativeBbox.x + relativeBbox.width
    ) {
      return text.length - 1;
    } else {
      const point = this.root.createSVGPoint();
      point.x = relativizedNormalizedMousePosition.x;
      point.y = relativizedNormalizedMousePosition.y;
      return this.element.getCharNumAtPosition(point);
    }
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

  // request
  // = { cursor : Int, text : String }
  // | { selection : { start : Int, end : Int }, text : String }
  _calculateMetrics({ cursor, selection, text }) {
    console.log(
      "[JS] calculating metrics!",
      "cursor",
      cursor,
      "selection",
      selection,
      "text",
      text
    );

    const normalizedText = this._normalizeTextForSvgElement(text);

    // Update the text node inside of the <text> element
    // Don't use innerHTML or else it replaces the text node and Elm will get
    //   very confused!
    this.element.childNodes[0].nodeValue = normalizedText;
    console.log("text is now", normalizedText);
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

  _normalizeTextForSvgElement(text) {
    // HTML/SVG will strip leading and trailing spaces, which affects how we
    // figure out the positions of each character in the node
    return text.replace(/^[ ]/, " ").replace(/[ ]$/, " ");
  }

  // cursorIndex always refers to the position AFTER a character. This gets
  // tricky because that means in to represent the cursor at the very
  // beginning of the string, the index needs to be -1.
  _normalizeCursorIndex({ cursorIndex, text }) {
    if (cursorIndex < -1) {
      return -1;
    } else if (cursorIndex > text.length - 1) {
      return text.length - 1;
    } else {
      return cursorIndex;
    }
  }

  // cursorIndex always refers to the position AFTER a character. This gets
  // tricky because that means in to represent the cursor at the very
  // beginning of the string, the index needs to be -1.
  //
  // getStartPositionOfChar and getEndPositionOfChar are documented here:
  //
  //   https://www.w3.org/TR/SVG2/text.html#TextSelectionImplementationNotes
  //
  _determineCursorPosition({ cursorIndex, text, bbox }) {
    let cursorPosition;

    if (text === "") {
      cursorPosition = bbox.x;
    } else if (cursorIndex === -1) {
      cursorPosition = this.element.getStartPositionOfChar(cursorIndex + 1).x;
    } else {
      cursorPosition = this.element.getEndPositionOfChar(cursorIndex).x;
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

function isGraphNodeEditorElement(node) {
  return (
    node.dataset != null &&
    node.dataset.id === "graph-node-editor" &&
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
      mutation.addedNodes.forEach(node => {
        //console.log("node", node);

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
        } else if (isGraphNodeEditorElement(node)) {
          console.log(
            `[JS] updating graph node editor: ${node.dataset.graphNodeId}`
          );
          const graphNodeEditor = findGraphNodeEditorBy(
            node.dataset.graphNodeId
          );
          graphNodeEditor.element = node;
        }
      });
    }
    /*
    else if (mutation.type === "characterData") {
      const possibleGraphNodeElement =
        mutation.target.parentNode.parentNode.parentNode;

      if (isGraphNodeElement(possibleGraphNodeElement)) {
        const graphNodeEditor = findGraphNodeEditorBy(
          possibleGraphNodeElement.dataset.graphNodeId
        );

        graphNodeEditor.runUpdateQueue();
      }
    }
    */
  });
});

svgTextElementAddedObserver.observe(svgElement, {
  childList: true,
  subtree: true,
  characterData: true
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

  graphNodeEditor._calculateMetrics(change);
});
*/

app.ports.updateGraphNodeEditorSelection.subscribe(update => {
  console.log("[JS] receiving request to updateNodeEditorSelection", update);

  const graphNodeEditor = findGraphNodeEditorBy(update.graphNodeId);

  if (update.type == null) {
    throw new Error(
      `Not sure what to do with selectionUpdate.type = ${update.type}!`
    );
  }

  // TODO: Put these back together
  graphNodeEditor.enqueueUpdate(update);
  graphNodeEditor.runUpdateQueue();
});
