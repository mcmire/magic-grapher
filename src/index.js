import "./index.css";

import { Elm } from "./Main.elm";

class GraphNode {
  constructor(root, nodeId, element) {
    this.root = root;
    this.nodeId = nodeId;
    this.element = element;
    this.editorText = new GraphNodeEditorText(
      root,
      nodeId,
      element.querySelector('[data-id="editor-text"]')
    );

    console.log("[JS] graph node initialized!");
  }

  fireEditorInitEvent() {
    this.editorText.fireInitEvent();
  }

  fireEditorKeyEvent(originalEvent) {
    this.editorText.fireKeyEvent(originalEvent);
  }

  determineEditorCursorIndex(mousePosition) {
    this.editorText.determineCursorIndex(mousePosition);
  }

  calculateEditorMetrics(request) {
    this.editorText.calculateMetrics(request);
  }
}

class GraphNodeEditorText {
  constructor(root, nodeId, element) {
    this.root = root;
    this.nodeId = nodeId;
    this.element = element;
  }

  fireInitEvent(originalEvent) {
    console.log("[JS] dispatching init event");
    const event = new CustomEvent("init");
    this.element.dispatchEvent(event);
  }

  fireKeyEvent(originalEvent) {
    console.log("[JS] dispatching key event");
    const event = new CustomEvent("key", { detail: originalEvent });
    this.element.dispatchEvent(event);
  }

  determineCursorIndex(mousePosition) {
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
    const cursorIndex = this.element.getCharNumAtPosition(point);

    this.calculateMetrics({ cursorIndex, text: this.element.textContent });
  }

  calculateMetrics(request) {
    const { text } = request;
    console.log("[JS] calculating metrics!", request);

    // Update the text node inside of the <text> element
    // Don't use innerHTML or else it replaces the text node and Elm will get
    //   very confused!
    this.element.childNodes[0].nodeValue = this._normalizeTextForSvgElement(
      text
    );
    const bbox = this.element.getBBox();

    const cursorIndex = this._normalizeCursorIndex({
      cursorIndex: request.cursorIndex,
      text
    });
    const cursorPosition = this._determineCursorPosition({
      cursorIndex,
      text,
      bbox
    });

    const detail = {
      nodeId: parseInt(this.nodeId, 10),
      width: bbox.width,
      height: bbox.height,
      cursorPosition: cursorPosition,
      text: request.text
    };

    console.log("[JS] dispatching metricsRecalculated", detail);
    // TODO: Convert this to a port?
    const event = new CustomEvent("metricsRecalculated", { detail });
    this.element.dispatchEvent(event);
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

function findGraphNodeBy(id) {
  const graphNode = graphNodes[id];

  if (graphNode == null) {
    throw new Error(`Can't find graph node with node id: ${id}`);
  }

  return graphNode;
}

function isGraphNodeElement(node) {
  return (
    node.dataset != null &&
    node.dataset.id === "graph-node" &&
    node.dataset.nodeId != null
  );
}

function isGraphNodeTextElement(node) {
  return (
    node.dataset != null &&
    node.dataset.id === "graph-node" &&
    node.dataset.nodeId != null
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

const graphNodes = {};
let numMutations = 0;
let keydownEventListener = null;

const svgTextElementAddedObserver = new MutationObserver(mutations => {
  mutations.forEach(mutation => {
    console.log(`[JS] mutation observed (${numMutations})`, mutation);
    numMutations++;

    if (mutation.type === "childList") {
      mutation.removedNodes.forEach(node => {
        if (isGraphNodeElement(node)) {
          console.log(`[JS] removing graph node: ${node.dataset.nodeId}`);
          delete graphNodes[node.dataset.nodeId];
        }
      });

      mutation.addedNodes.forEach(node => {
        console.log("node", node);

        if (isGraphNodeElement(node)) {
          console.log(`[JS] adding graph node: ${node.dataset.nodeId}`);
          const graphNode = new GraphNode(
            svgElement,
            node.dataset.nodeId,
            node
          );
          graphNodes[node.dataset.nodeId] = graphNode;
          graphNode.fireEditorInitEvent();
        }
      });
    }
  });
});

svgTextElementAddedObserver.observe(svgElement, {
  childList: true,
  subtree: true
});

app.ports.startListeningForNodeEditorKeyEvent.subscribe(nodeId => {
  console.log("[JS] start listening for graph node key event");

  if (keydownEventListener != null) {
    throw new Error("Something is already listening to keydown.");
  }

  keydownEventListener = event => {
    const graphNode = graphNodes[nodeId];

    if (graphNode == null) {
      throw new Error(
        `[startListeningForNodeEditorKeyEvent] ` +
          `Can't find graph node with node id: ${nodeId}`
      );
    }

    if (event.metaKey && event.key === "r") {
      window.reload();
    }

    if (isInterestingKeyEvent(event)) {
      event.preventDefault();
    }

    graphNode.fireEditorKeyEvent(event);
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

app.ports.calculateNodeContentMetrics.subscribe(change => {
  console.log("[JS] receiving request to calculateNodeContentMetrics", change);

  const graphNode = findGraphNodeBy(change.nodeId);

  graphNode.calculateEditorMetrics(change);
});

app.ports.determineNodeContentCursorIndex.subscribe(request => {
  console.log(
    "[JS] receiving request to determineNodeContentCursorIndex",
    request.nodeId,
    request.mousePosition.x,
    request.mousePosition.y
  );

  const graphNode = findGraphNodeBy(request.nodeId);

  graphNode.determineEditorCursorIndex(request.mousePosition);
});
