import { Elm } from "./Main.elm";

import "reset.css";
import "./index.css";
import "./styles/main.css";
import "./styles/node-content.css";

class NodeEditor {
  constructor(nodeId, element) {
    this.nodeId = nodeId;
    this.element = element;
    this.textElement = element.querySelector('[data-id="text"]');
    console.log("[JS] node editor initialized!");
  }

  calculateMetrics(request) {
    const { text } = request;
    console.log("[JS] calculating metrics!", request);

    // Update the text node inside of the <text> element
    // Don't use innerHTML or else it replaces the text node and Elm will get
    //   very confused!
    this.textElement.childNodes[0].nodeValue = this._normalizeTextForSvgElement(
      text
    );
    //console.log("numberOfChars", this.textElement.getNumberOfChars());
    const bbox = this.textElement.getBBox();

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
    this.textElement.dispatchEvent(event);
  }

  fireKeyEvent(originalEvent) {
    const event = new CustomEvent("key", { detail: originalEvent });
    this.textElement.dispatchEvent(event);
  }

  _normalizeTextForSvgElement(text) {
    if (text[text.length - 1] === " ") {
      // HTML/SVG will strip trailing spaces, which affects how we figure out
      // the positions of each character in the node
      return text.slice(0, -1) + " ";
    } else {
      return text;
    }
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
    // character. Usually it refers to the position before a character (so -1 is
    // a valid index) but if it's the last index in the string then it refers to
    // the position after the character.
    //
    // See this for more on getStartPositionOfChar and getEndPositionOfChar:
    //
    //   https://www.w3.org/TR/SVG2/text.html#TextSelectionImplementationNotes

    console.log("cursorIndex", cursorIndex, "text.length", text.length);

    let cursorPosition;

    if (text === "") {
      cursorPosition = bbox.x;
    } else if (cursorIndex === text.length) {
      cursorPosition = this.textElement.getEndPositionOfChar(cursorIndex - 1).x;
    } else {
      cursorPosition = this.textElement.getStartPositionOfChar(cursorIndex).x;
    }

    if (isNaN(cursorPosition)) {
      throw new Error("cursorPosition is not a number!");
    }

    return cursorPosition;
  }
}

function isNodeEditorElement(node) {
  return (
    node.dataset != null &&
    node.dataset.id === "node-editor" &&
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
    event.key.length === 1
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

const app = Elm.Main.init({
  node: document.querySelector("main")
});

const root = document.querySelector("[data-id='root']");

const nodeEditors = {};
let numMutations = 0;
let keydownEventListener = null;

const svgTextElementAddedObserver = new MutationObserver(mutations => {
  mutations.forEach(mutation => {
    console.log(`[JS] mutation observed (${numMutations})`, mutation);
    numMutations++;

    if (mutation.type === "childList") {
      mutation.addedNodes.forEach(node => {
        if (isNodeEditorElement(node)) {
          console.log(`[JS] adding node editor: ${node.dataset.nodeId}`);
          nodeEditors[node.dataset.nodeId] = new NodeEditor(
            node.dataset.nodeId,
            node
          );
        }
      });
      mutation.removedNodes.forEach(node => {
        if (
          isNodeEditorElement(node) &&
          !isNodeEditorElement(mutation.nextSibling)
        ) {
          console.log("[JS] removing node editor");
          delete nodeEditors[node.dataset.nodeId];
        }
      });
    } else if (mutation.type === "characterData") {
      // mutation.target is a text node, go up to the text ELEMENT
      const element = mutation.target.parentElement;

      if (element === this) {
        this._dispatch("change");
      }
    }
  });
});

svgTextElementAddedObserver.observe(root, {
  //characterDataOldValue: true,
  childList: true,
  subtree: true
});

app.ports.calculateNodeContentMetrics.subscribe(change => {
  console.log("[JS] receiving request to calculateNodeContentMetrics");

  const nodeEditor = nodeEditors[change.nodeId];

  if (nodeEditor == null) {
    throw new Error(`Can't find node editor with node id: ${change.nodeId}`);
  }

  nodeEditor.calculateMetrics(change);
});

app.ports.startListeningForNodeEditorKeyEvent.subscribe(nodeId => {
  console.log("[JS] start listening for node editor key event");

  const nodeEditor = nodeEditors[nodeId];

  if (nodeEditor == null) {
    //throw new Error(`Can't find node editor with node id: ${nodeId}`);
    // node editor may not be set yet -- wait a bit
  }

  if (keydownEventListener != null) {
    throw new Error("Something is already listening to keydown.");
  }

  keydownEventListener = event => {
    if (event.metaKey && event.key === "r") {
      window.reload();
    }

    if (isInterestingKeyEvent(event)) {
      event.preventDefault();
    }

    nodeEditor.fireKeyEvent(event);
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
