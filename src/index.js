import { Elm } from "./Main.elm";

import "reset.css";
import "./index.css";
import "./styles/main.css";

class NodeEditor {
  constructor(nodeId, element) {
    this.nodeId = nodeId;
    this.element = element;
    this.visibleTextElement = element.querySelector('[data-id="text-visible"]');
    this.hiddenTextElement = element.querySelector('[data-id="text-hidden"]');
    console.log("[JS] node editor initialized!");
  }

  calculateMetrics(request) {
    console.log("[JS] calculating metrics!", request);

    // Update the text node inside of the <text> element
    // Don't use innerHTML or else it replaces the text node and Elm will get
    //   very confused!
    this.visibleTextElement.childNodes[0].nodeValue = request.text;

    const bbox = this.visibleTextElement.getBBox();
    // Source: <https://www.w3.org/TR/SVG2/text.html#TextSelectionImplementationNotes>
    const cursorPosition =
      request.text === "" ?
        bbox.x : (
          request.cursorIndex === -1 ?
            this.visibleTextElement.getStartPositionOfChar(request.cursorIndex + 1).x :
            this.visibleTextElement.getEndPositionOfChar(request.cursorIndex).x
        );

    if (isNaN(cursorPosition)) {
      throw new Error("cursorPosition is not a number!");
    }

    const detail = {
      nodeId: parseInt(this.nodeId, 10),
      width: bbox.width,
      height: bbox.height,
      cursorPosition: cursorPosition,
      text: request.text
    };

    console.log("[JS] dispatching metricsRecalculated", detail);
    const event = new CustomEvent("metricsRecalculated", { detail });
    this.visibleTextElement.dispatchEvent(event);
  }
}

function isNodeEditorElement(node) {
  return node.dataset != null &&
    node.dataset.id === "node-editor" &&
    node.dataset.nodeId != null;
}

const app = Elm.Main.init({
  node: document.querySelector("main")
});

const root = document.querySelector("[data-id='root']");

const nodeEditors = {};
let numMutations = 0;

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
        if (isNodeEditorElement(node) && !isNodeEditorElement(mutation.nextSibling)) {
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
