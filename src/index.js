import { Elm } from "./Main.elm";

import "reset.css";
import "./index.css";
import "./styles/main.css";

const app = Elm.Main.init({
  node: document.querySelector("main")
});

function isSvgTextElement(element) {
  return element.tagName === "text" && element.dataset.nodeId != null;
}

const svgTextElementAddedObserver = new MutationObserver(mutations => {
  mutations.forEach(mutation => {
    /*
    if (mutation.type == "childList") {
      mutation.addedNodes.forEach(element => {
        if (isSvgTextElement(element)) {
          console.log("svg text element added!");
          const event = {
            nodeId: element.dataset.nodeId,
            bbox: element.getBBox()
          };
          app.ports.onSvgTextElementAdded.send(event);
        }
      })
    } else
    */
    if (mutation.type == "characterData") {
      const element = mutation.target.parentElement;

      if (isSvgTextElement(element)) {
        console.log("svg text element changed!");
        const event = {
          nodeId: parseInt(element.dataset.nodeId, 10),
          bbox: element.getBBox()
        };
        app.ports.onSvgTextElementAdded.send(event);
      }
    }
  });
});

svgTextElementAddedObserver.observe(
  document.querySelector("[data-id='root']"),
  {
    characterDataOldValue: true,
    childList: true,
    subtree: true
  }
);
