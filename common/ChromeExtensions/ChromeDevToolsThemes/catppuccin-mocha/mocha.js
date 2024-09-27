let url = chrome.runtime.getURL("mocha.css");

fetch(url).then(r => r.text()).then(css => chrome.devtools.panels.applyStyleSheet(css));
