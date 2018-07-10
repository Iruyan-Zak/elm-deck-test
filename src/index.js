require('./main.scss');

const Elm = require('./Main.elm');
const mountNode = document.getElementById('main');

const app = Elm.Main.embed(mountNode);


function getKeys() {
  keys = []
  for (var i = 0; i < localStorage.length; i++) {
    keys.push(localStorage.key(i));
  }
  return keys;
}

app.ports.getDeckReq.subscribe(function(key) {
  const obj = JSON.parse(localStorage.getItem(key));
  app.ports.getDeckRes.send(obj);
});

app.ports.setDeckReq.subscribe(function(args) {
  const [key, value] = args;
  localStorage.setItem(key, JSON.stringify(value));

  app.ports.getDeckNamesRes.send(getKeys());
});

app.ports.removeDeckReq.subscribe(function(key) {
  localStorage.removeItem(key);

  app.ports.getDeckNamesRes.send(getKeys());
});

app.ports.getDeckNamesReq.subscribe(function() {
  app.ports.getDeckNamesRes.send(getKeys());
});
