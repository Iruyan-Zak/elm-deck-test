require('./main.scss');

const Elm = require('./Main.elm');
const mountNode = document.getElementById('main');

const app = Elm.Main.embed(mountNode);


app.ports.getDeckReq.subscribe(function(key) {
  const obj = JSON.parse(localStorage.getItem(key));
  app.ports.getDeckRes.send(obj);
});

app.ports.setDeckReq.subscribe(function(args) {
  const [key, value] = args;
  console.log(value);
  localStorage.setItem(key, JSON.stringify(value));
  app.ports.setDeckRes.send(null);
});
