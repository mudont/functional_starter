import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var dfltModel = {
  "errorMsg": "",
  "password": "",
  "email": "",
  "protectedQuote": "",
  "quote": "",
  "token": "",
  "username": ""
};
var userId = localStorage.getItem('user-id');
var accessToken = localStorage.getItem('access-token');
var storedState = localStorage.getItem('model');

var startingState = storedState ? JSON.parse(storedState) : dfltModel;
if (userId && !startingState['username']) { startingState["username"] = userId };
if (accessToken && !startingState['token']) { startingState["token"] = accessToken };

console.log('DBG model initialized')

var elmApp = Elm.Main.init({
  node: document.getElementById('root'),
  flags: startingState,
});
console.log('DBG Elm initialized')

// -- LOCAL STORAGE
// Setup subscriptions to listen to the Elm App's requests
// to set/remove localStorage
elmApp.ports.setStorage.subscribe(function (state) {
  console.log('DBG setStorage called by Elm App. Setting...')
  localStorage.setItem('model', JSON.stringify(state));
  localStorage.setItem('user-id', state["username"])
  localStorage.setItem('access-token', state["token"])
});
elmApp.ports.removeStorage.subscribe(function () {
  console.log('DBG murali removeStorage called by Elm App. Removing')
  localStorage.removeItem('model');
  localStorage.removeItem('user-id')
  localStorage.removeItem('access-token')
});

// -- WEBSOCKETS

// Create  WebSocket client.
console.log("DBG Connecting to echo.websocket.org")
var socket = new WebSocket('wss://echo.websocket.org');

// When a command goes to the `sendMessage` port from the Elm app, 
// we pass the message along to the WebSocket.
elmApp.ports.sendWsMessage.subscribe(function (message) {
  socket.send(message);
});

// When a message comes into our WebSocket, we pass the message along
// to the `messageReceiver` port to the Elm App.
socket.addEventListener("message", function (event) {
  elmApp.ports.wsMessageReceiver.send(event.data);
});


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
