import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

var dfltModel = {
  "errorMsg": "",
  "password": "",
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
console.log('DBG murali init')
elmApp.ports.setStorage.subscribe(function (state) {
  console.log('DBG murali setStorage sub')
  localStorage.setItem('model', JSON.stringify(state));
  localStorage.setItem('user-id', state["username"])
  localStorage.setItem('access-token', state["token"])
});

elmApp.ports.removeStorage.subscribe(function () {

  console.log('DBG murali removeStorage sub')
  localStorage.removeItem('model');
});
// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
