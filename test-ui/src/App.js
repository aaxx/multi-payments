import React from 'react';
import MuiThemeProvider     from 'material-ui/styles/MuiThemeProvider';
import TextField            from 'material-ui/TextField';
import Dialog               from 'material-ui/Dialog';
import FlatButton           from 'material-ui/FlatButton';
import './App.css';

export default class App extends React.Component {
  render() {
    return (
      <MuiThemeProvider>
        <div className="App">Hello!</div>
      </MuiThemeProvider>
    );
  }
}
