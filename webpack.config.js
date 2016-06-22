var webpack = require('webpack');

module.exports = {
  context: __dirname + '/js',
  entry: {
    'home'      : './home.jsx',
    'game-index': './game-index.jsx',
    'new-game'  : './new-game.jsx',
    'game'      : './game.jsx',
  },
  output: {
    path: __dirname + '/static/js',
    filename: '[name].js'
  },
  resolve: {
    extensions: ['', '.js', '.jsx']
  },
  devtool: 'inline-source-map',
  plugins: [],
  module: {
    loaders: [
      {
        test: /\.jsx$/,
        exclude: /node_modules/,
        loader: 'babel',
        query: {
          cacheDirectory: true,
          presets: ['es2015', 'react']
        }
      },
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: 'babel',
        query: {
          cacheDirectory: true,
          presets: ['es2015']
        }
      },
      {
        test: /\.html$/,
        loader: 'file?name=[name].[ext]'
      }
    ]
  }
};
