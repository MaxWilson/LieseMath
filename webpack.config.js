var path = require("path");
var webpack = require("webpack");

var cfg = {
  entry: [
    "./temp/source/main.js"
  ],
  output: {
    path: path.join(__dirname, "publish"),
    filename: "bundle.js"
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env': {
        'NODE_ENV': JSON.stringify('production')
      }
    })
  ],
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: "source-map-loader"
      }
    ]
  }
};

module.exports = cfg;