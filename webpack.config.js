const path = require("path");
const webpack = require("webpack");
const HtmlWebpackPlugin = require('html-webpack-plugin');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CopyWebpackPlugin = require('copy-webpack-plugin');

const babelConf = {
  presets: [ 
    ["@babel/preset-env", {
      "modules":false,
      "corejs": 3,
      "useBuiltIns": "usage"
    }]
]}


function resolve(filePath) {
    return path.join(__dirname, filePath)
}

var isProduction = process.argv.indexOf("-p") >= 0;
console.log("Bundling for " + (isProduction ? "production" : "development") + "...");

var commonPlugins = [
    new HtmlWebpackPlugin({
        filename: resolve('./output/index.html'),
        template: resolve('./src/index.html')
    })
];

module.exports = {
    devtool: isProduction ? undefined : "source-map",
    entry: isProduction ? // We don't use the same entry for dev and production, to make HMR over style quicker for dev env
        {
            demo: [
                resolve('./src/BMath.fsproj'),
                resolve('./sass/main.sass')
            ]
        } : {
            app: [
                resolve('./src/BMath.fsproj')
            ],
            style: [
                resolve('./sass/main.sass')
            ]
        },
    mode: isProduction ? "production" : "development",
    output: {
        path: resolve('./output'),
        filename: isProduction ? '[name].[hash].js' : '[name].js'
    },
    plugins: isProduction ?
        commonPlugins.concat([
            new MiniCssExtractPlugin({
                filename: 'style.css'
            }),
            new CopyWebpackPlugin([
                { from: './public' }
            ])
        ])
        : commonPlugins.concat([
            new webpack.HotModuleReplacementPlugin(),
            new webpack.NamedModulesPlugin()
        ]),
    resolve: {
        modules: [
            "node_modules", 
            resolve("./node_modules/")
        ]
    },
    devServer: {
        contentBase: resolve('./public'),
        publicPath: "/",
        port: 8080,
        hot: true,
        inline: true
    },
    module: {
        rules: [
            {
              test: /\.fs(x|proj)?$/,
              use: {
                loader: "fable-loader",
                options: {
                  babel: babelConf,
                  define: isProduction ? [] : ["DEBUG"],
                  extra: { optimizeWatch: true }
                }
              }
            },
            {
              test: /\.js$/,
              include: path.resolve(__dirname, 'split'),
              exclude: /(node_modules|build)/,
              use: {
                loader: 'babel-loader',
                options: {
                  babel: babelConf
                }
              }
            },
            {
                test: /\.s?[ac]ss$/,
                use: [
                    isProduction ? MiniCssExtractPlugin.loader : 'style-loader',
                    'css-loader',
                    'sass-loader',
                ],
            }
        ]
    }
};
