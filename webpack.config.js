const path = require('path');
const MiniCssExtractPlugin = require("mini-css-extract-plugin");

module.exports = {
	entry: './www/base.mjs',
	plugins: [
		new MiniCssExtractPlugin({filename: "base.css"}),
	],
	module: {
		rules: [
			{
	 				test: /\.html$/i,
	 				loader: 'html-loader',
	 		},
 			{
 				test: /\.css$/i,
 				use: [MiniCssExtractPlugin.loader, "css-loader"],
 			}
		]
	},
	output: {
	    filename: 'base.js',
        library: 'Base',
	    path: path.resolve(__dirname, 'www/'),
	}
};
