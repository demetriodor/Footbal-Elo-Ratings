// scrape_elos_1910.js

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = 'elos_1910.html'

page.open('http://eloratings.net/1910', function (status) {
  var content = page.content;
  fs.write(path,content,'w')
  phantom.exit();
});