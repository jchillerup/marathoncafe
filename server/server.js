var http = require('http');
var url = require('url');
var connect = require('connect');

var app = connect.createServer(connect.static('display'));
var server = http.createServer(app).listen(8080);
var io = require('socket.io').listen(server, {log:false});


var sqlite3 = require('sqlite3');
var db = new sqlite3.Database('db.sqlite');

var state = { cur_scores: { 'GL1': 0, 'GL2': 0, 'GL3': 0, 'GL4': 0, 'GL5': 0, 'GL6': 0, 'GL7': 0, 'GL8': 0, 'ML2': 0, 'ML3': 0, 'ML4': 0, 'ML5': 0, 'ML6': 0, 'ML7': 0, 'ML8': 0, 'NY2': 0, 'NY3': 0, 'NY4': 0, 'NY5': 0, 'NY6': 0, 'NY7': 0, 'NY8': 0 } }; 

// load state from sqlite
db.serialize(function() { 
    db.each('SELECT kitchen, SUM(quantity) count FROM streger GROUP BY kitchen;', function(err, row) {
        state.cur_scores[row.kitchen] = row.count;
    });
});

// socket.io
io.sockets.on('connection', function (socket) {
    socket.emit('state', state);

    
    socket.on('streg', function(data) {
        var quantity = parseFloat(data.quantity);
        
        // update stored state
        // db.run('INSERT INTO streger VALUES (\''+data.kitchen+'\', '+quantity+', '+(new Date).getTime()+')');
        db.run('INSERT INTO streger VALUES (?,?,?)', [data.kitchen, quantity, (new Date).getTime()]);

        // update local state
        state.cur_scores[data.kitchen] += quantity;
        
        // tell listeners about this
        io.sockets.emit('state', state);
    });
});


// http admin interface
http.createServer(function(req, res) {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    
    if (req.url === "/reload") {
        io.sockets.emit('reload');
    }
    
    if (req.url === "/server_close") {
        .exit;
    }

    res.end(req.url);

}).listen(8081);