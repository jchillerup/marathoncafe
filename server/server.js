var http = require('http');
var url = require('url');
var qs = require('querystring');
var sys = require('sys');
var exec = require('child_process').exec;

var express = require('express');
var app = express();

app.use(express.static('display'));

var io = require('socket.io').listen(app.listen(8080));
var fs = require('fs');

var sqlite3 = require('sqlite3');
var db = new sqlite3.Database('db.sqlite');

var first = true;

var state = { cur_scores: { 
    '1A': 0,
    '2A': 0,
    '3A': 0,
    '4A': 0,
    '5A': 0,
    '6A': 0,
    '7A': 0,
    '1B': 0,
    '2B': 0,
    '3B': 0,
    '4B': 0,
    '5B': 0,
    '6B': 0,
    '7B': 0,
    '1C': 0,
    '2C': 0,
    '3C': 0,
    '4C': 0,
    '5C': 0,
    '6C': 0,
    '7C': 0,
    '1D': 0,
    '2D': 0,
    '3D': 0,
    '4D': 0,
    '5D': 0,
    '6D': 0,
    '7D': 0
}, jerseys: {yellow: null, green: null, dotted: null}}; 

// load state from sqlite
db.serialize(function() { 
    var buffer = "";
    
    db.each('SELECT * FROM streger ORDER BY timestamp', function(err, row) {
        if (first) {
            for (var kitchen in state.cur_scores) {
                buffer += kitchen+";0.25;"+Math.round(row.timestamp/1000)+"\n";
                buffer += kitchen+";0.25;"+Math.round(row.timestamp/1000)+"\n";
            }

            first = false;
        }
        buffer += row.kitchen + ";" + row.quantity + ";" + Math.round(row.timestamp/1000) + "\n";
    }, function() {
        console.log('writing file');
        fs.writeFileSync("data.csv", buffer);
    });
    
    db.each('SELECT kitchen, SUM(quantity) count FROM streger GROUP BY kitchen;', function(err, row) {
        state.cur_scores[row.kitchen] = row.count;
    
        // TODO: set jerseys here.
    });
});

// socket.io
io.sockets.on('connection', function (socket) {
    socket.emit('state', state);
    
    socket.on('streg', function(data) {
        var sender = socket.handshake.address;

        if (first) {
            buffer = "";
            for (var kitchen in state.cur_scores) {
                // 1391781601
                
                buffer += kitchen+";0;"+ Math.round((new Date).getTime()/1000) + "\n";
                buffer += kitchen+";0;"+ Math.round((new Date).getTime()/1000) + "\n";
            }
            
            fs.writeFileSync("data.csv", buffer);
            first = false;
        }
        
        var quantity = parseFloat(data.quantity);
        
        // update stored state
        db.run('INSERT INTO streger VALUES (?,?,?,?)', [data.kitchen, quantity, (new Date).getTime(), sender]);
        
        // Add to the CSV file
        var buffer = data.kitchen + ";" + data.quantity + ";" + Math.round((new Date).getTime()/1000) + "\n";
        fs.appendFileSync('data.csv', buffer);
        
        // update local state
        state.cur_scores[data.kitchen] += quantity;        

        // tell listeners about this
        io.sockets.emit('state', state);
    });
});


// http admin interface
http.createServer(function(req, res) {
    res.writeHead(200, {'Content-Type': 'text/plain'});
    
    var parsed = url.parse(req.url);
    var query = qs.parse(parsed['query']);

    switch(query['mode']) {
    case "reload":
        io.sockets.emit('reload');
        break;
        
    case "jersey":
        state.jerseys['yellow'] = query['yellow'];
        state.jerseys['green'] = query['green'];
        state.jerseys['dotted'] = query['dotted'];

        io.sockets.emit('state', state);

        break;
    default:
        console.log('Did not understand this on the MGMT interface: ' + JSON.stringify(query));
    }

    res.end(req.url);

}).listen(8081);



function runR() {
    console.log("Running R");
    exec("R --vanilla < marathoncafe.R", function(error, stdout, stderr) {
        setTimeout(runR, 5000);
    });

}

runR();