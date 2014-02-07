var http = require('http');
var url = require('url');
var qs = require('querystring');
var connect = require('connect');
var sys = require('sys');
var exec = require('child_process').exec;

var app = connect.createServer(connect.static('display'));
var server = http.createServer(app).listen(8080);
var io = require('socket.io').listen(server, {log:false});
var fs = require('fs');

var sqlite3 = require('sqlite3');
var db = new sqlite3.Database('db.sqlite');

var first = true;

var state = { cur_scores: { 'GL1': 0, 'GL2': 0, 'GL3': 0, 'GL4': 0, 'GL5': 0, 'GL6': 0, 'GL7': 0, 'GL8': 0, 'ML2': 0, 'ML3': 0, 'ML4': 0, 'ML5': 0, 'ML6': 0, 'ML7': 0, 'ML8': 0, 'NY2': 0, 'NY3': 0, 'NY4': 0, 'NY5': 0, 'NY6': 0, 'NY7': 0, 'NY8': 0 }, jerseys: {yellow: null, green: null, dotted: null}}; 

// load state from sqlite
db.serialize(function() { 
    var buffer = "";
    
    db.each('SELECT * FROM streger ORDER BY timestamp', function(err, row) {
        if (first) {
            for (var kitchen in state.cur_scores) {
                // 1391781601
                
                buffer += kitchen+";1;"+Math.round(row.timestamp/1000)+"\n";
                buffer += kitchen+";1;"+Math.round(row.timestamp/1000)+"\n";
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
        if (first) {
            for (var kitchen in state.cur_scores) {
                // 1391781601
                
                buffer += kitchen+";1;"+ Math.round((new Date).getTime()/1000) + "\n";
                buffer += kitchen+";1;"+ Math.round((new Date).getTime()/1000) + "\n";
            }

            first = false;
        }
        

        var quantity = parseFloat(data.quantity);
        
        // update stored state
        // db.run('INSERT INTO streger VALUES (\''+data.kitchen+'\', '+quantity+', '+(new Date).getTime()+')');
        db.run('INSERT INTO streger VALUES (?,?,?)', [data.kitchen, quantity, (new Date).getTime()]);
        
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


setInterval(function() {
    console.log("Runing R");
    exec("R --vanilla < marathoncafe.R", function(error, stdout, stderr) {
        console.log('done');
    });
    
}, 30000);