var scoreboard = require('scoreboard');
var redis = require('redis');
var http = require('http');
var url = require('url');
var connect = require('connect');


var server = http.createServer(connect.createServer(connect.static('display/'))).listen(80);
var io = require('socket.io').listen(server);




scoreboard.redis.createClient = function() {
    var client = redis.createClient(6379, 'localhost');
    client.auth();
    return client;
};

var Score = scoreboard.Score;
var scores = new Score();
var state = {cur_scores: {}};

function partial( fn /*, args...*/) {
  var aps = Array.prototype.slice,
    args = aps.call( arguments, 1 );
  
  return function() {
    return fn.apply( this, args.concat( aps.call( arguments ) ) );
  };
}



scores.leader({keys: ['streger']}).run(function(err, leaders) {
    for (var kitchen_idx in leaders) {
        var kitchen = leaders[kitchen_idx];
        var cur_scores = {};
        scores.client.zscore('streger', kitchen, (partial(function(kitchen, err, score) {
            
            state.cur_scores[kitchen] = parseInt(score);

        }, kitchen)));
    }
});


// http server
http.createServer(function (req, res) {
    res.writeHead(200, {'Content-Type': 'application/json'});
    
    var queryData = url.parse(req.url, true).query;
    switch(queryData.mode) {
        
    case "streg":
        var quantity = parseInt(queryData.quantity);
        
        // update stored state
        scores.index('streger', quantity, queryData.kitchen);
        
        // update local state
        state.cur_scores[queryData.kitchen] += quantity;
        
        // tell listeners about this
        io.sockets.emit('state', state);

        res.end(JSON.stringify({status: "OK"}));

    default:
        res.end(JSON.stringify(state));
        break;
    }
}).listen(8081, '0.0.0.0');

// socket.io
io.sockets.on('connection', function (socket) {
    socket.emit('state', state);
});