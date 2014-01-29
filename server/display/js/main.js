// establish a connection to the scoreboard server

$(function() {
    var state = new Backbone.Model();
    var socket = io.connect();
    
    var scoreboardview = new Scoreboard({ el: document.getElementById("scoreboard"), model: state });

    socket.on('state', function (data) {
        state.set(data.cur_scores);
    });
    
});
