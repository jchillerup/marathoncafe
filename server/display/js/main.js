// establish a connection to the scoreboard server

$(function() {
    var scores = new Backbone.Model();
    var jerseys = new Backbone.Model();
    var socket = io.connect();
    
    var scoreboardview = new Scoreboard({ el: document.getElementById("scoreboard"), model: scores });
    
//    var loggerview = new Logger({model: scores});
    //    $('#logContainer').append(loggerview.$el);
    
    var plotsview = new PlotsView({el: document.getElementById('plots')});

    socket.on('state', function (data) {
        scores.set(data.cur_scores);
        jerseys.set(data.jerseys);
    });
    
    jerseys.on('change', function() {
        console.log('NEW JERSEYS WHAT THE FUCK');
    });

    socket.on('reload', function(data) {
        window.location.reload();
    });
    
});
