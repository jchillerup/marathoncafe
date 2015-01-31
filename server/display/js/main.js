// establish a connection to the scoreboard server

$(function() {
    var scores = new Backbone.Model();
    var jerseys = new Backbone.Model();
    var socket = io.connect();
    
    var scoreboardview = new Scoreboard({ el: document.getElementById("scoreboard"), model: scores, jerseymodel: jerseys });
    
    var loggerview = new Logger({model: scores});
    $('#logContainer').append(loggerview.$el);
    
    var plotsview = new PlotsView({el: document.getElementById('plots')});

    socket.on('state', function (data) {
        scores.set(data.cur_scores);
        jerseys.set(data.jerseys);
    });
    
    // We're updating the yellow jersey more frequently than the others
    jerseys.on('change', function() {
        $('.green').removeClass('green');
        $('.dotted').removeClass('dotted');

        $("#score-" + jerseys.get('green')).addClass('green');
        $("#score-" + jerseys.get('dotted')).addClass('dotted');
    });

    socket.on('reload', function(data) {
        window.location.reload();
    });
    
});
